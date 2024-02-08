use std::collections::{BTreeSet,BTreeMap};
use std::fmt;

use std::sync::Mutex;
use std::marker::PhantomData;
use std::ops::{Deref,DerefMut};
use std::ptr::{self, NonNull};
use std::mem::{self, ManuallyDrop};
use std::alloc::{self, Layout};
use std::cell::Cell;
use std::num::NonZeroI64;

use cranelift::codegen::ir::{UserFuncName,FuncRef};
use cranelift::prelude::*;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module, FuncId};

use anyhow::{anyhow,bail,Result};
use once_cell::sync::Lazy;

const TRACE_LEVEL: i64 = 1;
const JIT_LEVEL:   i64 = 5;

extern "C" fn rust_print_form(x: Form) {
    println!("from jit print: {x}");
}
extern "C" fn rust_cons(car: Form, cdr: Form) -> Form {
    Form::new_pair(car, cdr)
}
extern "C" fn rust_eq(a: Form, b: Form) -> Form {
    Form::new_bool(ManuallyDrop::new(a) == ManuallyDrop::new(b))
}
extern "C" fn rust_cvec_form_grow(ptr: &mut Cvec<Form>) {
    ptr.grow()
}
extern "C" fn rust_drop_rc_form(f: Form) {
}

const TRACE_ID_OFFSET:   usize = 32;
const TRACE_OFFSET_MASK: usize = 0xFF_FF_FF_FF; // could be bigger
// https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/jit/examples/jit-minimal.rs
pub struct JIT {
    module: JITModule,
    ctx: codegen::Context,
    func_ctx: FunctionBuilderContext,
    int: Type,
    compiled: BTreeMap<ID, (FuncId,FuncId)>,
}
impl JIT {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap(); //?
        flag_builder.set("is_pic", "false").unwrap();
        flag_builder.set("preserve_frame_pointers", "true").unwrap(); // needed for Tail CallConv
        //println!("{:?}", flag_builder.iter().collect::<Vec<_>>());
        //flag_builder.set("tail", "true").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
        let mut jb = JITBuilder::with_isa(isa, default_libcall_names());
        jb.symbol("rust_print_form", rust_print_form as *const u8);
        jb.symbol("rust_cons", rust_cons as *const u8);
        jb.symbol("rust_eq", rust_eq as *const u8);
        jb.symbol("rust_cvec_form_grow", rust_cvec_form_grow as *const u8);
        jb.symbol("rust_drop_rc_form", rust_drop_rc_form as *const u8);
        let mut module = JITModule::new(jb);
        let int = module.target_config().pointer_type();
        let mut ctx = module.make_context();
        let mut func_ctx = FunctionBuilderContext::new();
        
        Self { module, ctx, func_ctx, int, compiled: BTreeMap::new() }
    }
    fn comple_trace(&mut self, id: ID, ops: &Vec<Op>) -> extern "C" fn(&mut Form, &mut Cvec<Form>, &mut Cvec<RetStackEntry>) -> usize {
        let mut sig_a = self.module.make_signature();
        sig_a.call_conv = isa::CallConv::Tail;
        sig_a.params.push(AbiParam::new(self.int));
        sig_a.params.push(AbiParam::new(self.int));
        sig_a.params.push(AbiParam::new(self.int));
        sig_a.returns.push(AbiParam::new(self.int));
        let func_a = self.module.declare_function(&format!("{id}_inner"), Linkage::Local, &sig_a).unwrap();

        let mut sig_b = self.module.make_signature();
        sig_b.params.push(AbiParam::new(self.int));
        sig_b.params.push(AbiParam::new(self.int));
        sig_b.params.push(AbiParam::new(self.int));
        sig_b.returns.push(AbiParam::new(self.int));
        let func_b = self.module.declare_function(&format!("{id}_outer"), Linkage::Local, &sig_b).unwrap();

        self.ctx.func.signature = sig_a;
        self.ctx.func.name = UserFuncName::user(0, func_a.as_u32());
        {
            let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut self.ctx.func, &mut self.func_ctx);
            let block = bcx.create_block();
            bcx.switch_to_block(block);
            bcx.append_block_params_for_function_params(block);
            let params = bcx.block_params(block).iter().cloned().collect::<Vec<_>>();

            let e_ptr = params[0];
            let tmp_stack_ptr = params[1]; //pub struct Cvec<T> { ptr: NonNull<T>, cap: usize, len: usize, }
            let ret_stack_ptr = params[2];

            let mut print_sig = self.module.make_signature();
            print_sig.params.push(AbiParam::new(self.int));
            let print_func = self.module.declare_function("rust_print_form", Linkage::Import, &print_sig).unwrap();
            let local_print_func = self.module.declare_func_in_func(print_func, bcx.func);

            let mut cons_sig = self.module.make_signature();
            cons_sig.params.push(AbiParam::new(self.int));
            cons_sig.params.push(AbiParam::new(self.int));
            cons_sig.returns.push(AbiParam::new(self.int));
            let cons_func = self.module.declare_function("rust_cons", Linkage::Import, &cons_sig).unwrap();
            let local_cons_func = self.module.declare_func_in_func(cons_func, bcx.func);

            let mut eq_sig = self.module.make_signature();
            eq_sig.params.push(AbiParam::new(self.int));
            eq_sig.params.push(AbiParam::new(self.int));
            eq_sig.returns.push(AbiParam::new(self.int));
            let eq_func = self.module.declare_function("rust_eq", Linkage::Import, &eq_sig).unwrap();
            let local_eq_func = self.module.declare_func_in_func(eq_func, bcx.func);

            let mut grow_cvec_form_sig = self.module.make_signature();
            grow_cvec_form_sig.params.push(AbiParam::new(self.int));
            let grow_cvec_form_func = self.module.declare_function("rust_cvec_form_grow", Linkage::Import, &grow_cvec_form_sig).unwrap();
            let local_cvec_form_grow_func = self.module.declare_func_in_func(grow_cvec_form_func, bcx.func);


            let mut drop_sig = self.module.make_signature();
            drop_sig.params.push(AbiParam::new(self.int));
            let drop_rc_form_func = self.module.declare_function("rust_drop_rc_form", Linkage::Import, &drop_sig).unwrap();
            let local_drop_rc_form_func = self.module.declare_func_in_func(drop_rc_form_func, bcx.func);

            //let call = bcx.ins().call(local_callee, &[add, add]);
            //bcx.inst_results(call)[0]

            fn type_assert(bcx: &mut FunctionBuilder, x: Value, tag: usize, local_print_func: FuncRef, int: Type) {
                let t = bcx.ins().band_imm(x, TAG_MASK as i64);
                let ok = bcx.ins().icmp_imm(IntCC::Equal, t, tag as i64);
                bcx.ins().trapz(ok, TrapCode::User(0));
            }
            fn stack_pop(bcx: &mut FunctionBuilder, int: Type, tmp_stack_ptr: Value, local_print_func: FuncRef, peek_only: bool) -> Value {
                let len = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_LEN_OFFSET as i32);
                let new_len = bcx.ins().iadd_imm(len, -1);
                if !peek_only {
                    bcx.ins().store(MemFlags::trusted(), new_len, tmp_stack_ptr, CVEC_LEN_OFFSET as i32);
                }
                let ptr = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_PTR_OFFSET as i32);
                let offset = bcx.ins().ishl_imm(new_len, 3);
                let item_ptr = bcx.ins().iadd(ptr, offset);
                let r = bcx.ins().load(int, MemFlags::trusted(), item_ptr, 0);
                r
            }
            fn stack_pop_prim(bcx: &mut FunctionBuilder, int: Type, tmp_stack_ptr: Value, local_print_func: FuncRef) -> Value {
                let loaded = stack_pop(bcx, int, tmp_stack_ptr, local_print_func, false);
                type_assert(bcx, loaded, TAG_PRIM, local_print_func, int);
                loaded
            }
            fn stack_push(bcx: &mut FunctionBuilder, int: Type, tmp_stack_ptr: Value, x: Value, local_cvec_form_grow_func: FuncRef, local_print_func: FuncRef) {
                let len = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_LEN_OFFSET as i32);
                let new_len = bcx.ins().iadd_imm(len, 1);
                let cap = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_CAP_OFFSET as i32);

                let grow = bcx.ins().icmp(IntCC::SignedGreaterThanOrEqual, new_len, cap);

                let grow_block = bcx.create_block();
                let merge_block = bcx.create_block();

                bcx.ins().brif(grow, grow_block, &[], merge_block, &[]);
                bcx.switch_to_block(grow_block);
                bcx.ins().call(local_cvec_form_grow_func, &[tmp_stack_ptr]);
                bcx.ins().jump(merge_block, &[]);
                bcx.switch_to_block(merge_block);
                stack_push_nocap(bcx, int, tmp_stack_ptr, x, local_print_func);
            }
            fn stack_push_nocap(bcx: &mut FunctionBuilder, int: Type, tmp_stack_ptr: Value, x: Value, local_print_func: FuncRef) {
                let len = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_LEN_OFFSET as i32);
                let new_len = bcx.ins().iadd_imm(len, 1);
                bcx.ins().store(MemFlags::trusted(), new_len, tmp_stack_ptr, CVEC_LEN_OFFSET as i32);
                let ptr = bcx.ins().load(int, MemFlags::trusted(), tmp_stack_ptr, CVEC_PTR_OFFSET as i32);
                let offset = bcx.ins().ishl_imm(len, 3);
                let item_ptr = bcx.ins().iadd(ptr, offset);
                bcx.ins().store(MemFlags::trusted(), x, item_ptr, 0);
            }

            // DOESN'T RC
            fn gen_eq(bcx: &mut FunctionBuilder, int: Type, a: Value, b: Value, local_eq_func: FuncRef, local_print_func: FuncRef) -> Value {
                let at = bcx.ins().band_imm(a, TAG_RC as i64);
                let bt = bcx.ins().band_imm(b, TAG_RC as i64);
                let tt = bcx.ins().band(at, bt);
                let rc = bcx.ins().icmp_imm(IntCC::Equal, tt, TAG_RC as i64);


                let const_block = bcx.create_block();
                let dyn_block = bcx.create_block();
                let merge_block = bcx.create_block();
                bcx.append_block_param(merge_block, int);
                bcx.ins().brif(rc, dyn_block, &[], const_block, &[]);

                bcx.switch_to_block(const_block);
                let e1 = bcx.ins().icmp(IntCC::Equal, a, b);
                let e = bcx.ins().sextend(int, e1);
                let r = bcx.ins().bor_imm(e, TAG_BOOL_FALSE as i64); //onst TAG_BOOL_FALSE: usize = 0b010; onst TAG_BOOL_TRUE:  usize = 0b011;
                bcx.ins().jump(merge_block, &[r]);

                bcx.switch_to_block(dyn_block);
                let call = bcx.ins().call(local_eq_func, &[a, b]);
                let r = bcx.inst_results(call)[0].clone();
                bcx.ins().jump(merge_block, &[r]);

                bcx.switch_to_block(merge_block);
                let merge_result = bcx.block_params(merge_block)[0].clone();
                merge_result
            }

            fn increment(bcx: &mut FunctionBuilder, int: Type, x: Value, local_print_func: FuncRef) {
                let tt = bcx.ins().band_imm(x, TAG_RC as i64);
                let rc = bcx.ins().icmp_imm(IntCC::Equal, tt, TAG_RC as i64);

                let incr_block = bcx.create_block();
                let merge_block = bcx.create_block();
                bcx.ins().brif(rc, incr_block, &[], merge_block, &[]);
                bcx.switch_to_block(incr_block);
                increment_unchecked(bcx, int, x, local_print_func);
                bcx.ins().jump(merge_block, &[]);

                bcx.switch_to_block(merge_block);
            }
            fn increment_unchecked(bcx: &mut FunctionBuilder, int: Type, x: Value, local_print_func: FuncRef) {
                let crc_inner_ptr = bcx.ins().band_imm(x, (!TAG_MASK) as i64);
                let count = bcx.ins().load(int, MemFlags::trusted(), crc_inner_ptr, CRC_INNER_RC_OFFSET as i32);
                let new_count = bcx.ins().iadd_imm(count, 1);
                bcx.ins().store(MemFlags::trusted(), new_count, crc_inner_ptr, CRC_INNER_RC_OFFSET as i32);
            }
            fn decrement(bcx: &mut FunctionBuilder, int: Type, x: Value, local_drop_rc_form_func: FuncRef, local_print_func: FuncRef) {
                let tt = bcx.ins().band_imm(x, TAG_RC as i64);
                let rc = bcx.ins().icmp_imm(IntCC::Equal, tt, TAG_RC as i64);

                let decr_block = bcx.create_block();
                let merge_block = bcx.create_block();
                bcx.ins().brif(rc, decr_block, &[], merge_block, &[]);
                bcx.switch_to_block(decr_block);
                decrement_unchecked(bcx, int, x, local_drop_rc_form_func, local_print_func);
                bcx.ins().jump(merge_block, &[]);

                bcx.switch_to_block(merge_block);
            }
            fn decrement_unchecked(bcx: &mut FunctionBuilder, int: Type, x: Value, local_drop_rc_form_func: FuncRef, local_print_func: FuncRef) {
                // if it's actually 0, we need to go back into rust to hit the free/drop
                let crc_inner_ptr = bcx.ins().band_imm(x, (!TAG_MASK) as i64);
                let count = bcx.ins().load(int, MemFlags::trusted(), crc_inner_ptr, CRC_INNER_RC_OFFSET as i32);

                let live_block = bcx.create_block();
                let dead_block = bcx.create_block();
                let merge_block = bcx.create_block();

                let rc_1 = bcx.ins().icmp_imm(IntCC::Equal, count, 1 as i64);
                bcx.ins().brif(rc_1, dead_block, &[], live_block, &[]);

                bcx.switch_to_block(live_block);
                let new_count = bcx.ins().iadd_imm(count, -1);
                bcx.ins().store(MemFlags::trusted(), new_count, crc_inner_ptr, CRC_INNER_RC_OFFSET as i32);
                bcx.ins().jump(merge_block, &[]);

                bcx.switch_to_block(dead_block);
                let call = bcx.ins().call(local_drop_rc_form_func, &[x]);
                bcx.ins().jump(merge_block, &[]);

                bcx.switch_to_block(merge_block);
            }

            fn gen_cr_unchecked_norc(bcx: &mut FunctionBuilder, int: Type, x: Value, is_cdr: bool) -> Value {
                let crc_inner_ptr = bcx.ins().band_imm(x, (!TAG_MASK) as i64);
                bcx.ins().load(int, MemFlags::trusted(), crc_inner_ptr, (CRC_INNER_DATA_OFFSET + if is_cdr { FORM_PAIR_CDR_OFFSET } else { FORM_PAIR_CAR_OFFSET }) as i32)
            }

            fn gen_cr(bcx: &mut FunctionBuilder, int: Type, x: Value, is_cdr: bool, local_drop_rc_form_func: FuncRef, local_print_func: FuncRef) -> Value {
                type_assert(bcx, x, TAG_PAIR, local_print_func, int);
                // need to RC increment whatever we get
                // and RC decrement this pair
                let result = gen_cr_unchecked_norc(bcx, int, x, is_cdr);
                increment(bcx, int, result, local_print_func);
                decrement_unchecked(bcx, int, x, local_drop_rc_form_func, local_print_func);
                result
                //const CRC_INNER_DATA_OFFSET:   usize = 8*1;
                //const FORM_PAIR_CAR_OFFSET:   usize = 8*0;
                //Ok(unsafe { &(*((self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>)).data.car })
                //Ok(unsafe { &(*((self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>)).data.cdr })
                //#[repr(C)]
                //pub struct CrcInner<T> {
                //    rc: Cell<usize>,
                //    data: T,
                //}
                //#[repr(C)]
                //struct FormPair {
                //    car: Form,
                //    cdr: Form,
                //}
            }
            let mut offset = 0;
            for op in ops {
                match op {
                    /*
                    Op::Guard       { const_value, side_val, side_cont, side_id, tbk } => {
                        println!("Guard(op) {const_value}");
                        if const_value != tmp_stack.last().unwrap() {
                            if self.traces.contains_key(side_id) {
                                if side_val.is_some() {
                                    tmp_stack.pop().unwrap();
                                }
                                println!("\tchaining trace to side trace");
                                id = *side_id;
                                offset = 0;
                                break; // break out of this trace and let infinate loop spin
                            } else {
                                println!("\tending playback b/c failed guard");
                                assert!(self.tracing.is_none());
                                let mut ntrace = Trace::follow_on(*side_id,tbk.clone());
                                if let Some(side_val) = side_val {
                                    *tmp_stack.last_mut().unwrap() = side_val.clone();
                                    *ntrace.tbk.stack_const.last_mut().unwrap() = false; // this might be able to be
                                                                                         // more precise, actually
                                }
                                self.tracing = Some(ntrace);
                                return Ok(Some((tmp_stack.pop().unwrap(), e, (**side_cont).clone())));
                            }
                        }
                    }
                    Op::Debug                                                          => {
                        println!("Debug(op) {}", tmp_stack.last().unwrap());
                    }
                    Op::Call        { len, nc, nc_id, statik }                    => {
                        println!("Call(op)");
                        if let Some(static_call_id) = statik {
                            if self.traces.contains_key(static_call_id) {
                                ret_stack.push((e.clone(), (*nc).clone(), Some(*nc_id)));
                                println!("\tchaining to call trace b/c Call with statik");
                                id = *static_call_id;
                                offset = 0;
                                break; // break out of this trace and let infinate loop spin
                            }
                        }
                        let func = &tmp_stack[tmp_stack.len()-*len];
                        match func.data as usize & TAG_MASK {
                            TAG_PRIM => {
                                let p = func.prim().unwrap();
                                let b = tmp_stack.pop().unwrap();
                                let a = if *len == 2 { None } else { assert!(*len == 3); Some(tmp_stack.pop().unwrap()) };
                                let result = eval_prim(p, b, a)?;
                                if self.traces.contains_key(nc_id) {
                                    *tmp_stack.last_mut().unwrap() = result; // for the prim itself
                                    println!("\tchaining to ret trace b/c Call with dyamic but primitive and next traced");
                                    id = *nc_id;
                                    offset = 0;
                                    break; // break out of this trace and let infinate loop spin
                                } else {
                                    println!("\tstopping playback to ret b/c Call with dyamic but primitive and next not-traced");
                                    tmp_stack.pop().unwrap(); // for the prim itself
                                    return Ok(Some((result, e, (**nc).clone())));
                                }
                            },
                            TAG_CLOSURE => {
                                let Closure { params: ps, e: ie, body: b, id: call_id, } = func.closure().unwrap();
                                if ps.len() != *len-1 {
                                    bail!("arguments length doesn't match");
                                }
                                ret_stack.push((e.clone(), (*nc).clone(), Some(*nc_id)));
                                if self.traces.contains_key(call_id) {
                                    println!("\tchaining to call trace b/c Call with dyamic but traced");
                                    e = ie.clone();
                                    id = *call_id;
                                    offset = 0;
                                    break; // break out of this trace and let infinate loop spin
                                } else {
                                    return Ok(Some((b.clone(), ie.clone(), Cont::Frame { syms: ps.clone(), id: *call_id, c: Crc::new(Cont::Eval { c: Crc::new(Cont::Ret { id: *call_id }) }) })));
                                }
                            },
                            ncomb => {
                                println!("Current stack is {tmp_stack:?}");
                                bail!("tried to call a non-comb {ncomb}")
                            },
                        }
                    }
                    Op::Tail(_len,_oid)                                                  => {
                        println!("Tail(op)");
                        // Huh, this actually has to know how many envs we pushed on so we can pop
                        // them off
                        unimplemented!();
                    }
                    Op::Return                                                         => {
                        // we should pop ret_stack, and if trace id isn't 0, we should try to jump
                        // to it from some sort of id/func table (also set e, and decrement crc<cont> when popping).
                        // If not, we need to return out to the interpreter.
                        println!("Return(op)");
                        let (e, nc, resume_data) = ret_stack.pop().unwrap();
                        if let Some(resume_id) = resume_data {
                            if self.traces.contains_key(&resume_id) {
                                id = resume_id;
                                offset = 0;
                                break; // break out of this trace and let infinate loop spin
                            }
                        }
                        println!("\tending playback b/c Return, attempting to resume trace");
                        self.try_resume_trace(resume_data);
                        return Ok(Some((tmp_stack.pop().unwrap(), e, (*nc).clone())));
                    }
                    */
                    Op::InlinePrim(p)                                               => {
                        /*
                        let b = tmp_stack.pop().unwrap();
                        let a = tmp_stack.pop().unwrap();
                        tmp_stack.pop().unwrap(); // pop the prim
                        tmp_stack.push(Form::new_int(a.int()? + b.int()?));
                        let cst = bcx.ins().iconst(self.int, 1337 << 3);
                        let _ = bcx.ins().call(local_print_func, &[cst]);
                        */
                        let b = stack_pop(&mut bcx, self.int, tmp_stack_ptr, local_print_func, false);
                        let a = if p.two_params() { Some(stack_pop(&mut bcx, self.int, tmp_stack_ptr, local_print_func, false)) } else { None };
                        let result = match p {
                            Prim::Car  => { gen_cr(&mut bcx, self.int, b, false, local_drop_rc_form_func, local_print_func) },
                            Prim::Cdr  => { gen_cr(&mut bcx, self.int, b, true,  local_drop_rc_form_func, local_print_func) },
                            Prim::Add | Prim::Sub | Prim::Mul | Prim::Div | Prim::Mod => {
                                let a = a.unwrap();
                                type_assert(&mut bcx, a, TAG_INT, local_print_func, self.int);
                                type_assert(&mut bcx, b, TAG_INT, local_print_func, self.int);
                                match p {
                                    Prim::Add  => { bcx.ins().iadd(a, b) }
                                    Prim::Sub  => { bcx.ins().isub(a, b) }
                                    Prim::Mul  => { bcx.ins().imul(a, b) }
                                    Prim::Div  => { bcx.ins().sdiv(a, b) }
                                    Prim::Mod  => { bcx.ins().srem(a, b) }
                                    _ => unreachable!(),
                                }
                            },
                            Prim::Eq   => {
                                // eq doesn't RC
                                let a = a.unwrap();
                                let r = gen_eq(&mut bcx, self.int, a, b, local_eq_func, local_print_func);
                                decrement(&mut bcx, self.int, a, local_drop_rc_form_func, local_print_func);
                                decrement(&mut bcx, self.int, b, local_drop_rc_form_func, local_print_func);
                                r
                            },
                            Prim::Cons => { let call = bcx.ins().call(local_cons_func, &[a.unwrap(), b]); bcx.inst_results(call)[0].clone() },
                        };
                        let _ = stack_pop_prim(&mut bcx, self.int, tmp_stack_ptr, local_print_func);
                        stack_push_nocap(&mut bcx, self.int, tmp_stack_ptr, result, local_print_func); // we just popped, so cap is fine
                        offset += 1;
                    }
                    Op::Define      { sym }                                            => {
                        let s = bcx.ins().iconst(self.int, unsafe { *((&mut Form::new_symbol(sym)) as *mut Form as *mut usize) } as i64);
                        let v = stack_pop(&mut bcx, self.int, tmp_stack_ptr, local_print_func, false);

                        let e = bcx.ins().load(self.int, MemFlags::trusted(), e_ptr, 0);
                        let kv = {
                            let call = bcx.ins().call(local_cons_func, &[s, v]);
                            bcx.inst_results(call)[0].clone()
                        };
                        let new_e = {
                            let call = bcx.ins().call(local_cons_func, &[kv, e]);
                            bcx.inst_results(call)[0].clone()
                        };
                        bcx.ins().store(MemFlags::trusted(), new_e, e_ptr, 0);
                        offset += 1;

                        //pub fn define(&self, s: &str, v: Form) -> Form {
                        //    Form::new_pair(Form::new_pair(Form::new_symbol(s), v), self.clone())
                        //}
                        //let v = tmp_stack.pop().unwrap();
                        //println!("Define(op) {sym} = {}", v);
                        //e = e.define(sym, v);
                    }
                    Op::Drop                                                           => {
                        let v = stack_pop(&mut bcx, self.int, tmp_stack_ptr, local_print_func, false);
                        decrement(&mut bcx, self.int, v, local_drop_rc_form_func, local_print_func);
                        offset += 1;
                        //println!("Drop(op) {}", tmp_stack.last().unwrap());
                        //tmp_stack.pop().unwrap();
                    }
                    Op::Const       ( con )                                            => {
                        println!("doing a const");
                        con.increment();
                        println!("incr");
                        let con_rc = con.is_rc();
                        let con = bcx.ins().iconst(self.int, unsafe { *(con as *const Form as *const usize) } as i64);
                        if con_rc {
                            increment_unchecked(&mut bcx, self.int, con, local_print_func);
                        }
                        stack_push(&mut bcx, self.int, tmp_stack_ptr, con, local_cvec_form_grow_func, local_print_func);
                        offset += 1;
                        /*
                        println!("Const(op) {con}");
                        tmp_stack.push(con.clone());
                        */
                    }
                    Op::Lookup      { sym }                                            => {
                        let e = bcx.ins().load(self.int, MemFlags::trusted(), e_ptr, 0);

                        let header_block = bcx.create_block();
                        bcx.append_block_param(header_block, self.int);

                        let body_block = bcx.create_block();
                        bcx.append_block_param(body_block, self.int);

                        let return_block = bcx.create_block();
                        bcx.append_block_param(return_block, self.int);

                        let error_block = bcx.create_block();

                        bcx.ins().jump(header_block, &[e]);
                        bcx.switch_to_block(header_block);
                        let e = bcx.block_params(header_block)[0].clone();

                        let is_nil = bcx.ins().icmp_imm(IntCC::Equal, e, TAG_NIL as i64);
                        bcx.ins().brif(is_nil, error_block, &[], body_block, &[e]);

                        bcx.switch_to_block(body_block);
                        let e = bcx.block_params(body_block)[0].clone();
                        let kv = gen_cr_unchecked_norc(&mut bcx, self.int, e,  false);
                        let k  = gen_cr_unchecked_norc(&mut bcx, self.int, kv, false);
                        let ne = gen_cr_unchecked_norc(&mut bcx, self.int, e,  true);
                        let is_sym = bcx.ins().icmp_imm(IntCC::Equal, k, unsafe { *((&mut Form::new_symbol(sym)) as *mut Form as *mut usize) } as i64);
                        bcx.ins().brif(is_sym, return_block, &[kv], header_block, &[ne]);

                        bcx.switch_to_block(error_block);
                        let cst = bcx.ins().iconst(self.int, (id.id.get() << TRACE_ID_OFFSET) | offset as i64);
                        bcx.ins().return_(&[cst]);

                        bcx.switch_to_block(return_block);
                        let kv = bcx.block_params(return_block)[0].clone();
                        let v  = gen_cr_unchecked_norc(&mut bcx, self.int, kv, true);
                        increment(&mut bcx, self.int, v, local_print_func);
                        stack_push(&mut bcx, self.int, tmp_stack_ptr, v, local_cvec_form_grow_func, local_print_func);
                        offset += 1;
                    }
                    Op::Guard       { const_value, side_val, side_cont, side_id, tbk } => {
                        let pass_block = bcx.create_block();
                        let fail_block = bcx.create_block();

                        const_value.increment();
                        let const_value = bcx.ins().iconst(self.int, unsafe { *(const_value as *const Form as *const usize) } as i64);
                        // NOTE THIS IS A PEEK NOT A POP
                        let v = stack_pop(&mut bcx, self.int, tmp_stack_ptr, local_print_func, true);
                        let eq_val = gen_eq(&mut bcx, self.int, v, const_value, local_eq_func, local_print_func); // doesn't RC
                        let eq = bcx.ins().icmp_imm(IntCC::Equal, eq_val, TAG_BOOL_TRUE as i64);
                        bcx.ins().brif(eq, pass_block, &[], fail_block, &[]);
                        bcx.switch_to_block(fail_block);

                        // for now, failed guards drop back to interpreter
                        let cst = bcx.ins().iconst(self.int, (id.id.get() << TRACE_ID_OFFSET) | offset as i64);
                        bcx.ins().return_(&[cst]);
                        bcx.switch_to_block(pass_block);
                        offset += 1;

                        /*
                        println!("Guard(op) {const_value}");
                        if const_value != tmp_stack.last().unwrap() {
                            if self.traces.contains_key(side_id) {
                                if side_val.is_some() {
                                    tmp_stack.pop().unwrap();
                                }
                                println!("\tchaining trace to side trace");
                                id = *side_id;
                                offset = 0;
                                break; // break out of this trace and let infinate loop spin
                            } else {
                                println!("\tending playback b/c failed guard");
                                assert!(self.tracing.is_none());
                                let mut ntrace = Trace::follow_on(*side_id,tbk.clone());
                                if let Some(side_val) = side_val {
                                    *tmp_stack.last_mut().unwrap() = side_val.clone();
                                    *ntrace.tbk.stack_const.last_mut().unwrap() = false; // this might be able to be
                                                                                         // more precise, actually
                                }
                                self.tracing = Some(ntrace);
                                return Ok(Some((tmp_stack.pop().unwrap(), e, (**side_cont).clone())));
                            }
                        }
                        */
                    }
                    _ => {
                        // quit out of trace!
                        println!("compiling back to interp because we can't compile {op:?}");
                        assert!(offset <= TRACE_OFFSET_MASK);
                        let cst = bcx.ins().iconst(self.int, (id.id.get() << TRACE_ID_OFFSET) | offset as i64);
                        bcx.ins().return_(&[cst]);
                        break;
                    }
                }
            }
            //[
            //  Define { sym: "n" },
            //  Define { sym: "faft_h" },
            //  Drop,
            //  Const(Form(Eq)),
            //  Lookup { sym: "n" },
            //  Const(Form(1)),
            //  InlinePrim(Eq),
            //  Guard { const_value: Form(false), side_val: Some(Form(('debug 1))), side_cont: Eval {..}, side_id: ID { id: 3 },  },
            //  Drop,
            //  Const(Form(Add)),
            //  Lookup { sym: "n" },
            //  Lookup { sym: "faft_h" },
            //  Lookup { sym: "faft_h" },
            //  Const(Form(Sub)),
            //  Lookup { sym: "n" },
            //  Const(Form(1)),
            //  InlinePrim(Sub),
            //  Call { len: 3, statik: None, nc: Call { n: 3, to_go: Form(nil), c: Ret { id: ID { id: 1 } } }, nc_id: ID { id: 4 } }
            //],

            bcx.seal_all_blocks();
            bcx.finalize();
        }
        println!("{:?}", self.ctx.func);
        self.module.define_function(func_a, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);

        self.ctx.func.signature = sig_b;
        self.ctx.func.name = UserFuncName::user(0, func_b.as_u32());
        {
            let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut self.ctx.func, &mut self.func_ctx);
            let block = bcx.create_block();
            bcx.switch_to_block(block);
            bcx.append_block_params_for_function_params(block);
            let local_func = self.module.declare_func_in_func(func_a, &mut bcx.func);
            let params = bcx.block_params(block).iter().cloned().collect::<Vec<_>>();
            let call = bcx.ins().call(local_func, &params);
            let value = {
                let results = bcx.inst_results(call);
                assert_eq!(results.len(), 1);
                results[0].clone()
            };
            bcx.ins().return_(&[value]);
            bcx.seal_all_blocks();
            bcx.finalize();
        }

        self.module.define_function(func_b, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);

        // perform linking
        self.module.finalize_definitions().unwrap();

        let code_b = self.module.get_finalized_function(func_b);
        let ptr_b = unsafe { mem::transmute::<_, _>(code_b) };
        self.compiled.insert(id, (func_a, func_b));
        ptr_b
    }
    // returns the id for the inner and the pointer for the outer
}

#[repr(C)]
pub struct Cvec<T> {
    ptr: NonNull<T>,
    cap: usize,
    len: usize,
}
const CVEC_PTR_OFFSET:   usize = 8*0;
const CVEC_CAP_OFFSET:   usize = 8*1;
const CVEC_LEN_OFFSET:   usize = 8*2;
unsafe impl<T: Send> Send for Cvec<T> {}
unsafe impl<T: Sync> Sync for Cvec<T> {}
impl<T> Cvec<T> {
    pub fn new() -> Self {
        assert!(mem::size_of::<T>() != 0, "no ZST");
        Cvec {
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
        }
    }
    fn grow(&mut self) {
        let (new_cap, new_layout) = if self.cap == 0 {
            (1, Layout::array::<T>(1).unwrap())
        } else {
            let new_cap = 2 * self.cap;
            let new_layout = Layout::array::<T>(new_cap).unwrap();
            (new_cap, new_layout)
        };
        assert!(new_layout.size() <= isize::MAX as usize, "allocation too large");
        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<T>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };
        self.ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }
    pub fn push(&mut self, elem: T) {
        if self.len == self.cap { self.grow(); }
        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.len), elem);
        }
        self.len += 1;
    }
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe {
                Some(ptr::read(self.ptr.as_ptr().add(self.len)))
            }
        }
    }
}
impl<T> Drop for Cvec<T> {
    fn drop(&mut self) {
        if self.cap != 0 {
            while let Some(_) = self.pop() {}
            let layout = Layout::array::<T>(self.cap).unwrap();
            unsafe {
                alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}
impl<T> Deref for Cvec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe {
            std::slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }
    }
}
impl<T> DerefMut for Cvec<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        unsafe {
            std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len)
        }
    }
}
impl<T: Clone> Clone for Cvec<T> {
    fn clone(&self) -> Cvec<T> {
        let layout = Layout::array::<T>(self.cap).unwrap();
        let ptr = match NonNull::new(unsafe { alloc::alloc(layout) } as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(layout),
        };
        for i in 0..self.len {
            unsafe { ptr::write(ptr.as_ptr().add(i), self[i].clone()); }
        }
        Self { ptr, cap: self.cap, len: self.len }
    }
}
impl<T: PartialEq> PartialEq for Cvec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}
impl<T: Eq> Eq for Cvec<T> {}
// insert, remove, into_iter, and drain all missing
impl<T: fmt::Display> fmt::Display for Cvec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for x in self.iter() {
            write!(f, " {}", x)?;
        }
        write!(f, " ]")?;
        Ok(())
    }
}
impl<T: fmt::Debug> fmt::Debug for Cvec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for x in self.iter() {
            write!(f, " {:?}", x)?;
        }
        write!(f, " ]")?;
        Ok(())
    }
}


#[repr(C)]
pub struct Crc<T> {
    ptr: NonNull<CrcInner<T>>,
    phantom: PhantomData<CrcInner<T>>
}
const CRC_INNER_RC_OFFSET:     usize = 8*0;
const CRC_INNER_DATA_OFFSET:   usize = 8*1;
#[repr(C)]
pub struct CrcInner<T> {
    rc: Cell<usize>,
    data: T,
}
impl<T> CrcInner<T> {
    pub unsafe fn increment(&self) {
        let old = self.rc.get();
        self.rc.set(old + 1);
        if old > isize::MAX as usize {
            std::process::abort();
        }
    }
}
impl<T> Crc<T> {
    pub fn new(data: T) -> Crc<T> {
        let boxed = Box::new(CrcInner { rc: Cell::new(1), data });
        Crc {
            ptr: NonNull::new(Box::into_raw(boxed)).unwrap(),
            phantom: PhantomData,
        }
    }
    pub fn into_ptr(self) -> *mut CrcInner<T> {
        ManuallyDrop::new(self).ptr.as_ptr() as *mut CrcInner<T>
    }
    pub fn from_ptr(ptr: *mut CrcInner<T>) -> Self {
        Crc {
            ptr: NonNull::new(ptr).unwrap(),
            phantom: PhantomData,
        }
    }
    pub fn from_ptr_clone(ptr: *mut CrcInner<T>) -> Self {
        unsafe { (*ptr).increment(); }
        Crc {
            ptr: NonNull::new(ptr).unwrap(),
            phantom: PhantomData,
        }
    }
}
unsafe impl<T: Sync+Send> Send for Crc<T> {}
unsafe impl<T: Sync+Send> Sync for Crc<T> {}
impl<T> Deref for Crc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        let inner = unsafe { self.ptr.as_ref() };
        &inner.data
    }
}
impl<T: fmt::Debug> fmt::Debug for Crc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.deref())
    }
}
impl<T> Clone for Crc<T> {
    fn clone(&self) -> Crc<T> {
        unsafe { self.ptr.as_ref().increment(); }
        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}
impl<T> Drop for Crc<T> {
    fn drop(&mut self) {
        let inner = unsafe { self.ptr.as_mut() };
        let old = inner.rc.get();
        inner.rc.set(old - 1);
        if old != 1 {
            return;
        }
        unsafe { drop(Box::from_raw(self.ptr.as_ptr())); }
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
#[repr(transparent)]
pub struct ID {
    pub id: NonZeroI64
}
impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[repr(C)]
pub struct Closure {
    params: Cvec<String>,
    e:      Form,
    body:   Form,
    id:     ID,
}

#[repr(C)]
pub struct Form {
    data: *const Form,
    phantom: PhantomData<Form>
}
const FORM_PAIR_CAR_OFFSET:   usize = 8*0;
const FORM_PAIR_CDR_OFFSET:   usize = 8*1;
#[repr(C)]
struct FormPair {
    car: Form,
    cdr: Form,
}
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
#[repr(usize)]
pub enum Prim {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Cons,
    Car,
    Cdr,
}
impl Prim {
    fn two_params(self) -> bool {
        match self {
            Prim::Car | Prim::Cdr => false,
            _                     => true,
        }
    }
}
/*
 * this better be a 64 bit platform
 * huh, if we only support i32s, then we have a lot more room for tags
 * 8 byte alignment gets us 3 bits, or uh 8 options
 * we'll choose 000 for ints to make math easy
 *
 *        000 - Int
 *        001 - Nil
 *        010 - Bool(false) // this is needlessly wasteful of the bits but hay - should take one of them over as a String probs
 *        011 - Bool(true)
 *        100 - Symbol - will want to convert into an Crc around a StringRawParts struct
 *        101 - Pair   - an Crc around a Pair struct
 *        110 - Closure- eek: Closure(Cvec<String>, Crc<Form>, Crc<Form>, ID),
 * xxxx   111 - Prim (xxxx for which one)
 *
 * I don't actually think we need our own repr(C) Cvec implementation, at least not for now - we can
 * make do with a CvecRawParts struct (without implementations)
 *  Hay I did it anyway
 *
 * in both cases, StringRawParts and CvecRawParts, we can rebuild slices from the raw parts for
 * read-only access, which is all we need (until Drop, at which point we should re-constitute them
 * from their raw parts, which is stable)
 *
 * For symbols, it would actually make sense to create the String, then leak it so it lasts for the
 * program, then deduplicate to it and pass the static const slice around
 * Could even fit entirely in the Form if the max length of a symbol is 2^16
 */
const TAG_OFFSET:     usize = 3;

const SYM_LEN_OFFSET: usize = 3;
const SYM_LEN_MASK:   usize = 0xFF; // could be bigger
const SYM_PTR_OFFSET: usize = 11;

const TAG_MASK:       usize = 0b111;
const TAG_INT:        usize = 0b000;
const TAG_NIL:        usize = 0b001;
const TAG_BOOL_FALSE: usize = 0b010;
const TAG_BOOL_TRUE:  usize = 0b011;
const TAG_SYMBOL:     usize = 0b100;
const TAG_PRIM:       usize = 0b101;

const TAG_CLOSURE:    usize = 0b110;
const TAG_PAIR:       usize = 0b111;

const TAG_RC:         usize = 0b110;


static SYMBOLS: Lazy<Mutex<BTreeMap<String,&'static str>>> = Lazy::new(Mutex::default);

impl Form {
    pub fn new_int(x: isize) -> Self {
        Self { data: (x << TAG_OFFSET) as *const Form, phantom: PhantomData }
    }
    pub fn new_nil() -> Self {
        Self { data: TAG_NIL as *const Form, phantom: PhantomData }
    }
    pub fn new_bool(b: bool) -> Self {
        Self { data: (if b { TAG_BOOL_TRUE } else { TAG_BOOL_FALSE }) as *const Form, phantom: PhantomData }
    }
    pub fn new_pair(car: Form, cdr: Form) -> Self {
        let p = Crc::new(FormPair { car, cdr }).into_ptr() as usize;
        assert!(p & TAG_MASK == 0);
        Self { data: (p | TAG_PAIR) as *const Form, phantom: PhantomData }
    }
    fn new_closure(params: Cvec<String>, e: Form, body: Form, ctx: &mut Ctx) -> Self {
        let p = Crc::new(Closure { params, e, body, id: ctx.alloc_id() }).into_ptr() as usize;
        assert!(p & TAG_MASK == 0);
        Self { data: (p | TAG_CLOSURE) as *const Form, phantom: PhantomData }
    }
    pub fn new_prim(p: Prim) -> Self {
        Self { data: (((p as usize) << TAG_OFFSET) | TAG_PRIM) as *const Form, phantom: PhantomData }
    }
    pub fn new_symbol(s: &str) -> Form {
        assert!(s.len() < SYM_LEN_MASK);
        let mut symbols = SYMBOLS.lock().unwrap();
        let ds = if let Some(ds) = symbols.get(s) {
            ds
        } else {
            // here we leak the memory of a new owned copy of s,
            // and then transmute it into an &'static str that we keep in our global
            // map for deduplication. Spicy stuff.
            let mut value = ManuallyDrop::new(s.to_owned());
            value.shrink_to_fit();
            let slice = unsafe { std::mem::transmute(value.as_str()) };
            symbols.insert(s.to_owned(), slice);
            slice
        };
        //println!("Deduped {s} to {ds}");
        Self { data: (((ds.as_ptr() as usize) << SYM_PTR_OFFSET) | (ds.len() << SYM_LEN_OFFSET) | TAG_SYMBOL) as *const Form, phantom: PhantomData }
    }

    pub fn is_rc(&self) -> bool {
        self.data as usize & TAG_RC == TAG_RC
    }
    pub fn int(&self) -> Result<isize> {
        if self.data as usize & TAG_MASK == TAG_INT {
            Ok(self.data as isize >> 3)
        } else {
            Err(anyhow!("car on not a pair"))
        }
    }
    pub fn car(&self) -> Result<&Form> {
        if self.data as usize & TAG_MASK == TAG_PAIR {
            Ok(unsafe { &(*((self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>)).data.car })
        } else {
            Err(anyhow!("car on not a pair"))
        }
    }
    pub fn cdr(&self) -> Result<&Form> {
        if self.data as usize & TAG_MASK == TAG_PAIR {
            Ok(unsafe { &(*((self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>)).data.cdr })
        } else {
            Err(anyhow!("cdr on not a pair"))
        }
    }
    pub fn closure(&self) -> Result<&Closure> {
        if self.data as usize & TAG_MASK == TAG_CLOSURE {
            Ok(unsafe { &(*((self.data as usize & !TAG_MASK) as *mut CrcInner<Closure>)).data })
        } else {
            Err(anyhow!("closure on on not a closure"))
        }
    }
    pub fn prim(&self) -> Result<Prim> {
        if self.data as usize & TAG_MASK == TAG_PRIM {
            Ok(unsafe { *(&((self.data as usize) >> TAG_OFFSET) as *const usize as *const Prim) })
        } else {
            Err(anyhow!("prim on on not a prim"))
        }
    }
    pub fn sym(&self) -> Result<&str> {
        if self.data as usize & TAG_MASK == TAG_SYMBOL {
            let len = ((self.data as usize) >> SYM_LEN_OFFSET) & SYM_LEN_MASK;
            let ptr = ((self.data as usize) >> SYM_PTR_OFFSET) as *const u8;
            Ok(std::str::from_utf8(unsafe { std::slice::from_raw_parts(ptr, len) }).unwrap())
        } else {
            Err(anyhow!("sym on on not a str"))
        }
    }
    fn truthy(&self) -> bool {
        match self.data as usize & TAG_MASK {
            TAG_NIL        => false,
            TAG_BOOL_FALSE => false,
            TAG_BOOL_TRUE  => true,
            _              => true,
        }
    }
    fn bool(&self) -> Result<bool> {
        match self.data as usize & TAG_MASK {
            TAG_BOOL_FALSE => Ok(false),
            TAG_BOOL_TRUE  => Ok(true),
            _              => Err(anyhow!("bool on not a bool")),
        }
    }
    fn pair(&self) -> Result<(&Form,&Form)> {
        if self.data as usize & TAG_MASK == TAG_PAIR {
            let crc_ptr = (self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>;
            Ok(unsafe { (&(*crc_ptr).data.car,&(*crc_ptr).data.cdr) })
        } else {
            Err(anyhow!("pair on not a pair"))
        }
    }
    fn is_nil(&self) -> bool {
        match self.data as usize & TAG_MASK {
            TAG_NIL        => true,
            _              => false,
        }
    }
    pub fn define(&self, s: &str, v: Form) -> Form {
        Form::new_pair(Form::new_pair(Form::new_symbol(s), v), self.clone())
    }
    pub fn append(&self, x: Form) -> Result<Form> {
        match self.data as usize & TAG_MASK {
            TAG_PAIR => self.cdr().unwrap().append(x).map(|x| Form::new_pair(self.car().unwrap().clone(), x)),
            TAG_NIL  => Ok(Form::new_pair(x, Form::new_nil())),
            _        => Err(anyhow!("append to not a pair")),
        }
    }
    pub fn root_env() -> Form {
        let mut e = Form::new_nil();
        for (s, v) in [
                ("+",    Form::new_prim(Prim::Add)),
                ("-",    Form::new_prim(Prim::Sub)),
                ("*",    Form::new_prim(Prim::Mul)),
                ("/",    Form::new_prim(Prim::Div)),
                ("%",    Form::new_prim(Prim::Mod)),
                ("cons", Form::new_prim(Prim::Cons)),
                ("cdr",  Form::new_prim(Prim::Cdr)),
                ("car",  Form::new_prim(Prim::Car)),
                ("=",    Form::new_prim(Prim::Eq)),
                ("nil",  Form::new_nil()),
        ] {
            e = e.define(s, v);
        }
        e
    }
    pub fn lookup(&self, s: &str) -> Result<&Form> {
        let mut e = self;
        loop {
            let (kv, ne) = e.pair()?;
            let (sp, v) = kv.pair()?;
            if sp.sym()? == s {
                return Ok(v);
            }
            e = ne;
        }
    }
    fn increment(&self) {
        match self.data as usize & TAG_MASK {
            TAG_INT | TAG_NIL | TAG_BOOL_FALSE | TAG_BOOL_TRUE | TAG_PRIM | TAG_SYMBOL => { /*println!("increment simple {self}");*/ },
            TAG_PAIR => {
                //println!("increment pair");
                unsafe { (*((self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair>)).increment(); }
            },
            TAG_CLOSURE => {
                //println!("increment pair");
                unsafe { (*((self.data as usize & !TAG_MASK) as *mut CrcInner<Closure>)).increment(); }
            },
            _ => unreachable!(),
        }
    }
}
impl Drop for Form {
    fn drop(&mut self) {
        match self.data as usize & TAG_MASK {
            TAG_INT | TAG_NIL | TAG_BOOL_FALSE | TAG_BOOL_TRUE | TAG_PRIM | TAG_SYMBOL => { /*println!("dropping simple {self}");*/  }, // doing nothing for symbol is fine
                                                                                                                                   // since it's deduplicated
            TAG_PAIR => {
                //println!("dropping pair");
                let _ = Crc::<FormPair>::from_ptr( (self.data as usize & !TAG_MASK) as *mut CrcInner<FormPair> );
            },
            TAG_CLOSURE => {
                //println!("dropping closure");
                let _ = Crc::<Closure>::from_ptr( (self.data as usize & !TAG_MASK) as *mut CrcInner<Closure> );
            },
            _ => unreachable!(),
        }
    }
}
impl Clone for Form {
    fn clone(&self) -> Self {
        self.increment();
        Self { data: self.data, phantom: PhantomData }
    }
}
impl PartialEq for Form {
    fn eq(&self, other: &Self) -> bool {
        match self.data as usize & TAG_MASK {
            TAG_INT | TAG_NIL | TAG_BOOL_FALSE | TAG_BOOL_TRUE | TAG_PRIM | TAG_SYMBOL => { self.data == other.data },
            TAG_PAIR => {
                if other.data as usize & TAG_MASK != TAG_PAIR {
                    return false;
                }
                self.car().unwrap() == other.car().unwrap() && self.cdr().unwrap() == other.cdr().unwrap()
            },
            TAG_CLOSURE => {
                if other.data as usize & TAG_MASK != TAG_CLOSURE {
                    return false;
                }
                let Closure { params, e, body, id, } = self.closure().unwrap();
                let Closure { params: oparams, e: oe, body: obody, id: oid, } = self.closure().unwrap();
                params == oparams && e == oe && body == obody && id == oid
            },
            _ => unreachable!(),
        }
    }
}
impl Eq for Form {}
impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.data as usize & TAG_MASK {
            TAG_INT => {
                //println!("printing int");
                write!(f, "{}", self.data as isize >> 3)?;
            },
            TAG_NIL => {
                //println!("printing nil");
                write!(f, "nil")?;
            },
            TAG_BOOL_FALSE => {
                //println!("printing false");
                write!(f, "false")?;
            },
            TAG_BOOL_TRUE => {
                //println!("printing true");
                write!(f, "true")?;
            },
            TAG_PAIR => {
                //println!("printinga  pair");
                write!(f, "({}", self.car().unwrap())?;
                let mut traverse = self.cdr().unwrap();
                loop {
                    match traverse.data as usize & TAG_MASK {
                        TAG_PAIR => {
                            write!(f, " {}", traverse.car().unwrap())?;
                            traverse = traverse.cdr().unwrap();
                        },
                        TAG_NIL => {
                            write!(f, ")")?;
                            return Ok(());
                        },
                        _ => {
                            write!(f, ". {traverse})")?;
                            return Ok(());
                        }
                    }
                }
            },
            TAG_PRIM => {
                //println!("printinga prim");
                write!(f, "{:?}", self.prim().unwrap())?;
            },
            TAG_SYMBOL => {
                //println!("printinga symbol");
                write!(f, "'{}", self.sym().unwrap())?;
            },
            TAG_CLOSURE => {
                //println!("printinga closure");
                let Closure { params, e, body, id, } = self.closure().unwrap();
                write!(f, "<{params} {e} {body} {id}>")?;
                //println!("doneprinting closure");
            },
            _ => unreachable!(),
        }
        Ok(())
    }
}
impl fmt::Debug for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Form({self})")
    }
}
impl From<String> for Form { fn from(item: String) -> Self { Form::new_symbol(&item) } }
impl From<&str>   for Form { fn from(item: &str)   -> Self { Form::new_symbol(item) } }
impl From<isize>  for Form { fn from(item: isize)  -> Self { Form::new_int(item) } }
impl From<bool>   for Form { fn from(item: bool)   -> Self { Form::new_bool(item) } }
impl<A: Into<Form>, B: Into<Form>> From<(A, B)> for Form {
    fn from(item: (A, B)) -> Self {
        Form::new_pair(item.0.into(), item.1.into())
    }
}


// This first Simple Lisp really is
//
// No fexprs, no mutation, no continuations, no macros, no strings.
// Int/Bool/Nil/Pair/Symbol/Closure/Prim.
//
// Figuring out GC between a JIT and Rust will be tricky.
// Can start with a like tracing-JIT-into-bytecode
// let's make our own Box, Crc, maybe Arc, Vec too?
// rustonomicon

fn eval_prim(f: Prim, b: Form, a: Option<Form>) -> Result<Form> {
    Ok(match f {
         Prim::Car => b.car()?.clone(),
         Prim::Cdr => b.cdr()?.clone(),
         _ => {
             let a = a.unwrap();
             match f {
                 Prim::Add  => Form::new_int(a.int()? + b.int()?),
                 Prim::Sub  => Form::new_int(a.int()? - b.int()?),
                 Prim::Mul  => Form::new_int(a.int()? * b.int()?),
                 Prim::Div  => Form::new_int(a.int()? / b.int()?),
                 Prim::Mod  => Form::new_int(a.int()? % b.int()?),
                 Prim::Cons => Form::new_pair(a, b),
                 Prim::Eq   => Form::new_bool(a == b),
                 _ => unreachable!(),
             }
         }
     })
}

// JIT Decisions
//  JIT Closure vs JIT Closure-Template
//      That is, do you treat the closed-over variables as constant
//          currently we do! if a lookup is not to one of our in-func defined variables, it's a
//          constant. done in trace_lookup
//      Or maybe more specifically, which closed over variables do you treat as constant
//          This will later inform optimistic inlining of primitives, I imagine
//  Inline or not
//  Rejoin branches or not
//      currently we trace into extended basic blocks, in the future stitch those together + const
//      prop to do more standard traces (after longer warm-up)
//
//      currently we basically just have lazy EBB bytecode construction
//          which I like!

#[derive(Debug)]
enum Op {
    Guard            { const_value: Form, side_val: Option<Form>, side_cont: Crc<Cont>, side_id: ID, tbk: TraceBookkeeping },
    Debug,
    Define           { sym: String },
    Const            (Form),
    Drop,
    Lookup           { sym: String },
    Call             { len: usize, statik: Option<ID>, nc: Crc<Cont>, nc_id: ID },
    InlinePrim(Prim),
    Tail(usize,Option<ID>),
    Return,
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Guard       { const_value, side_val:_, side_cont:_, side_id, tbk:_ } => write!(f, "Guard{side_id}({const_value})"),
            Op::Debug                                                                => write!(f, "Debug"),
            Op::Define      { sym }                                                  => write!(f, "Define({sym})"),
            Op::Const       ( con )                                                  => write!(f, "Const_{con}"),
            Op::Drop                                                                 => write!(f, "Drop"),
            Op::Lookup      { sym }                                                  => write!(f, "Lookup({sym})"),
            Op::Call        { len, nc:_, nc_id, statik }                             => write!(f, "Call{nc_id}({len},{statik:?})"),
            Op::InlinePrim(prim)                                                     => write!(f, "{prim:?}"),
            Op::Tail(len,oid)                                                        => write!(f, "Tail({len},{oid:?})"),
            Op::Return                                                               => write!(f, "Return"),
        }
    }
}
impl Op {
    fn cnst(&self) -> Result<Form> {
        match self {
            Op::Const(c) => Ok(c.clone()),
            _            => Err(anyhow!("const on not a const")),
        }
    }
}
#[derive(Debug,Clone)]
struct TraceBookkeeping {
    func_id: ID,
    stack_const: Vec<bool>,
    defined_names: BTreeSet<String>,
}

#[derive(Clone,Debug)]
enum Cont {
    MetaRet,
    Ret   {                        id: ID,                   },
    Eval  {                                     c: Crc<Cont> }, 
    Prim  { s: &'static str,       to_go: Form, c: Crc<Cont> },
    Call  { n: usize,              to_go: Form, c: Crc<Cont> },
    Frame { syms: Cvec<String>,    id: ID,      c: Crc<Cont> },
}
impl Cont {
    fn is_ret(&self) -> bool {
        match self {
            Cont::Ret { id: _ } => true,
            _                   => false,
        }
    }
}

#[derive(Debug)]
struct Trace {
    id: ID,
    ops: Vec<Op>,
    tbk: TraceBookkeeping,
}
impl Trace {
    fn new(id: ID, func_id: ID) -> Self {
        Trace { id, ops: vec![], tbk: TraceBookkeeping { stack_const: vec![], defined_names: BTreeSet::new(), func_id } }
    }
    fn follow_on(id: ID, tbk: TraceBookkeeping) -> Self {
        Trace { id, ops: vec![], tbk }
    }
}
impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Trace for {} (func {}) [", self.id, self.tbk.func_id)?;
        for op in &self.ops {
            write!(f, " {}", op)?;
        }
        write!(f, " ]")?;
        if !self.tbk.stack_const.is_empty() {
            write!(f, "[")?;
            for s in &self.tbk.stack_const {
                write!(f, " {}", s)?;
            }
            write!(f, " ]")?;
        }
        Ok(())
    }
}
struct Ctx {
    id_counter: i64,
    cont_count: BTreeMap<ID, i64>,
    tracing: Option<Trace>,
    traces: BTreeMap<ID, Vec<Op>>,
    compiled_traces: BTreeMap<ID, extern "C" fn(&mut Form, &mut Cvec<Form>, &mut Cvec<RetStackEntry>) -> usize>,
    trace_resume_data: BTreeMap<ID, TraceBookkeeping>,
    jit: JIT,
}
impl fmt::Display for Ctx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ctx")
    }
}
impl Ctx {
    fn new() -> Ctx {
        Ctx {
            id_counter: 0,
            cont_count: BTreeMap::new(),
            tracing: None,
            traces: BTreeMap::new(),
            compiled_traces: BTreeMap::new(),
            trace_resume_data: BTreeMap::new(),
            jit: JIT::new(),
        }
    }
    fn alloc_id(&mut self) -> ID {
        self.id_counter += 1;
        ID { id: NonZeroI64::new(self.id_counter).unwrap() }
    }
    fn trace_running(&self) -> bool { self.tracing.is_some() }

    // Though I guess that means call start should recieve the parameters
    //  also, for like variables, it should guard on what function
    //      if dynamic, interacts with the constant tracking
    //  8 options
    //      - not tracing, closure        - do stats
    //      - not tracing, prim           - do nothing
    //      - tracing, Static Prim        - inline prim
    //      - tracing, Static non-self    - inline call? (currently static call)
    //      - tracing, Static, tail-self  - emit tail (static) (we removed loop because it's a static jump back to the head trace regardless)
    //      - tracing, Static,nontail-self- emit call (static)
    //      - tracing, Dynamic,     tail  - emit tail
    //      - tracing, Dynamic, non-tail  - emit call
    fn trace_call(&mut self, call_len: usize, tmp_stack: &Cvec<Form>, nc: &Crc<Cont>) -> Option<ID> {

        // Needs to take and use parameters for mid-trace
        //  needs to guard on function called if non-constant
        println!("trace_call call_len={call_len},trace={:?}, tmp_stack {tmp_stack:?}", self.tracing);
        if let Some(trace) = &mut self.tracing {
            let statik = if trace.tbk.stack_const[trace.tbk.stack_const.len()-call_len] {
                // const - TODO: for now, we don't inline but we will want to later (based on what
                // metrics? can we run them simultaniously, heirarchially? with our new approach on
                // prims maybe (heck we may need to go farther, and remove the InlinePrim!)
                let func = &tmp_stack[tmp_stack.len()-call_len];
                match func.data as usize & TAG_MASK {
                    TAG_PRIM => {
                        let p = func.prim().unwrap();
                        if (&trace.tbk.stack_const[trace.tbk.stack_const.len()-call_len..]).iter().all(|x| *x) {
                            trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                            let b = trace.ops[trace.ops.len()-1].cnst().unwrap();
                            let (a,f) = if call_len == 3 {
                                (Some(trace.ops[trace.ops.len()-2].cnst().unwrap()), p)
                            } else { (None, p) };
                            for _ in 0..call_len {
                                trace.ops.push(Op::Drop);
                            }

                            trace.ops.push(Op::Const(eval_prim(f, b, a).unwrap()));
                            trace.tbk.stack_const.push(true);
                        } else { 
                            trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                            trace.ops.push(Op::InlinePrim(p));
                            trace.tbk.stack_const.push(false);
                        }

                        return None;
                    },
                    TAG_CLOSURE => {
                        let Closure { id, .. } = func.closure().unwrap();
                        if nc.is_ret() {
                            if *id == trace.tbk.func_id {
                                // we removed the loop opcode because this trace needs to know the
                                // func header trace id anyway
                                trace.ops.push(Op::Tail(call_len, Some(*id)));
                            } else {
                                // should be inline
                                trace.ops.push(Op::Tail(call_len, Some(*id)));
                            }
                            println!("Ending trace at loop/tail recursive call!");
                            println!("\t{}", trace);
                            self.traces.insert(trace.id, self.tracing.take().unwrap().ops);
                            return None;
                        }
                        // fall through to be a static call, though also would normally be inline
                        Some(*id)
                    },
                    b => panic!("bad func {b:?}"),
                }
            } else { None };

            // (normally not const) or has tmps - Call or TailCall
            if nc.is_ret() {
                trace.ops.push(Op::Tail(call_len,statik));
                println!("Ending trace at tail recursive call!");
                println!("\t{}", trace);
                self.traces.insert(trace.id, self.tracing.take().unwrap().ops);
                return None;
            } else {
                trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                self.id_counter += 1; let nc_id = ID { id: NonZeroI64::new(self.id_counter).unwrap() }; // HACK - I can't use the method cuz trace is borrowed
                trace.ops.push(Op::Call { len: call_len, statik, nc: Crc::clone(nc), nc_id });
                println!("Ending trace at call!");
                println!("\t{}", trace);
                self.trace_resume_data.insert(nc_id, trace.tbk.clone());
                self.traces.insert(trace.id, self.tracing.take().unwrap().ops);
                return Some(nc_id);
            }
        }
        None
    }
    fn trace_frame(&mut self, syms: &Cvec<String>, id: ID) {
        let inline = self.tracing.is_some();
        let entry = self.cont_count.entry(id).or_insert(0);
        println!("tracing call start for {id}, has been called {} times so far", *entry);
        *entry += 1;
        if *entry > TRACE_LEVEL && self.tracing.is_none() && self.traces.get(&id).is_none() {
            self.tracing = Some(Trace::new(id, id));
        }

        for s in syms.iter().rev() {
            self.trace_define(s, inline);
        }
        self.trace_drop(inline);
    }
    fn trace_define(&mut self, sym: &str, pop: bool) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Define { sym: sym.to_owned() });
            trace.tbk.defined_names.insert(sym.to_owned());
            if pop {
                trace.tbk.stack_const.pop().unwrap();
            }

        }
    }
    fn trace_drop(&mut self, pop: bool) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Drop);
            if pop {
                trace.tbk.stack_const.pop().unwrap();
            }
        }
    }
    fn trace_call_end(&mut self, id: ID, follow_on_trace_data: Option<ID>) {
        println!("tracing call end for {id} followon {follow_on_trace_data:?}");
        if let Some(trace) = &mut self.tracing {
            if trace.tbk.func_id == id {
                trace.ops.push(Op::Return);
                println!("Ending trace at end of call!");
                println!("\t{}", trace);
                self.traces.insert(trace.id, self.tracing.take().unwrap().ops);
            }
        }
        if self.tracing.is_none() {
            self.try_resume_trace(follow_on_trace_data);
        }
    }
    fn try_resume_trace(&mut self, follow_on_trace_data: Option<ID>) {
        if let Some(follow_id) = follow_on_trace_data {
            println!("looking follow-on trace {follow_id} in {:?}", self.trace_resume_data);
            if let Some(follow_tbk) = self.trace_resume_data.remove(&follow_id) {
                println!("starting follow-on trace {follow_id}, {follow_tbk:?}");
                let mut trace = Trace::follow_on(follow_id,follow_tbk);
                trace.tbk.stack_const.push(false); // fix with actual, if this ends up being a
                                                   // static call with static param list that isn't
                                                   // inlined for whatever reason...
                self.tracing = Some(trace);
            }
        }
    }
    fn trace_guard<T: Into<Form> + std::fmt::Debug>(&mut self, value: T, other: impl Fn()->(Option<Form>,Crc<Cont>)) {
        println!("Tracing guard {value:?}");
        if let Some(trace) = &mut self.tracing {
            let (side_val, side_cont) = other();
            self.id_counter += 1; let side_id = ID { id: NonZeroI64::new(self.id_counter).unwrap() }; // HACK - I can't use the method cuz trace is borrowed
            trace.ops.push(Op::Guard { const_value: value.into(), side_val, side_cont, side_id, tbk: trace.tbk.clone() });
        }
    }
    fn trace_debug(&mut self) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Debug);
        }
    }
    fn trace_lookup(&mut self, s: &str, f: &Form) {
        if let Some(trace) = &mut self.tracing {
            // constant depends on which env, and I think this is the only spot that cares for
            // closure jit vs lambda jit
            if trace.tbk.defined_names.contains(s) {
                trace.ops.push(Op::Lookup { sym: s.to_owned() });
                trace.tbk.stack_const.push(false);
            } else {
                trace.ops.push(Op::Const(f.clone()));
                trace.tbk.stack_const.push(true);
            }
        }
    }
    fn trace_constant(&mut self, c: &Form) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Const(c.clone()));
            trace.tbk.stack_const.push(true);
        }
    }
    fn trace_lambda(&mut self, _params: &[String], _e: &Form, _body: &Form) {
        if let Some(_trace) = &mut self.tracing {
            // TODO
            // kinda both also
            unimplemented!("trace lambda");
        }
    }
    // returns f, e, c for interp
    fn execute_trace_if_exists(&mut self,
                                  mut id: ID,
                                  mut e: Form,
                                  tmp_stack: &mut Cvec<Form>, 
                                  ret_stack: &mut Cvec<RetStackEntry>) -> Result<Option<(Form, Form, Cont)>> {
        if self.trace_running() {
            println!("Not playing back trace because recording trace");
            return Ok(None); // can't trace while running a trace for now (we don't inline now anyway),
                             // in the future it should just tack on the opcodes while jugging the proper 
                             // bookkeeping stacks
        }
        let mut offset = 0;
        loop {
            if offset == 0 {
                if let Some(f) = self.compiled_traces.get(&id) {
                    println!("Calling JIT function {id}, tmp_stack is {:?}!", tmp_stack);
                    println!("{}", unsafe { *((&mut e) as *mut Form as *mut usize) } as usize);
                    let trace_ret = f(&mut e, tmp_stack, ret_stack);
                    let new_id = ID { id: NonZeroI64::new((trace_ret >> TRACE_ID_OFFSET) as i64).unwrap() };
                    offset     = trace_ret &  TRACE_OFFSET_MASK;
                    println!("\tresult of call is new_id {new_id} and offset {offset}, tmp_stack is {:?}!", tmp_stack);
                    println!("{}", unsafe { *((&mut e) as *mut Form as *mut usize) } as usize);
                    println!("\te is {e}!");
                    // if we've returned a new trace to start, do so,
                    // otherwise fall through to trace interpretation immediately
                    if new_id != id {
                        id = new_id;
                        continue;
                    }
                }
            }
            if let Some(trace) = self.traces.get(&id) {

                let entry = self.cont_count.entry(id).or_insert(0);
                *entry += 1;
                if *entry > JIT_LEVEL && !self.compiled_traces.contains_key(&id) {
                    println!("Compiling trace for {id}!");
                    let outer_ptr = self.jit.comple_trace(id, trace);
                    self.compiled_traces.insert(id, outer_ptr);
                    continue;
                }

                println!("Starting trace playback");
                println!("Running trace {trace:?}, \n\ttmp_stack:{tmp_stack:?}");
                for b in trace.iter().skip(offset) {
                    match b {
                        Op::Guard       { const_value, side_val, side_cont, side_id, tbk } => {
                            println!("Guard(op) {const_value}");
                            if const_value != tmp_stack.last().unwrap() {
                                if self.traces.contains_key(side_id) {
                                    if side_val.is_some() {
                                        tmp_stack.pop().unwrap();
                                    }
                                    println!("\tchaining trace to side trace");
                                    id = *side_id;
                                    offset = 0;
                                    break; // break out of this trace and let infinate loop spin
                                } else {
                                    println!("\tending playback b/c failed guard");
                                    assert!(self.tracing.is_none());
                                    let mut ntrace = Trace::follow_on(*side_id,tbk.clone());
                                    if let Some(side_val) = side_val {
                                        *tmp_stack.last_mut().unwrap() = side_val.clone();
                                        *ntrace.tbk.stack_const.last_mut().unwrap() = false; // this might be able to be
                                                                                             // more precise, actually
                                    }
                                    self.tracing = Some(ntrace);
                                    return Ok(Some((tmp_stack.pop().unwrap(), e, (**side_cont).clone())));
                                }
                            }
                        }
                        Op::Debug                                                          => {
                            println!("Debug(op) {}", tmp_stack.last().unwrap());
                        }
                        Op::Define      { sym }                                            => {
                            let v = tmp_stack.pop().unwrap();
                            println!("Define(op) {sym} = {}", v);
                            e = e.define(sym, v);
                        }
                        Op::Const       ( con )                                            => {
                            println!("Const(op) {con}");
                            tmp_stack.push(con.clone());
                        }
                        Op::Drop                                                           => {
                            println!("Drop(op) {}", tmp_stack.last().unwrap());
                            tmp_stack.pop().unwrap();
                        }
                        Op::Lookup      { sym }                                            => {
                            println!("Lookup(op) {sym}");
                            tmp_stack.push(e.lookup(sym)?.clone());
                        }
                        Op::InlinePrim(prim)                                               => {
                            println!("InlinePrim(op) {prim:?}");
                            let b = tmp_stack.pop().unwrap();
                            let a = if prim.two_params() { Some(tmp_stack.pop().unwrap()) } else { None };
                            tmp_stack.pop().unwrap(); // pop the prim
                            tmp_stack.push(eval_prim(*prim, b, a)?);
                        }
                        Op::Call        { len, nc, nc_id, statik }                    => {
                            println!("Call(op)");
                            if let Some(static_call_id) = statik {
                                if self.traces.contains_key(static_call_id) {
                                    ret_stack.push(RetStackEntry { e: e.clone(), c: (*nc).clone(), i: Some(*nc_id) });
                                    println!("\tchaining to call trace b/c Call with statik");
                                    id = *static_call_id;
                                    offset = 0;
                                    break; // break out of this trace and let infinate loop spin
                                }
                            }
                            let func = &tmp_stack[tmp_stack.len()-*len];
                            match func.data as usize & TAG_MASK {
                                TAG_PRIM => {
                                    let p = func.prim().unwrap();
                                    let b = tmp_stack.pop().unwrap();
                                    let a = if *len == 2 { None } else { assert!(*len == 3); Some(tmp_stack.pop().unwrap()) };
                                    let result = eval_prim(p, b, a)?;
                                    if self.traces.contains_key(nc_id) {
                                        *tmp_stack.last_mut().unwrap() = result; // for the prim itself
                                        println!("\tchaining to ret trace b/c Call with dyamic but primitive and next traced");
                                        id = *nc_id;
                                        offset = 0;
                                        break; // break out of this trace and let infinate loop spin
                                    } else {
                                        println!("\tstopping playback to ret b/c Call with dyamic but primitive and next not-traced");
                                        tmp_stack.pop().unwrap(); // for the prim itself
                                        return Ok(Some((result, e, (**nc).clone())));
                                    }
                                },
                                TAG_CLOSURE => {
                                    let Closure { params: ps, e: ie, body: b, id: call_id, } = func.closure().unwrap();
                                    if ps.len() != *len-1 {
                                        bail!("arguments length doesn't match");
                                    }
                                    ret_stack.push(RetStackEntry { e: e.clone(), c: (*nc).clone(), i: Some(*nc_id) });
                                    if self.traces.contains_key(call_id) {
                                        println!("\tchaining to call trace b/c Call with dyamic but traced");
                                        e = ie.clone();
                                        id = *call_id;
                                        offset = 0;
                                        break; // break out of this trace and let infinate loop spin
                                    } else {
                                        return Ok(Some((b.clone(), ie.clone(), Cont::Frame { syms: ps.clone(), id: *call_id, c: Crc::new(Cont::Eval { c: Crc::new(Cont::Ret { id: *call_id }) }) })));
                                    }
                                },
                                ncomb => {
                                    println!("Current stack is {tmp_stack:?}");
                                    bail!("tried to call a non-comb {ncomb}")
                                },
                            }
                        }
                        Op::Tail(_len,_oid)                                                  => {
                            println!("Tail(op)");
                            // Huh, this actually has to know how many envs we pushed on so we can pop
                            // them off
                            unimplemented!();
                        }
                        Op::Return                                                         => {
                            println!("Return(op)");
                            let RetStackEntry { e: ne, c: nc, i: resume_data } = ret_stack.pop().unwrap();
                            e = ne;
                            if let Some(resume_id) = resume_data {
                                if self.traces.contains_key(&resume_id) {
                                    id = resume_id;
                                    offset = 0;
                                    break; // break out of this trace and let infinate loop spin
                                }
                            }
                            println!("\tending playback b/c Return, attempting to resume trace");
                            self.try_resume_trace(resume_data);
                            return Ok(Some((tmp_stack.pop().unwrap(), e, (*nc).clone())));
                        }
                    }
                }
            } else {
                 return Ok(None);
            }
        }
    }
}

#[repr(C)]
struct RetStackEntry {
    e: Form,
    c: Crc<Cont>,
    i: Option<ID>
}
pub fn eval(f: Form) -> Result<Form> {
    let mut ctx = Ctx::new();
    let mut f = f;
    let mut e = Form::root_env();
    let mut c = Cont::Eval { c: Crc::new(Cont::MetaRet) };

    let mut ret_stack: Cvec<RetStackEntry> = Cvec::new();
    let mut tmp_stack: Cvec<Form> = Cvec::new();

    loop {
        match c {
            Cont::MetaRet => {
                println!("Ctx was {ctx}");
                assert!(!ctx.trace_running());
                return Ok(f);
            }
            Cont::Prim { s, to_go, c: nc } => {
                match s {
                    "if" => {
                        let thn = to_go.car()?;
                        let els = to_go.cdr()?.car()?;
                        if f.truthy() {
                            ctx.trace_guard(true, || (Some(els.clone()), Crc::new(Cont::Eval { c: Crc::clone(&nc) })));
                            ctx.trace_drop(true);
                            f = thn.clone();
                        } else {
                            ctx.trace_guard(false, ||(Some(thn.clone()), Crc::new(Cont::Eval { c: Crc::clone(&nc) })));
                            ctx.trace_drop(true);
                            f = els.clone();
                        }
                        c = Cont::Eval { c: nc };
                    },
                    "or" => {
                        let other = to_go.car()?;
                        if !f.truthy() {
                            ctx.trace_guard(false, || (None, nc.clone()));
                            ctx.trace_drop(true);
                            f = other.clone();
                            c = Cont::Eval { c: nc };
                        } else {
                            ctx.trace_guard(true, || (Some(other.clone()), Crc::new(Cont::Eval { c: Crc::clone(&nc) })));
                            c = (*nc).clone();
                        }
                    },
                    "and" => {
                        let other = to_go.car()?;
                        if f.truthy() {
                            ctx.trace_guard(true, || (None, nc.clone()));
                            ctx.trace_drop(true);
                            f = other.clone();
                            c = Cont::Eval { c: nc };
                        } else {
                            ctx.trace_guard(false, || (Some(other.clone()), Crc::new(Cont::Eval { c: Crc::clone(&nc) })));
                            c = (*nc).clone();
                        }
                    },
                    "begin" => {
                        if to_go.is_nil() {
                            c = (*nc).clone();
                        } else {
                            ctx.trace_drop(true);
                            f = to_go.car()?.clone();
                            c = Cont::Eval { c: Crc::new(Cont::Prim { s: "begin", to_go: to_go.cdr()?.clone(), c: nc }) };
                        }
                    },
                    "debug" => {
                        println!("Debug: {f}");
                        ctx.trace_debug();
                        c = (*nc).clone();
                    },
                    "define" => {
                        let sym = to_go.sym()?;
                        ctx.trace_define(&sym, true);
                        e = e.define(sym, f.clone());
                        c = (*nc).clone();
                    },
                    _ => {
                        panic!("bad prim {s}");
                    }
                }
            },
            Cont::Ret {        id,       } => {
                let RetStackEntry { e: ne, c: nc, i: resume_data } = ret_stack.pop().unwrap();
                ctx.trace_call_end(id, resume_data);
                e = ne;
                if let Some(nc_id) = resume_data {
                    tmp_stack.push(f); // ugly dance pt 1
                    if let Some((fp, ep, cp)) = ctx.execute_trace_if_exists(nc_id, e.clone(), &mut tmp_stack, &mut ret_stack)? {
                        f = fp;
                        e = ep;
                        c = cp;
                        println!("After executing return trace, f={f}, tmp_stack is {tmp_stack:?}");
                        continue;
                    } else {
                        f = tmp_stack.pop().unwrap(); //ugly dance pt2
                    }
                }
                c = (*nc).clone();
            },
            Cont::Call { n, to_go, c: nc } => {
                tmp_stack.push(f);
                if to_go.is_nil() {
                    let resume_data = ctx.trace_call(n, &mut tmp_stack, &nc);
                    let func = tmp_stack[tmp_stack.len()-n].clone();
                    match func.data as usize & TAG_MASK {
                        TAG_PRIM => {
                            let p = func.prim().unwrap();
                            let b = tmp_stack.pop().unwrap();
                            let a = if n == 2 { None } else { assert!(n == 3); Some(tmp_stack.pop().unwrap()) };
                            f = eval_prim(p, b, a)?;
                            tmp_stack.pop().unwrap(); // for the prim itself
                            c = (*nc).clone();
                        },
                        TAG_CLOSURE => {
                            let Closure { params: ps, e: ie, body: b, id, } = func.closure().unwrap();
                            if ps.len() != n-1 {
                                bail!("arguments length doesn't match");
                            }
                            ret_stack.push(RetStackEntry { e: e.clone(), c: nc, i: resume_data });
                            if let Some((fp, ep, cp)) = ctx.execute_trace_if_exists(*id, ie.clone(), &mut tmp_stack, &mut ret_stack)? {
                                f = fp;
                                e = ep;
                                c = cp;
                                println!("After executing trace, f={f}, tmp_stack is {tmp_stack:?}");
                            } else {
                                println!("replacing {e} with {ie}");
                                e = ie.clone();
                                c = Cont::Frame { syms: ps.clone(), id: *id, c: Crc::new(Cont::Eval { c: Crc::new(Cont::Ret { id: *id }) }) };
                                f = b.clone();
                            }
                        },
                        ncomb => {
                            println!("Current stack is {tmp_stack:?}");
                            bail!("tried to call a non-comb {ncomb}")
                        },
                    }
                } else {
                    f = to_go.car()?.clone();
                    c = Cont::Eval { c: Crc::new(Cont::Call { n: n+1, to_go: to_go.cdr()?.clone(), c: nc }) };
                }
            }
            Cont::Frame { syms, id, c: nc } => {
                ctx.trace_frame(&syms, id);
                for s in syms.into_iter().rev() {
                    e = e.define(&s, tmp_stack.pop().unwrap());
                }
                tmp_stack.pop().unwrap(); // for the func value
                c = (*nc).clone();
            }
            Cont::Eval { c: nc } => {
                let tmp = f;
                match tmp.data as usize & TAG_MASK {
                    TAG_SYMBOL => {
                        let s = tmp.sym().unwrap();
                        f = e.lookup(s)?.clone();
                        ctx.trace_lookup(s, &f);
                        c = (*nc).clone();
                    },
                    TAG_PAIR => {
                        let (car, cdr) = tmp.pair().unwrap();
                        match car.data as usize & TAG_MASK {
                            TAG_SYMBOL => {
                                let s = car.sym().unwrap();
                                match s {
                                    "if" => {
                                        f = cdr.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "if", to_go: cdr.cdr()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    // and/or has to short-circut, so special form
                                    // just like Scheme (bad ;) )
                                    "or" => {
                                        f = cdr.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "or", to_go: cdr.cdr()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    "and" => {
                                        f = cdr.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "and", to_go: cdr.cdr()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    "begin" => {
                                        f = cdr.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "begin", to_go: cdr.cdr()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    "debug" => {
                                        f = cdr.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "debug", to_go: cdr.cdr()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    "define" => {
                                        // note the swap, evaluating the second not the first (define a value..)
                                        f = cdr.cdr()?.car()?.clone();
                                        c = Cont::Eval { c: Crc::new(Cont::Prim { s: "define", to_go: cdr.car()?.clone(), c: nc }) };
                                        continue;
                                    }
                                    "quote" => {
                                        f = cdr.car()?.clone();
                                        ctx.trace_constant(&f);
                                        c = (*nc).clone();
                                        continue;
                                    }
                                    // (lambda (a b) body)
                                    "lambda" => {
                                        let mut params_vec = Cvec::new();
                                        let mut params = cdr.car()?;
                                        while let Ok((ncar, ncdr)) = params.pair() {
                                            params_vec.push(ncar.sym()?.to_string());
                                            params = ncdr;
                                        }
                                        let body = cdr.cdr()?.car()?;
                                        ctx.trace_lambda(&params_vec, &e, &body);
                                        f = Form::new_closure(params_vec, e.clone(), body.clone(), &mut ctx);
                                        c = (*nc).clone();
                                        continue;
                                    }
                                    _ => { /* fallthrough */ }
                                }
                            }
                            _ => { /* fallthrough */ }
                        }
                        f = car.clone();
                        c = Cont::Eval { c: Crc::new(Cont::Call { n: 1, to_go: cdr.clone(), c: nc }) };
                    },
                    _ =>  {
                        // value, no eval
                        f = tmp;
                        ctx.trace_constant(&f);
                        c = (*nc).clone();
                    }
                }
            }
        }
    }
}
