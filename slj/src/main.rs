#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::mem;

use anyhow::Result;

use cranelift::codegen::ir::UserFuncName;
use cranelift::prelude::*;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module};

use sl::{eval,Form,Crc,Cvec,Prim,ID};

extern "C" fn rust_add1(x: Form, y: Form) -> Form {
    println!("Add 1");
    Form::new_int(x.int().unwrap() + y.int().unwrap())
}
extern "C" fn rust_add2(x: isize, y: isize) -> isize {
    println!("Add 2");
    x + y
}

fn main() -> Result<()> {
    // our Form shennigins will only work on 64 bit platforms
    assert!(std::mem::size_of::<usize>() == 8);

    // started from
    // https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/jit/examples/jit-minimal.rs
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
    jb.symbol("rust_add1", rust_add1 as *const u8);
    jb.symbol("rust_add2", rust_add2 as *const u8);
    let mut module = JITModule::new(jb);
    let int = module.target_config().pointer_type();
    let mut ctx = module.make_context();
    let mut func_ctx = FunctionBuilderContext::new();

    let mut sig_a = module.make_signature();
    sig_a.call_conv = isa::CallConv::Tail;
    sig_a.params.push(AbiParam::new(int));
    sig_a.returns.push(AbiParam::new(int));
    let func_a = module.declare_function("a", Linkage::Local, &sig_a).unwrap();

    let mut sig_b = module.make_signature();
    sig_b.call_conv = isa::CallConv::Tail;
    sig_b.returns.push(AbiParam::new(int));
    let func_b = module.declare_function("b", Linkage::Local, &sig_b).unwrap();

    let mut sig_c = module.make_signature();
    //sig_b.call_conv = isa::CallConv::Tail;
    sig_c.params.push(AbiParam::new(int));
    sig_c.returns.push(AbiParam::new(int));
    let func_c = module.declare_function("c", Linkage::Local, &sig_c).unwrap();

    ctx.func.signature = sig_a;
    ctx.func.name = UserFuncName::user(0, func_a.as_u32());
    {
        let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        bcx.append_block_params_for_function_params(block);
        let param = bcx.block_params(block)[0];

        let cst = bcx.ins().iconst(int, 3 << 3);
        let add = bcx.ins().iadd(cst, param);
        let cr = {
            let mut sig = module.make_signature();
            sig.params.push(AbiParam::new(int));
            sig.params.push(AbiParam::new(int));
            sig.returns.push(AbiParam::new(int));
            //let callee = module.declare_function("rust_add1", Linkage::Import, &sig).unwrap();
            let callee = module.declare_function("rust_add2", Linkage::Import, &sig).unwrap();
            let local_callee = module.declare_func_in_func(callee, bcx.func);
            let call = bcx.ins().call(local_callee, &[add, add]);
            bcx.inst_results(call)[0]
        };
        bcx.ins().return_(&[cr]);

        //let sh = bcx.ins().sshr_imm(param, 3);
        //bcx.ins().return_(&[sh]);

        bcx.seal_all_blocks();
        bcx.finalize();
    }
    module.define_function(func_a, &mut ctx).unwrap();
    module.clear_context(&mut ctx);
    //module.finalize_definitions().unwrap(); can be done multiple times

    ctx.func.signature = sig_b;
    ctx.func.name = UserFuncName::user(0, func_b.as_u32());
    {
        let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        let local_func = module.declare_func_in_func(func_a, &mut bcx.func);
        let arg = bcx.ins().iconst(int, 30 << 3);
        bcx.ins().return_call(local_func, &[arg]);
        /*
        let call = bcx.ins().call(local_func, &[arg]);
        let value = {
            let results = bcx.inst_results(call);
            assert_eq!(results.len(), 1);
            results[0].clone()
        };
        bcx.ins().return_(&[value]);
        */
        bcx.seal_all_blocks();
        bcx.finalize();
    }

    module.define_function(func_b, &mut ctx).unwrap();
    module.clear_context(&mut ctx);

    ctx.func.signature = sig_c;
    ctx.func.name = UserFuncName::user(0, func_c.as_u32());
    {
        let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = bcx.create_block();
        bcx.switch_to_block(block);
        bcx.append_block_params_for_function_params(block);
        let param = bcx.block_params(block)[0];

        let local_func = module.declare_func_in_func(func_a, &mut bcx.func);
        let call = bcx.ins().call(local_func, &[param]);
        let value = {
            let results = bcx.inst_results(call);
            assert_eq!(results.len(), 1);
            results[0].clone()
        };
        bcx.ins().return_(&[value]);

        bcx.seal_all_blocks();
        bcx.finalize();
    }
    module.define_function(func_c, &mut ctx).unwrap();
    module.clear_context(&mut ctx);



    // perform linking
    module.finalize_definitions().unwrap();

    let code_a = module.get_finalized_function(func_a);
    let ptr_a = unsafe { mem::transmute::<_, extern "C" fn (Form) -> Form>(code_a) };

    let code_b = module.get_finalized_function(func_b);
    let ptr_b = unsafe { mem::transmute::<_, extern "C" fn () -> Form>(code_b) };

    let code_c = module.get_finalized_function(func_c);
    let ptr_c = unsafe { mem::transmute::<_, extern "C" fn (Form) -> Form>(code_c) };

    //let res = ptr_b();
    //println!("sucessful run with result {res}");
    //let res = ptr_a(23);
    //println!("sucessful 2 run with result {res}");
    let res = ptr_a(Form::new_int(1337));
    println!("sucessful 1 run with result {res}");

    let res = ptr_b();
    println!("sucessful 2 run with result {res}");

    let res = ptr_c(Form::new_int(1337));
    println!("sucessful 3 run with result {res}");

    //return Ok(());

    fn alias(a: Crc<u64>, b: Crc<u64>) {
        println!("a: {}, b: {}", *a, *b);
    }
    let x = Crc::new(1);
    alias(Crc::clone(&x), x);
    let rc_u64_size = std::mem::size_of::<Crc<u64>>();
    assert!(rc_u64_size == 8);
    println!("for our Crc, we have size {}", rc_u64_size);

    let begn = Form::new_symbol("begin");
    println!("this should be begin {begn}");

    let i  = Form::new_int(23);
    let n  = Form::new_nil();
    let bf = Form::new_bool(false);
    let bt = Form::new_bool(true);

    let p  = Form::new_pair(Form::new_int(50), Form::new_nil());

    let pra  = Form::new_prim(Prim::Add);
    let pre  = Form::new_prim(Prim::Eq);

    let s = Form::new_symbol("woopwpp");


    let mut params = Cvec::new();
    params.push("a".to_owned());
    params.push("b".to_owned());

    println!("{i} {n} {bf} {bt} {p} {pra} {pre} {s}");

    let mut my_vec: Cvec<Form> = Cvec::new();
    my_vec.push(i);
    my_vec.push(n);
    my_vec.push(bf);
    my_vec.push(bt);
    my_vec.push(p);
    my_vec.push(pra);
    my_vec.push(pre);
    my_vec.push(s);
    my_vec.push(begn);


    println!(" from vec {}", my_vec[3]);
    for i in my_vec.iter() {
        println!(" from vec {}", i);
    }
    println!("{my_vec}");

    my_vec[3] = Form::new_symbol("replaced");

    println!(" from vec {}", my_vec[3]);
    for i in my_vec.iter() {
        println!(" from vec {}", i);
    }
    println!("{my_vec}");


    let input = "
    (begin
        (debug 1)
        ;(debug (= 1 2))
        ;(debug (+ 2 3))
        ;(define a (+ 1 (* 3 4)))

        ;(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
        ;(debug 'gonna_fact_it)
        ;(debug fact)
        ;(debug (fact 400))

        ;(define fact2 (lambda (n a) (if (= n 1) a (fact2 (- n 1) (* n a)))))
        ;(debug 'gonna_fact2_it)
        ;(debug fact2)
        ;(debug (fact2 400 1))





        (define faft_h (lambda (faft_h n) (if (= n 1) (debug 1) (+ n (faft_h faft_h (- n 1))))))
        (define faft (lambda (n) (faft_h faft_h n)))

        (debug 'gonna_faft_it)
        (debug faft)
        (debug (faft 6))
        (debug 'gonna_faft_it2)
        (debug (faft 10))
        ;(debug (faft 400))

        ;(define faft2 (lambda (n a) (if (= n 1) a (faft2 (- n 1) (+ n a)))))
        ;(debug 'gonna_faft2_it)
        ;(debug faft2)
        ;(debug (faft2 6 1))
        ;(debug (faft2 400 1))





        ;(define fib (lambda (n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))))
        ;(debug 'gonna_fib_it)
        ;(debug fib)
        ;(debug (fib 10))

        ;(debug a)
        ;(define b (cons 1 (cons 2 (cons 3 nil))))
        ;(debug b)
        ;(debug (car b))
        ;(debug (cdr b))
        ;(if (= 1 2) (+ 2 3) (* 2 2))
        (or false false )
    )
        ";
    let parsed_input = grammar::TermParser::new().parse(input)?;
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    println!("Yep that was all?");
    let evaled = eval(parsed_input.clone())?;
    println!("evaled: {evaled}");
    Ok(())
}
