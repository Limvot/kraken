use std::rc::Rc;
use std::collections::{BTreeSet,BTreeMap};
use std::fmt;

use anyhow::{anyhow,bail,Result};


// This first Simple Lisp really is
//
// No fexprs, no mutation, no continuations, no macros, no strings.
// Int/Bool/Nil/Pair/Symbol/Closure/Prim.
//
// Figuring out GC between a JIT and Rust will be tricky.
// Can start with a like tracing-JIT-into-bytecode
// let's make our own Box, Rc, maybe Arc, Vec too?
// rustonomicon

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub struct ID {
    id: i64
}
impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[derive(Debug)]
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(Rc<Form>, Rc<Form>),
    Closure(Vec<String>, Rc<Form>, Rc<Form>, ID),
    Prim(Prim),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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
fn eval_prim(f: Prim, b: Rc<Form>, a: Option<Rc<Form>>) -> Result<Rc<Form>> {
    Ok(match f {
         Prim::Car => b.car()?,
         Prim::Cdr => b.cdr()?,
         _ => {
             let a = a.unwrap();
             match f {
                 Prim::Add  => Form::new_int(a.int()? + b.int()?),
                 Prim::Sub  => Form::new_int(a.int()? - b.int()?),
                 Prim::Mul  => Form::new_int(a.int()? * b.int()?),
                 Prim::Div  => Form::new_int(a.int()? / b.int()?),
                 Prim::Mod  => Form::new_int(a.int()? % b.int()?),
                 Prim::Cons => Form::new_pair(a, b),
                 Prim::Eq   => Form::new_bool(a.my_eq(&b)),
                 _ => unreachable!(),
             }
         }
     })
}

impl Form {
    fn my_eq(&self, o: &Rc<Form>) -> bool {
        match self {
            Form::Nil                   => o.is_nil(),
            Form::Int(i)                => if let Ok(oi) = o.int()  { *i == oi } else { false },
            Form::Bool(b)               => if let Ok(ob) = o.bool() { *b == ob } else { false },
            Form::Symbol(s)             => if let Ok(os) = o.sym()  {  s == os } else { false },
            Form::Pair(a,b)             => if let Ok((oa,ob)) = o.pair() { a.my_eq(&oa) && b.my_eq(&ob) } else { false },
            Form::Closure(_, _, _, _)   => false,
            Form::Prim(p)               => match &**o { Form::Prim(op) => p == op, _ => false },
        }
    }
    pub fn new_pair(car: Rc<Form>, cdr: Rc<Form>) -> Rc<Form> {
        Rc::new(Form::Pair(car, cdr))
    }
    pub fn new_nil() -> Rc<Form> {
        Rc::new(Form::Nil)
    }
    pub fn new_int(i: i32) -> Rc<Form> {
        Rc::new(Form::Int(i))
    }
    pub fn new_bool(b: bool) -> Rc<Form> {
        Rc::new(Form::Bool(b))
    }
    fn new_closure(params: Vec<String>, env: Rc<Form>, body: Rc<Form>, ctx: &mut Ctx) -> Rc<Form> {
        Rc::new(Form::Closure(params, env, body, ctx.alloc_id()))
    }
    fn truthy(&self) -> bool {
        match self {
            Form::Bool(b) => *b,
            Form::Nil     => false,
            _             => true,
        }
    }
    fn bool(&self) -> Result<bool> {
        match self {
            Form::Bool(b) => Ok(*b),
            _ => Err(anyhow!("bool on not a bool")),
        }
    }
    fn int(&self) -> Result<i32> {
        match self {
            Form::Int(i) => Ok(*i),
            _ => Err(anyhow!("int on not a int")),
        }
    }
    fn prim(&self) -> Result<Prim> {
        match self {
            Form::Prim(p) => Ok(*p),
            _             => Err(anyhow!("prim on not a prim")),
        }
    }
    fn sym(&self) -> Result<&str> {
        match self {
            Form::Symbol(s) => Ok(s),
            _ => Err(anyhow!("sym on not a sym")),
        }
    }
    fn pair(&self) -> Result<(Rc<Form>,Rc<Form>)> {
        match self {
            Form::Pair(car, cdr) => Ok((Rc::clone(car),Rc::clone(cdr))),
            _ => Err(anyhow!("pair on not a pair {self}")),
        }
    }
    fn car(&self) -> Result<Rc<Form>> {
        match self {
            Form::Pair(car, _cdr) => Ok(Rc::clone(car)),
            _ => Err(anyhow!("car on not a pair")),
        }
    }
    fn cdr(&self) -> Result<Rc<Form>> {
        match self {
            Form::Pair(_car, cdr) => Ok(Rc::clone(cdr)),
            _ => Err(anyhow!("cdr on not a pair")),
        }
    }
    fn is_nil(&self) -> bool {
        match self {
            Form::Nil            => true,
            _                    => false,
        }
    }
    pub fn append(&self, x: Rc<Form>) -> Result<Rc<Form>> {
        match self {
            Form::Pair(car, cdr) => cdr.append(x).map(|x| Rc::new(Form::Pair(Rc::clone(car), x))),
            Form::Nil            => Ok(Rc::new(Form::Pair(x, Rc::new(Form::Nil)))),
            _                    => Err(anyhow!("append to not a pair")),
        }
    }
    pub fn root_env() -> Rc<Form> {
        let mut e = Form::new_nil();
        for (s, v) in [
                ("+",    Rc::new(Form::Prim(Prim::Add))),
                ("-",    Rc::new(Form::Prim(Prim::Sub))),
                ("*",    Rc::new(Form::Prim(Prim::Mul))),
                ("/",    Rc::new(Form::Prim(Prim::Div))),
                ("%",    Rc::new(Form::Prim(Prim::Mod))),
                ("cons", Rc::new(Form::Prim(Prim::Cons))),
                ("cdr",  Rc::new(Form::Prim(Prim::Cdr))),
                ("car",  Rc::new(Form::Prim(Prim::Car))),
                ("=",    Rc::new(Form::Prim(Prim::Eq))),
                ("nil",  Form::new_nil()),
        ] {
            e = e.define(s.to_string(), v);
        }
        e
    }
    pub fn lookup(self: &Rc<Self>, s: &str) -> Result<Rc<Form>> {
        let mut e = Rc::clone(self);
        loop {
            let (kv, ne) = e.pair()?;
            let (sp, v) = kv.pair()?;
            if sp.sym()? == s {
                return Ok(v);
            }
            e = ne;
        }
    }
    pub fn define(self: &Rc<Self>, s: String, v: Rc<Form>) -> Rc<Form> {
        Form::new_pair(Form::new_pair(Rc::new(Form::Symbol(s)), v), Rc::clone(self))
    }
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

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Guard       { const_value, side_val:_, side_cont:_, side_id, tbk:_ } => write!(f, "Guard{side_id}({const_value})"),
            Op::Debug                                                          => write!(f, "Debug"),
            Op::Define      { sym }                                            => write!(f, "Define({sym})"),
            Op::Const       ( con )                                            => write!(f, "Const_{con}"),
            Op::Drop                                                           => write!(f, "Drop"),
            Op::Lookup      { sym }                                            => write!(f, "Lookup({sym})"),
            Op::Call        { len, nc:_, nc_id, statik }                       => write!(f, "Call{nc_id}({len},{statik:?})"),
            Op::InlinePrim(prim)                                               => write!(f, "{prim:?}"),
            Op::Tail(len,oid)                                                  => write!(f, "Tail({len},{oid:?})"),
            Op::Return                                                         => write!(f, "Return"),
        }
    }
}
impl Op {
    fn cnst(&self) -> Result<Rc<Form>> {
        match self {
            Op::Const(c) => Ok(Rc::clone(c)),
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
#[derive(Debug)]
enum Op {
    Guard            { const_value: Rc<Form>, side_val: Option<Rc<Form>>, side_cont: Rc<Cont>, side_id: ID, tbk: TraceBookkeeping },
    Debug,
    Define           { sym: String },
    Const            (      Rc<Form> ),
    Drop,
    Lookup           { sym: String },
    Call             { len: usize, statik: Option<ID>, nc: Rc<Cont>, nc_id: ID },
    InlinePrim(Prim),
    Tail(usize,Option<ID>),
    Return,
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

#[derive(Debug)]
struct Ctx {
    id_counter: i64,
    cont_count: BTreeMap<ID, i64>,
    tracing: Option<Trace>,
    traces: BTreeMap<ID, Trace>,
    trace_resume_data: BTreeMap<ID, TraceBookkeeping>,
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
            trace_resume_data: BTreeMap::new(),
        }
    }
    fn alloc_id(&mut self) -> ID {
        self.id_counter += 1;
        ID { id: self.id_counter }
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
    fn trace_call(&mut self, call_len: usize, tmp_stack: &Vec<Rc<Form>>, nc: &Rc<Cont>) -> Option<ID> {

        // Needs to take and use parameters for mid-trace
        //  needs to guard on function called if non-constant
        println!("trace_call call_len={call_len},trace={:?}, tmp_stack {tmp_stack:?}", self.tracing);
        if let Some(trace) = &mut self.tracing {

            let statik = if trace.tbk.stack_const[trace.tbk.stack_const.len()-call_len] {
                // const - TODO: for now, we don't inline but we will want to later (based on what
                // metrics? can we run them simultaniously, heirarchially? with our new approach on
                // prims maybe (heck we may need to go farther, and remove the InlinePrim!)
                match &*tmp_stack[tmp_stack.len()-call_len] {
                    Form::Prim(p) => {
                        if (&trace.tbk.stack_const[trace.tbk.stack_const.len()-call_len..]).iter().all(|x| *x) {
                            trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                            let b = trace.ops[trace.ops.len()-1].cnst().unwrap();
                            let (a,f) = if call_len == 3 {
                                (Some(trace.ops[trace.ops.len()-2].cnst().unwrap()), p)
                            } else { (None, p) };
                            for _ in 0..call_len {
                                trace.ops.push(Op::Drop);
                            }

                            trace.ops.push(Op::Const(eval_prim(*f, b, a).unwrap()));
                            trace.tbk.stack_const.push(true);
                        } else { 
                            trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                            trace.ops.push(Op::InlinePrim(*p));
                            trace.tbk.stack_const.push(false);
                        }

                        return None;
                    },
                    Form::Closure(_ps, _e, _b, id) => {
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
                            self.traces.insert(trace.id, self.tracing.take().unwrap());
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
                self.traces.insert(trace.id, self.tracing.take().unwrap());
                return None;
            } else {
                trace.tbk.stack_const.truncate(trace.tbk.stack_const.len()-call_len);
                self.id_counter += 1; let nc_id = ID { id: self.id_counter }; // HACK - I can't use the method cuz trace is borrowed
                trace.ops.push(Op::Call { len: call_len, statik, nc: Rc::clone(nc), nc_id });
                println!("Ending trace at call!");
                println!("\t{}", trace);
                self.trace_resume_data.insert(nc_id, trace.tbk.clone());
                self.traces.insert(trace.id, self.tracing.take().unwrap());
                return Some(nc_id);
            }
        }
        None
    }
    fn trace_frame(&mut self, syms: &Vec<String>, id: ID) {
        let inline = self.tracing.is_some();
        let entry = self.cont_count.entry(id).or_insert(0);
        println!("tracing call start for {id}, has been called {} times so far", *entry);
        *entry += 1;
        if *entry > 1 && self.tracing.is_none() && self.traces.get(&id).is_none() {
            self.tracing = Some(Trace::new(id, id));
        }

        for s in syms.iter().rev() {
            self.trace_define(s, inline);
        }
        self.trace_drop(inline);
    }
    fn trace_call_end(&mut self, id: ID, follow_on_trace_data: Option<ID>) {
        println!("tracing call end for {id} followon {follow_on_trace_data:?}");
        if let Some(trace) = &mut self.tracing {
            if trace.tbk.func_id == id {
                trace.ops.push(Op::Return);
                println!("Ending trace at end of call!");
                println!("\t{}", trace);
                self.traces.insert(trace.id, self.tracing.take().unwrap());
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
    fn trace_guard<T: Into<Form> + std::fmt::Debug>(&mut self, value: T, other: impl Fn()->(Option<Rc<Form>>,Rc<Cont>)) {
        println!("Tracing guard {value:?}");
        if let Some(trace) = &mut self.tracing {
            let (side_val, side_cont) = other();
            self.id_counter += 1; let side_id = ID { id: self.id_counter }; // HACK - I can't use the method cuz trace is borrowed
            trace.ops.push(Op::Guard { const_value: Rc::new(value.into()), side_val, side_cont, side_id, tbk: trace.tbk.clone() });
        }
    }
    fn trace_debug(&mut self) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Debug);
        }
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
    fn trace_lookup(&mut self, s: &str, f: &Rc<Form>) {
        if let Some(trace) = &mut self.tracing {
            // constant depends on which env, and I think this is the only spot that cares for
            // closure jit vs lambda jit
            if trace.tbk.defined_names.contains(s) {
                trace.ops.push(Op::Lookup { sym: s.to_owned() });
                trace.tbk.stack_const.push(false);
            } else {
                trace.ops.push(Op::Const(Rc::clone(f)));
                trace.tbk.stack_const.push(true);
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

    fn trace_constant(&mut self, c: &Rc<Form>) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Const(Rc::clone(c)));
            trace.tbk.stack_const.push(true);
        }
    }
    fn trace_lambda(&mut self, _params: &[String], _e: &Rc<Form>, _body: &Rc<Form>) {
        if let Some(_trace) = &mut self.tracing {
            // TODO
            // kinda both also
            unimplemented!("trace lambda");
        }
    }

    // returns f, e, c for interp
    fn execute_trace_if_exists(&mut self,
                                  id: ID, 
                                  e: &Rc<Form>,
                                  tmp_stack: &mut Vec<Rc<Form>>, 
                                  ret_stack: &mut Vec<(Rc<Form>, Rc<Cont>, Option<ID>)>) -> Result<Option<(Rc<Form>, Rc<Form>, Cont)>> {
        if self.trace_running() {
            println!("Not playing back trace because recording trace");
            return Ok(None); // can't trace while running a trace for now (we don't inline now anyway),
                             // in the future it should just tack on the opcodes while jugging the proper 
                             // bookkeeping stacks
        }
        if let Some(mut trace) = self.traces.get(&id) {
            println!("Starting trace playback");
            let mut e = Rc::clone(e);
            loop {
                println!("Running trace {trace}, \n\ttmp_stack:{tmp_stack:?}");
                for b in trace.ops.iter() {
                    match b {
                        Op::Guard       { const_value, side_val, side_cont, side_id, tbk } => {
                            println!("Guard(op) {const_value}");
                            if !const_value.my_eq(tmp_stack.last().unwrap()) {
                                if let Some(new_trace) = self.traces.get(side_id) {
                                    if side_val.is_some() {
                                        tmp_stack.pop().unwrap();
                                    }
                                    println!("\tchaining trace to side trace");
                                    trace = new_trace;
                                    break; // break out of this trace and let infinate loop spin
                                } else {
                                    println!("\tending playback b/c failed guard");
                                    assert!(self.tracing.is_none());
                                    let mut ntrace = Trace::follow_on(*side_id,tbk.clone());
                                    if let Some(side_val) = side_val {
                                        *tmp_stack.last_mut().unwrap() = Rc::clone(side_val);
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
                            e = e.define(sym.clone(), v);
                        }
                        Op::Const       ( con )                                            => {
                            println!("Const(op) {con}");
                            tmp_stack.push(Rc::clone(con));
                        }
                        Op::Drop                                                           => {
                            println!("Drop(op) {}", tmp_stack.last().unwrap());
                            tmp_stack.pop().unwrap();
                        }
                        Op::Lookup      { sym }                                            => {
                            println!("Lookup(op) {sym}");
                            tmp_stack.push(e.lookup(sym)?);
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
                                if let Some(new_trace) = self.traces.get(static_call_id) {
                                    ret_stack.push((Rc::clone(&e), (*nc).clone(), Some(*nc_id)));
                                    println!("\tchaining to call trace b/c Call with statik");
                                    trace = new_trace;
                                    break; // break out of this trace and let infinate loop spin
                                }
                            }
                            match &*Rc::clone(&tmp_stack[tmp_stack.len()-*len]) {
                                Form::Closure(ps, ie, b, call_id) => {
                                    if ps.len() != *len-1 {
                                        bail!("arguments length doesn't match");
                                    }
                                    ret_stack.push((Rc::clone(&e), (*nc).clone(), Some(*nc_id)));
                                    if let Some(new_trace) = self.traces.get(call_id) {
                                        println!("\tchaining to call trace b/c Call with dyamic but traced");
                                        e = Rc::clone(ie);
                                        trace = new_trace;
                                        break; // break out of this trace and let infinate loop spin
                                    } else {
                                        return Ok(Some((Rc::clone(&b), Rc::clone(ie), Cont::Frame { syms: ps.clone(), id: *call_id, c: Rc::new(Cont::Eval { c: Rc::new(Cont::Ret { id: *call_id }) }) })));
                                    }
                                },
                                Form::Prim(p) => {
                                    let b = tmp_stack.pop().unwrap();
                                    let a = if *len == 2 { None } else { assert!(*len == 3); Some(tmp_stack.pop().unwrap()) };
                                    let result = eval_prim(*p, b, a)?;
                                    if let Some(new_trace) = self.traces.get(nc_id) {
                                        *tmp_stack.last_mut().unwrap() = result; // for the prim itself
                                        println!("\tchaining to ret trace b/c Call with dyamic but primitive and next traced");
                                        trace = new_trace;
                                        break; // break out of this trace and let infinate loop spin
                                    } else {
                                        println!("\tstopping playback to ret b/c Call with dyamic but primitive and next not-traced");
                                        tmp_stack.pop().unwrap(); // for the prim itself
                                        return Ok(Some((result, e, (**nc).clone())));
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
                            let (e, nc, resume_data) = ret_stack.pop().unwrap();
                            if let Some(resume_id) = resume_data {
                                if let Some(new_trace) = self.traces.get(&resume_id) {
                                    println!("\tchaining to return trace b/c Return {resume_id} - {new_trace:?}");
                                    trace = new_trace;
                                    break; // break out of this trace and let infinate loop spin
                                }
                            }
                            println!("\tending playback b/c Return, attempting to resume trace");
                            self.try_resume_trace(resume_data);
                            return Ok(Some((tmp_stack.pop().unwrap(), e, (*nc).clone())));
                        }
                    }
                }
            }
        } else {
            Ok(None)
        }
    }
}
#[derive(Clone,Debug)]
enum Cont {
    MetaRet,
    Ret   {                        id: ID,                      },
    Eval  {                                         c: Rc<Cont> }, 
    Prim  { s: &'static str,       to_go: Rc<Form>, c: Rc<Cont> },
    Call  { n: usize,              to_go: Rc<Form>, c: Rc<Cont> },
    Frame { syms: Vec<String>,     id: ID,          c: Rc<Cont> },
}
impl Cont {
    fn is_ret(&self) -> bool {
        match self {
            Cont::Ret { id: _ } => true,
            _                   => false,
        }
    }
}

pub fn eval(f: Rc<Form>) -> Result<Rc<Form>> {
    let mut ctx = Ctx::new();
    let mut f = f;
    let mut e = Form::root_env();
    let mut c = Cont::Eval { c: Rc::new(Cont::MetaRet) };

    let mut ret_stack: Vec<(Rc<Form>, Rc<Cont>, Option<ID>)> = vec![];
    let mut tmp_stack: Vec<Rc<Form>> = vec![];

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
                            ctx.trace_guard(true, || (Some(Rc::clone(&els)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            ctx.trace_drop(true);
                            f = thn;
                        } else {
                            ctx.trace_guard(false, ||(Some(Rc::clone(&thn)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            ctx.trace_drop(true);
                            f = els;
                        }
                        c = Cont::Eval { c: nc };
                    },
                    "or" => {
                        let other = to_go.car()?;
                        if !f.truthy() {
                            ctx.trace_guard(false, || (None, nc.clone()));
                            ctx.trace_drop(true);
                            f = other;
                            c = Cont::Eval { c: nc };
                        } else {
                            ctx.trace_guard(true, || (Some(Rc::clone(&other)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            c = (*nc).clone();
                        }
                    },
                    "and" => {
                        let other = to_go.car()?;
                        if f.truthy() {
                            ctx.trace_guard(true, || (None, nc.clone()));
                            ctx.trace_drop(true);
                            f = other;
                            c = Cont::Eval { c: nc };
                        } else {
                            ctx.trace_guard(false, || (Some(Rc::clone(&other)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            c = (*nc).clone();
                        }
                    },
                    "begin" => {
                        if to_go.is_nil() {
                            c = (*nc).clone();
                        } else {
                            ctx.trace_drop(true);
                            f = to_go.car()?;
                            c = Cont::Eval { c: Rc::new(Cont::Prim { s: "begin", to_go: to_go.cdr()?, c: nc }) };
                        }
                    },
                    "debug" => {
                        println!("Debug: {f}");
                        ctx.trace_debug();
                        c = (*nc).clone();
                    },
                    "define" => {
                        let sym = to_go.sym()?.to_string();
                        ctx.trace_define(&sym, true);
                        e = e.define(sym, Rc::clone(&f));
                        c = (*nc).clone();
                    },
                    _ => {
                        panic!("bad prim {s}");
                    }
                }
            },
            Cont::Ret {        id,       } => {
                let (ne, nc, resume_data) = ret_stack.pop().unwrap();
                ctx.trace_call_end(id, resume_data);
                e = ne;
                if let Some(nc_id) = resume_data {
                    tmp_stack.push(f); // ugly dance pt 1
                    if let Some((fp, ep, cp)) = ctx.execute_trace_if_exists(nc_id, &e, &mut tmp_stack, &mut ret_stack)? {
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
                    match &*Rc::clone(&tmp_stack[tmp_stack.len()-n]) {
                        Form::Closure(ps, ie, b, id) => {
                            if ps.len() != n-1 {
                                bail!("arguments length doesn't match");
                            }
                            ret_stack.push((Rc::clone(&e), nc, resume_data));
                            if let Some((fp, ep, cp)) = ctx.execute_trace_if_exists(*id, ie, &mut tmp_stack, &mut ret_stack)? {
                                f = fp;
                                e = ep;
                                c = cp;
                                println!("After executing trace, f={f}, tmp_stack is {tmp_stack:?}");
                            } else {
                                println!("replacing {e} with {ie}");
                                e = Rc::clone(ie);
                                c = Cont::Frame { syms: ps.clone(), id: *id, c: Rc::new(Cont::Eval { c: Rc::new(Cont::Ret { id: *id }) }) };
                                f = Rc::clone(&b);
                            }
                        },
                        Form::Prim(p) => {
                            let b = tmp_stack.pop().unwrap();
                            let a = if n == 2 { None } else { assert!(n == 3); Some(tmp_stack.pop().unwrap()) };
                            f = eval_prim(*p, b, a)?;
                            tmp_stack.pop().unwrap(); // for the prim itself
                            c = (*nc).clone();
                        },
                        ncomb => {
                            println!("Current stack is {tmp_stack:?}");
                            bail!("tried to call a non-comb {ncomb}")
                        },
                    }
                } else {
                    f = to_go.car()?;
                    c = Cont::Eval { c: Rc::new(Cont::Call { n: n+1, to_go: to_go.cdr()?, c: nc }) };
                }
            }
            Cont::Frame { syms, id, c: nc } => {
                ctx.trace_frame(&syms, id);
                for s in syms.into_iter().rev() {
                    e = e.define(s, tmp_stack.pop().unwrap());
                }
                tmp_stack.pop().unwrap(); // for the func value
                c = (*nc).clone();
            }
            Cont::Eval { c: nc } => {
                let tmp = f;
                match &*tmp {
                    Form::Symbol(s) => {
                        f = e.lookup(s)?;
                        ctx.trace_lookup(s, &f);
                        c = (*nc).clone();
                    },
                    Form::Pair(car, cdr) => {
                        match &**car {
                            Form::Symbol(s) if s == "if" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "if", to_go: cdr.cdr()?, c: nc }) };
                            }
                            // and/or has to short-circut, so special form
                            // just like Scheme (bad ;) )
                            Form::Symbol(s) if s == "or" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "or", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s) if s == "and" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "and", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s) if s == "begin" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "begin", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s) if s == "debug" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "debug", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s) if s == "define" => {
                                // note the swap, evaluating the second not the first (define a value..)
                                f = cdr.cdr()?.car()?;
                                c = Cont::Eval { c: Rc::new(Cont::Prim { s: "define", to_go: cdr.car()?, c: nc }) };
                            }
                            Form::Symbol(s) if s == "quote" => {
                                f = cdr.car()?;
                                ctx.trace_constant(&f);
                                c = (*nc).clone();
                            }
                            // (lambda (a b) body)
                            Form::Symbol(s) if s == "lambda" => {
                                let mut params_vec = vec![];
                                let mut params = cdr.car()?;
                                while let Ok((ncar, ncdr)) = params.pair() {
                                    params_vec.push(ncar.sym()?.to_string());
                                    params = ncdr;
                                }
                                let body = cdr.cdr()?.car()?;
                                ctx.trace_lambda(&params_vec, &e, &body);
                                f = Form::new_closure(params_vec, Rc::clone(&e), body, &mut ctx);
                                c = (*nc).clone();
                            }
                            _ => {
                                f = Rc::clone(car);
                                c = Cont::Eval { c: Rc::new(Cont::Call { n: 1, to_go: Rc::clone(cdr), c: nc }) };
                            }
                        }
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

impl From<String> for Form { fn from(item: String) -> Self { Form::Symbol(item) } }
impl From<&str>   for Form { fn from(item: &str)   -> Self { Form::Symbol(item.to_owned()) } }
impl From<i32>    for Form { fn from(item: i32)    -> Self { Form::Int(item) } }
impl From<bool>   for Form { fn from(item: bool)   -> Self { Form::Bool(item) } }
impl<A: Into<Form>, B: Into<Form>> From<(A, B)> for Form {
    fn from(item: (A, B)) -> Self {
        Form::Pair(Rc::new(item.0.into()), Rc::new(item.1.into()))
    }
}

impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Form::Nil                   => write!(f, "nil"),
            Form::Int(i)                => write!(f, "{i}"),
            Form::Bool(b)               => write!(f, "{b}"),
            Form::Symbol(s)             => write!(f, "'{s}"),
            Form::Pair(car, cdr)   => {
                write!(f, "({}", car)?;
                let mut traverse: Rc<Form> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        Form::Pair(ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        Form::Nil => {
                            write!(f, ")")?;
                            return Ok(());
                        },
                        x => {
                            write!(f, ". {x})")?;
                            return Ok(());
                        },
                    }
                }
            },
            Form::Closure(params, _inner_env, _code, id) => {
                write!(f, "<closure{} {:?}>", id, params)
            }
            Form::Prim(p) => {
                match p {
                    Prim::Add => write!(f, "+"),
                    Prim::Sub => write!(f, "-"),
                    Prim::Mul => write!(f, "*"),
                    Prim::Div => write!(f, "/"),
                    Prim::Mod => write!(f, "%"),
                    Prim::Cons => write!(f, "cons"),
                    Prim::Car => write!(f, "car"),
                    Prim::Cdr => write!(f, "cdr"),
                    Prim::Eq  => write!(f, "="),
                }
            }
        }
    }
}
