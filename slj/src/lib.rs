use std::rc::Rc;
use std::collections::BTreeMap;
use std::fmt;
use std::cell::RefCell;

use anyhow::{anyhow,bail,Result};


// This first Simple Lisp really is
//
// No fexprs, no mutation, no continuations, no macros, no strings.
// Int/Bool/Nil/Pair/Symbol/Closure/Prim.
//
// Figuring out GC between a JIT and Rust will be tricky.
// Can start with a like tracing-JIT-into-bytecode
// Replcing Env with pairs or somesuch would make JIT interop easier I think, because we wouldn't
// have to deal with refcell, but then we would again for mutation.
// Maybe doing all allocation on the Rust side with #[no_mangle] functions would make things easier
// mmmm no let's make our own Box, Rc, maybe Arc, Vec too?
// rustonomicon

// What if we're cute and use the ID
// like we will eventually use value tagging
// like, use the same encoding
// interned symbols and all
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
    Closure(Vec<String>, Rc<RefCell<Env>>, Rc<Form>, ID),
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
    fn new_pair(car: Rc<Form>, cdr: Rc<Form>) -> Rc<Form> {
        Rc::new(Form::Pair(car, cdr))
    }
    fn new_nil() -> Rc<Form> {
        Rc::new(Form::Nil)
    }
    fn new_int(i: i32) -> Rc<Form> {
        Rc::new(Form::Int(i))
    }
    fn new_bool(b: bool) -> Rc<Form> {
        Rc::new(Form::Bool(b))
    }
    fn new_closure(params: Vec<String>, env: Rc<RefCell<Env>>, body: Rc<Form>, ctx: &mut Ctx) -> Rc<Form> {
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
            _ => Err(anyhow!("pair on not a pair")),
        }
    }
    fn car(&self) -> Result<Rc<Form>> {
        match self {
            Form::Pair(car, cdr) => Ok(Rc::clone(car)),
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
}

#[derive(Debug)]
pub struct Env {
    u: Option<Rc<RefCell<Env>>>,
    // split  this into
    // BTreeMap<String, usize>
    // Vec<usize> so that traced code can refer by index
    m: BTreeMap<String, Rc<Form>>
}
impl Env {
    pub fn root_env() -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            u: None,
            m: [
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
            ].into_iter().map(|(s,p)| (s.to_owned(), p)).collect()
        }))
    }
    pub fn chain(o: &Rc<RefCell<Env>>) -> Rc<RefCell<Env>> {
        Rc::new(RefCell::new(Env {
            u: Some(Rc::clone(o)),
            m: BTreeMap::new(),
        }))
    }
    pub fn lookup(&self, s: &str) -> Result<Rc<Form>> {
        if let Some(r) = self.m.get(s) {
            Ok(Rc::clone(r))
        } else if let Some(u) = &self.u {
            u.borrow().lookup(s)
        } else {
            bail!("lookup of {s} failed")
        }
    }
    pub fn define(&mut self, s: String, v: Rc<Form>) {
        // no mutation, shadowing in inner scope ok
        assert!(!self.m.contains_key(&s));
        self.m.insert(s, v);
    }
}

#[derive(Debug)]
enum Op {
    Guard { const_value: Rc<Form>, side: (Option<Rc<Form>>, Rc<Cont>) },
    Debug,
    Define     { sym: String },
    Const      { con: Rc<Form> },
    Lookup     { sym: String },
    InlinePrim { prim: Prim, params: Vec<usize> },
    Call       { params: Vec<usize>, nc: Rc<Cont> },
    Loop(Vec<usize>),
    Return,
}
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Guard       { const_value, side } => write!(f, "Guard({const_value})"),
            Op::Debug                             => write!(f, "Debug"),
            Op::Define      { sym }               => write!(f, "Define({sym})"),
            Op::Const       { con }               => write!(f, "Const_{con}"),
            Op::Lookup      { sym }               => write!(f, "Lookup({sym})"),
            Op::InlinePrim  { prim, params }      => write!(f, "{:?}({:?})", prim, params),
            Op::Call        { params, nc }        => write!(f, "Call({:?})", params),
            Op::Loop(params)                      => write!(f, "Loop({:?})", params),
            Op::Return                            => write!(f, "Return"),
        }
    }
}
#[derive(Debug)]
struct Trace {
    id: ID,
    // needs to track which are constants
    ops: Vec<Op>,
    param_stack: Vec<usize>,
}
impl Trace {
    fn new(id: ID) -> Self {
        Trace { id, ops: vec![], param_stack: vec![] }
    }
}
impl fmt::Display for Trace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Trace for {} [", self.id)?;
        for op in &self.ops {
            write!(f, " {}", op)?;
        }
        write!(f, " ]")?;
        if !self.param_stack.is_empty() {
            write!(f, "[")?;
            for s in &self.param_stack {
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
    func_calls: BTreeMap<ID, i64>,
    tracing: Option<Trace>,
    traces: BTreeMap<ID, Trace>,
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
            func_calls: BTreeMap::new(),
            tracing: None,
            traces: BTreeMap::new(),
        }
    }
    fn alloc_id(&mut self) -> ID {
        self.id_counter += 1;
        ID { id: self.id_counter }
    }
    fn trace_running(&self) -> bool { self.tracing.is_some() }

    fn trace_call_bit(&mut self) {
        if let Some(trace) = &mut self.tracing {
            trace.param_stack.push(trace.ops.len()-1);
        }
    }
    // Though I guess that means call start should recieve the parameters
    //  also, for like variables, it should guard on what function
    //      if dynamic, interacts with the constant tracking
    //  7 options
    //      - not tracing, closure        - do stats
    //      - not tracing, prim           - do nothing
    //      - tracing, Constant Prim      - inline prim
    //      - tracing, Constant Closure   - inline call
    //      - tracing, Static, tail-self  - emit loop
    //      - tracing, Static,nontail-self- emit call
    //      - tracing, Dynamic, other     - emit call
    //
    //      inline call is slightly tricky, have to add our own Env accounting
    //      emit call is trickier, because we either have to stop or postpone tracing
    //          use return stack, and count the post-return as it's own trace?
    //              weirder, but would eventually jive with continuations better?
    //              eh for now use trace stack in ctx and cont stack out, have them match?
    fn trace_call_start(&mut self, arg_len: usize, id: Option<ID>, nc: &Rc<Cont>) {

        // Needs to take and use parameters for mid-trace
        //  needs to guard on function called if non-constant

        if let Some(id) = id {
            let entry = self.func_calls.entry(id).or_insert(0);
            println!("tracing call start for {id}, has been called {} times so far", *entry);
            *entry += 1;
            if *entry > 1 && self.tracing.is_none() && self.traces.get(&id).is_none() {
                self.tracing = Some(Trace::new(id));
                return; // don't record self, of course
            }
        }
        if let Some(trace) = &mut self.tracing {
            let f_params = trace.param_stack.split_off(trace.param_stack.len()-arg_len-1); // include function
            if let Some(id) = id {
                if trace.id == id {
                    // check for tail recursion
                    if trace.param_stack.is_empty() {
                        trace.ops.push(Op::Loop(f_params));
                        println!("Ending trace at tail recursive call!");
                        println!("\t{}", trace);
                        self.traces.insert(id, self.tracing.take().unwrap());
                        return;
                    } else {
                        // call, and also we have to suspend tracing?
                        // can treat same as dynamic call if suspend, same thing really
                    }
                }
            }
            // either inline (prim/closure) or dynamic call
            //trace.ops.push(Op::Call(f_params));
            //InlinePrim { prim: Prim, params: Vec<usize> },
            //Call       { params: Vec<usize>, nc: Rc<Cont> },
        }
    }
    fn trace_call_end(&mut self, id: ID) {
        // associate with it or something
        println!("tracing call end for {id}");
        if let Some(trace) = &mut self.tracing {
            if trace.id == id {
                trace.ops.push(Op::Return);
                println!("Ending trace at end of call!");
                println!("\t{}", trace);
                self.traces.insert(id, self.tracing.take().unwrap());
            }
        }
    }
    fn trace_guard<T: Into<Form> + std::fmt::Debug >(&mut self, value: T, other: impl Fn()->(Option<Rc<Form>>,Rc<Cont>)) {
        println!("Tracing guard {value:?}");
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Guard { const_value: Rc::new(value.into()), side: other() });
        }
    }
    fn trace_debug(&mut self) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Debug);
        }
    }
    fn trace_define(&mut self, sym: &str) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Define { sym: sym.to_owned() });
        }
    }
    fn trace_lookup(&mut self, s: &str) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Lookup { sym: s.to_owned() });
            // constant depends on which env
        }
    }
    fn trace_constant(&mut self, c: &Rc<Form>) {
        if let Some(trace) = &mut self.tracing {
            trace.ops.push(Op::Const { con: Rc::clone(c) });
        }
    }
    fn trace_lambda(&mut self, params: &[String], e: &Rc<RefCell<Env>>, body: &Rc<Form>) {
        if let Some(trace) = &mut self.tracing {
            // TODO
        }
    }
}
#[derive(Clone,Debug)]
enum Cont {
    MetaRet,
    Ret  {                        id: ID,                      },
    Eval {                                         c: Rc<Cont> }, 
    Prim { s: &'static str,       to_go: Rc<Form>, c: Rc<Cont> },
    Call {                        to_go: Rc<Form>, c: Rc<Cont> },
}

pub fn eval(f: Rc<Form>) -> Result<Rc<Form>> {
    let mut ctx = Ctx::new();
    let mut f = f;
    let mut e = Env::root_env();
    let mut c = Cont::Eval { c: Rc::new(Cont::MetaRet) };

    let mut ret_stack: Vec<(Rc<RefCell<Env>>, Rc<Cont>)> = vec![];
    let mut tmp_stack: Vec<Vec<Rc<Form>>> = vec![];

    loop {
        match c {
            Cont::MetaRet => {
                println!("Ctx was {ctx}");
                assert!(!ctx.trace_running());
                return Ok(f);
            }
            Cont::Ret {        id,       } => {
                let (ne, nc) = ret_stack.pop().unwrap();
                ctx.trace_call_end(id);
                e = ne;
                c = (*nc).clone();
            },
            Cont::Prim { s, to_go, c: nc } => {
                match s {
                    "if" => {
                        let thn = to_go.car()?;
                        let els = to_go.cdr()?.car()?;
                        if f.truthy() {
                            ctx.trace_guard(true, || (Some(Rc::clone(&els)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            f = thn;
                        } else {
                            ctx.trace_guard(false, ||(Some(Rc::clone(&thn)), Rc::new(Cont::Eval { c: Rc::clone(&nc) })));
                            f = els;
                        }
                        c = Cont::Eval { c: nc };
                    },
                    "or" => {
                        let other = to_go.car()?;
                        if !f.truthy() {
                            ctx.trace_guard(false, || (None, nc.clone()));
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
                        ctx.trace_define(&sym);
                        e.borrow_mut().define(sym, Rc::clone(&f));
                        c = (*nc).clone();
                    },
                    _ => {
                        panic!("bad prim {s}");
                    }
                }
            },
            // If we pull out temporaries from Cont::Call &
            // change Ret to be bare, and then put the temps
            // and the return continuation on a stack Frame
            // outside the loop, then the built continuation is
            // exactly what the trace will need to continue,
            // and the stack can store the trace the continuation
            // is a continuation of also, for tracking/tracing
            //
            // The trace will also have to figure out it's representation
            // for temps vs the index offsets currently (or maybe go through
            // offsets back to (now pruned, optimized) stack? is it the offsets that aren't
            // constants?)
            //
            // Actually, I think we can move all computation into Wasm-Esque bytecode generation
            // in the trace, with the trace functions returning the computed values and passed in
            // &mut stack? Then optimization is walking the trace backwards, basically
            // re-linearizeing the induced tree structure, swapping out consts for sub-trees.
            // I think a Wasm like bytecode would be easy to compile to wasm, and should be easy
            // to compile w/ cranelyft (I mean, they do) but also just because abstract interp of a
            // stack machine should be quite easy, right?
            Cont::Call { to_go, c: nc } => {
                let evaled: &mut Vec<Rc<Form>> = tmp_stack.last_mut().unwrap();
                ctx.trace_call_bit();
                evaled.push(f);
                if to_go.is_nil() {
                    let evaled = tmp_stack.pop().unwrap();
                    // do call
                    let arg_len = evaled.len() - 1;
                    let mut evaled_iter = evaled.into_iter();
                    let comb = evaled_iter.next().unwrap();
                    match &*comb {
                        Form::Closure(ps, ie, b, id) => {
                            if ps.len() != arg_len {
                                bail!("arguments length doesn't match");
                            }
                            let new_env = Env::chain(&ie);
                            for (name, value) in ps.iter().zip(evaled_iter) {
                                new_env.borrow_mut().define(name.to_string(), value);
                            }
                            ctx.trace_call_start(arg_len, Some(*id), &nc);
                            ret_stack.push((Rc::clone(&e), nc));
                            c = Cont::Eval { c: Rc::new(Cont::Ret { id: *id }) };
                            f = Rc::clone(&b);
                            e = new_env;
                        },
                        Form::Prim(p) => {
                            ctx.trace_call_start(arg_len, None, &nc);
                            let a = evaled_iter.next().unwrap();
                            f = match comb.prim().unwrap() {
                                Prim::Car => a.car()?,
                                Prim::Cdr => a.cdr()?,
                                _ => {
                                    let b = evaled_iter.next().unwrap();
                                    match comb.prim().unwrap() {
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
                            };
                            c = (*nc).clone();
                        },
                        _ => {
                            bail!("tried to call a non-comb {}", comb)
                        },
                    }
                } else {
                    f = to_go.car()?;
                    c = Cont::Eval { c: Rc::new(Cont::Call { to_go: to_go.cdr()?, c: nc }) };
                }
            }
            Cont::Eval { c: nc } => {
                let tmp = f;
                match &*tmp {
                    Form::Symbol(s) => {
                        ctx.trace_lookup(s);
                        f = e.borrow().lookup(s)?;
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
                            // This is a fast and loose ~simple lisp~, so just go for it
                            // and can have convention that this is always top levelish
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
                                // Later on, the id of the closure should maybe be augmented
                                // or replaced with the id of the code it was made out of?
                                ctx.trace_lambda(&params_vec, &e, &body);
                                f = Form::new_closure(params_vec, Rc::clone(&e), body, &mut ctx);
                                c = (*nc).clone();
                            }
                            _ => {
                                f = Rc::clone(car);
                                tmp_stack.push(vec![]);
                                c = Cont::Eval { c: Rc::new(Cont::Call { to_go: Rc::clone(cdr), c: nc }) };
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

// optimized as a function based off side table of id keyed -> opt
// that id might be nice for debugging too
// Symbol ID's could actually be used for environment lookups
//  this is just interning
// todo, strings not symbols?
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
            Form::Closure(params, inner_env, code, id) => {
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
