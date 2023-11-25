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
// mmmm no let's make our own Box, Rc, maybe Arc
// rustonomicon

// What if we're cute and use the ID
// like we will eventually use value tagging
// like, use the same encoding
// interned symbols and all
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
pub struct ID {
    id: i64
}

#[derive(Debug)]
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String, RefCell<Option<ID>>),
    Pair(Rc<Form>, Rc<Form>, RefCell<Option<ID>>),
    Closure(Vec<String>, Rc<RefCell<Env>>, Rc<Form>, RefCell<Option<ID>>),
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
            Form::Nil => o.is_nil(),
            Form::Int(i) => if let Ok(oi) = o.int() { *i == oi } else { false },
            Form::Bool(b) => if let Ok(ob) = o.bool() { *b == ob } else { false },
            Form::Symbol(s, _id) => if let Ok(os) = o.sym() { s == os } else { false },
            Form::Pair(a,b,_id) => if let Ok((oa,ob)) = o.pair() { a.my_eq(&oa) && b.my_eq(&ob) } else { false },
            Form::Closure(_, _, _, _) => false,
            Form::Prim(p) => match &**o { Form::Prim(op) => p == op, _ => false },
        }
    }
    fn new_pair(car: Rc<Form>, cdr: Rc<Form>) -> Rc<Form> {
        Rc::new(Form::Pair(car, cdr, RefCell::new(None)))
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
    fn new_closure(params: Vec<String>, env: Rc<RefCell<Env>>, body: Rc<Form>) -> Rc<Form> {
        Rc::new(Form::Closure(params, env, body, RefCell::new(None)))
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
            Form::Symbol(s, _id) => Ok(s),
            _ => Err(anyhow!("sym on not a sym")),
        }
    }
    fn pair(&self) -> Result<(Rc<Form>,Rc<Form>)> {
        match self {
            Form::Pair(car, cdr, _id) => Ok((Rc::clone(car),Rc::clone(cdr))),
            _ => Err(anyhow!("pair on not a pair")),
        }
    }
    fn car(&self) -> Result<Rc<Form>> {
        match self {
            Form::Pair(car, _cdr, _id) => Ok(Rc::clone(car)),
            _ => Err(anyhow!("car on not a pair")),
        }
    }
    fn cdr(&self) -> Result<Rc<Form>> {
        match self {
            Form::Pair(_car, cdr, _id) => Ok(Rc::clone(cdr)),
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
            Form::Pair(car, cdr, _id) => cdr.append(x).map(|x| Rc::new(Form::Pair(Rc::clone(car), x, RefCell::new(None)))),
            Form::Nil            => Ok(Rc::new(Form::Pair(x, Rc::new(Form::Nil), RefCell::new(None)))),
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
                ("+", Rc::new(Form::Prim(Prim::Add))),
                ("-", Rc::new(Form::Prim(Prim::Sub))),
                ("*", Rc::new(Form::Prim(Prim::Mul))),
                ("/", Rc::new(Form::Prim(Prim::Div))),
                ("%", Rc::new(Form::Prim(Prim::Mod))),
                ("cons", Rc::new(Form::Prim(Prim::Cons))),
                ("cdr", Rc::new(Form::Prim(Prim::Cdr))),
                ("car", Rc::new(Form::Prim(Prim::Car))),
                ("=", Rc::new(Form::Prim(Prim::Eq))),
                ("nil", Form::new_nil()),
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
struct Trace {
    id: ID,
}
impl Trace {
    fn new(id: ID) -> Self {
        Trace { id }
    }
}

#[derive(Debug)]
struct Ctx {
    id_counter: i64,
    func_calls: BTreeMap<ID, i64>,
    tracing: Option<Trace>,
}
impl Ctx {
    fn new() -> Ctx {
        Ctx {
            id_counter: 0,
            func_calls: BTreeMap::new(),
            tracing: None,
        }
    }
    fn alloc_id(&mut self) -> ID {
        self.id_counter += 1;
        ID { id: self.id_counter }
    }
    fn trace_call_start(&mut self, id: &RefCell<Option<ID>>) {
        // shenanigins for controlling the guard
        {
            if id.borrow().is_none() {
                let new_id = self.alloc_id();
                id.replace(Some(new_id));
            }
        }
        let id = id.borrow().unwrap();
        let entry = self.func_calls.entry(id).or_insert(0);
        *entry += 1;
        if *entry > 10 && self.tracing.is_none() {
            self.tracing = Some(Trace::new(id));
        }
    }
    fn trace_call_end(&mut self, id: &RefCell<Option<ID>>) {
        let id = { *id.borrow() };
        // associate with it or something
    }
}
enum Cont {
    MetaRet,
    Ret  { e: Rc<RefCell<Env>>,                    c: Box<Cont> },
    Eval {                                         c: Box<Cont> }, 
    Prim { s: &'static str,       to_go: Rc<Form>, c: Box<Cont> },
    Call { evaled: Vec<Rc<Form>>, to_go: Rc<Form>, c: Box<Cont> },
}

pub fn eval(f: Rc<Form>) -> Result<Rc<Form>> {
    let mut ctx = Ctx::new();
    let mut f = f;
    let mut e = Env::root_env();
    let mut c = Cont::Eval { c: Box::new(Cont::MetaRet) };

    loop {
        match c {
            Cont::MetaRet => {
                println!("Ctx were {ctx:?}");
                return Ok(f);
            }
            Cont::Ret { e: ne, c: nc } => {
                e = ne;
                c = *nc;
            },
            Cont::Prim { s, to_go, c: nc } => {
                match s {
                    "if" => {
                        if f.truthy() {
                            f = to_go.car()?;
                        } else {
                            f = to_go.cdr()?.car()?;
                        }
                        c = Cont::Eval { c: nc };
                    },
                    "or" => {
                        if !f.truthy() {
                            f = to_go.car()?;
                            c = Cont::Eval { c: nc };
                        } else {
                            c = *nc;
                        }
                    },
                    "and" => {
                        if f.truthy() {
                            f = to_go.car()?;
                            c = Cont::Eval { c: nc };
                        } else {
                            c = *nc;
                        }
                    },
                    "begin" => {
                        if to_go.is_nil() {
                            c = *nc;
                        } else {
                            f = to_go.car()?;
                            c = Cont::Eval { c: Box::new(Cont::Prim { s: "begin", to_go: to_go.cdr()?, c: nc }) };
                        }
                    },
                    "debug" => {
                        println!("Debug: {f}");
                        c = *nc;
                    },
                    "define" => {
                        e.borrow_mut().define(to_go.sym()?.to_string(), Rc::clone(&f));
                        c = *nc;
                    },
                    _ => {
                        panic!("bad prim {s}");
                    }
                }
            },
            Cont::Call { mut evaled, to_go, c: nc } => {
                evaled.push(f);
                if to_go.is_nil() {
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
                            ctx.trace_call_start(id);
                            c = Cont::Eval { c: Box::new(Cont::Ret { e: Rc::clone(&e), c: nc }) };
                            f = Rc::clone(&b);
                            e = new_env;
                        },
                        Form::Prim(p) => {
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
                            c = *nc;
                        },
                        _ => {
                            bail!("tried to call a non-comb {}", comb)
                        },
                    }
                } else {
                    f = to_go.car()?;
                    c = Cont::Eval { c: Box::new(Cont::Call { evaled, to_go: to_go.cdr()?, c: nc }) };
                }
            }
            Cont::Eval { c: nc } => {
                let tmp = f;
                match &*tmp {
                    Form::Symbol(s, _id) => {
                        f = e.borrow().lookup(s)?;
                        c = *nc;
                    },
                    Form::Pair(car, cdr, _id) => {
                        match &**car {
                            Form::Symbol(s, _id) if s == "if" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "if", to_go: cdr.cdr()?, c: nc }) };
                            }
                            // and/or has to short-circut, so special form
                            // just like Scheme (bad ;) )
                            Form::Symbol(s, _id) if s == "or" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "or", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s, _id) if s == "and" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "and", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s, _id) if s == "begin" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "begin", to_go: cdr.cdr()?, c: nc }) };
                            }
                            Form::Symbol(s, _id) if s == "debug" => {
                                f = cdr.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "debug", to_go: cdr.cdr()?, c: nc }) };
                            }
                            // This is a fast and loose ~simple lisp~, so just go for it
                            // and can have convention that this is always top levelish
                            Form::Symbol(s, _id) if s == "define" => {
                                // note the swap, evaluating the second not the first (define a value..)
                                f = cdr.cdr()?.car()?;
                                c = Cont::Eval { c: Box::new(Cont::Prim { s: "define", to_go: cdr.car()?, c: nc }) };
                            }
                            Form::Symbol(s, _id) if s == "quote" => {
                                f = cdr.car()?;
                                c = *nc;
                            }
                            // (lambda (a b) body)
                            Form::Symbol(s, _id) if s == "lambda" => {
                                let mut params_vec = vec![];
                                let mut params = cdr.car()?;
                                while let Ok((ncar, ncdr)) = params.pair() {
                                    params_vec.push(ncar.sym()?.to_string());
                                    params = ncdr;
                                }
                                let body = cdr.cdr()?.car()?;
                                f = Form::new_closure(params_vec, Rc::clone(&e), body);
                                c = *nc;
                            }
                            _ => {
                                f = Rc::clone(car);
                                c = Cont::Eval { c: Box::new(Cont::Call { evaled: vec![], to_go: Rc::clone(cdr), c: nc }) };
                            }
                        }
                    },
                    _ =>  {
                        // value, no eval
                        f = tmp;
                        c = *nc;
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
impl From<String> for Form { fn from(item: String) -> Self { Form::Symbol(item, RefCell::new(None)) } }
impl From<&str>   for Form { fn from(item: &str)   -> Self { Form::Symbol(item.to_owned(), RefCell::new(None)) } }
impl From<i32>    for Form { fn from(item: i32)    -> Self { Form::Int(item) } }
impl From<bool>   for Form { fn from(item: bool)   -> Self { Form::Bool(item) } }
impl<A: Into<Form>, B: Into<Form>> From<(A, B)> for Form {
    fn from(item: (A, B)) -> Self {
        Form::Pair(Rc::new(item.0.into()), Rc::new(item.1.into()), RefCell::new(None))
    }
}

impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Form::Nil                   => write!(f, "nil"),
            Form::Int(i)                => write!(f, "{i}"),
            Form::Bool(b)               => write!(f, "{b}"),
            Form::Symbol(s, _id)        => write!(f, "'{s}"),
            Form::Pair(car, cdr, _id)   => {
                write!(f, "({}", car)?;
                let mut traverse: Rc<Form> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        Form::Pair(ref carp, ref cdrp, ref _id) => {
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
                write!(f, "<closure {:?}>", params)
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
