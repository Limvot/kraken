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
struct Stats {
    id_counter: i64,
    func_calls: BTreeMap<ID, i64>,
}
impl Stats {
    fn new() -> Stats {
        Stats {
            id_counter: 0,
            func_calls: BTreeMap::new()
        }
    }
    fn alloc_id(&mut self) -> ID {
        self.id_counter += 1;
        ID { id: self.id_counter }
    }
    fn count_call(&mut self, id: &RefCell<Option<ID>>) {
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
    }
}

pub fn eval(f: Rc<Form>) -> Result<Rc<Form>> {
    let e = Env::root_env();
    let mut stats = Stats::new();
    let to_ret = tree_walker_eval(f, e, &mut stats)?;
    println!("Stats were {stats:?}");
    Ok(to_ret)
}

// add functions
// variables
// optimized as a function based off side table of id keyed -> opt
// that id might be nice for debugging too
// Symbol ID's could actually be used for environment lookups
//  this is just interning

fn tree_walker_eval(f: Rc<Form>, e: Rc<RefCell<Env>>, stats: &mut Stats) -> Result<Rc<Form>> {
    println!("tree_walker_eval({f})");
    Ok(match &*f {
        Form::Symbol(s, _id) => e.borrow().lookup(s)?,
        Form::Pair(car, cdr, _id) => {
            match &**car {
                Form::Symbol(s, _id) if s == "if" => {
                    if tree_walker_eval(cdr.car()?, Rc::clone(&e), stats)?.truthy() {
                        tree_walker_eval(cdr.cdr()?.car()?, e, stats)?
                    } else {
                        tree_walker_eval(cdr.cdr()?.cdr()?.car()?, e, stats)?
                    }

                }
                Form::Symbol(s, _id) if s == "begin" => {
                    let mut last_result = Form::new_nil();
                    let mut traverse = Rc::clone(cdr);
                    while let Ok((ncar, ncdr)) = traverse.pair() {
                        traverse = ncdr;
                        last_result = tree_walker_eval(ncar, Rc::clone(&e), stats)?;
                    }
                    last_result

                }
                Form::Symbol(s, _id) if s == "debug" => {
                    println!("debug: {}", tree_walker_eval(cdr.car()?, e, stats)?);
                    Form::new_nil()
                }
                // This is a fast and loose ~simple lisp~, so just go for it
                // and can have convention that this is always top levelish
                Form::Symbol(s, _id) if s == "define" => {
                    let v = tree_walker_eval(cdr.cdr()?.car()?, Rc::clone(&e), stats)?;
                    e.borrow_mut().define(cdr.car()?.sym()?.to_string(), v);
                    Form::new_nil()
                }
                Form::Symbol(s, _id) if s == "quote" => {
                    cdr.car()?
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
                    Form::new_closure(params_vec, Rc::clone(&e), body)
                }
                _ => {
                    let comb = tree_walker_eval(Rc::clone(car), Rc::clone(&e), stats)?;
                    match &*comb {
                        Form::Closure(ps, ie, b, id) => {
                            stats.count_call(id);
                            let mut arguments_vec = vec![];
                            let mut arguments = Rc::clone(cdr);
                            while let Ok((ncar, ncdr)) = arguments.pair() {
                                arguments_vec.push(tree_walker_eval(ncar, Rc::clone(&e), stats)?);
                                arguments = ncdr;
                            }
                            if ps.len() != arguments_vec.len() {
                                bail!("arguments length doesn't match");
                            }
                            let new_env = Env::chain(&e);
                            for (name, value) in ps.iter().zip(arguments_vec.into_iter()) {
                                new_env.borrow_mut().define(name.to_string(), value);
                            }
                            tree_walker_eval(Rc::clone(b), new_env, stats)?
                        },
                        Form::Prim(p) => {
                            let a = tree_walker_eval(cdr.car()?, Rc::clone(&e), stats)?;
                            match comb.prim().unwrap() {
                                Prim::Car => a.car()?,
                                Prim::Cdr => a.cdr()?,
                                _ => {
                                    let b = tree_walker_eval(cdr.cdr()?.car()?, Rc::clone(&e), stats)?;
                                    match comb.prim().unwrap() {
                                        Prim::Add => Form::new_int(a.int()? + b.int()?),
                                        Prim::Sub => Form::new_int(a.int()? - b.int()?),
                                        Prim::Mul => Form::new_int(a.int()? * b.int()?),
                                        Prim::Div => Form::new_int(a.int()? / b.int()?),
                                        Prim::Mod => Form::new_int(a.int()? % b.int()?),
                                        Prim::Cons => Form::new_pair(a, b),
                                        Prim::Eq  => Form::new_bool(a.my_eq(&b)),
                                        _ => unreachable!(),
                                    }
                                }
                            }
                        },
                        _ => {
                            bail!("tried to call a non-comb {}", comb)
                        },
                    }
                }
            }
        },
        _ => f
    })
}

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
