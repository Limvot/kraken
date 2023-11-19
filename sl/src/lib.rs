use std::rc::Rc;
use std::collections::BTreeMap;
use std::fmt;

use anyhow::{anyhow,Result};

#[derive(Debug, Eq, PartialEq)]
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(Rc<Form>,Rc<Form>),
    Prim(Prim),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum Prim {
    Add,
    Mul,
    Eq,
}

impl Form {
    fn new_nil() -> Rc<Form> {
        Rc::new(Form::Nil)
    }
    fn new_int(i: i32) -> Rc<Form> {
        Rc::new(Form::Int(i))
    }
    fn new_bool(b: bool) -> Rc<Form> {
        Rc::new(Form::Bool(b))
    }
    fn truthy(&self) -> bool {
        match self {
            Form::Bool(b) => *b,
            Form::Nil     => false,
            _             => true,
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
}

pub struct Env {
    m: BTreeMap<String, Rc<Form>>
}
impl Env {
    pub fn root_env() -> Env {
        Env {
            m: [
                ("+", Rc::new(Form::Prim(Prim::Add))),
                ("*", Rc::new(Form::Prim(Prim::Mul))),
                ("=", Rc::new(Form::Prim(Prim::Eq))),
            ].into_iter().map(|(s,p)| (s.to_owned(), p)).collect()
        }
    }
    pub fn lookup(&self, s: &str) -> Result<Rc<Form>> {
        Ok(Rc::clone(self.m.get(s).ok_or(anyhow!("lookup failed"))?))
    }
}

pub fn tree_walker_eval(f: Rc<Form>, e: &mut Env) -> Result<Rc<Form>> {
    Ok(match &*f {
        Form::Symbol(s) => e.lookup(s)?,
        Form::Pair(car, cdr) => {
            match &**car {
                Form::Symbol(s) if s == "if" => {
                    if tree_walker_eval(cdr.car()?, e)?.truthy() {
                        tree_walker_eval(cdr.cdr()?.car()?, e)?
                    } else {
                        tree_walker_eval(cdr.cdr()?.cdr()?.car()?, e)?
                    }

                }
                _ => {
                    let comb = tree_walker_eval(Rc::clone(car), e)?;
                    let a = tree_walker_eval(cdr.car()?, e)?;
                    let b = tree_walker_eval(cdr.cdr()?.car()?, e)?;
                    match comb.prim().unwrap() {
                        Prim::Add => Form::new_int(a.int()? + b.int()?),
                        Prim::Mul => Form::new_int(a.int()? * b.int()?),
                        Prim::Eq  => Form::new_bool(a == b),
                    }
                }
            }
        },
        _ => f
    })
}

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
            Form::Pair(car, cdr)        => {
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
            Form::Prim(p) => {
                match p {
                    Prim::Add => write!(f, "+"),
                    Prim::Mul => write!(f, "*"),
                    Prim::Eq  => write!(f, "="),
                }
            }
        }
    }
}
