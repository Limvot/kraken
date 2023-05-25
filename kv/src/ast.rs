use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;

impl From<i32>  for Form { fn from(item: i32)  -> Self { Form::Int(item) } }
impl From<bool> for Form { fn from(item: bool) -> Self { Form::Bool(item) } }
// todo, strings not symbols?
impl From<String> for Form { fn from(item: String) -> Self { Form::Symbol(item) } }
impl From<&str> for Form { fn from(item: &str) -> Self { Form::Symbol(item.to_owned()) } }

impl<A: Into<Form>, B: Into<Form>> From<(A, B)> for Form {
    fn from(item: (A, B)) -> Self {
        Form::Pair(Rc::new(item.0.into()), Rc::new(item.1.into()))
    }
}

impl Form {
    pub fn truthy(&self) -> bool {
        match self {
            Form::Bool(b) => *b,
            Form::Nil     => false,
            _             => true,
        }
    }
    pub fn int(&self) -> Option<i32> {
        match self {
            Form::Int(i) => Some(*i),
            _ => None,
        }
    }
    pub fn sym(&self) -> Option<&str> {
        match self {
            Form::Symbol(s) => Some(s),
            _ => None,
        }
    }
    pub fn car(&self) -> Option<Rc<Form>> {
        match self {
            Form::Pair(car, _cdr) => Some(Rc::clone(car)),
            _ => None,
        }
    }
    pub fn cdr(&self) -> Option<Rc<Form>> {
        match self {
            Form::Pair(_car, cdr) => Some(Rc::clone(cdr)),
            _ => None,
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            Form::Nil            => true,
            _                    => false,
        }
    }
    pub fn append(&self, x: Rc<Form>) -> Option<Form> {
        match self {
            Form::Pair(car, cdr) => cdr.append(x).map(|x| Form::Pair(Rc::clone(car), Rc::new(x))),
            Form::Nil            => Some(Form::Pair(x, Rc::new(Form::Nil))),
            _                    => None,
        }
    }
}
impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Form::Nil                   => write!(f, "nil"),
            Form::Int(i)                => write!(f, "{i}"),
            Form::Bool(b)               => write!(f, "{b}"),
            Form::Symbol(s)             => write!(f, "{s}"),
            Form::Cell(c)               => write!(f, "@{}", c.borrow()),
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
            Form::PrimComb(name, _f)    => write!(f, "<{name}>"),
            Form::DeriComb { se, de, params, body } => {
                write!(f, "<{} {} {}>", de.as_ref().unwrap_or(&"".to_string()), params, body)
            },
            Form::ContComb(_)    => write!(f, "<cont>"),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Cell(RefCell<Rc<Form>>),
    Pair(Rc<Form>,Rc<Form>),
    PrimComb(String, fn(Rc<Form>, Rc<Form>, Cont, Cont) -> Cursor),
    DeriComb { se: Rc<Form>, de: Option<String>, params: String, body: Rc<Form> },
    ContComb(Cont),
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Cont {
    Exit,
    MetaRet,
    CatchRet { nc: Box<Cont>,  restore_meta: Box<Cont> },
    Eval     {              e: Rc<Form>, nc: Box<Cont> },
    Call     { p: Rc<Form>, e: Rc<Form>, nc: Box<Cont> },
    PramEval { eval_limit: i32, to_eval: Rc<Form>, collected: Option<Rc<Form>>, e: Rc<Form>, pf: fn(Rc<Form>, Rc<Form>, Cont, Cont) -> Cursor, nc: Box<Cont> },
}
#[derive(Debug, Eq, PartialEq, Clone)]
enum Ins {
    Const(Rc<Form>),
    EnvGet(i32),
    Eq,
    Lt,
    Leq,
    Gt,
    Geq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    CombP,
    CellP,
    IntP,
    BoolP,
    SymbolP,
    PairP,
    NilP,
    Exit,
    MetaRet,
    CatchRet,
    Bad(&'static str),
}
pub struct Cursor { f: Rc<Form>, c: Cont, metac: Cont, new_bytecode: Option<Ins> }

pub fn eval(e: Rc<Form>, f: Rc<Form>) -> Rc<Form> {
    let mut cursor = Cursor { f, c: Cont::Eval { e, nc: Box::new(Cont::MetaRet) }, metac: Cont::Exit, new_bytecode: None };
    loop {
        let Cursor { f, c, metac, new_bytecode } = cursor;
        if let Some(bc) = new_bytecode {
            //println!("{:?}", bc);
        }
        match c {
            Cont::Exit => {
                println!("{:?}", Ins::Exit);
                return f;
            },
            Cont::MetaRet => {
                cursor = Cursor { f: f, c: metac.clone(), metac: metac, new_bytecode: Some(Ins::MetaRet) };
            },
            Cont::CatchRet { nc, restore_meta } => {
                cursor = Cursor { f: f, c: *nc, metac: *restore_meta, new_bytecode: Some(Ins::CatchRet) };
            },
            Cont::PramEval { eval_limit, to_eval, collected, e, pf, nc } => {
                let next_collected = if let Some(collected) = collected {
                    Rc::new(collected.append(f).unwrap())
                } else { Rc::new(Form::Nil) };
                if eval_limit == 0 || to_eval.is_nil() {
                    // hmm, it should probs reverse collected
                    let mut traverse = to_eval;
                    let mut next_collected = next_collected;
                    while !traverse.is_nil() {
                        next_collected = Rc::new(next_collected.append(traverse.car().unwrap()).unwrap());
                        traverse = traverse.cdr().unwrap();
                    }
                    cursor = pf(e, next_collected, *nc, metac);
                } else {
                    cursor = Cursor { f: to_eval.car().unwrap(), c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::PramEval { eval_limit: eval_limit - 1,
                                                                                                                                 to_eval: to_eval.cdr().unwrap(),
                                                                                                                                 collected: Some(next_collected),
                                                                                                                                 e, pf, nc }) }, metac, new_bytecode: None };
                }
            },
            Cont::Eval { e, nc } => {
                match *f {
                    Form::Pair(ref comb, ref p) => {
                        cursor = Cursor { f: Rc::clone(comb), c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::Call { p: Rc::clone(p), e: e, nc: nc }) }, metac, new_bytecode: None }
                    },
                    Form::Symbol(ref s) => {
                        let mut t = Rc::clone(&e);
                        let mut dist = 0;
                        while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                            t = t.cdr().unwrap();
                            dist += 1;
                        }
                        // Hmm, we need to know if this is from this call or not for if we should EnvGet
                        //cursor = Cursor { f: t.car().unwrap().cdr().unwrap(), c: *nc, metac, new_bytecode: Some(Ins::EnvGet(dist)) };
                        cursor = Cursor { f: t.car().unwrap().cdr().unwrap(), c: *nc, metac, new_bytecode: Some(Ins::Bad("don't know if envget or const or nothing (think called prim vs passed prim vs parameter), and how to maintain that info")) };
                    },
                    _ => {
                        cursor = Cursor { f: Rc::clone(&f), c: *nc, metac, new_bytecode: Some(Ins::Const(f)) };
                    },
                }
            },
            Cont::Call { p, e, nc } => {
                match *f {
                    Form::PrimComb(ref _n,  ref f) => {
                        cursor = f(e, p, *nc, metac);
                    },
                    Form::DeriComb{ref se, ref de, ref params, ref body } => {
                        let mut new_e = Rc::clone(se);
                        if let Some(de) = de {
                            new_e = assoc(de, Rc::clone(&e), new_e);
                        }
                        new_e = assoc(params, p, new_e);
                        cursor = Cursor { f: Rc::clone(body), c: Cont::Eval { e: new_e, nc: nc }, metac, new_bytecode: Some(Ins::Bad("deri-comb call")) };
                    },
                    Form::ContComb(ref c) => {
                        cursor = Cursor { f: p.car().unwrap(), c: Cont::Eval { e, nc: Box::new(c.clone()) }, metac: Cont::CatchRet { nc: nc, restore_meta: Box::new(metac) }, new_bytecode: Some(Ins::Bad("cont-comb call")) };
                    },
                    _ => panic!("Tried to call not a Prim/DeriComb/ContComb {:?}, nc was {:?}", f, nc),
                }
            },
        }
    }
}

fn assoc(k: &str, v: Rc<Form>, l: Rc<Form>) -> Rc<Form> {
    Rc::new(Form::Pair(
                Rc::new(Form::Pair(
                        Rc::new(Form::Symbol(k.to_owned())),
                        v)),
                l))
}
fn assoc_vec(kvs: Vec<(&str, Rc<Form>)>) -> Rc<Form> {
    let mut to_ret = Rc::new(Form::Nil);
    for (k, v) in kvs {
        to_ret = assoc(k, v, to_ret);
    }
    to_ret
}

pub fn root_env() -> Rc<Form> {
    assoc_vec(vec![
        ("eval", Rc::new(Form::PrimComb("eval".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: ps.car().unwrap(), c: Cont::Eval { e: ps.cdr().unwrap().car().unwrap(), nc: Box::new(c) }, metac, new_bytecode: Some(Ins::Bad("EvalSetEnv")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("reset", Rc::new(Form::PrimComb("reset".to_owned(), |e, p, c, metac| {
            Cursor { f: p.car().unwrap(),
                     c: Cont::Eval { e: e, nc: Box::new(Cont::MetaRet) },
                     metac: Cont::CatchRet { nc: Box::new(c), restore_meta: Box::new(metac) }, new_bytecode: Some(Ins::Bad("reset")) }
        }))),
        ("shift", Rc::new(Form::PrimComb("shift".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {

                        let c_comb_p = Rc::new(Form::Pair(Rc::new(Form::ContComb(c)),
                                                          Rc::new(Form::Nil)));
                        Cursor { f: ps.car().unwrap(),
                                 c: Cont::Call { p: c_comb_p,
                                                 e: e,
                                                 nc: Box::new(Cont::MetaRet) },
                                 metac: Cont::CatchRet { nc: Box::new(metac.clone()), restore_meta: Box::new(metac) }, new_bytecode: Some(Ins::Bad("shift")) }

                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        // (vau de params body)
        ("vau", Rc::new(Form::PrimComb("vau".to_owned(), |e, p, c, metac| {
            let de     = p.car().unwrap().sym().map(|s| s.to_owned());
            let params = p.cdr().unwrap().car().unwrap().sym().unwrap().to_owned();
            let body   = p.cdr().unwrap().cdr().unwrap().car().unwrap();

            Cursor { f: Rc::new(Form::DeriComb { se: e, de, params, body }), c: c, metac: metac, new_bytecode: Some(Ins::Bad("vau")) }
        }))),
        ("=", Rc::new(Form::PrimComb("=".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap() == ps.cdr().unwrap().car().unwrap())), c, metac, new_bytecode: Some(Ins::Eq)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("<", Rc::new(Form::PrimComb("<".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() < ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Lt) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("<=", Rc::new(Form::PrimComb("<=".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() <= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Leq) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        (">", Rc::new(Form::PrimComb(">".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() > ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Gt) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        (">=", Rc::new(Form::PrimComb(">=".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() >= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Geq) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("if", Rc::new(Form::PrimComb("if".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: 1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        if ps.car().unwrap().truthy() {
                            Cursor { f: ps.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac, new_bytecode: Some(Ins::Bad("EnsBranchPositive")) }
                        } else {
                            Cursor { f: ps.cdr().unwrap().cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac, new_bytecode: Some(Ins::Bad("EnsBranchFalse")) }
                        }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("cell", Rc::new(Form::PrimComb("cell".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Cell(RefCell::new(ps.car().unwrap()))), c, metac, new_bytecode: Some(Ins::Bad("cell")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("set", Rc::new(Form::PrimComb("set".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        match &*ps.car().unwrap() {
                            Form::Cell(cell) => Cursor { f: cell.replace(ps.cdr().unwrap().car().unwrap()), c: c, metac, new_bytecode: Some(Ins::Bad("set")) },
                            _             => panic!("set on not cell"),
                        }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("get", Rc::new(Form::PrimComb("get".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        match &*ps.car().unwrap() {
                            Form::Cell(cell) => Cursor { f: Rc::clone(&cell.borrow()), c: c, metac, new_bytecode: Some(Ins::Bad("get")) },
                            _             => panic!("get on not cell"),
                        }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("cons", Rc::new(Form::PrimComb("cons".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Pair(ps.car().unwrap(), ps.cdr().unwrap().car().unwrap())), c: c, metac, new_bytecode: Some(Ins::Bad("cons")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("car", Rc::new(Form::PrimComb("car".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: ps.car().unwrap().car().unwrap(), c: c, metac, new_bytecode: Some(Ins::Bad("car")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("cdr", Rc::new(Form::PrimComb("cdr".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: ps.car().unwrap().cdr().unwrap(), c: c, metac, new_bytecode: Some(Ins::Bad("cdr")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("quote", Rc::new(Form::PrimComb("quote".to_owned(), |_e, p, c, metac| {
            Cursor { f: p.car().unwrap(), c: c, metac: metac, new_bytecode: Some(Ins::Bad("quote - emit as const?")) }
        }))),

        //("debug", Rc::new(Form::PrimComb("debug".to_owned(), |e, p, c| {
            ////println!("Debug: {:?}", eval(Rc::clone(&e), p.car().unwrap()));
            //println!("Debug: {}", eval(Rc::clone(&e), p.car().unwrap()));
            ////PossibleTailCall::TailCall(e, p.cdr().unwrap().car().unwrap())
            //Cursor { f: p.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: c }, new_bytecode: None };
        //}))),
        ("assert", Rc::new(Form::PrimComb("assert".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: 1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        let thing = ps.car().unwrap();
                        if !thing.truthy() {
                            println!("Assert failed: {:?}", thing);
                        }
                        assert!(thing.truthy());
                        Cursor { f: ps.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac, new_bytecode: Some(Ins::Bad("assert")) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("+", Rc::new(Form::PrimComb("+".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() + ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Add)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("-", Rc::new(Form::PrimComb("-".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() - ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Sub)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("*", Rc::new(Form::PrimComb("*".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() * ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Mul)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("/", Rc::new(Form::PrimComb("/".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() / ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Div)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("%", Rc::new(Form::PrimComb("%".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() % ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Mod)}
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("&", Rc::new(Form::PrimComb("&".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() & ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::And) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("|", Rc::new(Form::PrimComb("|".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() | ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Or) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("^", Rc::new(Form::PrimComb("^".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap() ^ ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac, new_bytecode: Some(Ins::Xor) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),

        ("comb?", Rc::new(Form::PrimComb("comb?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::PrimComb(_n, _f)  => true,
                            Form::DeriComb { .. }   => true,
                            _                       => false,
                        })), c, metac, new_bytecode: Some(Ins::CombP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("cell?", Rc::new(Form::PrimComb("cell?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::Cell(_c) => true,
                            _              => false,
                        })), c, metac, new_bytecode: Some(Ins::CellP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("pair?", Rc::new(Form::PrimComb("pair?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::Pair(_a,_b) => true,
                            _                 => false,
                        })), c, metac, new_bytecode: Some(Ins::PairP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("symbol?", Rc::new(Form::PrimComb("symbol?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::Symbol(_) => true,
                            _               => false,
                        })), c, metac, new_bytecode: Some(Ins::SymbolP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("int?", Rc::new(Form::PrimComb("int?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::Int(_) => true,
                            _            => false,
                        })), c, metac, new_bytecode: Some(Ins::IntP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("bool?", Rc::new(Form::PrimComb("bool?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                            Form::Bool(_) => true,
                            _             => false,
                        })), c, metac, new_bytecode: Some(Ins::BoolP) }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: None }
        }))),
        ("nil?", Rc::new(Form::PrimComb("nil?".to_owned(), |e, p, c, metac| {
            Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit: -1, to_eval: p, collected: None, e, pf: |e, ps, c, metac| {
                        Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().is_nil())), c, metac, new_bytecode: None }
                    },
                    nc: Box::new(c),
                }
            , metac, new_bytecode: Some(Ins::NilP) }
        }))),

        // consts
        ("true",  Rc::new(Form::Bool(true))),
        ("false", Rc::new(Form::Bool(false))),
        ("nil",   Rc::new(Form::Nil)),
    ])
}

