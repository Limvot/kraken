use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;

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
            Form::PrimComb { eval_limit, ins }    => write!(f, "<{eval_limit}> - {ins:?}"),
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
    PrimComb { eval_limit: i32, ins: PrimCombI },
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
    PramEval { eval_limit: i32, to_eval: Rc<Form>, collected: Option<Rc<Form>>, e: Rc<Form>, ins: PrimCombI, nc: Box<Cont> },
}
pub struct Cursor { f: Rc<Form>, c: Cont, metac: Cont }

pub fn eval(e: Rc<Form>, f: Rc<Form>) -> Rc<Form> {
    let mut cursor = Cursor { f, c: Cont::Eval { e, nc: Box::new(Cont::MetaRet) }, metac: Cont::Exit };
    loop {
        let Cursor { f, c, metac } = cursor;
        match c {
            Cont::Exit => {
                return f;
            },
            Cont::MetaRet => {
                cursor = Cursor { f: f, c: metac.clone(), metac: metac };
            },
            Cont::CatchRet { nc, restore_meta } => {
                cursor = Cursor { f: f, c: *nc, metac: *restore_meta };
            },
            Cont::PramEval { eval_limit, to_eval, collected, e, ins, nc } => {
                let next_collected = if let Some(collected) = collected {
                    Rc::new(collected.append(f).unwrap())
                } else { Rc::new(Form::Nil) };
                if eval_limit == 0 || to_eval.is_nil() {
                    let mut traverse = to_eval;
                    let mut next_collected = next_collected;
                    while !traverse.is_nil() {
                        next_collected = Rc::new(next_collected.append(traverse.car().unwrap()).unwrap());
                        traverse = traverse.cdr().unwrap();
                    }
                    cursor = impl_prim(ins, e, next_collected, *nc, metac);
                } else {
                    cursor = Cursor { f: to_eval.car().unwrap(), c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::PramEval { eval_limit: eval_limit - 1,
                                                                                                                                 to_eval: to_eval.cdr().unwrap(),
                                                                                                                                 collected: Some(next_collected),
                                                                                                                                 e, ins, nc }) }, metac };
                }
            },
            Cont::Eval { e, nc } => {
                match *f {
                    Form::Pair(ref comb, ref p) => {
                        cursor = Cursor { f: Rc::clone(comb), c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::Call { p: Rc::clone(p), e: e, nc: nc }) }, metac }
                    },
                    Form::Symbol(ref s) => {
                        let mut t = Rc::clone(&e);
                        let mut dist = 0;
                        while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                            t = t.cdr().unwrap();
                            dist += 1;
                        }
                        cursor = Cursor { f: t.car().unwrap().cdr().unwrap(), c: *nc, metac };
                    },
                    _ => {
                        cursor = Cursor { f: Rc::clone(&f), c: *nc, metac };
                    },
                }
            },
            Cont::Call { p, e, nc } => {
                match *f {
                    Form::PrimComb { eval_limit, ins } => {
                        cursor = Cursor { f: Rc::new(Form::Nil), c: Cont::PramEval { eval_limit, to_eval: p, collected: None, e, ins, nc: nc }, metac };
                    }
                    Form::DeriComb{ref se, ref de, ref params, ref body } => {
                        let mut new_e = Rc::clone(se);
                        if let Some(de) = de {
                            new_e = assoc(de, Rc::clone(&e), new_e);
                        }
                        new_e = assoc(params, p, new_e);
                        cursor = Cursor { f: Rc::clone(body), c: Cont::Eval { e: new_e, nc: nc }, metac };
                    },
                    Form::ContComb(ref c) => {
                        cursor = Cursor { f: p.car().unwrap(), c: Cont::Eval { e, nc: Box::new(c.clone()) }, metac: Cont::CatchRet { nc: nc, restore_meta: Box::new(metac) } };
                    },
                    _ => panic!("Tried to call not a Prim/DeriComb/ContComb {:?}, nc was {:?}", f, nc),
                }
            },
        }
    }
}

pub fn assoc(k: &str, v: Rc<Form>, l: Rc<Form>) -> Rc<Form> {
    Rc::new(Form::Pair(
                Rc::new(Form::Pair(
                        Rc::new(Form::Symbol(k.to_owned())),
                        v)),
                l))
}
pub fn assoc_vec(kvs: Vec<(&str, Rc<Form>)>) -> Rc<Form> {
    let mut to_ret = Rc::new(Form::Nil);
    for (k, v) in kvs {
        to_ret = assoc(k, v, to_ret);
    }
    to_ret
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum PrimCombI {
        Eval,
        Vau,
        If,

        Reset,
        Shift,

        Cell,
        Set,
        Get,

        Cons,
        Car,
        Cdr,
        Quote,
        Assert,

        Eq,
        Lt,
        LEq,
        Gt,
        GEq,

        Plus,
        Minus,
        Mult,
        Div,
        Mod,
        And,
        Or,
        Xor,

        CombP,
        CellP,
        PairP,
        SymbolP,
        IntP,
        BoolP,
        NilP,
}

pub fn impl_prim(ins: PrimCombI, e: Rc<Form>, ps: Rc<Form>, c: Cont, metac: Cont) -> Cursor {
    match ins {
        PrimCombI::Eval => Cursor { f: ps.car().unwrap(), c: Cont::Eval { e: ps.cdr().unwrap().car().unwrap(), nc: Box::new(c) }, metac },
        PrimCombI::Vau => {
            let de     = ps.car().unwrap().sym().map(|s| s.to_owned());
            let params = ps.cdr().unwrap().car().unwrap().sym().unwrap().to_owned();
            let body   = ps.cdr().unwrap().cdr().unwrap().car().unwrap();

            Cursor { f: Rc::new(Form::DeriComb { se: e, de, params, body }), c: c, metac: metac }
        },
        PrimCombI::If => if ps.car().unwrap().truthy() {
                             Cursor { f: ps.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                         } else {
                             Cursor { f: ps.cdr().unwrap().cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                         },

        PrimCombI::Reset => Cursor { f: ps.car().unwrap(),
                                     c: Cont::Eval { e: e, nc: Box::new(Cont::MetaRet) },
                                     metac: Cont::CatchRet { nc: Box::new(c), restore_meta: Box::new(metac) } },
        PrimCombI::Shift => Cursor { f: ps.car().unwrap(),
                                     c: Cont::Call { p: Rc::new(Form::Pair(Rc::new(Form::ContComb(c)),
                                                              Rc::new(Form::Nil))),
                                                     e: e,
                                                     nc: Box::new(Cont::MetaRet) },
                                     metac: Cont::CatchRet { nc: Box::new(metac.clone()), restore_meta: Box::new(metac) } },

        PrimCombI::Cell => Cursor { f: Rc::new(Form::Cell(RefCell::new(ps.car().unwrap()))), c, metac },
        PrimCombI::Set  => match &*ps.car().unwrap() {
                             Form::Cell(cell) => Cursor { f: cell.replace(ps.cdr().unwrap().car().unwrap()), c: c, metac },
                             _             => panic!("set on not cell"),
                         },
        PrimCombI::Get => match &*ps.car().unwrap() {
                            Form::Cell(cell) => Cursor { f: Rc::clone(&cell.borrow()), c: c, metac },
                            _             => panic!("get on not cell"),
                        },

        PrimCombI::Cons  => Cursor { f: Rc::new(Form::Pair(ps.car().unwrap(), ps.cdr().unwrap().car().unwrap())), c: c, metac },
        PrimCombI::Car   => Cursor { f: ps.car().unwrap().car().unwrap(), c: c, metac },
        PrimCombI::Cdr   => Cursor { f: ps.car().unwrap().cdr().unwrap(), c: c, metac },
        PrimCombI::Quote => Cursor { f: ps.car().unwrap(), c: c, metac: metac },
        PrimCombI::Assert => {
                                let thing = ps.car().unwrap();
                                if !thing.truthy() {
                                    println!("Assert failed: {:?}", thing);
                                }
                                assert!(thing.truthy());
                                Cursor { f: ps.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                            },

        PrimCombI::Eq     => Cursor { f: Rc::new(Form::Bool(ps.car().unwrap()                == ps.cdr().unwrap().car().unwrap())), c, metac },
        PrimCombI::Lt     => Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap()  < ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::LEq    => Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() <= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Gt     => Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap()  > ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::GEq    => Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().int().unwrap() >= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },

        PrimCombI::Plus   => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   + ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Minus  => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   - ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Mult   => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   * ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Div    => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   / ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Mod    => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   % ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::And    => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   & ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Or     => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   | ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
        PrimCombI::Xor    => Cursor { f: Rc::new(Form::Int(ps.car().unwrap().int().unwrap()   ^ ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },

        PrimCombI::CombP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                                Form::PrimComb { eval_limit, ins } => true,
                                Form::DeriComb { .. }              => true,
                                _                                  => false,
                            })), c, metac },
        PrimCombI::CellP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                                Form::Cell(_c) => true,
                                _              => false,
                            })), c, metac },
        PrimCombI::PairP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                                Form::Pair(_a,_b) => true,
                                _                 => false,
                            })), c, metac },
        PrimCombI::SymbolP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                               Form::Symbol(_) => true,
                               _               => false,
                           })), c, metac },
        PrimCombI::IntP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                               Form::Int(_) => true,
                               _            => false,
                           })), c, metac },
        PrimCombI::BoolP => Cursor { f: Rc::new(Form::Bool(match &*ps.car().unwrap() {
                                Form::Bool(_) => true,
                                _             => false,
                            })), c, metac },
        PrimCombI::NilP =>  Cursor { f: Rc::new(Form::Bool(ps.car().unwrap().is_nil())), c, metac },
    }
}


// Have eval?/maybe Cont?/maybe Cursor? parameterized on value type?
// Parameterized on prim implementation?
// Should opt impl use same prim implementation but trace values through accessors/constructors?
//  with some special handling of If/Vau/etc?

pub fn root_env() -> Rc<Form> {
    assoc_vec(vec![
        ("eval",    Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Eval    })),
        ("vau",     Rc::new(Form::PrimComb { eval_limit:  0, ins: PrimCombI::Vau     })),
        ("if",      Rc::new(Form::PrimComb { eval_limit:  1, ins: PrimCombI::If      })),

        ("reset",   Rc::new(Form::PrimComb { eval_limit:  0, ins: PrimCombI::Reset   })),
        ("shift",   Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Shift   })),

        ("cell",    Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Cell    })),
        ("set",     Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Set     })),
        ("get",     Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Get     })),

        ("cons",    Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Cons    })),
        ("car",     Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Car     })),
        ("cdr",     Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Cdr     })),
        ("quote",   Rc::new(Form::PrimComb { eval_limit:  0, ins: PrimCombI::Quote   })),
        ("assert",  Rc::new(Form::PrimComb { eval_limit:  1, ins: PrimCombI::Assert  })),

        ("=",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Eq      })),
        ("<",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Lt      })),
        ("<=",      Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::LEq     })),
        (">",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Gt      })),
        (">=",      Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::GEq     })),

        ("+",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Plus    })),
        ("-",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Minus   })),
        ("*",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Mult    })),
        ("/",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Div     })),
        ("%",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Mod     })),
        ("&",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::And     })),
        ("|",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Or      })),
        ("^",       Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::Xor     })),

        ("comb?",   Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::CombP   })),
        ("cell?",   Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::CellP   })),
        ("pair?",   Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::PairP   })),
        ("symbol?", Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::SymbolP })),
        ("int?",    Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::IntP    })),
        ("bool?",   Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::BoolP   })),
        ("nil?",    Rc::new(Form::PrimComb { eval_limit: -1, ins: PrimCombI::NilP    })),

        ("true",    Rc::new(Form::Bool(true))),
        ("false",   Rc::new(Form::Bool(false))),
        ("nil",     Rc::new(Form::Nil)),
    ])
}

