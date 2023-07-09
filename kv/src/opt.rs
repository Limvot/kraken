use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;

use crate::eval::{FormT,Cont,Cursor,PrimCombI};
use crate::basic::Form;

#[derive(Debug, Eq, PartialEq)]
pub enum OptForm {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Cell(RefCell<Rc<OptForm>>),
    Pair(Rc<OptForm>,Rc<OptForm>),
    PrimComb { eval_limit: i32, ins: PrimCombI },
    DeriComb { se: Rc<OptForm>, de: Option<String>, params: String, body: Rc<OptForm>, code: Option<Rc<RefCell<Vec<ByteCode>>>> },
    ContComb(Cont<OptForm>),

    // Hmm this needs to wrap both outside and inside, as inside can still emit code... 
    // or do we need to wrap everything b/c side effects (mutation and assert, at least)...
    CodeForm { code_offset: usize, code: Rc<RefCell<Vec<ByteCode>>>, inner_value: Rc<OptForm> },
}
#[derive(Debug, Eq, PartialEq)]
pub enum ByteCode {
    Ins(PrimCombI)
}

impl fmt::Display for OptForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OptForm::Nil                   => write!(f, "nil"),
            OptForm::Int(i)                => write!(f, "{i}"),
            OptForm::Bool(b)               => write!(f, "{b}"),
            OptForm::Symbol(s)             => write!(f, "{s}"),
            OptForm::Cell(c)               => write!(f, "@{}", c.borrow()),
            OptForm::Pair(car, cdr)        => {
                write!(f, "({}", car)?;
                let mut traverse: Rc<OptForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        OptForm::Pair(ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        OptForm::Nil => {
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
            OptForm::PrimComb { eval_limit, ins }    => write!(f, "<{eval_limit}> - {ins:?}"),
            OptForm::DeriComb { se: _, de, params, body, code: _ } => {
                write!(f, "<{} {} {}>", de.as_ref().unwrap_or(&"".to_string()), params, body)
            },
            OptForm::ContComb(_)    => write!(f, "<cont>"),
            OptForm::CodeForm { code_offset, inner_value, .. }    => write!(f, "~code{code_offset}~({inner_value})"),
        }
    }
}

impl From<&Form> for Rc<OptForm> {
    fn from(x: &Form) -> Self {
        Rc::new(match x {
            Form::Nil                           => OptForm::Nil,
            Form::Int(i)                        => OptForm::Int(*i),
            Form::Bool(b)                       => OptForm::Bool(*b),
            Form::Symbol(s)                     => OptForm::Symbol(s.to_owned()),
            Form::Cell(cell)                    => panic!("bad"),
            Form::Pair(car,cdr)                 => OptForm::Pair((&**car).into(), (&**cdr).into()),
            Form::PrimComb { eval_limit, ins }  => OptForm::PrimComb { eval_limit: *eval_limit, ins: *ins },
            Form::DeriComb { .. }               => panic!("bad"),
            Form::ContComb(c)                   => panic!("bad"),
        })
    }
}

impl OptForm {
    fn nil() -> Rc<Self> {
        Rc::new(OptForm::Nil)
    }
    fn truthy(&self) -> bool {
        match self {
            OptForm::Bool(b) => *b,
            OptForm::Nil     => false,
            OptForm::CodeForm { inner_value, .. } => inner_value.truthy(),
            _                => true,
        }
    }
    fn int(&self) -> Option<i32> {
        match self {
            OptForm::Int(i) => Some(*i),
            OptForm::CodeForm { inner_value, .. } => inner_value.int(),
            _               => None,
        }
    }
    fn append(&self, x: Rc<OptForm>) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(car, cdr) => cdr.append(x).map(|x| Rc::new(OptForm::Pair(Rc::clone(car), x))),
            OptForm::Nil            => Some(Rc::new(OptForm::Pair(x, Rc::new(OptForm::Nil)))),
            OptForm::CodeForm { inner_value, .. } => inner_value.append(x),
            _                       => None,
        }
    }
    pub fn congruent(&self, other: &Form) -> bool {
        match other {
            Form::Nil                           => *self == OptForm::Nil,
            Form::Int(i)                        => *self == OptForm::Int(*i),
            Form::Bool(b)                       => *self == OptForm::Bool(*b),
            Form::Symbol(s)                     => *self == OptForm::Symbol(s.to_owned()),
            Form::Cell(cell)                    => panic!("bad"),
            Form::Pair(car,cdr)                 => match self { OptForm::Pair(carp, cdrp) => carp.congruent(car) && cdrp.congruent(cdr), _ => false },
            Form::PrimComb { eval_limit, ins }  => *self == OptForm::PrimComb { eval_limit: *eval_limit, ins: *ins },
            Form::DeriComb { .. }               => panic!("bad"),
            Form::ContComb(c)                   => panic!("bad"),
        }
    }
}

impl FormT for OptForm {
    fn sym(&self) -> Option<&str> {
        match self {
            OptForm::Symbol(s) => Some(s),
            OptForm::CodeForm { inner_value, .. } => inner_value.sym(),
            _                  => None,
        }
    }
    fn pair(&self) -> Option<(Rc<OptForm>,Rc<OptForm>)> {
        match self {
            OptForm::Pair(car, cdr) => Some((Rc::clone(car),Rc::clone(cdr))),
            OptForm::CodeForm { inner_value, .. } => inner_value.pair(),
            _                       => None,
        }
    }
    fn car(&self) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(car, _cdr) => Some(Rc::clone(car)),
            OptForm::CodeForm { inner_value, .. } => inner_value.car(),
            _                        => None,
        }
    }
    fn cdr(&self) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(_car, cdr) => Some(Rc::clone(cdr)),
            OptForm::CodeForm { inner_value, .. } => inner_value.cdr(),
            _                        => None,
        }
    }
    fn is_nil(&self) -> bool {
        match self {
            OptForm::Nil            => true,
            OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
            _                       => false,
        }
    }
    fn call(&self, p: Rc<Self>, e: Rc<Self>, nc: Box<Cont<Self>>, metac: Cont<Self>) -> Cursor<Self> {
        match self {
            OptForm::PrimComb{eval_limit, ins} => {
                Cursor { f: Self::nil(), c: Cont::PramEval { eval_limit: *eval_limit, to_eval: p, collected: None, e, ins: *ins, nc: nc }, metac }
            }
            OptForm::DeriComb {se, de, params, body, code} => {
                let mut new_e = Rc::clone(se);
                if let Some(de) = de {
                    new_e = assoc(&de, Rc::clone(&e), new_e);
                }
                new_e = assoc(&params, p, new_e);
                Cursor { f: Rc::clone(body), c: Cont::Eval { e: new_e, nc: nc }, metac }
            }
            OptForm::ContComb(c) => {
                Cursor { f: p.car().unwrap(), c: Cont::Eval { e, nc: Box::new(c.clone()) }, metac: Cont::CatchRet { nc: nc, restore_meta: Box::new(metac) } }
            }
            OptForm::CodeForm { inner_value, .. } => {
                inner_value.call(p, e, nc, metac)
            }
            _ => {
                panic!("Tried to call not a Prim/DeriComb/ContComb {:?}, nc was {:?}", self, nc);
           }
        }
    }
    fn impl_prim(ins: PrimCombI, e: Rc<Self>, ps: Vec<Rc<Self>>, c: Cont<Self>, metac: Cont<Self>) -> Cursor<Self> {
        match ins {
            PrimCombI::Eval => Cursor { f: Rc::clone(&ps[0]), c: Cont::Eval { e: Rc::clone(&ps[1]), nc: Box::new(c) }, metac },
            PrimCombI::Vau => {
                let de     = ps[0].sym().map(|s| s.to_owned());
                let params = ps[1].sym().unwrap().to_owned();
                let body   = Rc::clone(&ps[2]);
                Cursor { f: Rc::new(OptForm::DeriComb { se: e, de, params, body, code: Some(Rc::new(RefCell::new(vec![]))) }), c, metac }
            },
            PrimCombI::If => if ps[0].truthy() {
                                 Cursor { f: Rc::clone(&ps[1]), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                             } else {
                                 Cursor { f: Rc::clone(&ps[2]), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                             },
     
            PrimCombI::Reset => Cursor { f: Rc::clone(&ps[0]),
                                         c: Cont::Eval { e: e, nc: Box::new(Cont::MetaRet) },
                                         metac: Cont::CatchRet { nc: Box::new(c), restore_meta: Box::new(metac) } },
            PrimCombI::Shift => Cursor { f: Rc::clone(&ps[0]),
                                         c: Cont::Call { p: Rc::new(OptForm::Pair(Rc::new(OptForm::ContComb(c)),
                                                                  Rc::new(OptForm::Nil))),
                                                         e: e,
                                                         nc: Box::new(Cont::MetaRet) },
                                         metac: Cont::CatchRet { nc: Box::new(metac.clone()), restore_meta: Box::new(metac) } },
            PrimCombI::Assert => {
                                    let thing = Rc::clone(&ps[0]);
                                    if !thing.truthy() {
                                        println!("Assert failed: {:?}", thing);
                                    }
                                    assert!(thing.truthy());
                                    Cursor { f: Rc::clone(&ps[1]), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                                },
     
            PrimCombI::Cell => Cursor { f: Rc::new(OptForm::Cell(RefCell::new(Rc::clone(&ps[0])))), c, metac },
            PrimCombI::Set  => match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                 OptForm::Cell(cell) => Cursor { f: cell.replace(Rc::clone(&ps[1])), c, metac },
                                 _             => panic!("set on not cell"),
                             },
            PrimCombI::Get => match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                OptForm::Cell(cell) => Cursor { f: Rc::clone(&cell.borrow()), c, metac },
                                _             => panic!("get on not cell"),
                            },
     
            PrimCombI::Cons  => Cursor { f: Rc::new(OptForm::Pair(Rc::clone(&ps[0]), Rc::clone(&ps[1]))), c, metac },
            PrimCombI::Car   => Cursor { f: ps[0].car().unwrap(), c, metac },
            PrimCombI::Cdr   => Cursor { f: ps[0].cdr().unwrap(), c, metac },
            PrimCombI::Quote => Cursor { f: Rc::clone(&ps[0]),                c, metac },
     
            PrimCombI::Eq     => Cursor { f: Rc::new(OptForm::Bool(ps[0]                == ps[1])),                c, metac },
            PrimCombI::Lt     => Cursor { f: Rc::new(OptForm::Bool(ps[0].int().unwrap()  < ps[1].int().unwrap())), c, metac },
            PrimCombI::LEq    => Cursor { f: Rc::new(OptForm::Bool(ps[0].int().unwrap() <= ps[1].int().unwrap())), c, metac },
            PrimCombI::Gt     => Cursor { f: Rc::new(OptForm::Bool(ps[0].int().unwrap()  > ps[1].int().unwrap())), c, metac },
            PrimCombI::GEq    => Cursor { f: Rc::new(OptForm::Bool(ps[0].int().unwrap() >= ps[1].int().unwrap())), c, metac },
     
            PrimCombI::Plus   => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   + ps[1].int().unwrap())), c, metac },
            PrimCombI::Minus  => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   - ps[1].int().unwrap())), c, metac },
            PrimCombI::Mult   => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   * ps[1].int().unwrap())), c, metac },
            PrimCombI::Div    => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   / ps[1].int().unwrap())), c, metac },
            PrimCombI::Mod    => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   % ps[1].int().unwrap())), c, metac },
            PrimCombI::And    => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   & ps[1].int().unwrap())), c, metac },
            PrimCombI::Or     => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   | ps[1].int().unwrap())), c, metac },
            PrimCombI::Xor    => Cursor { f: Rc::new(OptForm::Int(ps[0].int().unwrap()   ^ ps[1].int().unwrap())), c, metac },
     
            PrimCombI::CombP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                    OptForm::PrimComb { .. } => true,
                                    OptForm::DeriComb { .. } => true,
                                    _                        => false,
                                })), c, metac },
            PrimCombI::CellP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                    OptForm::Cell(_c) => true,
                                    _                 => false,
                                })), c, metac },
            PrimCombI::PairP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                    OptForm::Pair(_a,_b) => true,
                                    _                    => false,
                                })), c, metac },
            PrimCombI::SymbolP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                   OptForm::Symbol(_) => true,
                                   _                  => false,
                               })), c, metac },
            PrimCombI::IntP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                   OptForm::Int(_) => true,
                                   _               => false,
                               })), c, metac },
            PrimCombI::BoolP => Cursor { f: Rc::new(OptForm::Bool(match &*ps[0] {
                                // TODO OptForm::CodeForm { inner_value, .. } => inner_value.is_nil(),
                                    OptForm::Bool(_) => true,
                                    _             => false,
                                })), c, metac },
            PrimCombI::NilP =>  Cursor { f: Rc::new(OptForm::Bool(ps[0].is_nil())), c, metac },
        }
    }
}

fn assoc(k: &str, v: Rc<OptForm>, l: Rc<OptForm>) -> Rc<OptForm> {
    Rc::new(OptForm::Pair(
                Rc::new(OptForm::Pair(
                        Rc::new(OptForm::Symbol(k.to_owned())),
                        v)),
                l))
}
