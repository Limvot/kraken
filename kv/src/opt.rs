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
    DeriComb { se: Rc<OptForm>, de: Option<String>, params: String, body: Rc<OptForm> },
    ContComb(Cont<OptForm>),
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
            OptForm::DeriComb { se: _, de, params, body } => {
                write!(f, "<{} {} {}>", de.as_ref().unwrap_or(&"".to_string()), params, body)
            },
            OptForm::ContComb(_)    => write!(f, "<cont>"),
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
    fn nil() -> Rc<Self> {
        Rc::new(OptForm::Nil)
    }
    fn truthy(&self) -> bool {
        match self {
            OptForm::Bool(b) => *b,
            OptForm::Nil     => false,
            _                => true,
        }
    }
    fn int(&self) -> Option<i32> {
        match self {
            OptForm::Int(i) => Some(*i),
            _               => None,
        }
    }
    fn sym(&self) -> Option<&str> {
        match self {
            OptForm::Symbol(s) => Some(s),
            _                  => None,
        }
    }
    fn pair(&self) -> Option<(Rc<OptForm>,Rc<OptForm>)> {
        match self {
            OptForm::Pair(car, cdr) => Some((Rc::clone(car),Rc::clone(cdr))),
            _                       => None,
        }
    }
    fn car(&self) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(car, _cdr) => Some(Rc::clone(car)),
            _                        => None,
        }
    }
    fn cdr(&self) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(_car, cdr) => Some(Rc::clone(cdr)),
            _                        => None,
        }
    }
    fn is_nil(&self) -> bool {
        match self {
            OptForm::Nil            => true,
            _                       => false,
        }
    }
    fn append(&self, x: Rc<OptForm>) -> Option<Rc<OptForm>> {
        match self {
            OptForm::Pair(car, cdr) => cdr.append(x).map(|x| Rc::new(OptForm::Pair(Rc::clone(car), x))),
            OptForm::Nil            => Some(Rc::new(OptForm::Pair(x, Rc::new(OptForm::Nil)))),
            _                       => None,
        }
    }
    fn call(&self, p: Rc<Self>, e: Rc<Self>, nc: Box<Cont<Self>>, metac: Cont<Self>) -> Cursor<Self> {
        match self {
            OptForm::PrimComb{eval_limit, ins} => {
                Cursor { f: Self::nil(), c: Cont::PramEval { eval_limit: *eval_limit, to_eval: p, collected: None, e, ins: *ins, nc: nc }, metac }
            }
            OptForm::DeriComb {se, de, params, body} => {
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
            _ => {
                panic!("Tried to call not a Prim/DeriComb/ContComb {:?}, nc was {:?}", self, nc);
           }
        }
    }
    fn impl_prim(ins: PrimCombI, e: Rc<Self>, ps: Rc<Self>, c: Cont<Self>, metac: Cont<Self>) -> Cursor<Self> {
        match ins {
            PrimCombI::Eval => Cursor { f: ps.car().unwrap(), c: Cont::Eval { e: ps.cdr().unwrap().car().unwrap(), nc: Box::new(c) }, metac },
            PrimCombI::Vau => {
                let de     = ps.car().unwrap().sym().map(|s| s.to_owned());
                let params = ps.cdr().unwrap().car().unwrap().sym().unwrap().to_owned();
                let body   = ps.cdr().unwrap().cdr().unwrap().car().unwrap();
     
                Cursor { f: Rc::new(OptForm::DeriComb { se: e, de, params, body }), c, metac }
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
                                         c: Cont::Call { p: Rc::new(OptForm::Pair(Rc::new(OptForm::ContComb(c)),
                                                                  Rc::new(OptForm::Nil))),
                                                         e: e,
                                                         nc: Box::new(Cont::MetaRet) },
                                         metac: Cont::CatchRet { nc: Box::new(metac.clone()), restore_meta: Box::new(metac) } },
            PrimCombI::Assert => {
                                    let thing = ps.car().unwrap();
                                    if !thing.truthy() {
                                        println!("Assert failed: {:?}", thing);
                                    }
                                    assert!(thing.truthy());
                                    Cursor { f: ps.cdr().unwrap().car().unwrap(), c: Cont::Eval { e: e, nc: Box::new(c) }, metac }
                                },
     
            PrimCombI::Cell => Cursor { f: Rc::new(OptForm::Cell(RefCell::new(ps.car().unwrap()))), c, metac },
            PrimCombI::Set  => match &*ps.car().unwrap() {
                                 OptForm::Cell(cell) => Cursor { f: cell.replace(ps.cdr().unwrap().car().unwrap()), c, metac },
                                 _             => panic!("set on not cell"),
                             },
            PrimCombI::Get => match &*ps.car().unwrap() {
                                OptForm::Cell(cell) => Cursor { f: Rc::clone(&cell.borrow()), c, metac },
                                _             => panic!("get on not cell"),
                            },
     
            PrimCombI::Cons  => Cursor { f: Rc::new(OptForm::Pair(ps.car().unwrap(), ps.cdr().unwrap().car().unwrap())), c, metac },
            PrimCombI::Car   => Cursor { f: ps.car().unwrap().car().unwrap(), c, metac },
            PrimCombI::Cdr   => Cursor { f: ps.car().unwrap().cdr().unwrap(), c, metac },
            PrimCombI::Quote => Cursor { f: ps.car().unwrap(),                c, metac },
     
            PrimCombI::Eq     => Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap()                == ps.cdr().unwrap().car().unwrap())),                c, metac },
            PrimCombI::Lt     => Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap().int().unwrap()  < ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::LEq    => Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap().int().unwrap() <= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Gt     => Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap().int().unwrap()  > ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::GEq    => Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap().int().unwrap() >= ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
     
            PrimCombI::Plus   => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   + ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Minus  => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   - ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Mult   => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   * ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Div    => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   / ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Mod    => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   % ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::And    => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   & ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Or     => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   | ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
            PrimCombI::Xor    => Cursor { f: Rc::new(OptForm::Int(ps.car().unwrap().int().unwrap()   ^ ps.cdr().unwrap().car().unwrap().int().unwrap())), c, metac },
     
            PrimCombI::CombP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                    OptForm::PrimComb { .. } => true,
                                    OptForm::DeriComb { .. } => true,
                                    _                     => false,
                                })), c, metac },
            PrimCombI::CellP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                    OptForm::Cell(_c) => true,
                                    _              => false,
                                })), c, metac },
            PrimCombI::PairP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                    OptForm::Pair(_a,_b) => true,
                                    _                 => false,
                                })), c, metac },
            PrimCombI::SymbolP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                   OptForm::Symbol(_) => true,
                                   _               => false,
                               })), c, metac },
            PrimCombI::IntP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                   OptForm::Int(_) => true,
                                   _            => false,
                               })), c, metac },
            PrimCombI::BoolP => Cursor { f: Rc::new(OptForm::Bool(match &*ps.car().unwrap() {
                                    OptForm::Bool(_) => true,
                                    _             => false,
                                })), c, metac },
            PrimCombI::NilP =>  Cursor { f: Rc::new(OptForm::Bool(ps.car().unwrap().is_nil())), c, metac },
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
