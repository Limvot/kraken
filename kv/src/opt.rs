use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;

use crate::eval::{FormT,Cont,Cursor,PrimCombI};
use crate::basic::Form;

#[derive(Debug, Eq, PartialEq)]
struct Trace {}

#[derive(Debug, Eq, PartialEq)]
pub struct OptForm {
    inner: OptFormInner,
    trace: Option<Rc<Trace>>
}

#[derive(Debug, Eq, PartialEq)]
pub enum OptFormInner {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Cell(RefCell<Rc<OptForm>>),
    Pair(Rc<OptForm>,Rc<OptForm>),
    PrimComb { eval_limit: i32, ins: PrimCombI },
    DeriComb { se: Rc<OptForm>, de: Option<String>, params: String, body: Rc<OptForm>, code: Option<Rc<RefCell<Vec<ByteCode>>>> },
    ContComb(Cont<OptForm>),
}
#[derive(Debug, Eq, PartialEq)]
pub enum ByteCode {
    Ins(PrimCombI)
}

impl fmt::Display for OptForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            OptFormInner::Nil                   => write!(f, "nil"),
            OptFormInner::Int(i)                => write!(f, "{i}"),
            OptFormInner::Bool(b)               => write!(f, "{b}"),
            OptFormInner::Symbol(s)             => write!(f, "{s}"),
            OptFormInner::Cell(c)               => write!(f, "@{}", c.borrow()),
            OptFormInner::Pair(car, cdr)        => {
                write!(f, "({}", car)?;
                let mut traverse: Rc<OptForm> = Rc::clone(cdr);
                loop {
                    match &traverse.inner {
                        OptFormInner::Pair(ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        OptFormInner::Nil => {
                            write!(f, ")")?;
                            return Ok(());
                        },
                        x => {
                            write!(f, ". {})", traverse)?;
                            return Ok(());
                        },
                    }
                }
            },
            OptFormInner::PrimComb { eval_limit, ins }    => write!(f, "<{eval_limit}> - {ins:?}"),
            OptFormInner::DeriComb { se: _, de, params, body, code: _ } => {
                write!(f, "<{} {} {}>", de.as_ref().unwrap_or(&"".to_string()), params, body)
            },
            OptFormInner::ContComb(_)    => write!(f, "<cont>"),
        }
    }
}

impl From<&Form> for Rc<OptForm> {
    fn from(x: &Form) -> Self {
        match x {
            Form::Nil                           => OptForm::new(OptFormInner::Nil, None),
            Form::Int(i)                        => OptForm::new(OptFormInner::Int(*i), None),
            Form::Bool(b)                       => OptForm::new(OptFormInner::Bool(*b), None),
            Form::Symbol(s)                     => OptForm::new(OptFormInner::Symbol(s.to_owned()), None),
            Form::Cell(cell)                    => panic!("bad"),
            Form::Pair(car,cdr)                 => OptForm::new(OptFormInner::Pair((&**car).into(), (&**cdr).into()), None),
            Form::PrimComb { eval_limit, ins }  => OptForm::new(OptFormInner::PrimComb { eval_limit: *eval_limit, ins: *ins }, None),
            Form::DeriComb { .. }               => panic!("bad"),
            Form::ContComb(c)                   => panic!("bad"),
        }
    }
}

impl OptForm {
    fn tc(&self) -> Option<Rc<Trace>> {
        self.trace.as_ref().map(Rc::clone)
    }
    fn tor(&self, o: &Rc<OptForm>) -> Option<Rc<Trace>> {
        match &self.trace {
            Some(t) => Some(Rc::clone(t)),
            None    => o.trace.as_ref().map(Rc::clone)
        }
    }
    fn new(inner: OptFormInner, trace: Option<Rc<Trace>>) -> Rc<Self> { Rc::new(OptForm { inner, trace }) }
    fn truthy(&self) -> bool {
        match &self.inner {
            OptFormInner::Bool(b) => *b,
            OptFormInner::Nil     => false,
            _                     => true,
        }
    }
    fn int(&self) -> Option<i32> {
        match &self.inner {
            OptFormInner::Int(i)                       => Some(*i),
            _                                          => None,
        }
    }
    fn append(&self, x: Rc<OptForm>) -> Option<Rc<OptForm>> {
        let trace = self.tor(&x);
        match &self.inner {
            OptFormInner::Pair(car, cdr) => cdr.append(x).map(|x| OptForm::new(OptFormInner::Pair(Rc::clone(car), x), trace)),
            OptFormInner::Nil            => Some(OptForm::new(OptFormInner::Pair(x, OptForm::new(OptFormInner::Nil, trace.as_ref().map(Rc::clone))), trace)),
            _                            => None,
        }
    }
    pub fn congruent(&self, other: &Form) -> bool {
        match other {
            Form::Nil                           => self.inner == OptFormInner::Nil,
            Form::Int(i)                        => self.inner == OptFormInner::Int(*i),
            Form::Bool(b)                       => self.inner == OptFormInner::Bool(*b),
            Form::Symbol(s)                     => self.inner == OptFormInner::Symbol(s.to_owned()),
            Form::Cell(cell)                    => panic!("bad"),
            Form::Pair(car,cdr)                 => match &self.inner { OptFormInner::Pair(carp, cdrp) => carp.congruent(car) && cdrp.congruent(cdr), _ => false },
            Form::PrimComb { eval_limit, ins }  => self.inner == OptFormInner::PrimComb { eval_limit: *eval_limit, ins: *ins },
            Form::DeriComb { .. }               => panic!("bad"),
            Form::ContComb(c)                   => panic!("bad"),
        }
    }
}

impl FormT for OptForm {
    fn sym(&self) -> Option<&str> {
        match &self.inner {
            OptFormInner::Symbol(s) => Some(s),
            _                  => None,
        }
    }
    fn pair(&self) -> Option<(Rc<OptForm>,Rc<OptForm>)> {
        match &self.inner {
            OptFormInner::Pair(car, cdr) => Some((Rc::clone(car),Rc::clone(cdr))),
            _                       => None,
        }
    }
    fn car(&self) -> Option<Rc<OptForm>> {
        match &self.inner {
            OptFormInner::Pair(car, _cdr) => Some(Rc::clone(car)),
            _                        => None,
        }
    }
    fn cdr(&self) -> Option<Rc<OptForm>> {
        match &self.inner {
            OptFormInner::Pair(_car, cdr) => Some(Rc::clone(cdr)),
            _                        => None,
        }
    }
    fn is_nil(&self) -> bool {
        match &self.inner {
            OptFormInner::Nil            => true,
            _                       => false,
        }
    }
    fn call(&self, p: Rc<Self>, e: Rc<Self>, nc: Box<Cont<Self>>, metac: Cont<Self>) -> Cursor<Self> {
        match &self.inner {
            OptFormInner::PrimComb{eval_limit, ins} => {
                Cursor { f: OptForm::new(OptFormInner::Nil, None), c: Cont::PramEval { eval_limit: *eval_limit, to_eval: p, collected: None, e, ins: *ins, nc: nc }, metac }
            }
            OptFormInner::DeriComb {se, de, params, body, code} => {
                // TODO: Add traces here?
                let mut new_e = Rc::clone(se);
                if let Some(de) = de {
                    new_e = assoc(&de, Rc::clone(&e), new_e);
                }
                new_e = assoc(&params, p, new_e);
                Cursor { f: Rc::clone(body), c: Cont::Eval { e: new_e, nc: nc }, metac }
            }
            OptFormInner::ContComb(c) => {
                // TODO: Add traces here?
                Cursor { f: p.car().unwrap(), c: Cont::Eval { e, nc: Box::new(c.clone()) }, metac: Cont::CatchRet { nc: nc, restore_meta: Box::new(metac) } }
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
                // Add traces here? If no on-going?
                let de     = ps[0].sym().map(|s| s.to_owned());
                let params = ps[1].sym().unwrap().to_owned();
                let body   = Rc::clone(&ps[2]);
                Cursor { f: OptForm::new(OptFormInner::DeriComb { se: e, de, params, body, code: Some(Rc::new(RefCell::new(vec![]))) }, None), c, metac }
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
                                         // Trace here
                                         c: Cont::Call { p: OptForm::new(OptFormInner::Pair(OptForm::new(OptFormInner::ContComb(c), None),
                                                                                            OptForm::new(OptFormInner::Nil, None)), None),
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
     
            PrimCombI::Cell => Cursor { f: OptForm::new(OptFormInner::Cell(RefCell::new(Rc::clone(&ps[0]))), ps[0].tc()), c, metac },
            PrimCombI::Set  => match &ps[0].inner {
                                 OptFormInner::Cell(cell) => Cursor { f: cell.replace(Rc::clone(&ps[1])), c, metac },
                                 _             => panic!("set on not cell"),
                             },
            PrimCombI::Get => match &ps[0].inner {
                                OptFormInner::Cell(cell) => Cursor { f: Rc::clone(&cell.borrow()), c, metac },
                                _             => panic!("get on not cell"),
                            },
     
            PrimCombI::Cons  => Cursor { f: OptForm::new(OptFormInner::Pair(Rc::clone(&ps[0]), Rc::clone(&ps[1])), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Car   => Cursor { f: ps[0].car().unwrap(), c, metac },
            PrimCombI::Cdr   => Cursor { f: ps[0].cdr().unwrap(), c, metac },
            PrimCombI::Quote => Cursor { f: Rc::clone(&ps[0]),                c, metac },
     
            PrimCombI::Eq     => Cursor { f: OptForm::new(OptFormInner::Bool(ps[0]                == ps[1]), ps[0].tor(&ps[1])),                c, metac },
            PrimCombI::Lt     => Cursor { f: OptForm::new(OptFormInner::Bool(ps[0].int().unwrap()  < ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::LEq    => Cursor { f: OptForm::new(OptFormInner::Bool(ps[0].int().unwrap() <= ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Gt     => Cursor { f: OptForm::new(OptFormInner::Bool(ps[0].int().unwrap()  > ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::GEq    => Cursor { f: OptForm::new(OptFormInner::Bool(ps[0].int().unwrap() >= ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
     
            PrimCombI::Plus   => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   + ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Minus  => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   - ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Mult   => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   * ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Div    => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   / ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Mod    => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   % ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::And    => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   & ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Or     => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   | ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
            PrimCombI::Xor    => Cursor { f: OptForm::new(OptFormInner::Int(ps[0].int().unwrap()   ^ ps[1].int().unwrap()), ps[0].tor(&ps[1])), c, metac },
     
            PrimCombI::CombP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::PrimComb { .. } => true,
                                    OptFormInner::DeriComb { .. } => true,
                                    _                        => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::CellP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::Cell(_c) => true,
                                    _                 => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::PairP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::Pair(_a,_b) => true,
                                    _                    => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::SymbolP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::Symbol(_) => true,
                                    _                  => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::IntP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::Int(_) => true,
                                    _               => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::BoolP => Cursor { f: OptForm::new(OptFormInner::Bool(match &ps[0].inner {
                                    OptFormInner::Bool(_) => true,
                                    _             => false,
                                }), ps[0].tc()), c, metac },
            PrimCombI::NilP =>  Cursor { f: OptForm::new(OptFormInner::Bool(ps[0].is_nil()), ps[0].tc()), c, metac },
        }
    }
}

fn assoc(k: &str, v: Rc<OptForm>, l: Rc<OptForm>) -> Rc<OptForm> {
    let at = v.tc();
    let bt = v.tc();
    let tort = v.tor(&l);
    OptForm::new(OptFormInner::Pair(
                OptForm::new(OptFormInner::Pair(
                        OptForm::new(OptFormInner::Symbol(k.to_owned()), at),
                        v), bt),
                l), tort)
}
