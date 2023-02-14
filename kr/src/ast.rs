use std::fmt;
use std::rc::Rc;
use std::convert::From;
use std::collections::BTreeSet;
use std::result::Result;

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

pub enum PossibleTailCall {
    Result(Rc<Form>),
    TailCall(Rc<Form>, Rc<Form>),
}
#[derive(Debug, Eq, PartialEq)]
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(Rc<Form>,Rc<Form>),
    PrimComb(String, fn(Rc<Form>, Rc<Form>) -> PossibleTailCall),
    DeriComb { se: Rc<Form>, de: Option<String>, params: String, body: Rc<Form> },
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
            Form::Int(i)                => write!(f, "{}", i),
            Form::Bool(b)               => write!(f, "{}", b),
            Form::Symbol(s)             => write!(f, "{}", s),
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
                            write!(f, ". {})", x)?;
                            return Ok(());
                        },
                    }
                }
            },
            Form::PrimComb(name, _f)    => write!(f, "<{}>", name),
            Form::DeriComb { se, de, params, body } => {
                write!(f, "<{} {} {}>", de.as_ref().unwrap_or(&"".to_string()), params, body)
            },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NeededIds {
    True,
    None,
    Some(BTreeSet<i32>),
}
impl NeededIds {
    fn union(&self, other: &NeededIds) -> Self {
        match self {
            NeededIds::True => NeededIds::True,
            NeededIds::None => other.clone(),
            NeededIds::Some(set) => match other {
                NeededIds::True => NeededIds::True,
                NeededIds::None => self.clone(),
                NeededIds::Some(oset) => NeededIds::Some(set.union(oset).cloned().collect()),
            },
        }
    }
}
pub enum PossibleMarkedTailCall {
    Result(Rc<MarkedForm>),
    TailCall(Rc<MarkedForm>, Rc<MarkedForm>),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MarkedForm {
    Value(Rc<Form>),
    SuspendedSymbol { ids: NeededIds, name: String, crdi_carb: Option<(i32, bool)> },

    SuspendedPair(NeededIds, Rc<MarkedForm>, Rc<MarkedForm>),

    PrimComb { name: String, wrap_level: i32, f: fn(Rc<MarkedForm>, Rc<MarkedForm>) -> PossibleMarkedTailCall },

    DeriComb { ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: i32, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm> },
}
impl MarkedForm {
    pub fn unval(self: &Rc<MarkedForm>) -> Result<Rc<MarkedForm>, &'static str> {
        match &**self {
            MarkedForm::Value(form) => {
                match &**form {
                    Form::Nil => Ok(Rc::clone(self)),
                    Form::Int(i)=> Ok(Rc::clone(self)),
                    Form::Bool(b)=> Ok(Rc::clone(self)),
                    Form::PrimComb(n, f)=> Err("tried to unval a PrimComb that was the simpler version, need to figure this out"),
                    Form::DeriComb { se, de, params, body }=> Ok(Rc::clone(self)),

                    Form::Symbol(s) =>   Ok(Rc::new(MarkedForm::SuspendedSymbol { ids: NeededIds::True, name: s.clone(), crdi_carb: None })), 
                    Form::Pair(car,cdr) => Ok(Rc::new(MarkedForm::SuspendedPair( NeededIds::True,
                                                                                 Rc::new(MarkedForm::Value(Rc::clone(car))).unval()?,
                                                                                 Rc::new(MarkedForm::Value(Rc::clone(cdr))).unval()? ))),
                }
            },
            MarkedForm::SuspendedSymbol { ids, name, crdi_carb } => Err("trying to unval a suspended symbol"),
            MarkedForm::SuspendedPair(ids, car, cdr) => Err("trying to unval a suspended pair"),
            MarkedForm::PrimComb { name, wrap_level, f } => Ok(Rc::clone(self)),
            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => Ok(Rc::clone(self)),
        }
    }
}
impl fmt::Display for MarkedForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkedForm::Value(form)                              => write!(f, "{}", form),
            MarkedForm::SuspendedSymbol { ids, name, crdi_carb } => write!(f, "{:?}#{}({:?})", ids, name, crdi_carb),
            MarkedForm::PrimComb { name, wrap_level, .. }        => write!(f, "<{}{}>", name, wrap_level),

            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => write!(f, "{:?}#<{}/{:?}/{}/{}/{:?}/{:?}/{}>", ids, se, de, id, wrap_level, sequence_params, rest_params, body),

            MarkedForm::SuspendedPair(ids, car, cdr) => {
                write!(f, "{:?}#{{{}", ids, car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::SuspendedPair(ref _ids, ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        MarkedForm::Value(form) if match &**form { Form::Nil => true, _ => false } => {
                            write!(f, "}}")?;
                            return Ok(());
                        },
                        x => {
                            write!(f, ". {}}}", x)?;
                            return Ok(());
                        },
                    }
                }
            },
        }
    }
}
