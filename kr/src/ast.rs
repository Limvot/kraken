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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct EnvID(i32);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NeededIds {
    True,
    None,
    Some(BTreeSet<EnvID>),
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
pub struct Ctx {
    e : Rc<MarkedForm>,
}
impl Default for Ctx {
    fn default() -> Ctx {
        Ctx { e: Rc::new(MarkedForm::Value(root_env())) }
    }
}
pub fn partial_eval(ctx: Ctx, x: Rc<MarkedForm>) -> Result<(Ctx,Rc<MarkedForm>), String> {
    //Err("Not implemented".into())
    match &*x {
        MarkedForm::SuspendedSymbol { name, crdi_carb }       => {
            // (EnvID, i32, bool)
            if let Some((id, idx, rest)) = crdi_carb {
                // lookup in stack
                Ok((ctx, x))
            } else {
                // lookup in current env, replace with value (might be it's own SuspendedSymbol - maybe path should be split out?)
                Ok((ctx, x))
            }
        },
        MarkedForm::SuspendedPair(ids, car, cdr) => {
            let (ctx, car) = partial_eval(ctx, Rc::clone(car))?;
            let (ctx, cdr) = partial_eval(ctx, Rc::clone(cdr))?;
            // update IDs
            Ok((ctx, Rc::new(MarkedForm::SuspendedPair(ids.clone(), car, cdr))))
        },
        MarkedForm::PrimComb { .. }              => Ok((ctx, x)),
        MarkedForm::DeriComb { .. }              => Ok((ctx, x)),
        MarkedForm::Value(form)                  => Ok((ctx, x)),
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MarkedForm {
    Value(Rc<Form>),

    SuspendedSymbol { name: String, crdi_carb: Option<(EnvID, i32, bool)> },
    SuspendedPair(NeededIds, Rc<MarkedForm>, Rc<MarkedForm>),

    PrimComb { name: String, wrap_level: i32, f: fn(Rc<MarkedForm>, Rc<MarkedForm>) -> PossibleMarkedTailCall },

    DeriComb { ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: EnvID, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm> },
}
impl MarkedForm {
    pub fn unval(self: &Rc<MarkedForm>) -> Result<Rc<MarkedForm>, &'static str> {
        match &**self {
            MarkedForm::Value(form) => {
                match &**form {
                    Form::Nil            => Ok(Rc::clone(self)),
                    Form::Int(i)         => Ok(Rc::clone(self)),
                    Form::Bool(b)        => Ok(Rc::clone(self)),
                    Form::PrimComb(n, f) => Err("tried to unval a PrimComb that was the simpler version, need to figure this out"),
                    Form::DeriComb { .. }=> Ok(Rc::clone(self)),

                    Form::Symbol(s)      => Ok(Rc::new(MarkedForm::SuspendedSymbol { name: s.clone(), crdi_carb: None })), 
                    Form::Pair(car,cdr)  => Ok(Rc::new(MarkedForm::SuspendedPair(NeededIds::True,
                                                                                 Rc::new(MarkedForm::Value(Rc::clone(car))).unval()?,
                                                                                 Rc::new(MarkedForm::Value(Rc::clone(cdr))).unval()? ))),
                }
            },
            MarkedForm::SuspendedSymbol { .. }       => Err("trying to unval a suspended symbol"),
            MarkedForm::SuspendedPair(ids, car, cdr) => Err("trying to unval a suspended pair"),
            MarkedForm::PrimComb { .. }              => Ok(Rc::clone(self)),
            MarkedForm::DeriComb { .. }              => Ok(Rc::clone(self)),
        }
    }
}
impl fmt::Display for MarkedForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkedForm::Value(form)                              => write!(f, "{}", form),
            MarkedForm::SuspendedSymbol { name, crdi_carb }      => write!(f, "{}({:?})", name, crdi_carb),
            MarkedForm::PrimComb { name, wrap_level, .. }        => write!(f, "<{}{}>", name, wrap_level),

            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => write!(f, "{:?}#<{}/{:?}/{:?}/{}/{:?}/{:?}/{}>", ids, se, de, id, wrap_level, sequence_params, rest_params, body),

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

pub fn eval(e: Rc<Form>, f: Rc<Form>) -> Rc<Form> {
    let mut e = e;
    let mut x = Option::Some(f);
    loop {
        let cur = x.take().unwrap();
        //println!("Evaluating {:?} in {:?}", cur, e);
        match *cur {
            Form::Symbol(ref s) => {
                let mut t = e;
                //println!("Looking up {} in {:?}", s, t);
                //println!("Looking up {}", s);
                while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                    t = t.cdr().unwrap();
                }
                return t.car().unwrap().cdr().unwrap();
            },
            Form::Pair(ref c, ref p) => {
                let comb = eval(Rc::clone(&e), Rc::clone(c));
                match *comb {
                    Form::PrimComb(ref _n,  ref f) => match f(e, Rc::clone(p)) {
                        PossibleTailCall::Result(r) => return r,
                        PossibleTailCall::TailCall(ne, nx) => {
                            e = ne;
                            x = Some(nx);
                        },
                    },
                    Form::DeriComb{ref se, ref de, ref params, ref body } => {
                        let mut new_e = Rc::clone(se);
                        if let Some(de) = de {
                            new_e = assoc(de, Rc::clone(&e), new_e);
                        }
                        new_e = assoc(params, Rc::clone(p), new_e);
                        // always a tail call
                        e = new_e;
                        x = Some(Rc::clone(body));
                    },
                    _ => panic!("Tried to call not a Prim/DeriComb {:?}", comb),
                }
            },
            _ => return cur,
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
        // TODO: Should be properly tail recursive
        ("eval", Rc::new(Form::PrimComb("eval".to_owned(), |e, p| {
            //println!("To get eval body, evaluating {:?} in {:?}", p.car(), e);
            let b = eval(Rc::clone(&e), p.car().unwrap());
            let e = if let Some(ne) = p.cdr().unwrap().car() {
                //println!("To get eval env, evaluating {:?} in {:?}", ne, e);
                eval(e, ne)
            } else { e };
            //println!("Evaling {:?} in {:?}", b, e);
            PossibleTailCall::TailCall(e, b)
        }))),
        // (vau de params body)
        ("vau", Rc::new(Form::PrimComb("vau".to_owned(), |e, p| {
            let de     = p.car().unwrap().sym().map(|s| s.to_owned());
            let params = p.cdr().unwrap().car().unwrap().sym().unwrap().to_owned();
            let body   = p.cdr().unwrap().cdr().unwrap().car().unwrap();

            PossibleTailCall::Result(Rc::new(Form::DeriComb { se: e, de, params, body }))
        }))),
        ("=", Rc::new(Form::PrimComb("=".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a == b)))
        }))),
        ("<", Rc::new(Form::PrimComb("<".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a.int().unwrap() < b.int().unwrap())))
        }))),
        (">", Rc::new(Form::PrimComb(">".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a.int().unwrap() > b.int().unwrap())))
        }))),
        ("<=", Rc::new(Form::PrimComb("<=".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a.int().unwrap() <= b.int().unwrap())))
        }))),
        (">=", Rc::new(Form::PrimComb(">=".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a.int().unwrap() >= b.int().unwrap())))
        }))),
        ("if", Rc::new(Form::PrimComb("if".to_owned(), |e, p| {
            if eval(Rc::clone(&e), p.car().unwrap()).truthy() {
                PossibleTailCall::TailCall(e, p.cdr().unwrap().car().unwrap())
            } else if let Some(els) = p.cdr().unwrap().cdr().and_then(|x| x.car()) {
                PossibleTailCall::TailCall(e, els)
            } else {
                // should we really allow this? (2 arg if with no else)
                PossibleTailCall::Result(Rc::new(Form::Nil))
            }
        }))),
        ("cons", Rc::new(Form::PrimComb("cons".to_owned(), |e, p| {
            let h = eval(Rc::clone(&e), p.car().unwrap());
            let t = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Pair(h, t)))
        }))),
        ("car", Rc::new(Form::PrimComb("car".to_owned(), |e, p| {
            PossibleTailCall::Result(eval(Rc::clone(&e), p.car().unwrap()).car().unwrap())
        }))),
        ("cdr", Rc::new(Form::PrimComb("cdr".to_owned(), |e, p| {
            PossibleTailCall::Result(eval(Rc::clone(&e), p.car().unwrap()).cdr().unwrap())
        }))),
        ("quote", Rc::new(Form::PrimComb("quote".to_owned(), |_e, p| {
            PossibleTailCall::Result(p.car().unwrap())
        }))),

        ("debug", Rc::new(Form::PrimComb("debug".to_owned(), |e, p| {
            //println!("Debug: {:?}", eval(Rc::clone(&e), p.car().unwrap()));
            println!("Debug: {}", eval(Rc::clone(&e), p.car().unwrap()));
            PossibleTailCall::TailCall(e, p.cdr().unwrap().car().unwrap())
        }))),
        ("assert", Rc::new(Form::PrimComb("assert".to_owned(), |e, p| {
            let thing = eval(Rc::clone(&e), p.car().unwrap());
            if !thing.truthy() {
                println!("Assert failed: {:?}", thing);
            }
            assert!(thing.truthy());
            PossibleTailCall::TailCall(e, p.cdr().unwrap().car().unwrap())
        }))),

        ("+", Rc::new(Form::PrimComb("+".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a + b)))
        }))),
        ("-", Rc::new(Form::PrimComb("-".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a - b)))
        }))),
        ("*", Rc::new(Form::PrimComb("*".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a * b)))
        }))),
        ("/", Rc::new(Form::PrimComb("/".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a / b)))
        }))),
        ("%", Rc::new(Form::PrimComb("%".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a % b)))
        }))),
        ("&", Rc::new(Form::PrimComb("&".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a & b)))
        }))),
        ("|", Rc::new(Form::PrimComb("|".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a | b)))
        }))),
        ("^", Rc::new(Form::PrimComb("^".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            PossibleTailCall::Result(Rc::new(Form::Int(a ^ b)))
        }))),

        ("comb?", Rc::new(Form::PrimComb("comb?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::PrimComb(_n, _f)  => true,
                Form::DeriComb { .. } => true,
                _                     => false,
            })))
        }))),
        ("pair?", Rc::new(Form::PrimComb("pair?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Pair(_a,_b) => true,
                _                 => false,
            })))
        }))),
        ("symbol?", Rc::new(Form::PrimComb("symbol?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Symbol(_) => true,
                _               => false,
            })))
        }))),
        ("int?", Rc::new(Form::PrimComb("int?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Int(_) => true,
                _            => false,
            })))
        }))),
        // maybe bool? but also could be derived. Nil def
        ("bool?", Rc::new(Form::PrimComb("bool?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Bool(_) => true,
                _             => false,
            })))
        }))),
        ("nil?", Rc::new(Form::PrimComb("nil?".to_owned(), |e, p| {
            PossibleTailCall::Result(Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Nil => true,
                _         => false,
            })))
        }))),

        // consts
        ("true",  Rc::new(Form::Bool(true))),
        ("false", Rc::new(Form::Bool(false))),
        ("nil",   Rc::new(Form::Nil)),
    ])
}
