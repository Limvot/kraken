use std::fmt;
use std::rc::Rc;
use std::convert::From;
use std::collections::{BTreeSet,BTreeMap,hash_map::DefaultHasher};
use std::result::Result;
use std::iter;
use std::hash::{Hash,Hasher};


// TODO:
// -extend vau & env logic and SuspendedPair PE with sequence_params & wrap_level
// -add current-hashes to if
// -expand combiner_Return_ok with (func ...params) | func doesn't take de and func+params are return ok
// -add recursive drop redundent veval
// -mark rec-hash on DeriComb
// -add compiler
//
// -use current fake comb ids instead of hashes
//      -do they have to be added to not-under thing like hashes
// -make cons work only for SuspendedParam/Env

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
impl fmt::Display for MarkedForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkedForm::Nil                   => write!(f, "nil"),
            MarkedForm::Int(i)                => write!(f, "{}", i),
            MarkedForm::Bool(b)               => write!(f, "{}", b),
            MarkedForm::Symbol(s)             => write!(f, "{}", s),
            MarkedForm::Pair(hash, ids, car, cdr)   => {
                //write!(f, "{:?}#({}", ids, car)?;
                write!(f, "({}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair(ref hash, ref ids, ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        MarkedForm::Nil => {
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
            MarkedForm::SuspendedEnvEval { hash, ids, x, e }            => write!(f, "({:?}){{Sveval {} {}}}", ids, x, e),
            MarkedForm::SuspendedIf      { hash, ids, c, t, e }         => write!(f, "({:?}){{Sif {} {} {}}}", ids, c, t, e),
            MarkedForm::SuspendedSymbol(name)                           => write!(f, "{}", name),
            MarkedForm::SuspendedEnvLookup { name, id }                 => write!(f, "{:?}({:?}env)", name, id),
            MarkedForm::SuspendedParamLookup { name, id, cdr_num, car } => write!(f, "{:?}({:?}{}{})", name, id, cdr_num, car),
            MarkedForm::PrimComb { name, wrap_level, .. }               => write!(f, "<{}{}>", name, wrap_level),

            //MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => write!(f, "{:?}#[{}/{:?}/{:?}/{}/{:?}/{:?}/{}]", ids, se, de, id, wrap_level, sequence_params, rest_params, body),
            MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
                //let env_form = format!("{}", se);
                write!(f, "{:?}#[{:?}/{:?}/{:?}/{}/{:?}/{:?}/{}]", ids, lookup_name, de, id, wrap_level, sequence_params, rest_params, body)
            },

            MarkedForm::SuspendedPair{ hash, ids, attempted, car, cdr } => {
                //write!(f, "{:?}{:?}#{{{}", ids, attempted, car)?;
                write!(f, "{{{}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair(ref hash, ref ids, ref carp, ref cdrp) => {
                            write!(f, " {}", carp)?;
                            traverse = Rc::clone(cdrp);
                        },
                        MarkedForm::Nil => {
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
fn massoc(k: &str, v: Rc<MarkedForm>, l: Rc<MarkedForm>) -> Rc<MarkedForm> {
    MarkedForm::new_pair(
                MarkedForm::new_pair(
                        Rc::new(MarkedForm::Symbol(k.to_owned())),
                        v),
                l)
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
        ("eval", Rc::new(Form::PrimComb("eval".to_owned(), |e, p| {
            let b = eval(Rc::clone(&e), p.car().unwrap());
            let e = eval(e, p.cdr().unwrap().car().unwrap());
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
            } else {
                PossibleTailCall::TailCall(e, p.cdr().unwrap().cdr().unwrap().car().unwrap())
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

#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct EnvID(i32);
#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct MFHash(u64);
impl MFHash {
    pub fn combine(&self, other: &MFHash) -> Self {
        let mut h = DefaultHasher::new();
        "combine/".hash(&mut h); self.0.hash(&mut h); other.hash(&mut h);
        MFHash(h.finish())
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum NeededIds {
    True(BTreeSet<EnvID>),
    None(BTreeSet<EnvID>),
    Some(BTreeSet<EnvID>,BTreeSet<EnvID>),
}
impl NeededIds {
    fn new_true()           -> Self { NeededIds::True(                         BTreeSet::new()) }
    fn new_none()           -> Self { NeededIds::None(                         BTreeSet::new()) }
    fn new_single(i: EnvID) -> Self { NeededIds::Some(iter::once(i).collect(), BTreeSet::new()) }
    fn needs_nothing(&self) -> bool {
        match self {
            NeededIds::True(    under) => false,
            NeededIds::None(    under) => under.is_empty(),
            NeededIds::Some(set,under) => false,
        }
    }
    fn is_true(&self) -> bool {
        match self {
            NeededIds::True(    under) => true,
            NeededIds::None(    under) => false,
            NeededIds::Some(set,under) => false,
        }
    }
    fn under(&self) -> &BTreeSet<EnvID> {
        match self {
            NeededIds::True(under)        => under,
            NeededIds::None(under)        => under,
            NeededIds::Some(needed,under) => under,
        }
    }
    fn union(&self, other: &NeededIds) -> Self {
        match self {
            // add assert that otherhashes!={} -> hashes=={}
            NeededIds::True(under)      => NeededIds::True(under.union(other.under()).cloned().collect()),
            NeededIds::None(under)      => other.union_under(under),
            NeededIds::Some(set, under) => match other {
                NeededIds::True(ounder)      => NeededIds::True(                                    under.union(ounder).cloned().collect()),
                NeededIds::None(ounder)      => NeededIds::Some(set.clone(),                        under.union(ounder).cloned().collect()),
                NeededIds::Some(oset,ounder) => NeededIds::Some(set.union(oset).cloned().collect(), under.union(ounder).cloned().collect()),
            },
        }
    }
    fn union_without(&self, other: &NeededIds, without: EnvID) -> Self {
        self.union(other).without(without)
    }
    fn without(self, without: EnvID) -> Self {
        match self {
            NeededIds::True(_)      => self,
            NeededIds::None(_)      => self,
            NeededIds::Some(set, under) => {
                let new: BTreeSet<EnvID> = set.into_iter().filter(|x| *x != without).collect();
                if new.is_empty() {
                    NeededIds::None(under)
                } else {
                    NeededIds::Some(new, under)
                }
            },
        }
    }
    fn without_under(self, without: &EnvID) -> Self {
        match self {
            NeededIds::True(     under) => NeededIds::True(      under.into_iter().filter(|x| x != without).collect()),
            NeededIds::None(     under) => NeededIds::None(      under.into_iter().filter(|x| x != without).collect()),
            NeededIds::Some(set, under) => NeededIds::Some(set, (under.into_iter().filter(|x| x != without).collect())),
        }
    }
    fn union_under(&self, other: &BTreeSet<EnvID>) -> Self {
        match self {
            // add assert that otherhashes!={} -> hashes=={}
            NeededIds::True(     under)  => NeededIds::True(             other.union(under).cloned().collect()),
            NeededIds::None(     under)  => NeededIds::None(             other.union(under).cloned().collect()),
            NeededIds::Some(set, under)  => NeededIds::Some(set.clone(), other.union(under).cloned().collect()),
        }
    }
    // This should kinda eliminate True, as it can't progress, but we still want true in the sense
    // that it could contain all sorts of IDs
    // True(<nonempty>) should really only exist if kkkkkkkkkkkkkk
    fn add_under(&self, u: EnvID) -> Self {
        match self {
            NeededIds::True(     under) => NeededIds::True(             under.iter().cloned().chain(iter::once(u)).collect()),
            NeededIds::None(     under) => NeededIds::None(             under.iter().cloned().chain(iter::once(u)).collect()),
            NeededIds::Some(set, under) => NeededIds::Some(set.clone(), under.iter().cloned().chain(iter::once(u)).collect()),
        }
    }
    fn add_id(&self, i: EnvID) -> Self {
        match self {
            NeededIds::True(     under) => NeededIds::True(                                                    under.clone()),
            NeededIds::None(     under) => NeededIds::Some(iter::once(i).collect(),                            under.clone()),
            NeededIds::Some(set, under) => NeededIds::Some(set.iter().cloned().chain(iter::once(i)).collect(), under.clone()),
        }
    }
    fn may_contain_id(&self, i: EnvID) -> bool {
        match self {
            NeededIds::True(     under) => true,
            NeededIds::None(     under) => false,
            NeededIds::Some(set, under) => set.contains(&i),
        }
    }
}

#[derive(Clone)]
pub struct BCtx {
    id_counter: i32
}
impl BCtx {
    pub fn new_id(mut self) -> (Self, EnvID) {
        let new_id = EnvID(self.id_counter);
        self.id_counter += 1;
        (self, new_id)
    }
}


// force is for drop_redundent_eval, which will be integrated now
// memo is only for recording currently executing hashes (calls and if's, current for us)
// only_head is not currently used
//only_head env env_counter memo env_stack force
#[derive(Clone)]
pub struct DCtx {
    e : Rc<MarkedForm>,
    current_id: Option<EnvID>,
    sus_env_stack: Rc<BTreeMap<EnvID, Rc<MarkedForm>>>,
    sus_prm_stack: Rc<BTreeMap<EnvID, Rc<MarkedForm>>>,
    real_set: Rc<BTreeSet<EnvID>>,
    fake_set: Rc<BTreeSet<EnvID>>,
    fake_if_set: Rc<BTreeSet<EnvID>>,
    ident: usize,
}
impl DCtx {
    pub fn copy_set_env(&self, e: &Rc<MarkedForm>) -> Self {
        DCtx { e: Rc::clone(e), current_id: self.current_id.clone(), sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack), real_set: Rc::clone(&self.real_set), fake_set: Rc::clone(&self.fake_set), fake_if_set: Rc::clone(&self.fake_if_set), ident: self.ident+1  }
    }
    //pub fn copy_push_hash(&self, h: MFHash) -> Result<Self,&'static str> {
        //if !self.current.contains(&h) {
            //Ok(DCtx { e: Rc::clone(&self.e), sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack), real_set: Rc::clone(&self.real_set), current: Rc::new(self.current.iter().cloned().chain(iter::once(h)).collect()), ident: self.ident })
        //} else {
            //Err("hash already in")
        //}
    //}
    pub fn copy_push_frame(&self, id: EnvID, se: &Rc<MarkedForm>, de: &Option<String>, e: Option<Rc<MarkedForm>>, rest_params: &Option<String>, prms: Option<Rc<MarkedForm>>, body: &Rc<MarkedForm>) -> Result<Self,EnvID> {
        let mut sus_env_stack = Rc::clone(&self.sus_env_stack);
        let mut sus_prm_stack = Rc::clone(&self.sus_prm_stack);
        let mut real_set = (*self.real_set).clone();
        let mut fake_set = (*self.fake_set).clone();
        if self.fake_if_set.contains(&id) {
            println!("Fake if real rec stopper");
            return Err(id);
        }
        if (e.is_some() || prms.is_some()) {
            real_set.insert(id.clone());
            // We're not actually not under fake still!
            //fake_set.remove(&id);
        } else {
            if fake_set.contains(&id) {
                return Err(id.clone());
            }
            fake_set.insert(id.clone());
            real_set.remove(&id);
        }
        let inner_env = if let Some(de) = de {
            let de_val = if let Some(e) = e {
                Rc::make_mut(&mut sus_env_stack).insert(id.clone(), Rc::clone(&e));
                e
            } else {
                Rc::make_mut(&mut sus_env_stack).remove(&id);
                Rc::new(MarkedForm::SuspendedEnvLookup { name: Some(de.clone()), id: id.clone() })
            };
            massoc(de, de_val, Rc::clone(se))
        } else { Rc::clone(se) };
        // not yet supporting sequence params
        let inner_env = if let Some(p)  = rest_params {
            let p_val = if let Some(prms) = prms {
                Rc::make_mut(&mut sus_prm_stack).insert(id.clone(), Rc::clone(&prms));
                prms
            } else {
                Rc::make_mut(&mut sus_prm_stack).remove(&id);
                Rc::new(MarkedForm::SuspendedParamLookup { name: Some(p.clone()), id: id.clone(), cdr_num: 0, car: false })
            };
            massoc(p, p_val, inner_env)
        } else { inner_env };
        Ok(DCtx { e: inner_env, current_id: Some(id), sus_env_stack, sus_prm_stack, real_set: Rc::new(real_set), fake_set: Rc::new(fake_set), fake_if_set: Rc::clone(&self.fake_if_set), ident: self.ident+1 })
    }
    pub fn copy_push_fake_if(&self) -> Self {
        let new_fake_if_set = if let Some(current_id) = self.current_id.as_ref() {
            let mut x = (*self.fake_if_set).clone();
            x.insert(current_id.clone());
            Rc::new(x)
        } else { Rc::clone(&self.fake_if_set) };
        DCtx { e: Rc::clone(&self.e), current_id: self.current_id.clone(), sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack), real_set: Rc::clone(&self.real_set), fake_set: Rc::clone(&self.fake_set), fake_if_set: new_fake_if_set, ident: self.ident+1 }
    }

    pub fn can_progress(&self, ids: NeededIds) -> bool {
        // check if ids is true || ids intersection EnvIDs in our stacks is non empty || ids.hashes - current is non empty
        match ids {
            NeededIds::True(    under) => under.is_empty()                   || (!(self.fake_set.union(&self.fake_if_set).cloned().collect::<BTreeSet<EnvID>>()).is_superset(&under)), //true, - if we have hashes, that means we don't know what's in but can't progress b/c hashes
            NeededIds::None(    under) =>                                        !(self.fake_set.union(&self.fake_if_set).cloned().collect::<BTreeSet<EnvID>>()).is_superset(&under),
            NeededIds::Some(ids,under) => (!self.real_set.is_disjoint(&ids)) || (!(self.fake_set.union(&self.fake_if_set).cloned().collect::<BTreeSet<EnvID>>()).is_superset(&under)),
        }
    }
}

pub fn new_base_ctxs() -> (BCtx,DCtx) {
    let bctx = BCtx { id_counter: 0 };
    let (bctx, root_env) = mark(root_env(), bctx);
    (bctx, DCtx { e: root_env, current_id: None, sus_env_stack: Rc::new(BTreeMap::new()), sus_prm_stack: Rc::new(BTreeMap::new()), real_set: Rc::new(BTreeSet::new()), fake_set: Rc::new(BTreeSet::new()), fake_if_set: Rc::new(BTreeSet::new()), ident: 0 } )
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum Attempted {
    True(Option<NeededIds>),
    False,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MarkedForm {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(MFHash, NeededIds, Rc<MarkedForm>,Rc<MarkedForm>),

    SuspendedSymbol(String),
    SuspendedParamLookup { name: Option<String>, id: EnvID, cdr_num: i32, car: bool },
    SuspendedEnvLookup   { name: Option<String>, id: EnvID },
    // resume hash is folded into ids
    SuspendedPair { hash: MFHash, ids: NeededIds, attempted: Attempted, car: Rc<MarkedForm>, cdr: Rc<MarkedForm>},

    SuspendedEnvEval { hash: MFHash, ids: NeededIds, x: Rc<MarkedForm>, e: Rc<MarkedForm> },
    SuspendedIf      { hash: MFHash, ids: NeededIds, c: Rc<MarkedForm>, t: Rc<MarkedForm>, e: Rc<MarkedForm> },

    PrimComb { name: String, nonval_ok: bool, takes_de: bool, wrap_level: i32, f: fn(BCtx,DCtx,Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>),String> },
    DeriComb { hash: MFHash, lookup_name: Option<String>, ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: EnvID, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm> },
}
impl MarkedForm {
    pub fn new_suspended_env_eval(x: Rc<MarkedForm>, e: Rc<MarkedForm>) -> Rc<MarkedForm> {
        let mut h = DefaultHasher::new();
        "SuspendedEnvEval(x,e)".hash(&mut h); x.hash().hash(&mut h); e.hash().hash(&mut h);
        Rc::new(MarkedForm::SuspendedEnvEval{ hash: MFHash(h.finish()), ids: e.ids(), x, e })
    }
    pub fn new_suspended_if(c: Rc<MarkedForm>, t: Rc<MarkedForm>, e: Rc<MarkedForm>, rec_under: Option<EnvID>) -> Rc<MarkedForm> {
        let mut h = DefaultHasher::new();
        "SuspendedIf(c,t,e)".hash(&mut h); c.hash().hash(&mut h); t.hash().hash(&mut h); e.hash().hash(&mut h);
        let new_ids = c.ids().union(&t.ids()).union(&e.ids());
        let new_ids = if let Some(rec_under) = rec_under { new_ids.add_under(rec_under) } else { new_ids };
        Rc::new(MarkedForm::SuspendedIf{ hash: MFHash(h.finish()), ids: new_ids, c, t, e })
    }
    pub fn new_pair(car: Rc<MarkedForm>, cdr: Rc<MarkedForm>) -> Rc<MarkedForm> {
        let mut h = DefaultHasher::new();
        "Pair(ids,car,cdr)".hash(&mut h); car.hash().hash(&mut h); cdr.hash().hash(&mut h);
        let new_ids = car.ids().union(&cdr.ids());
        //println!("For new pair, union of {:?} and {:?} is  {:?}", car.ids(), cdr.ids(), new_ids); 
        Rc::new(MarkedForm::Pair(MFHash(h.finish()), new_ids, car, cdr))
    }
    pub fn new_suspended_pair(attempted: Attempted, car: Rc<MarkedForm>, cdr: Rc<MarkedForm>, rec_under: Option<EnvID>) -> Rc<MarkedForm> {
        let mut h = DefaultHasher::new();
        "SuspendedPair".hash(&mut h); attempted.hash(&mut h); car.hash().hash(&mut h); cdr.hash().hash(&mut h);
        let ids = car.ids().union(&cdr.ids());
        let ids = match ids {
            NeededIds::True(_)      => ids,
            NeededIds::None(under) => match &attempted {
                                           Attempted::False            => NeededIds::True(under),
                                           Attempted::True(Some(oids)) => oids.union_under(&under),
                                           Attempted::True(None)       => NeededIds::None(under),
                                       },
            NeededIds::Some(_,_)    => ids,
        };
        let ids = if let Some(rec_under) = rec_under { ids.add_under(rec_under) } else { ids };
        Rc::new(MarkedForm::SuspendedPair{ hash: MFHash(h.finish()), attempted, ids, car, cdr })
    }
    pub fn new_deri_comb(se: Rc<MarkedForm>, lookup_name: Option<String>, de: Option<String>, id: EnvID, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm>, rec_under: Option<EnvID>) -> Rc<MarkedForm> {
        let mut h = DefaultHasher::new();
        "DeriComb".hash(&mut h); se.hash().hash(&mut h); de.hash(&mut h); id.hash(&mut h); wrap_level.hash(&mut h);
        sequence_params.hash(&mut h); rest_params.hash(&mut h); body.hash().hash(&mut h);
        let ids = se.ids().union_without(&body.ids(), id.clone());
        let ids = if let Some(rec_under) = rec_under {
            ids.add_under(rec_under)
        } else {
            ids.without_under(&id)
        };
        Rc::new(MarkedForm::DeriComb{ hash: MFHash(h.finish()), lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body })
    }
    pub fn tag_name(self: &Rc<MarkedForm>, name: &str) -> Rc<MarkedForm> {
        match &**self {
            MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } =>
                 Rc::new(MarkedForm::DeriComb { hash: hash.clone(), lookup_name: Some(name.to_owned()), ids: ids.clone(), se: Rc::clone(se), de: de.clone(), id: id.clone(), wrap_level: *wrap_level, sequence_params: sequence_params.clone(), rest_params: rest_params.clone(), body: Rc::clone(body) }),
            _ => Rc::clone(self),
        }
    }
    pub fn hash(&self) -> MFHash {
        let mut h = DefaultHasher::new();
        {
            let state = &mut h;
            match self {
                MarkedForm::Nil                                            => { "Nil".hash(state); },
                MarkedForm::Int(i)                                         => { "Int(i)".hash(state); i.hash(state); },
                MarkedForm::Bool(b)                                        => { "Bool(b)".hash(state); b.hash(state); },
                MarkedForm::Symbol(s)                                      => { "Symbol(s)".hash(state); s.hash(state); }, 
                MarkedForm::Pair(hash,ids,car,cdr)                         => { return *hash; },
                MarkedForm::SuspendedSymbol(name)                          => { "SuspendedSymbol(name)".hash(state); name.hash(state); },
                MarkedForm::SuspendedParamLookup { name, id, cdr_num, car }=> { "SuspendedParamLookup".hash(state); name.hash(state); id.hash(state); cdr_num.hash(state); car.hash(state); },
                MarkedForm::SuspendedEnvEval { hash, .. }                  => { return *hash; },
                MarkedForm::SuspendedIf      { hash, .. }                  => { return *hash; },
                MarkedForm::SuspendedEnvLookup { name, id }                => { "SuspendedEnvLookup".hash(state); name.hash(state); id.hash(state); },
                MarkedForm::SuspendedPair{ hash, .. }                      => { return *hash; },
                MarkedForm::PrimComb { name, wrap_level, .. }              => { "PrimComb".hash(state); name.hash(state); wrap_level.hash(state); },
                MarkedForm::DeriComb { hash, .. }                          => { return *hash; },
            }
        }
        MFHash(h.finish())
    }
    pub fn wrap_level(&self) -> Option<i32> {
        match self {
            MarkedForm::PrimComb { wrap_level, .. } => Some(*wrap_level),
            MarkedForm::DeriComb { wrap_level, .. } => Some(*wrap_level),
            _ => None,
        }
    }
    pub fn decrement_wrap_level(&self) -> Option<Rc<Self>> {
        match self {
            MarkedForm::PrimComb { name, nonval_ok, takes_de, wrap_level, f } => Some(Rc::new(MarkedForm::PrimComb { name: name.clone(), nonval_ok: *nonval_ok, takes_de: *takes_de, wrap_level: wrap_level-1, f: *f })),
            MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => 
                 Some(Rc::new(MarkedForm::DeriComb { hash: hash.clone(), lookup_name: lookup_name.clone(), ids: ids.clone(), se: Rc::clone(se), de: de.clone(), id: id.clone(), wrap_level: *wrap_level-1, sequence_params: sequence_params.clone(), rest_params: rest_params.clone(), body: Rc::clone(body) })),
            //Some(MarkedForm::new_deri_comb(Rc::clone(se), lookup_name.clone(), de.clone(), id.clone(), wrap_level-1, sequence_params.clone(), rest_params.clone(), Rc::clone(body))),

            _ => None,
        }
    }
    pub fn ids(&self) -> NeededIds {
        match self {
            MarkedForm::Nil                              => NeededIds::new_none(),
            MarkedForm::Int(i)                           => NeededIds::new_none(),
            MarkedForm::Bool(b)                          => NeededIds::new_none(),
            MarkedForm::Symbol(s)                        => NeededIds::new_none(), 
            MarkedForm::Pair(hash,ids,car,cdr)           => ids.clone(),
            MarkedForm::SuspendedSymbol(name)            => NeededIds::new_true(),
            MarkedForm::SuspendedEnvLookup { id, .. }    => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedParamLookup { id, .. }  => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedEnvEval { ids, ..}      => ids.clone(),
            MarkedForm::SuspendedIf      { ids, ..}      => ids.clone(),
            MarkedForm::SuspendedPair{ ids, .. }         => ids.clone(),
            MarkedForm::PrimComb { .. }                  => NeededIds::new_none(),
            MarkedForm::DeriComb { ids, .. }             => ids.clone(),
        }
    }
    // TODO: this might be essentially the same as NeededIds.nothing_needed()
    pub fn is_value(&self) -> bool {
        match match self {
            MarkedForm::Nil                             => return true,
            MarkedForm::Int(i)                          => return true,
            MarkedForm::Bool(b)                         => return true,
            MarkedForm::Symbol(s)                       => return true, 
            MarkedForm::SuspendedSymbol(name)           => return false,
            MarkedForm::SuspendedEnvLookup { id, .. }   => return false,
            MarkedForm::SuspendedParamLookup { id, .. } => return false,
            MarkedForm::SuspendedEnvEval { ids, ..}     => return false,
            MarkedForm::SuspendedIf      { ids, ..}     => return false,
            MarkedForm::SuspendedPair{ ids, .. }        => return false,
            MarkedForm::PrimComb { .. }                 => return true,
            // TODO ths might be wrong as it could have captured some suspended computation
            // On the other hand, that would surely show up in ids, right?
            MarkedForm::Pair(hash,ids,car,cdr)          => ids.clone(),
            MarkedForm::DeriComb { ids, .. }            => ids.clone(),
        } {
            NeededIds::True(hashes)     => false,
            NeededIds::None(hashes)     => true,
            NeededIds::Some(ids,hashes) => false,
        }
    }
    pub fn unval(self: &Rc<MarkedForm>) -> Result<Rc<MarkedForm>, &'static str> {
        match &**self {
            MarkedForm::Nil                          => Ok(Rc::clone(self)),
            MarkedForm::Int(i)                       => Ok(Rc::clone(self)),
            MarkedForm::Bool(b)                      => Ok(Rc::clone(self)),
            MarkedForm::Symbol(s)                    => Ok(Rc::new(MarkedForm::SuspendedSymbol(s.clone()))), 
            MarkedForm::Pair(hash,ids,car,cdr)       => Ok(MarkedForm::new_suspended_pair( Attempted::False, car.unval()?, Rc::clone(cdr), None)),
            MarkedForm::SuspendedSymbol(name)        => Err("trying to unval a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }    => Err("trying to unval a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }  => Err("trying to unval a suspended param lookup"),
            MarkedForm::SuspendedEnvEval { .. }      => Err("trying to unval a suspended env eval"),
            MarkedForm::SuspendedIf      { .. }      => Err("trying to unval a suspended if"),
            MarkedForm::SuspendedPair{ ids, .. }     => Err("trying to unval a suspended pair"),
            MarkedForm::PrimComb { .. }              => Ok(Rc::clone(self)),
            MarkedForm::DeriComb { .. }              => Ok(Rc::clone(self)),
        }
    }
    pub fn truthy(&self) -> Result<bool,&'static str> {
        match self {
            MarkedForm::Nil                          => Ok(false),
            MarkedForm::Int(i)                       => Ok(true),
            MarkedForm::Bool(b)                      => Ok(*b),
            MarkedForm::Symbol(s)                    => Ok(true), 
            MarkedForm::Pair(hash,ids,car,cdr)       => Ok(true),
            MarkedForm::SuspendedSymbol(name)        => Err("trying to truthy a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }    => Err("trying to truthy a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }  => Err("trying to truthy a suspended param lookup"),
            MarkedForm::SuspendedEnvEval { .. }      => Err("trying to truthy a suspended env eval"),
            MarkedForm::SuspendedIf      { .. }      => Err("trying to truthy a suspended if"),
            MarkedForm::SuspendedPair{ ids, .. }     => Err("trying to truthy a suspended pair"),
            MarkedForm::PrimComb { .. }              => Ok(true),
            MarkedForm::DeriComb { .. }              => Ok(true),
        }
    }
    pub fn sym(&self) -> Result<&str,&'static str> {
        match self {
            MarkedForm::Symbol(s)                    => Ok(s), 
            _                                        => Err("not a symbol"),
        }
    }
    pub fn int(&self) -> Result<i32,&'static str> {
        match self {
            MarkedForm::Int(i)                       => Ok(*i), 
            _                                        => Err("not a int"),
        }
    }
    pub fn car(&self) -> Result<Rc<MarkedForm>, String> {
        match self {
            MarkedForm::Pair(hash,ids,car,cdr)                                  => Ok(Rc::clone(car)),
            MarkedForm::SuspendedParamLookup { name, id, cdr_num, car } if !car => Ok(Rc::new(MarkedForm::SuspendedParamLookup { name: name.clone(), id: id.clone(), cdr_num: *cdr_num, car: true })),
            _                                                                   => Err(format!("not a pair for car: {}", self)),
        }
    }
    pub fn cdr(&self) -> Result<Rc<MarkedForm>, String> {
        match self {
            MarkedForm::Pair(hash,ids,car,cdr)                          => Ok(Rc::clone(cdr)),
            MarkedForm::SuspendedParamLookup { name, id, cdr_num, car } => Ok(Rc::new(MarkedForm::SuspendedParamLookup { name: name.clone(), id: id.clone(), cdr_num: *cdr_num+1, car: *car })),
            _                                                           => Err(format!("not a pair for cdr: {}", self)),
        }
    }
}

pub fn mark(form: Rc<Form>, bctx: BCtx) -> (BCtx, Rc<MarkedForm>) {
    match &*form {
        Form::Nil                   => (bctx, Rc::new(MarkedForm::Nil)),
        Form::Int(i)                => (bctx, Rc::new(MarkedForm::Int(*i))),
        Form::Bool(b)               => (bctx, Rc::new(MarkedForm::Bool(*b))),
        Form::Symbol(s)             => (bctx, Rc::new(MarkedForm::Symbol(s.clone()))),
        Form::Pair(car, cdr)        => {
            let (bctx, car) = mark(Rc::clone(car),bctx);
            let (bctx, cdr) = mark(Rc::clone(cdr),bctx);
            (bctx, MarkedForm::new_pair(car, cdr))
        },
        Form::DeriComb { se, de, params, body } => {
            panic!();
        },
        Form::PrimComb(name, _f)    => {
            (bctx, match &name[..] {
                "eval" => Rc::new(MarkedForm::PrimComb { name: "eval".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    println!("Ok, this is inside eval looking at {}", p);
                    if !p.car()?.is_value() {
                        Err("can't eval without form being a value, since we're changing the env".to_owned())
                    } else {
                        println!("Ok, returning new suspended env eval with");
                        println!("\t{} {}", p.car()?.unval()?, p.cdr()?.car()?);
                        Ok((bctx, MarkedForm::new_suspended_env_eval(p.car()?.unval()?, p.cdr()?.car()?)))
                    }
                }}),
                "vau" => Rc::new(MarkedForm::PrimComb { name: "vau".to_owned(), nonval_ok: false, takes_de: true, wrap_level: 0, f: |bctx, dctx, p| {
                    let de     = p.car()?.sym().map(|s| s.to_owned()).ok();
                    let params = p.cdr()?.car()?.sym()?.to_owned();
                    let body   = p.cdr()?.cdr()?.car()?.unval()?;
                    let se = Rc::clone(&dctx.e);
                    let (bctx, id) = bctx.new_id();
                    let wrap_level = 0;
                    let sequence_params = vec![];
                    let rest_params = Some(params);
                    Ok((bctx, MarkedForm::new_deri_comb( se, None, de, id, wrap_level, sequence_params, rest_params, body, None )))
                }}),
                "if" => Rc::new(MarkedForm::PrimComb { name: "if".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 0, f: |bctx, dctx, p| {
                    Ok((bctx, MarkedForm::new_suspended_if(p.car()?.unval()?, p.cdr()?.car()?.unval()?, p.cdr()?.cdr()?.car()?.unval()?, None)))
                }}),
                // TODO: handle these in the context of paritals
                "cons" => Rc::new(MarkedForm::PrimComb { name: "cons".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let h =  p.car()?;
                    //println!("Consing with head {}", h);
                    let t =  p.cdr()?.car()?;
                    //println!("Consing with tail {}", t);
                    Ok((bctx, MarkedForm::new_pair(h, t)))
                }}),
                "car" => Rc::new(MarkedForm::PrimComb { name: "car".to_owned(), nonval_ok: true, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, p.car()?.car()?))
                }}),
                "cdr" => Rc::new(MarkedForm::PrimComb { name: "cdr".to_owned(), nonval_ok: true, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, p.car()?.cdr()?))
                }}),
                "quote" => Rc::new(MarkedForm::PrimComb { name: "quote".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 0, f: |bctx, dctx, p| {
                    Ok((bctx, p.car()?))
                }}),
                "debug" => Rc::new(MarkedForm::PrimComb { name: "debug".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    // This one is a bit weird - we put the wrap level at 1 so both sides are pe'd,
                    // and always returns Err so that it's not optimized away
                    Err("debug can't be partial-evaluated away".to_owned())
                }}),
                // ditto
                "assert" => Rc::new(MarkedForm::PrimComb { name: "assert".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let cond = p.car()?;
                    if !cond.truthy()? {
                        println!("Assert failed: {:?}", cond);
                    }
                    assert!(cond.truthy()?);
                    Ok((bctx, p.cdr()?.car()?))
                }}),
                "=" => Rc::new(MarkedForm::PrimComb { name: "=".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
                    //println!("DOing (= {} {}) = {}", a, b, a==b);
                    // TODO: double check that this ignores ids etc. It should, since
                    // wrap_level=1 should mean that everything's a value
                    // also, it should just check by hash then?
                    Ok((bctx, Rc::new(MarkedForm::Bool(a == b))))
                }}),
                "<" => Rc::new(MarkedForm::PrimComb { name: "<".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
                    Ok((bctx, Rc::new(MarkedForm::Bool(a.int()? < b.int()?))))
                }}),
                ">" => Rc::new(MarkedForm::PrimComb { name: ">".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
                    Ok((bctx, Rc::new(MarkedForm::Bool(a.int()? > b.int()?))))
                }}),
                "<=" => Rc::new(MarkedForm::PrimComb { name: "<=".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
                    Ok((bctx, Rc::new(MarkedForm::Bool(a.int()? <= b.int()?))))
                }}),
                ">=" => Rc::new(MarkedForm::PrimComb { name: ">=".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
                    Ok((bctx, Rc::new(MarkedForm::Bool(a.int()? >= b.int()?))))
                }}),
                "+" => Rc::new(MarkedForm::PrimComb { name: "+".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a + b))))
                }}),
                "-" => Rc::new(MarkedForm::PrimComb { name: "-".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a - b))))
                }}),
                "*" => Rc::new(MarkedForm::PrimComb { name: "*".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a * b))))
                }}),
                "/" => Rc::new(MarkedForm::PrimComb { name: "/".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a / b))))
                }}),
                "%" => Rc::new(MarkedForm::PrimComb { name: "%".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a % b))))
                }}),
                "&" => Rc::new(MarkedForm::PrimComb { name: "&".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a & b))))
                }}),
                "|" => Rc::new(MarkedForm::PrimComb { name: "|".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a | b))))
                }}),
                "^" => Rc::new(MarkedForm::PrimComb { name: "^".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?.int()?;
                    let b =  p.cdr()?.car()?.int()?;
                    Ok((bctx, Rc::new(MarkedForm::Int(a ^ b))))
                }}),
                "comb?" => Rc::new(MarkedForm::PrimComb { name: "comb?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::PrimComb { .. }  => true,
                        MarkedForm::DeriComb { .. }  => true,
                        _                            => false,
                    }))))
                }}),
                "pair?" => Rc::new(MarkedForm::PrimComb { name: "pair?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Pair(_h, _i, _a,_b) => true,
                        _                               => false,
                    }))))
                }}),
                "symbol?" => Rc::new(MarkedForm::PrimComb { name: "symbol?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Symbol(_) => true,
                        _                     => false,
                    }))))
                }}),
                "int?" => Rc::new(MarkedForm::PrimComb { name: "int?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Int(_) => true,
                        _                  => false,
                    }))))
                }}),
                // maybe bool? but also could be derived. Nil def
                "bool?" => Rc::new(MarkedForm::PrimComb { name: "bool?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Bool(_) => true,
                        _                   => false,
                    }))))
                }}),
                "nil?" => Rc::new(MarkedForm::PrimComb { name: "nil?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Nil => true,
                        _               => false,
                    }))))
                }}),
                _ => panic!("gah! don't have partial eval version of {}", name),
            })
        },
    }
}

pub fn combiner_return_ok(x: &Rc<MarkedForm>, check_id: Option<EnvID>) -> bool {
    match match &**x {
        MarkedForm::Nil                                        => return true,
        MarkedForm::Int(_)                                     => return true,
        MarkedForm::Bool(_)                                    => return true,
        MarkedForm::Symbol(_)                                  => return true,
        // Hmm, we allow Pair to included suspended now...
        // so now we have to be extra careful
        MarkedForm::Pair(h,ids,car,cdr)                        => ids,

        MarkedForm::SuspendedSymbol(_)                         => return false,
        MarkedForm::SuspendedParamLookup { id, .. }            => return check_id.map(|check_id| *id != check_id).unwrap_or(true),
        MarkedForm::SuspendedEnvLookup   { id, .. }            => return check_id.map(|check_id| *id != check_id).unwrap_or(true),

        MarkedForm::SuspendedEnvEval { e, .. }                 => return combiner_return_ok(e, check_id),
        MarkedForm::SuspendedIf      { c, t, e, .. }           => return combiner_return_ok(c, check_id.clone()) &&
                                                                         combiner_return_ok(t, check_id.clone()) &&
                                                                         combiner_return_ok(e, check_id),
        MarkedForm::SuspendedPair { car, cdr, .. }             => {
           // expand with (func ...params) | func doesn't take de and func+params are return ok
           return false
        },

        MarkedForm::PrimComb { .. }                            => return true,
        MarkedForm::DeriComb { ids, .. }                       => ids,
    } {
        NeededIds::True(    _under) => false,
        NeededIds::None(    _under) => true,
        NeededIds::Some(ids,_under) => check_id.map(|check_id| !ids.contains(&check_id)).unwrap_or(true),
    }
    //; Handles let 4.3 through macro level leaving it as (<comb wraplevel=1 (y) (+ y x 12)> 13)
    //; need handling of symbols (which is illegal for eval but ok for calls) to push it farther
    //(combiner_return_ok (rec-lambda combiner_return_ok (func_result env_id)
    //    (cond   ((not (later_head? func_result)) (not (check_for_env_id_in_result env_id func_result)))
    //    ; special cases now
    //    ;   *(veval body {env}) => (combiner_return_ok {env})
    //    ;       The reason we don't have to check body is that this form is only creatable in ways that body was origionally a value and only need {env}
    //    ;           Either it's created by eval, in which case it's fine, or it's created by something like (eval (array veval x de) de2) and the array has checked it,
    //    ;           or it's created via literal vau invocation, in which case the body is a value.
    //            ((and (marked_array? func_result)
    //                  (prim_comb? (idx (.marked_array_values func_result) 0))
    //                  (= 'veval (.prim_comb_sym (idx (.marked_array_values func_result) 0)))
    //                  (= 3 (len (.marked_array_values func_result)))
    //                  (combiner_return_ok (idx (.marked_array_values func_result) 2) env_id))                                       true)
    //    ;   (func ...params) => (and (doesn't take de func) (foldl combiner_return_ok (cons func params)))
    //    ;
    //            ((and (marked_array? func_result)
    //                  (not (comb_takes_de? (idx (.marked_array_values func_result) 0) (len (.marked_array_values func_result))))
    //                  (foldl (lambda (a x) (and a (combiner_return_ok x env_id))) true (.marked_array_values func_result)))         true)

    //    ;   So that's enough for macro like, but we would like to take it farther
    //    ;       For like (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a)))))
    //    ;           we get to (+ 13 x 12) not being a value, and it reconstructs
    //    ;           (<comb wraplevel=1 (y) (+ y x 12)> 13)
    //    ;           and that's what eval gets, and eval then gives up as well.

    //    ;           That will get caught by the above cases to remain the expansion (<comb wraplevel=1 (y) (+ y x 12)> 13),
    //    ;           but ideally we really want another case to allow (+ 13 x 12) to bubble up
    //    ;           I think it would be covered by the (func ...params) case if a case is added to allow symbols to be bubbled up if their
    //    ;           needed for progress wasn't true or the current environment, BUT this doesn't work for eval, just for functions,
    //    ;           since eval changes the entire env chain (but that goes back to case 1, and might be eliminated at compile if it's an env reachable from the func).
    //    ;
    //    ;
    //    ;   Do note a key thing to be avoided is allowing any non-val inside a comb, since that can cause a fake env's ID to
    //    ;   reference the wrong env/comb in the chain.
    //    ;   We do allow calling eval with a fake env, but since it's only callable withbody value and is strict (by calling this)
    //    ;   about it's return conditions, and the env it's called with must be ok in the chain, and eval doesn't introduce a new scope, it works ok.
    //    ;   We do have to be careful about allowing returned later symbols from it though, since it could be an entirely different env chain.

    //            (true false)
    //    )
    //))
}

pub fn partial_eval(bctx_in: BCtx, dctx_in: DCtx, form: Rc<MarkedForm>) -> (BCtx,Rc<MarkedForm>) {
    let mut bctx = bctx_in;
    let mut dctx = dctx_in;
    let mut next_form = Some(form);
    let mut force = false;
    loop {
        let x = next_form.take().unwrap();
        //println!("{:ident$}PE: {}", "", x, ident=dctx.ident*4);
        if !(force || dctx.can_progress(x.ids())) {
            //println!("{:ident$}Shouldn't go!", "", ident=dctx.ident*4);
            return (bctx, x);
        }
        //println!("{:ident$}({}) PE(force:{}) {:?} (because of {:?})", "", dctx.ident, force, x, x.ids(), ident=dctx.ident*4);
        println!("{:ident$}({}) PE(force:{}) {} (because of {:?})", "", dctx.ident, force, x, x.ids(), ident=dctx.ident*4);
        match partial_eval_step(&x, force, bctx.clone(), &mut dctx) {
            Ok((new_bctx,new_force,new_form)) => {
                bctx = new_bctx; force = new_force; next_form = Some(new_form);
                println!("{:ident$}({}) was ok, result was {}", "", dctx.ident, next_form.as_ref().unwrap(), ident=dctx.ident*4);
            }
            Err(msg)                          => {
                println!("{:ident$}({}) was error, reconstructing (error was {})", "", dctx.ident, msg, ident=dctx.ident*4);
                return (bctx, x);
            }
        }
        // basic Drop redundent veval
        // Old one was recursive over parameters to combs, which we might need, since the redundent veval isn't captured by
        // ids. TODO!
        if let Some(form) = next_form.as_ref() {
            if let MarkedForm::SuspendedEnvEval { x, e, .. } = &**form {
                if (combiner_return_ok(&x, None) || *e == dctx.e) {
                    next_form = Some(Rc::clone(x));
                    force = true;
                }
            }
        }
    }
}
fn partial_eval_step(x: &Rc<MarkedForm>, forced: bool, bctx: BCtx, dctx: &mut DCtx) -> Result<(BCtx,bool,Rc<MarkedForm>), String>  {
    //println!("{:ident$}({}) {}", "", dctx.ident, x, ident=dctx.ident*4);
    match &**x {
        MarkedForm::Pair(h,ids,car,cdr) => {
            //println!("{:ident$}pair ({}) {}", "", dctx.ident, x, ident=dctx.ident*4);
            let (bctx, car) = partial_eval(bctx, dctx.clone(), Rc::clone(car));
            let (bctx, cdr) = partial_eval(bctx, dctx.clone(), Rc::clone(cdr));
            Ok((bctx, false, MarkedForm::new_pair(car, cdr)))
        },
        MarkedForm::SuspendedSymbol(name) => {
            println!("Lookin up symbol {}", name);
            let mut t = Rc::clone(&dctx.e);
            while name != t.car()?.car()?.sym()? {
                t = t.cdr()?;
            }
            println!("found it, pair is {}", t.car()?);
            Ok((bctx, false, t.car()?.cdr()?.tag_name(name)))
        },
        MarkedForm::SuspendedEnvLookup { name, id } => {
            if let Some(v) = dctx.sus_env_stack.get(id) {
                Ok((bctx, false, if let Some(name) = name { v.tag_name(name) } else { Rc::clone(v) }))
            } else {
                Err("failed env lookup (forced)".to_owned())
            }
        },
        MarkedForm::SuspendedParamLookup { name, id, cdr_num, car } => {
            if let Some(v) = dctx.sus_prm_stack.get(id) {
                let mut translated_value = if let Some(name) = name { v.tag_name(name) } else { Rc::clone(v) };
                for i in 0..*cdr_num {
                    translated_value = translated_value.cdr()?;
                }
                if *car {
                    translated_value = translated_value.car()?;
                }
                Ok((bctx, false, translated_value))
            } else {
                Err("failed param lookup (forced)".to_owned())
            }
        },
        MarkedForm::SuspendedEnvEval { x, e, .. } => {
            // this bit is a little tricky - we'd like to tail call, but we can't lose our env
            // if it fails.
            let (bctx, e) = partial_eval(bctx, dctx.clone(),          Rc::clone(e));
            let (bctx, x) = partial_eval(bctx, dctx.copy_set_env(&e), Rc::clone(x));
            if x.is_value() {
                Ok((bctx, false, x))
            } else {
                Ok((bctx, false, MarkedForm::new_suspended_env_eval(x, e)))
            }
            // Note also that we drop redundent vevals at the bottom of the loop tail-call loop
            // with force
        },
        MarkedForm::SuspendedIf { c, t, e, .. } => {
            let (bctx, c) = partial_eval(bctx, dctx.clone(), Rc::clone(c));
            if let Ok(b) = c.truthy() {
                if b {
                    Ok((bctx, false, Rc::clone(t)))
                } else {
                    Ok((bctx, false, Rc::clone(e)))
                }
            } else {
                // TODO: Need to add hash checking to this one
                //let new_if_hash = MarkedForm::new_suspended_if(Rc::clone(&c), Rc::clone(t), Rc::clone(e), None).hash();
                //println!("IF HASH {:?} ? {:?}", new_if_hash, dctx.current);
                //match dctx.copy_push_hash(new_if_hash.clone()) {
                    //Ok(dctx) => {
                        //println!("SIF hash fine, doing both subs");
                        let dctx = dctx.copy_push_fake_if();
                        let (bctx, t) = partial_eval(bctx, dctx.clone(), Rc::clone(t));
                        let (bctx, e) = partial_eval(bctx, dctx.clone(), Rc::clone(e));
                        Ok((bctx, false, MarkedForm::new_suspended_if(c,t,e, None)))
                    //},
                    //Err(rec_stop_msg) => {
                        //println!("SIF hash stopped {}", rec_stop_msg);
                        //Ok((bctx, false, MarkedForm::new_suspended_if(c, Rc::clone(t), Rc::clone(e), Some(new_if_hash))))
                    //}
                //}
            }
        },
        MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
            // TODO: figure out wrap level, sequence params, etc
            if forced || !se.ids().needs_nothing() {
            //if ids.is_true() || !se.ids().needs_nothing() {
                let old_se_ids = se.ids();
                let se = if !se.ids().needs_nothing() {
                    // the current env is our new se
                    Rc::clone(&dctx.e)
                } else {
                    Rc::clone(se)
                };

                let ident_amount = dctx.ident*4;

                match dctx.copy_push_frame(id.clone(), &se, &de, None, &rest_params, None, body) {
                    Ok(inner_dctx) => {
                        println!("{:ident$}Doing a body deri for {:?} because ({} || {:?}) which is {}", "", lookup_name, forced, old_se_ids, x, ident=ident_amount);
                        println!("{:ident$}and also body ids is {:?}", "", body.ids(), ident=ident_amount);
                        //println!("{:ident$}and fake is {:?} and fake_if is {:?}", "", , ident=ident_amount);
                        let (bctx, body) = partial_eval(bctx, inner_dctx, Rc::clone(&body));
                        println!("{:ident$}result was {}", "", body, ident=ident_amount);
                        Ok((bctx, false, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), body, None)))
                    },
                    Err(rec_stop_under) => {
                        println!("{:ident$}call of {:?} failed b/c rec_stop_under", "", lookup_name, ident=dctx.ident*4);
                        //maybe_rec_hash = Some(rec_stop_hash);
                        // TODO: should this mark the hash on DeriComb?
                        //Err("recursion stopped in dericomb".to_owned())
                        Ok((bctx, false, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), Rc::clone(body), Some(rec_stop_under))))
                        //Ok((bctx, false, MarkedForm::new_suspended_pair( new_attempted, car, cdr, maybe_rec_hash )))
                    },
                }
            } else {
                //panic!("impossible {}", x);
                Err("impossible!?".to_owned())
            }
        },
        MarkedForm::SuspendedPair { hash, ids, attempted, car, cdr } => {
            let (    bctx, mut car) = partial_eval(bctx, dctx.clone(), Rc::clone(car));
            let (mut bctx, mut cdr) = partial_eval(bctx, dctx.clone(), Rc::clone(cdr));
            let mut new_attempted = attempted.clone();
            let mut maybe_rec_under = None;
            let mut return_ok = false;
            while let Some(wrap_level) = car.wrap_level() {
                if wrap_level > 0 {
                    fn map_unval_peval(bctx: BCtx, dctx: DCtx, x: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>),String> {
                        match &*x {
                            MarkedForm::Pair(h, ids, x_car, x_cdr) => {
                                let (bctx, new_x_car) = partial_eval(bctx, dctx.clone(), x_car.unval()?);
                                let (bctx, new_x_cdr) = map_unval_peval(bctx, dctx.clone(), Rc::clone(x_cdr))?;
                                return Ok((bctx, MarkedForm::new_pair(new_x_car, new_x_cdr)));
                            },
                            MarkedForm::Nil => return Ok((bctx,x)),
                            _               => return Err("not a list".to_owned()),
                        }
                    }
                    match map_unval_peval(bctx.clone(), dctx.clone(), Rc::clone(&cdr)) {
                        Ok((new_bctx, new_cdr)) => {
                            car = car.decrement_wrap_level().unwrap();
                            cdr = new_cdr;
                            bctx = new_bctx;
                        }
                        Err(msg) => {
                            println!("{:ident$} evaling parameters failed b/c {}", "", msg, ident=dctx.ident*4);
                            break;
                        }
                    }
                } else {
                    // check to see if can do call
                    // We might want to enable not pure values for cons/car/cdr?
                    match &*car {
                        MarkedForm::PrimComb { name, nonval_ok, takes_de, wrap_level, f} => {
                            if !nonval_ok && !cdr.is_value() {
                                break;
                            }
                            new_attempted = Attempted::True(if *takes_de { Some(dctx.e.ids()) } else { None });
                            let ident_amount = dctx.ident*4;
                            println!("{:ident$}doing a call eval of {}", "", name, ident=ident_amount);
                            println!("{:ident$}parameters {} are? a val because {:?}", "", cdr, cdr.ids(), ident=ident_amount);
                            match f(bctx.clone(), dctx.clone(), Rc::clone(&cdr)) {
                                Ok((bctxp, r)) => {
                                    // force true b/c might be a tail call
                                    return Ok((bctx, true, r));
                                    //return Ok((bctx, name == "eval" || name == "if", r));
                                },
                                Err(msg) => {
                                    println!("{:ident$} call to {} failed {:?}", "", name, msg, ident=ident_amount);
                                },
                            }
                        }
                        MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
                            if !cdr.is_value() {
                                break;
                            }
                            new_attempted = Attempted::True(if de.is_some() { Some(dctx.e.ids()) } else { None });
                            if de.is_some() && dctx.e.ids().may_contain_id(id.clone()) {
                                // The current environment may contain a reference to our ID, which
                                // means if we take that environment, if we then PE that
                                // environment we will replace it with our real environment that
                                // still has a dynamic reference to the current environment, which
                                // will be an infinate loop
                                break;
                            }
                            // not yet supporting sequence params
                            match dctx.copy_push_frame(id.clone(), &se, &de, Some(Rc::clone(&dctx.e)), &rest_params, Some(Rc::clone(&cdr)), body) {
                                Ok(inner_dctx) => {
                                    let ident_amount = inner_dctx.ident*4;
                                    //println!("{:ident$}doing a call eval of {} in {}", "", body, inner_dctx.e, ident=inner_dctx.ident*4);
                                    println!("{:ident$}doing a call eval of {:?}", "", lookup_name, ident=ident_amount);
                                    //println!("{:ident$}with_parameters {}", "", cdr, ident=ident_amount);

                                    //Here is where we could do a tail call instead, but there
                                    //would be no recovery back into the call-form...
                                    let (bctx, r) =  partial_eval(bctx.clone(), inner_dctx, Rc::clone(body));
                                    if combiner_return_ok(&r, Some(id.clone())) {
                                        return Ok((bctx, false, r));
                                    }
                                },
                                Err(rec_stop_under) => {
                                    println!("{:ident$}call of {:?} failed b/c rec_stop_under", "", lookup_name, ident=dctx.ident*4);
                                    maybe_rec_under = Some(rec_stop_under);
                                },
                            }
                        },
                        _ => {},
                    }
                    break;
                }
            }
            // Call failed, do the re-wrap-up ourselves b/c of our possibly advanced wrap/params
            Ok((bctx, false, MarkedForm::new_suspended_pair( new_attempted, car, cdr, maybe_rec_under )))
        },
        // Values should never get here b/c ids UNLESS FORCE HAH
        _                                        => Err("value evaled".to_owned()),
    }
}
