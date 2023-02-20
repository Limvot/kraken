use std::fmt;
use std::rc::Rc;
use std::convert::From;
use std::collections::{BTreeSet,BTreeMap};
use std::result::Result;
use std::iter;

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

    pub fn marked(&self, bctx: BCtx) -> (BCtx, Rc<MarkedForm>) {
        match &*self {
            Form::Nil                   => (bctx, Rc::new(MarkedForm::Nil)),
            Form::Int(i)                => (bctx, Rc::new(MarkedForm::Int(*i))),
            Form::Bool(b)               => (bctx, Rc::new(MarkedForm::Bool(*b))),
            Form::Symbol(s)             => (bctx, Rc::new(MarkedForm::Symbol(s.clone()))),
            Form::Pair(car, cdr)        => {
                let (bctx, car) = car.marked(bctx);
                let (bctx, cdr) = cdr.marked(bctx);
                (bctx, Rc::new(MarkedForm::Pair(NeededIds::new_none(), car, cdr)))
            },
            Form::DeriComb { se, de, params, body } => {
                let (bctx, se)     = se.marked(bctx);
                let (bctx, body)   = body.marked(bctx);
                let (bctx, new_id) = bctx.new_id();
                (bctx, Rc::new(MarkedForm::DeriComb { ids: NeededIds::new_none(), se, de: de.clone(),
                                                     id: new_id, wrap_level: 0, sequence_params: vec![],
                                                     rest_params: Some(params.clone()), body }))
            },
            Form::PrimComb(name, _f)    => {
                (bctx, match &name[..] {
                    // should be able to take in wrap_level != 1 and do stuff
                    "eval" => Rc::new(MarkedForm::PrimComb { name: "eval".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        // put in partial eval logic, maybe?
                        let b = p.car()?.unval()?;
                        let e = p.cdr()?.car()?;
                        println!("Doing Eval (via tail call) of {} in {}", b, e);
                        Ok((bctx, PossibleMarkedTailCall::TailCall(e, b)))
                    }}),
                    // (vau de params body), should be able to take in wrap_level != 1 and do stuff
                    "vau" => Rc::new(MarkedForm::PrimComb { name: "vau".to_owned(), wrap_level: 0, f: |bctx, dctx, p| {
                        let de     = p.car()?.sym().map(|s| s.to_owned()).ok();
                        let params = p.cdr()?.car()?.sym()?.to_owned();
                        let body   = p.cdr()?.cdr()?.car()?.unval()?;
                        let se = Rc::clone(&dctx.e);
                        let (bctx, id) = bctx.new_id();
                        // TODO: figure out wrap level, sequence params, etc
                        let wrap_level = 0;
                        let sequence_params = vec![];
                        let rest_params = Some(params);
                        //
                        let inner_dctx = dctx.copy_push_frame(id.clone(), &se, &de, None, &rest_params, None);
                        let (bctx, body) = partial_eval(bctx, inner_dctx, Rc::clone(&body))?;
                        //
                        //
                        let ids = dctx.e.ids().union(&body.ids());
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(
                            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body },
                        ))))
                    }}),
                    // TODO: handle vif etc
                    "if" => Rc::new(MarkedForm::PrimComb { name: "if".to_owned(), wrap_level: 0, f: |bctx, dctx, p| {
                        let (bctx, cond) = partial_eval(bctx, dctx.clone(), p.car()?.unval()?)?;
                        let e = Rc::clone(&dctx.e);
                        if cond.truthy()? {
                            Ok((bctx, PossibleMarkedTailCall::TailCall(e, p.cdr()?.car()?.unval()?)))
                        } else {
                            Ok((bctx, PossibleMarkedTailCall::TailCall(e, p.cdr()?.cdr()?.car()?.unval()?)))
                        }
                    }}),
                    // TODO: handle these in the context of paritals
                    "cons" => Rc::new(MarkedForm::PrimComb { name: "cons".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let h =  p.car()?;
                        let t =  p.cdr()?.car()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Pair(h.ids().union(&t.ids()), h, t)))))
                    }}),
                    "car" => Rc::new(MarkedForm::PrimComb { name: "car".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(p.car()?.car()?)))
                    }}),
                    "cdr" => Rc::new(MarkedForm::PrimComb { name: "cdr".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(p.car()?.cdr()?)))
                    }}),
                    "quote" => Rc::new(MarkedForm::PrimComb { name: "quote".to_owned(), wrap_level: 0, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(p.car()?)))
                    }}),
                    // This one needs to control eval to print debug before continuint
                    // which is werid to PE
                    "debug" => Rc::new(MarkedForm::PrimComb { name: "debug".to_owned(), wrap_level: 0, f: |bctx, dctx, p| {
                        let e = Rc::clone(&dctx.e);
                        Ok((bctx, PossibleMarkedTailCall::TailCall(e, p.cdr()?.car()?)))
                    }}),
                    // ditto
                    "assert" => Rc::new(MarkedForm::PrimComb { name: "assert".to_owned(), wrap_level: 0, f: |bctx, dctx, p| {
                        let (bctx, cond) = partial_eval(bctx, dctx.clone(), p.car()?)?;
                        if !cond.truthy()? {
                            println!("Assert failed: {:?}", cond);
                        }
                        assert!(cond.truthy()?);
                        let e = Rc::clone(&dctx.e);
                        Ok((bctx, PossibleMarkedTailCall::TailCall(e, p.cdr()?.car()?)))
                    }}),
                    // (vau de params body), should be able to take in wrap_level != 1 and do stuff
                    "=" => Rc::new(MarkedForm::PrimComb { name: "=".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?;
                        let b =  p.cdr()?.car()?;
                        println!("DOing (= {} {}) = {}", a, b, a==b);
                        // TODO: double check that this ignores ids etc. It should, since
                        // wrap_level=1 should mean that everything's a value
                        // also, it should just check by hash then?
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(a == b)))))
                    }}),
                    "<" => Rc::new(MarkedForm::PrimComb { name: "<".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?;
                        let b =  p.cdr()?.car()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(a.int()? < b.int()?)))))
                    }}),
                    ">" => Rc::new(MarkedForm::PrimComb { name: ">".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?;
                        let b =  p.cdr()?.car()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(a.int()? > b.int()?)))))
                    }}),
                    "<=" => Rc::new(MarkedForm::PrimComb { name: "<=".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?;
                        let b =  p.cdr()?.car()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(a.int()? <= b.int()?)))))
                    }}),
                    ">=" => Rc::new(MarkedForm::PrimComb { name: ">=".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?;
                        let b =  p.cdr()?.car()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(a.int()? >= b.int()?)))))
                    }}),
                    "+" => Rc::new(MarkedForm::PrimComb { name: "+".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a + b)))))
                    }}),
                    "-" => Rc::new(MarkedForm::PrimComb { name: "-".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a - b)))))
                    }}),
                    "*" => Rc::new(MarkedForm::PrimComb { name: "*".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a * b)))))
                    }}),
                    "/" => Rc::new(MarkedForm::PrimComb { name: "/".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a / b)))))
                    }}),
                    "%" => Rc::new(MarkedForm::PrimComb { name: "%".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a % b)))))
                    }}),
                    "&" => Rc::new(MarkedForm::PrimComb { name: "&".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a & b)))))
                    }}),
                    "|" => Rc::new(MarkedForm::PrimComb { name: "|".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a | b)))))
                    }}),
                    "^" => Rc::new(MarkedForm::PrimComb { name: "^".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        let a =  p.car()?.int()?;
                        let b =  p.cdr()?.car()?.int()?;
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Int(a ^ b)))))
                    }}),
                    "comb?" => Rc::new(MarkedForm::PrimComb { name: "comb?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::PrimComb { .. }  => true,
                            MarkedForm::DeriComb { .. }  => true,
                            _                            => false,
                        })))))
                    }}),
                    "pair?" => Rc::new(MarkedForm::PrimComb { name: "pair?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::Pair(_i, _a,_b) => true,
                            _                           => false,
                        })))))
                    }}),
                    "symbol?" => Rc::new(MarkedForm::PrimComb { name: "symbol?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::Symbol(_) => true,
                            _                     => false,
                        })))))
                    }}),
                    "int?" => Rc::new(MarkedForm::PrimComb { name: "int?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::Int(_) => true,
                            _                  => false,
                        })))))
                    }}),
                    // maybe bool? but also could be derived. Nil def
                    "bool?" => Rc::new(MarkedForm::PrimComb { name: "bool?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::Bool(_) => true,
                            _                   => false,
                        })))))
                    }}),
                    "nil?" => Rc::new(MarkedForm::PrimComb { name: "nil?".to_owned(), wrap_level: 1, f: |bctx, dctx, p| {
                        Ok((bctx, PossibleMarkedTailCall::Result(Rc::new(MarkedForm::Bool(match &* p.car()? {
                            MarkedForm::Nil => true,
                            _               => false,
                        })))))
                    }}),
                    _ => panic!("gah! don't have partial eval version of {}", name),
                })
            },
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
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Hash(u64);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NeededIds {
    True(BTreeSet<Hash>),
    None(BTreeSet<Hash>),
    Some(BTreeSet<EnvID>,BTreeSet<Hash>),
}
impl NeededIds {
    fn new_true()           -> Self { NeededIds::True(                         BTreeSet::new()) }
    fn new_none()           -> Self { NeededIds::None(                         BTreeSet::new()) }
    fn new_single(i: EnvID) -> Self { NeededIds::Some(iter::once(i).collect(), BTreeSet::new()) }
    fn hashes(&self) -> &BTreeSet<Hash> {
        match self {
            NeededIds::True(hashes)     => hashes,
            NeededIds::None(hashes)     => hashes,
            NeededIds::Some(set,hashes) => hashes,
        }

    }
    fn union(&self, other: &NeededIds) -> Self {
        match self {
            NeededIds::True(hashes)      => NeededIds::True(hashes.union(other.hashes()).cloned().collect()),
            NeededIds::None(hashes)      => other.union_hashes(hashes),
            NeededIds::Some(set, hashes) => match other {
                NeededIds::True(ohashes)      => NeededIds::True(hashes.union(ohashes).cloned().collect()),
                NeededIds::None(ohashes)      => NeededIds::Some(set.clone(), hashes.union(ohashes).cloned().collect()),
                NeededIds::Some(oset,ohashes) => NeededIds::Some(set.union(oset).cloned().collect(), hashes.union(ohashes).cloned().collect()),
            },
        }
    }
    fn union_hashes(&self, other: &BTreeSet<Hash>) -> Self {
        match self {
            NeededIds::True(hashes)      => NeededIds::True(             other.union(hashes).cloned().collect()),
            NeededIds::None(hashes)      => NeededIds::None(             other.union(hashes).cloned().collect()),
            NeededIds::Some(set, hashes) => NeededIds::Some(set.clone(), other.union(hashes).cloned().collect()),
        }
    }
    fn add_hash(&self, h: Hash) -> Self {
        match self {
            NeededIds::True(hashes)      => NeededIds::True(             hashes.iter().cloned().chain(iter::once(h)).collect()),
            NeededIds::None(hashes)      => NeededIds::None(             hashes.iter().cloned().chain(iter::once(h)).collect()),
            NeededIds::Some(set, hashes) => NeededIds::Some(set.clone(), hashes.iter().cloned().chain(iter::once(h)).collect()),
        }
    }
    fn add_id(&self, i: EnvID) -> Self {
        match self {
            NeededIds::True(hashes)      => NeededIds::True(                                                    hashes.clone()),
            NeededIds::None(hashes)      => NeededIds::Some(iter::once(i).collect(),                            hashes.clone()),
            NeededIds::Some(set, hashes) => NeededIds::Some(set.iter().cloned().chain(iter::once(i)).collect(), hashes.clone()),
        }
    }
}
pub enum PossibleMarkedTailCall {
    Result(Rc<MarkedForm>),
    TailCall(Rc<MarkedForm>, Rc<MarkedForm>),
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


// force is for drop_redundent_eval
// memo is only for recording currently executing hashes (calls and if's)
// only_head is not currently used
//only_head env env_counter memo env_stack force
//
//I feel like ctx should only be used for things passed back up?
//or maybe we split into an upwards and downwards ctx
//bidirectional is only env_counter, though a true memoization would also count
//ok
//we have Ctx - env_counter (maybe true memoization eventually)
//and Dctx - env env_stack memo(currently_executing) force
//
//env_stack should be something like BTreeMap<EnvID, (EnvLookup,ParamLookupMain)>
// SuspendedEnvLookup   { name: Option<String>, id: EnvID },
// SuspendedParamLookup { name: Option<String>, id: EnvID, cdr_num: i32, car: bool },
#[derive(Clone)]
pub struct DCtx {
    e : Rc<MarkedForm>,
    sus_env_stack: Rc<BTreeMap<EnvID, Rc<MarkedForm>>>,
    sus_prm_stack: Rc<BTreeMap<EnvID, Rc<MarkedForm>>>,
    real_set: Rc<BTreeSet<EnvID>>,
    force: bool,
    current: Rc<BTreeSet<Hash>>,
}
impl DCtx {
    pub fn copy_set_env(&self, e: &Rc<MarkedForm>) -> Self {
        DCtx { e: Rc::clone(e), sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack), real_set: Rc::clone(&self.real_set), force: self.force, current: Rc::clone(&self.current)  }
    }
    pub fn copy_push_frame(&self, id: EnvID, se: &Rc<MarkedForm>, de: &Option<String>, e: Option<Rc<MarkedForm>>, rest_params: &Option<String>, prms: Option<Rc<MarkedForm>>) -> Self {
        let mut sus_env_stack = Rc::clone(&self.sus_env_stack);
        let mut sus_prm_stack = Rc::clone(&self.sus_prm_stack);
        let mut real_set = Rc::clone(&self.real_set);
        if (e.is_some() || prms.is_some()) {
            Rc::make_mut(&mut real_set).insert(id.clone());
        }
        let inner_env = if let Some(de) = de {
            let de_val = if let Some(e) = e {
                Rc::make_mut(&mut sus_env_stack).insert(id.clone(), Rc::clone(&e));
                e
            } else {
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
                Rc::new(MarkedForm::SuspendedParamLookup { name: Some(p.clone()), id: id.clone(), cdr_num: 0, car: false })
            };
            massoc(p, p_val, inner_env)
        } else { inner_env };
        // Push on current frame hash?!
        let new_current = Rc::clone(&self.current);
        DCtx { e: inner_env, sus_env_stack, sus_prm_stack, real_set, force: self.force, current: new_current }
    }

    pub fn can_progress(&self, ids: NeededIds) -> bool {
        // check if ids is true || ids intersection EnvIDs in our stacks is non empty || ids.hashes - current is non empty
        match ids {
            NeededIds::True(hashes)     => true,
            NeededIds::None(hashes)     =>                                        !self.current.is_superset(&hashes),
            NeededIds::Some(ids,hashes) => (!self.real_set.is_disjoint(&ids)) || (!self.current.is_superset(&hashes)),
        }
    }
}

pub fn new_base_ctxs() -> (BCtx,DCtx) {
    let bctx = BCtx { id_counter: 0 };
    let (bctx, root_env) = root_env().marked(bctx);
    (bctx, DCtx { e: root_env, sus_env_stack: Rc::new(BTreeMap::new()), sus_prm_stack: Rc::new(BTreeMap::new()), real_set: Rc::new(BTreeSet::new()), force: false, current: Rc::new(BTreeSet::new()) } )
}

pub fn partial_eval(bctx: BCtx, dctx: DCtx, x: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>), String> {
    println!("PE: {}", x);
    let should_go = dctx.force || dctx.can_progress(x.ids());
    if !should_go {
        println!("Shouldn't go!");
        return Ok((bctx, x));
    }
    match &*x {
        MarkedForm::SuspendedSymbol(name) => {
            let mut t = Rc::clone(&dctx.e);
            while name != t.car()?.car()?.sym()? {
                t = t.cdr()?;
            }
            return Ok((bctx, t.car()?.cdr()?));
        },
        MarkedForm::SuspendedEnvLookup { name, id }       => {
            if let Some(v) = dctx.sus_env_stack.get(id) {
                Ok((bctx, Rc::clone(v)))
            } else {
                Ok((bctx, x))
            }
        },
        MarkedForm::SuspendedParamLookup { name, id, cdr_num, car }       => {
            if let Some(v) = dctx.sus_prm_stack.get(id) {
                Ok((bctx, Rc::clone(v)))
            } else {
                Ok((bctx, x))
            }
        },
        MarkedForm::SuspendedPair { ids, attempted, car, cdr } => {
            let (    bctx, mut car) = partial_eval(bctx, dctx.clone(), Rc::clone(car))?;
            let (mut bctx, mut cdr) = partial_eval(bctx, dctx.clone(), Rc::clone(cdr))?;
            while let Some(wrap_level) = car.wrap_level() {
                if wrap_level > 0 {
                    fn map_unval_peval(bctx: BCtx, dctx: DCtx, x: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>),String> {
                        match &*x {
                            MarkedForm::Pair(ids, x_car, x_cdr) => {
                                let (bctx, new_x_car) = partial_eval(bctx, dctx.clone(), x_car.unval()?)?;
                                let (bctx, new_x_cdr) = map_unval_peval(bctx, dctx.clone(), Rc::clone(x_cdr))?;
                                return Ok((bctx, Rc::new(MarkedForm::Pair(new_x_car.ids().union(&new_x_cdr.ids()), new_x_car, new_x_cdr))));
                            },
                            MarkedForm::Nil => return Ok((bctx,x)),
                            _               => return Err("not a list".to_owned()),
                        }
                    }
                    //asdf
                    if let Ok((new_bctx, new_cdr)) = map_unval_peval(bctx.clone(), dctx.clone(), Rc::clone(&cdr)) {
                        car = car.decrement_wrap_level().unwrap();
                        cdr = new_cdr;
                        bctx = new_bctx;
                    } else {
                        break;
                    }
                } else {
                    // check to see if can do call
                    if !cdr.is_value() {
                        break;
                    }
                    if let Ok((bctx, r)) = match &*car {
                        MarkedForm::PrimComb { name, wrap_level, f} => f(bctx.clone(), dctx.clone(), Rc::clone(&cdr)),
                        MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
                            // not yet supporting sequence params
                            let inner_dcts = dctx.copy_push_frame(id.clone(), &se, &de, Some(Rc::clone(&dctx.e)), &rest_params, Some(Rc::clone(&cdr)));
                            // check for id in it?
                            partial_eval(bctx.clone(), inner_dcts, Rc::clone(body)).map(|(ibctx, res)| { (ibctx, PossibleMarkedTailCall::Result(res)) })
                        },
                        _ => break,
                    } {
                        match r {
                            PossibleMarkedTailCall::Result(result) => return Ok((bctx, result)),
                            // Sigh, no tail-callin right now
                            PossibleMarkedTailCall::TailCall(new_env, next) => {
                                println!("Doing tail call of {} in {}", next, new_env);
                                if let Ok((new_bctx, res)) = partial_eval(bctx.clone(), dctx.copy_set_env(&new_env), Rc::clone(&next)) {
                                    println!("Doing tail call result is {}", res);
                                    return Ok((new_bctx, res));
                                } else {
                                    println!("Tail call failed");
                                    if new_env == dctx.e {
                                        return Ok((bctx, next));
                                    } else {
                                        // maybe this should enplace the TailCall with an eval
                                        break; // break out to reconstruction
                                    }
                                }
                            }
                        }
                    } else {
                        break; // failed function call
                    }
                }
            }
            // update IDs
            let new_ids = car.ids().union(&cdr.ids());
            let new_ids = if let Attempted::True(Some(id))   = attempted   { new_ids.add_id(id.clone())     } else { new_ids };
            // This would come from trying it again
            //let new_ids = if let Some(hash) = resume_hash { new_ids.add_hash(hash) } else { new_ids };
            let new_attempted = attempted.clone();
            Ok((bctx, Rc::new(MarkedForm::SuspendedPair{ ids: new_ids, attempted: new_attempted, car, cdr })))
        },
        MarkedForm::Pair(ids,car,cdr)                => {
            let (bctx, car) = partial_eval(bctx, dctx.clone(), Rc::clone(car))?;
            let (bctx, cdr) = partial_eval(bctx, dctx,         Rc::clone(cdr))?;
            Ok((bctx, Rc::new(MarkedForm::Pair(car.ids().union(&cdr.ids()),car, cdr))))
        },
        MarkedForm::PrimComb { .. }              => Ok((bctx, x)),
        // Sub stuff
        MarkedForm::DeriComb { .. }              => Ok((bctx, x)),
        _                                        => Ok((bctx, x)),
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Attempted {
    True(Option<EnvID>),
    False,
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MarkedForm {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(NeededIds, Rc<MarkedForm>,Rc<MarkedForm>),

    SuspendedSymbol(String),
    SuspendedParamLookup { name: Option<String>, id: EnvID, cdr_num: i32, car: bool },
    SuspendedEnvLookup   { name: Option<String>, id: EnvID },
    // resume hash is folded into ids
    SuspendedPair { ids: NeededIds, attempted: Attempted, car: Rc<MarkedForm>, cdr: Rc<MarkedForm>},

    PrimComb { name: String, wrap_level: i32, f: fn(BCtx,DCtx,Rc<MarkedForm>) -> Result<(BCtx,PossibleMarkedTailCall),String> },
    DeriComb { ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: EnvID, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm> },
}
impl MarkedForm {
    pub fn wrap_level(&self) -> Option<i32> {
        match self {
            MarkedForm::PrimComb { name, wrap_level, f} => Some(*wrap_level),
            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => Some(*wrap_level),
            _ => None,
        }
    }
    pub fn decrement_wrap_level(&self) -> Option<Rc<Self>> {
        match self {
            MarkedForm::PrimComb { name, wrap_level, f } => Some(Rc::new(MarkedForm::PrimComb { name: name.clone(), wrap_level: wrap_level-1, f: *f })),
            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => Some(Rc::new(MarkedForm::DeriComb { ids: ids.clone(), se: Rc::clone(se), de: de.clone(), id: id.clone(), wrap_level: wrap_level-1, sequence_params: sequence_params.clone(), rest_params: rest_params.clone(), body: Rc::clone(body) })),
            _ => None,
        }
    }
    pub fn ids(&self) -> NeededIds {
        match self {
            MarkedForm::Nil                              => NeededIds::new_none(),
            MarkedForm::Int(i)                           => NeededIds::new_none(),
            MarkedForm::Bool(b)                          => NeededIds::new_none(),
            MarkedForm::Symbol(s)                        => NeededIds::new_none(), 
            MarkedForm::Pair(ids,car,cdr)                => ids.clone(),
            MarkedForm::SuspendedSymbol(name)            => NeededIds::new_true(),
            MarkedForm::SuspendedEnvLookup { id, .. }    => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedParamLookup { id, .. }  => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedPair{ ids, .. }         => ids.clone(),
            MarkedForm::PrimComb { .. }                  => NeededIds::new_none(),
            MarkedForm::DeriComb { ids, .. }             => ids.clone(),
        }
    }
    pub fn is_value(&self) -> bool {
        match match self {
            MarkedForm::Nil                             => return true,
            MarkedForm::Int(i)                          => return true,
            MarkedForm::Bool(b)                         => return true,
            MarkedForm::Symbol(s)                       => return true, 
            MarkedForm::SuspendedSymbol(name)           => return false,
            MarkedForm::SuspendedEnvLookup { id, .. }   => return false,
            MarkedForm::SuspendedParamLookup { id, .. } => return false,
            MarkedForm::SuspendedPair{ ids, .. }        => return false,
            MarkedForm::PrimComb { .. }                 => return true,
            MarkedForm::Pair(ids,car,cdr)               => ids.clone(),
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
            MarkedForm::Pair(ids,car,cdr)            => Ok(Rc::new(MarkedForm::SuspendedPair { ids: NeededIds::new_true(), attempted: Attempted::False, car: car.unval()?, cdr: Rc::clone(cdr)})),
            MarkedForm::SuspendedSymbol(name)        => Err("trying to unval a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }    => Err("trying to unval a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }  => Err("trying to unval a suspended param lookup"),
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
            MarkedForm::Pair(ids,car,cdr)            => Ok(true),
            MarkedForm::SuspendedSymbol(name)        => Err("trying to truthy a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }    => Err("trying to truthy a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }  => Err("trying to truthy a suspended param lookup"),
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
    pub fn car(&self) -> Result<Rc<MarkedForm>, &'static str> {
        match self {
            MarkedForm::Pair(ids,car,cdr)            => Ok(Rc::clone(car)),
            _                                        => Err("not a pair"),
        }
    }
    pub fn cdr(&self) -> Result<Rc<MarkedForm>, &'static str> {
        match self {
            MarkedForm::Pair(ids,car,cdr)            => Ok(Rc::clone(cdr)),
            _                                        => Err("not a pair"),
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
            MarkedForm::Pair(ids, car, cdr)   => {
                //write!(f, "{:?}#({}", ids, car)?;
                write!(f, "({}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair(ref ids, ref carp, ref cdrp) => {
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
            MarkedForm::SuspendedSymbol(name)                           => write!(f, "{}", name),
            MarkedForm::SuspendedEnvLookup { name, id }                 => write!(f, "{:?}({:?}env)", name, id),
            MarkedForm::SuspendedParamLookup { name, id, cdr_num, car } => write!(f, "{:?}({:?}{}{})", name, id, cdr_num, car),
            MarkedForm::PrimComb { name, wrap_level, .. }               => write!(f, "<{}{}>", name, wrap_level),

            MarkedForm::DeriComb { ids, se, de, id, wrap_level, sequence_params, rest_params, body } => write!(f, "{:?}#<{}/{:?}/{:?}/{}/{:?}/{:?}/{}>", ids, se, de, id, wrap_level, sequence_params, rest_params, body),

            MarkedForm::SuspendedPair{ ids, attempted, car, cdr } => {
                //write!(f, "{:?}{:?}#{{{}", ids, attempted, car)?;
                write!(f, "{{{}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair(ref ids, ref carp, ref cdrp) => {
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
    Rc::new(MarkedForm::Pair(
                l.ids().union(&v.ids()),
                Rc::new(MarkedForm::Pair(
                        v.ids(),
                        Rc::new(MarkedForm::Symbol(k.to_owned())),
                        v)),
                l))
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
