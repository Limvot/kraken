use std::fmt;
use std::rc::Rc;
use std::convert::From;
use std::collections::{BTreeSet,BTreeMap,HashMap,hash_map::DefaultHasher};
use std::hash::{Hash,Hasher};
use std::result;
use std::iter;

use crate::ast::{root_env,Form};

use anyhow::{anyhow, bail, Result, Context};


fn massoc(k: &str, v: Rc<MarkedForm>, l: Rc<MarkedForm>) -> Rc<MarkedForm> {
    MarkedForm::new_pair(
                MarkedForm::new_pair(
                        Rc::new(MarkedForm::Symbol(k.to_owned())),
                        v),
                l)
}

/**
 * Now, split into head | tails
 * where things from head are guarenteed to progress, and tails are later possibly needed values
 * for calculation of ok_to_return etc
 *
 * also, under_fake_if_in_body | under_fake_body
 *  the DEnv under_fake_body prevents additional fake calls
 *  the DEnv under_fake_if_in_body prevents normal calls in addition to fake calls
 *
 *  These mark if that stopped it, and thus progress can proceede if it's not a subset of the current ones 
 *  They are stripped by the inciting fake body / if (what if there are more than one?)
 *      HOW TO KNOW WHICH INCITING IF? Oh duh it's a recursive partial-eval call just keep track at the callsite
 *
 * True represented as an id? 0?
 * Runtime represented as an id? -1?
 *
 * Attempted Calls are marked with what was needed by their final body, no need for attempted
 *  (though if the calls were to generate it live, I suppose it could be, but I don't think there's
 *  any need - maybe for when p-eing params even though you know the call can't progress. I think
 *  this should just be an option)
 *
 *  Errors - 2 types
 *      actual error - propegate up with context
 *      can't progress - MoreNeeded error with ID. Caught by the wrapper for re-creation?
 *                              wait can it even be re-created?
 *                              maybe we just legit need all that error handling
 *
 *                              for prim calls it can be recreated, it's just the prim call.
 *                                  that's a common enough case (well, actually, is it just eval and debug anyway? - ALSO the parameter-unval-peval mapping in calling)
 *                                  everything else normally has enough given their wrap level to
 *                                  either be a real error or return progress
 *                                  *maybe* assert needs special handling? Maybe just for error propagation (stopping there for post?)?
 *
 *  combiner_return_ok can be cast entirely as an ID check
 *
 *  the curent basic drop_redundent_eval is also just checking combiner_return_ok(x,None) to make
 *  sure isn't a suspended_symbol which doesn't have it's ID yet, but that doesn't matter?
 *  also it could be done with an ID=True check? (which would be broader, I guess, but about as legit anyway which is none at all)
 *  WAIT the current one is checking return_ok(x, None) OR e == e
 *      that can't be right, surely it'd be return_ok(x, e.id)
 *      oh e.id doesn't exist? I think I've gotten it backwards.
 *      It can removed anyway, as values would eval away
 *      non values that would be looked up in the upper env are itneresting, as I don't think we can tell...
 *          wait no it's always ok to unwrap suspended_eval unless there is a True remaining
 *              the Env or the subvalue would have been captured via partial eval
 *              ALSO UNLESS BELOW
 *
 *          Also, I think it can only have a true if it hasn't been evaluated yet
 *              technically, the env could end in a suspended-param or suspended-env
 *                  make sure the lookup accounts for that, that's a *WEIRD* one
 *                      can't unwrap if it ends in a lookup, & that lookup ID is contained in the body
 *
 *  ALL THE ENV reasoning only holds if ENVs are just cons chains with supsended params or suspended env params
 *
 *  suspended if needs special gating head|tail attention
 *      mainly if rec-stopped, cond & rec takes head, relegating rest
 *
 *  for suspended calls, the rec_mapping function needs to differentiate between a parameter
 *  failing because of an error and failing because of a needed ID
 *      it will also note if map_error hits a non-cons/nil, which means the entire leve of eval needs to be abandoned
 *          since it won't be able to recurse in
 *      BUT it should allow car ID errors and continue
 *          OH should it? wait where would an ID error even come from?
 *          that's not a generally allowed error
 *      true Errors should propegate up
 *
 *      it's a bit trickey!
 *  The result of a DeriCall's IDs, if not a value, needs to become (part of, depending on if non-val parameters are allowed) the SuspendedCall's head IDs
 *      ditto for rec-stop, but the call-under bit
 *
 *
*
*
*  THE NEW INVARIENT
*   it's in heads if it can possibly be put forwards
*       it only can't if it's been blocked by a call or if rec stopper
*           and in those cases, ENV is saved on the node
*           SO True should never really be left after evaluation
*           and any node is always evaluated as far as it can be before being returned
 */

// 0 is equiv of true, -1 is equiv to runtime
#[derive(Debug, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct EnvID(i32);
const runtime_id: EnvID = EnvID(-1);
const true_id: EnvID = EnvID(0);

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct NeededIds {
    heads: BTreeSet<EnvID>,
    tails: BTreeSet<EnvID>,

    body_stopped: BTreeSet<EnvID>,
    if_stopped: BTreeSet<EnvID>,
}
impl fmt::Display for NeededIds {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.heads.is_empty() && self.tails.is_empty() && self.body_stopped.is_empty() && self.if_stopped.is_empty() {
            write!(f, "NeedsNone");
        } else {
            write!(f, "Needs");
            if !self.heads.is_empty() {
                write!(f, "H{:?}", self.heads);
            }
            if !self.tails.is_empty() {
                write!(f, "T{:?}", self.tails);
            }
            if !self.body_stopped.is_empty() {
                write!(f, "B{:?}", self.body_stopped);
            }
            if !self.if_stopped.is_empty() {
                write!(f, "I{:?}", self.if_stopped);
            }
        }
        Ok(())
    }
}

impl NeededIds {
    fn new_true()           -> Self { NeededIds { heads: iter::once(true_id).collect(), tails: BTreeSet::new(), body_stopped: BTreeSet::new(), if_stopped: BTreeSet::new() } }
    fn new_none()           -> Self { NeededIds { heads: BTreeSet::new(),               tails: BTreeSet::new(), body_stopped: BTreeSet::new(), if_stopped: BTreeSet::new() } }
    fn new_single(i: EnvID) -> Self { NeededIds { heads: iter::once(i).collect(),       tails: BTreeSet::new(), body_stopped: BTreeSet::new(), if_stopped: BTreeSet::new() } }
    fn union(&self, other: &NeededIds) -> Self {
        NeededIds {
            heads:          self.heads.union(&other.heads).cloned().collect(),
            tails:          self.tails.union(&other.tails).cloned().collect(),
            body_stopped:   self.body_stopped.union(&other.body_stopped).cloned().collect(),
            if_stopped:     self.if_stopped.union(&other.if_stopped).cloned().collect(),
        }
    }
    //fn union_without(&self, other: &NeededIds, without: &EnvID) -> Self {
        //NeededIds {
            //heads:          self.heads.union(&other.heads)              .filter(|x| *x != without).cloned().collect(),
            //tails:          self.tails.union(&other.tails)              .filter(|x| *x != without).cloned().collect(),
            //body_stopped:   self.body_stopped.union(&other.body_stopped).filter(|x| *x != without).cloned().collect(),
            //if_stopped:     self.if_stopped.union(&other.if_stopped)    .filter(|x| *x != without).cloned().collect(),
        //}
    //}
    fn without(&self, without: &EnvID) -> Self {
        NeededIds {
            heads:          self.heads.iter()       .filter(|x| *x != without).cloned().collect(),
            tails:          self.tails.iter()       .filter(|x| *x != without).cloned().collect(),
            body_stopped:   self.body_stopped.iter().filter(|x| *x != without).cloned().collect(),
            if_stopped:     self.if_stopped.iter()  .filter(|x| *x != without).cloned().collect(),
        }
    }
    fn union_into_tail(&self, other: &NeededIds, without_tail: Option<&EnvID>) -> Self {
        let new_tails = other.heads.union(&other.tails).filter(|x| without_tail.is_none() || *x != without_tail.unwrap());
        let tails: BTreeSet<EnvID> = self.tails.iter().chain(new_tails).cloned().collect();
        assert!(!tails.contains(&true_id));
        NeededIds {
            heads:          self.heads.clone(),
            tails:          tails,
            body_stopped:   self.body_stopped.clone(),
            if_stopped:     self.if_stopped.clone(),
        }
    }
    fn add_body_under(&self, u: EnvID) -> Self {
        let body_with_id = self.body_stopped.iter().cloned().chain(iter::once(u)).collect();
        if self.heads.contains(&true_id) {
            NeededIds {
                heads:          self.heads.iter().filter(|x| **x != true_id).cloned().collect(),
                tails:          self.tails.clone(),
                body_stopped:   body_with_id,
                if_stopped:     self.if_stopped.clone(),
            }
        } else {
            NeededIds {
                heads:          self.heads.clone(),
                tails:          self.tails.clone(),
                body_stopped:   body_with_id,
                if_stopped:     self.if_stopped.clone(),
            }
        }
    }
    fn add_if_under(&self, u: EnvID) -> Self {
        let if_with_id = self.if_stopped.iter().cloned().chain(iter::once(u)).collect();
        if self.heads.contains(&true_id) {
            NeededIds {
                heads:          self.heads.iter().filter(|x| **x != true_id).cloned().collect(),
                tails:          self.tails.clone(),
                body_stopped:   self.body_stopped.clone(),
                if_stopped:     if_with_id,
            }
        } else {
            NeededIds {
                heads:          self.heads.clone(),
                tails:          self.tails.clone(),
                body_stopped:   self.body_stopped.clone(),
                if_stopped:     if_with_id,
            }
        }
    }
    fn may_contain_id(&self, i: &EnvID) -> bool {
        self.heads.contains(i) || self.tails.contains(i)
    }
    fn contains_if_stop(&self, i: &EnvID) -> bool {
        self.if_stopped.contains(i)
    }
}

// 0 is equiv of true, -1 is equiv to runtime
#[derive(Clone)]
pub struct BCtx {
    id_counter: i32,
    memo: HashMap<Rc<MarkedForm>, (BTreeSet<(u64,u64,EnvID)>, Option<u64>, Rc<MarkedForm>)>,
    used_ids: Vec<(bool,BTreeSet<EnvID>)>,
}
impl BCtx {
    pub fn new_id(mut self) -> (Self, EnvID) {
        let new_id = EnvID(self.id_counter);
        self.id_counter += 1;
        (self, new_id)
    }
    pub fn set_uses_env(mut self, x: bool) -> Self {
        self.used_ids.last_mut().unwrap().0 = x;
        self
    }
    pub fn get_uses_env(&self) -> bool {
        self.used_ids.last().unwrap().0
    }
    pub fn pop_uses_env(mut self) -> Self {
        if let Some(last) = self.used_ids.last_mut() {
            last.0 = false;
        }
        self
    }
    pub fn add_id(mut self, x: EnvID) -> Self {
        self.used_ids.last_mut().unwrap().1.insert(x);
        self
    }
    pub fn pop_id_frame(mut self, x: &EnvID) -> Self {
        if let Some(last) = self.used_ids.last_mut() {
            last.1.remove(x);
        }
        self
    }
    pub fn push_used_ids(mut self) -> Self {
        self.used_ids.push((false, BTreeSet::new()));
        self
    }
    pub fn pop_used_ids(mut self) -> (Self, (bool,BTreeSet<EnvID>)) {
        let to_ret = self.used_ids.pop().unwrap();
        if let Some(last) = self.used_ids.last_mut() {
            last.0 = last.0 || to_ret.0;
            last.1.extend(to_ret.1.iter().cloned());
        }
        (self, to_ret)
    }
}

enum PushFrameResult {
    Ok(DCtx),
    UnderBody(EnvID),
    UnderIf(EnvID),
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}
// memo is only for recording currently executing hashes (calls and if's, current for us)
// only_head is not currently used
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
    pub fn real_hash_set(&self, relevant: Option<BTreeSet<EnvID>>) -> BTreeSet<(u64,u64,EnvID)> {
        self.real_set.iter().filter(|x| relevant.as_ref().map_or(true, |s| s.contains(x)))
                            .map(|id| (calculate_hash(&self.sus_env_stack.get(id)),
                                       calculate_hash(&self.sus_prm_stack.get(id)),
                                       id.clone())).collect()
    }
    pub fn copy_set_env(&self, e: &Rc<MarkedForm>) -> Self {
        DCtx { e: Rc::clone(e), current_id: self.current_id.clone(),
              sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack),
              real_set: Rc::clone(&self.real_set), fake_set: Rc::clone(&self.fake_set), fake_if_set: Rc::clone(&self.fake_if_set), ident: self.ident+1  }
    }
    fn copy_push_frame(&self, id: EnvID, se: &Rc<MarkedForm>, de: &Option<String>, e: Option<Rc<MarkedForm>>,
                           rest_params: &Option<String>, prms: Option<Rc<MarkedForm>>, body: &Rc<MarkedForm>) -> PushFrameResult {
        let mut sus_env_stack = Rc::clone(&self.sus_env_stack);
        let mut sus_prm_stack = Rc::clone(&self.sus_prm_stack);
        let mut real_set = (*self.real_set).clone();
        let mut fake_set = (*self.fake_set).clone();
        if self.fake_if_set.contains(&id) {
            //println!("Fake if real rec stopper");
            return PushFrameResult::UnderIf(id);
        }
        if (e.is_some() && prms.is_some()) {
            real_set.insert(id.clone());
            // We're not actually not under fake still!
            //fake_set.remove(&id);
        } else {
            if fake_set.contains(&id) {
                //println!("Fake body rec stopper");
                return PushFrameResult::UnderBody(id.clone());
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
                MarkedForm::new_suspended_env_lookup(Some(de.clone()), id.clone())
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
                MarkedForm::new_suspended_param_lookup(Some(p.clone()), id.clone(), 0, false, false)
            };
            massoc(p, p_val, inner_env)
        } else { inner_env };
        PushFrameResult::Ok(DCtx { e: inner_env, current_id: Some(id), sus_env_stack, sus_prm_stack,
                  real_set: Rc::new(real_set), fake_set: Rc::new(fake_set), fake_if_set: Rc::clone(&self.fake_if_set), ident: self.ident+1 })
    }
    pub fn copy_push_fake_if(&self) -> (Option<EnvID>, Self) {
        let (could_stop, new_fake_if_set) = if let Some(current_id) = self.current_id.as_ref() {
            let mut x = (*self.fake_if_set).clone();
            let could_stop = if !x.contains(current_id) { Some(current_id.clone()) } else { None };
            x.insert(current_id.clone());
            (could_stop, Rc::new(x))
        } else { (None, Rc::clone(&self.fake_if_set)) };
        (could_stop, DCtx { e: Rc::clone(&self.e), current_id: self.current_id.clone(), sus_env_stack: Rc::clone(&self.sus_env_stack), sus_prm_stack: Rc::clone(&self.sus_prm_stack),
                            real_set: Rc::clone(&self.real_set), fake_set: Rc::clone(&self.fake_set), fake_if_set: new_fake_if_set, ident: self.ident+1 })
    }

    //pub fn can_progress(&self, ids: NeededIds) -> bool {
    pub fn can_progress(&self, x: &Rc<MarkedForm>) -> bool {
        let ids = x.ids();
        // check if ids is true || ids intersection EnvIDs in our stacks is non empty || ids.hashes - current is non empty
        let all_needed:   BTreeSet<EnvID> = ids.heads.union(&ids.tails).filter(|x| **x != true_id).cloned().collect();
        let all_possible: BTreeSet<EnvID> = self.real_set.union(&self.fake_set).cloned().collect();
        let ok = all_possible.is_superset(&all_needed);
        if !ok {
            println!("Gah - needed {:?}", all_needed);
            println!("Gah - have total (fake and real) {:?}", all_possible);
            println!("it: {}", x);
        }
        assert!(ok);
        ids.heads.contains(&true_id) || !self.real_set.is_disjoint(&ids.heads) || !self.fake_set.is_superset(&ids.body_stopped) || !self.fake_if_set.is_superset(&ids.if_stopped)
    }
}

pub fn new_base_ctxs() -> (BCtx,DCtx) {
    let bctx = BCtx { id_counter: true_id.0 + 1, memo: HashMap::new(), used_ids: vec![] };
    let (bctx, root_env) = mark(root_env(), bctx);
    (bctx, DCtx { e: root_env, current_id: None, sus_env_stack: Rc::new(BTreeMap::new()), sus_prm_stack: Rc::new(BTreeMap::new()),
                  real_set: Rc::new(BTreeSet::new()), fake_set: Rc::new(BTreeSet::new()), fake_if_set: Rc::new(BTreeSet::new()), ident: 0 } )
}
impl Hash for MarkedForm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            MarkedForm::Nil                               => { "Nil".hash(state); },
            MarkedForm::Int(i)                            => { "Int".hash(state);    i.hash(state); },
            MarkedForm::Bool(b)                           => { "Bool".hash(state);   b.hash(state); },
            MarkedForm::Symbol(s)                         => { "Symbol".hash(state); s.hash(state); },

            MarkedForm::Pair(hash, ids, car, cdr)         => { hash.hash(state); },
            MarkedForm::SuspendedSymbol(hash, env, name)  => { hash.hash(state); },
            MarkedForm::SuspendedParamLookup { hash, .. } => { hash.hash(state); },
            MarkedForm::SuspendedEnvLookup   { hash, .. } => { hash.hash(state); },
            MarkedForm::SuspendedPair        { hash, .. } => { hash.hash(state); },
            MarkedForm::SuspendedEnvEval     { hash, .. } => { hash.hash(state); },
            MarkedForm::SuspendedIf          { hash, .. } => { hash.hash(state); },
            MarkedForm::DeriComb             { hash, .. } => { hash.hash(state); },
            MarkedForm::PrimComb             { name, .. } => { "PrimComb".hash(state); name.hash(state); },
        }
    }
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MarkedForm {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(u64, NeededIds, Rc<MarkedForm>, Rc<MarkedForm>),

    SuspendedSymbol(u64, Option<Rc<MarkedForm>>, String), // Needs IDs if Env chains into suspended
    SuspendedParamLookup { hash: u64, name: Option<String>, id: EnvID, cdr_num: i32, car: bool, evaled: bool },
    SuspendedEnvLookup   { hash: u64, name: Option<String>, id: EnvID },
    SuspendedPair        { hash: u64, ids: NeededIds, env: Option<Rc<MarkedForm>>, car: Rc<MarkedForm>, cdr: Rc<MarkedForm>, attempted: Option<NeededIds>, under_if: Option<EnvID>},

    SuspendedEnvEval     { hash: u64, ids: NeededIds, x: Rc<MarkedForm>, e: Rc<MarkedForm> },
    SuspendedIf          { hash: u64, ids: NeededIds, id_env: Option<(EnvID,Rc<MarkedForm>)>, c: Rc<MarkedForm>, t: Rc<MarkedForm>, e: Rc<MarkedForm> },

    PrimComb             { name: String, nonval_ok: bool, takes_de: bool, wrap_level: i32, f: fn(BCtx,DCtx,Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> },
    DeriComb             { hash: u64, lookup_name: Option<String>, ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: EnvID, wrap_level: i32,
                           sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm> },
}
impl MarkedForm {
    pub fn new_suspended_symbol(e: Option<Rc<MarkedForm>>, name: String) -> Rc<MarkedForm> {
        Rc::new(MarkedForm::SuspendedSymbol(calculate_hash(&("SuspendedSymbol", &e, &name)), e, name))
    }
    pub fn new_suspended_param_lookup(name: Option<String>, id: EnvID, cdr_num: i32, car: bool, evaled: bool) -> Rc<MarkedForm> {
        Rc::new(MarkedForm::SuspendedParamLookup { hash: calculate_hash(&("SuspendedParamLookup", &name, &id, &cdr_num, &car, &evaled)), name, id, cdr_num, car, evaled })
    }
    pub fn new_suspended_env_lookup(name: Option<String>, id: EnvID) -> Rc<MarkedForm> {
        Rc::new(MarkedForm::SuspendedEnvLookup { hash: calculate_hash(&("SuspendedEnvLookup", &name, &id)), name, id })
    }
    pub fn new_suspended_if(id_env: Option<(EnvID,Rc<MarkedForm>)>, c: Rc<MarkedForm>, t: Rc<MarkedForm>, e: Rc<MarkedForm>) -> Rc<MarkedForm> {
        // if either t or e stopped because of our fake if (SO CHECK IF IT WAS US AT PUSH TIME),
        // guard on our condition with that branch in tail
        let c_ids = c.ids();
        let t_ids = t.ids();
        let e_ids = e.ids();

        // and if we had an if_stop, we grab env
        let mut n_id_env = None;
        let ids = if let Some((this_id, maybe_env)) = id_env {
            let ids = if t_ids.contains_if_stop(&this_id) {
                n_id_env = Some((this_id.clone(), Rc::clone(&maybe_env)));
                c_ids.union_into_tail(&t_ids, Some(&true_id)).union(&maybe_env.ids())
            } else { c_ids.union(&t_ids) };
            if e_ids.contains_if_stop(&this_id) {
                n_id_env = Some((this_id, Rc::clone(&maybe_env)));
                ids.union_into_tail(&e_ids, Some(&true_id)).union(&maybe_env.ids())
            } else { ids.union(&e_ids) }
        } else {
            c_ids.union(&t_ids).union(&e_ids)
        };
        Rc::new(MarkedForm::SuspendedIf { hash: calculate_hash(&("SuspendedIf", &ids, &n_id_env, &c, &t, &e)), ids, id_env: n_id_env, c, t, e })
    }
    pub fn new_suspended_env_eval(x: Rc<MarkedForm>, e: Rc<MarkedForm>) -> Rc<MarkedForm> {
        let ids = if e.is_legal_env_chain().unwrap_or(true) {
            e.ids().union(&x.ids())
        } else {
            e.ids().union_into_tail(&x.ids(), Some(&true_id))
        };
        Rc::new(MarkedForm::SuspendedEnvEval { hash: calculate_hash(&("SuspendedEnvEval", &ids, &x, &e)), ids, x, e })
    }
    pub fn new_pair(car: Rc<MarkedForm>, cdr: Rc<MarkedForm>) -> Rc<MarkedForm> {
        let new_ids = car.ids().union(&cdr.ids());
        Rc::new(MarkedForm::Pair(calculate_hash(&("SuspendedPair", &car, &cdr)), new_ids, car, cdr))
    }
    pub fn new_suspended_pair(env: Option<Rc<MarkedForm>>, attempted: Option<NeededIds>, car: Rc<MarkedForm>, cdr: Rc<MarkedForm>, under_if: Option<EnvID>) -> Rc<MarkedForm> {
        // differentiate between call and if rec_under
        let ids = car.ids().union(&cdr.ids());
        let ids = if let Some(attempted) = &attempted {
            //attempted.union_into_tail(&ids, if env.is_some() || attempted.may_contain_id(&true_id) { Some(&true_id) } else { None })
            ids.union(attempted)
        } else {
            ids
        };
        let ids = if let Some(rec_under) =&under_if   {   ids.add_if_under(rec_under.clone()) } else { ids };
        let ids = if let Some(env)       =&env        {   ids.union(&env.ids())               } else { ids };

        Rc::new(MarkedForm::SuspendedPair{ hash: calculate_hash(&("SuspendedPair", &ids, &env, &car, &cdr, &attempted, &under_if)), ids, env, car, cdr, attempted, under_if })
    }
    fn new_raw_deri_comb(lookup_name: Option<String>, ids: NeededIds, se: Rc<MarkedForm>, de: Option<String>, id: EnvID, wrap_level: i32,
               sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm>) -> Rc<MarkedForm> {
        Rc::new(MarkedForm::DeriComb { hash: calculate_hash(&("DeriComb", &lookup_name, &ids, &se, &de, &id, &wrap_level, &sequence_params, &rest_params, &body)), lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body })
    }
    pub fn new_deri_comb(se: Rc<MarkedForm>, lookup_name: Option<String>, de: Option<String>, id: EnvID, wrap_level: i32, sequence_params: Vec<String>, rest_params: Option<String>, body: Rc<MarkedForm>, rec_under: Option<EnvID>) -> Rc<MarkedForm> {
        // HERE! Body ids might cause it to want to evaluate but it won't if SE isn't a legal env
        // do we ever need body ids except for true?
        // and can we remove se at some point?
        //let ids = if !se.is_legal_env_chain().unwrap() { se.ids() } else { se.ids().union_without(&body.ids(), &id) };
        //let ids = if body.ids().may_contain_id(&true_id) { se.ids().union(&NeededIds::new_true()) } else { se.ids() };
        let ids = if !se.is_legal_env_chain().unwrap() { se.ids().union_into_tail(&body.ids(), Some(&id)) } else { se.ids().union(&body.ids().without(&id)) };
        let ids = if let Some(rec_under) = rec_under {
            ids.add_body_under(rec_under)
        } else {
            ids
        };
        MarkedForm::new_raw_deri_comb(lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body)
    }
    pub fn tag_name(self: &Rc<MarkedForm>, new_name: &str) -> Rc<MarkedForm> {
        match &**self {
            MarkedForm::DeriComb {  hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } =>
                MarkedForm::new_raw_deri_comb(Some(new_name.to_owned()), ids.clone(), se.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), Rc::clone(body)),
            MarkedForm::SuspendedParamLookup { hash, name, id, cdr_num, car, evaled } => MarkedForm::new_suspended_param_lookup(Some(new_name.to_owned()), id.clone(), *cdr_num, *car, *evaled),
            _ => Rc::clone(self),
        }
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
                 Some(MarkedForm::new_raw_deri_comb(lookup_name.clone(), ids.clone(), Rc::clone(se), de.clone(), id.clone(), *wrap_level-1, sequence_params.clone(), rest_params.clone(), Rc::clone(body))),

            _ => None,
        }
    }
    pub fn ids(&self) -> NeededIds {
        match self {
            MarkedForm::Nil                              => NeededIds::new_none(),
            MarkedForm::Int(i)                           => NeededIds::new_none(),
            MarkedForm::Bool(b)                          => NeededIds::new_none(),
            MarkedForm::Symbol(s)                        => NeededIds::new_none(), 
            MarkedForm::Pair( hash,ids,car,cdr)          => ids.clone(),
            MarkedForm::SuspendedSymbol( hash,sus, name) => if let Some(sus) = sus { sus.ids() } else { NeededIds::new_true() },
            MarkedForm::SuspendedEnvLookup { id, .. }    => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedParamLookup { id, .. }  => NeededIds::new_single(id.clone()),
            MarkedForm::SuspendedEnvEval { ids, ..}      => ids.clone(),
            MarkedForm::SuspendedIf      { ids, ..}      => ids.clone(),
            MarkedForm::SuspendedPair{ ids, .. }         => ids.clone(),
            MarkedForm::PrimComb { .. }                  => NeededIds::new_none(),
            MarkedForm::DeriComb { ids, .. }             => ids.clone(),
        }
    }
    // note, this can be entirely ID based, but this should be more efficient
    pub fn is_value(&self) -> bool {
        let ids = match self {
            MarkedForm::Nil                              => return true,
            MarkedForm::Int(i)                           => return true,
            MarkedForm::Bool(b)                          => return true,
            MarkedForm::Symbol(s)                        => return true, 
            MarkedForm::SuspendedSymbol( hash,sus, name) => return false,
            MarkedForm::SuspendedEnvLookup { id, .. }    => return false,
            MarkedForm::SuspendedParamLookup { id, .. }  => return false,
            MarkedForm::SuspendedEnvEval { ids, ..}      => return false,
            MarkedForm::SuspendedIf      { ids, ..}      => return false,
            MarkedForm::SuspendedPair{ ids, .. }         => return false,
            MarkedForm::PrimComb { .. }                  => return true,
            MarkedForm::Pair( hash,ids,car,cdr)          => ids.clone(),
            MarkedForm::DeriComb { ids, .. }             => ids.clone(),
        };
        ids.heads.is_empty() && ids.tails.is_empty()
    }
    pub fn is_pair(&self) -> bool {
        match self {
            MarkedForm::Pair( hash,ids,car,cdr)   => true,
            _                                     => false,
        }
    }
    pub fn is_suspended_param(&self) -> bool {
        match self {
            MarkedForm::SuspendedParamLookup { .. } => true,
            _                                       => false,
        }
    }
    pub fn is_suspended_env(&self) -> bool {
        match self {
            MarkedForm::SuspendedEnvLookup { .. } => true,
            _                                     => false,
        }
    }
    pub fn unval(self: &Rc<MarkedForm>) -> Result<Rc<MarkedForm>> {
        match &**self {
            MarkedForm::Nil                              => Ok(Rc::clone(self)),
            MarkedForm::Int(i)                           => Ok(Rc::clone(self)),
            MarkedForm::Bool(b)                          => Ok(Rc::clone(self)),
            MarkedForm::Symbol(s)                        => Ok(MarkedForm::new_suspended_symbol(None, s.clone())),
            MarkedForm::Pair( hash,ids,car,cdr)          => Ok(MarkedForm::new_suspended_pair(None, Some(NeededIds::new_true()), car.unval()?, Rc::clone(cdr), None)),
            MarkedForm::SuspendedSymbol( hash,sus, name) => bail!("trying to unval a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }        => bail!("trying to unval a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }      => bail!("trying to unval a suspended param lookup"),
            MarkedForm::SuspendedEnvEval { .. }          => bail!("trying to unval a suspended env eval"),
            MarkedForm::SuspendedIf      { .. }          => bail!("trying to unval a suspended if"),
            MarkedForm::SuspendedPair{ ids, .. }         => bail!("trying to unval a suspended pair"),
            MarkedForm::PrimComb { .. }                  => Ok(Rc::clone(self)),
            MarkedForm::DeriComb { .. }                  => Ok(Rc::clone(self)),
        }
    }
    pub fn is_legal_env_chain(&self) -> Result<bool> {
        let res = match self {
            MarkedForm::Nil                          => Ok(true),
            MarkedForm::Pair( hash,ids,car,cdr)            => {
                match &**car {
                    MarkedForm::Pair( hash,idsp,carp,cdrp) => {
                        match &**cdrp {
                            MarkedForm::SuspendedSymbol( hash,sus, name)   => Ok(false),
                            MarkedForm::SuspendedEnvEval { .. }      => Ok(false),
                            MarkedForm::SuspendedIf      { .. }      => Ok(false),
                            MarkedForm::SuspendedPair{ ids, .. }     => Ok(false),
                            _                                        => Ok(carp.is_sym() && cdr.is_legal_env_chain()?),
                        }
                    },
                    _                                => Ok(false)
                }
            },

            // maybe these should be legal?
            MarkedForm::SuspendedEnvLookup { .. }    => Ok(true),
            MarkedForm::SuspendedParamLookup { .. }  => Ok(false),

            MarkedForm::SuspendedSymbol( hash,sus, name)   => Ok(false),
            MarkedForm::SuspendedEnvEval { .. }      => Ok(false),
            MarkedForm::SuspendedIf      { .. }      => Ok(false),
            MarkedForm::SuspendedPair{ ids, .. }     => Ok(false),

            MarkedForm::Int(i)                       => bail!("bad env {}", self),
            MarkedForm::Bool(b)                      => bail!("bad env {}", self),
            MarkedForm::Symbol(s)                    => bail!("bad env {}", self),
            MarkedForm::PrimComb { .. }              => bail!("bad env {}", self),
            MarkedForm::DeriComb { .. }              => bail!("bad env {}", self),
        };
        //println!("I was legal {:?} - {}", res, self);
        res
    }
    pub fn truthy(&self) -> Result<bool> {
        match self {
            MarkedForm::Nil                          => Ok(false),
            MarkedForm::Int(i)                       => Ok(true),
            MarkedForm::Bool(b)                      => Ok(*b),
            MarkedForm::Symbol(s)                    => Ok(true), 
            MarkedForm::Pair( hash,ids,car,cdr)            => Ok(true),
            MarkedForm::SuspendedSymbol( hash,sus, name)   => bail!("trying to truthy a suspended symbol"),
            MarkedForm::SuspendedEnvLookup { .. }    => bail!("trying to truthy a suspended env lookup"),
            MarkedForm::SuspendedParamLookup { .. }  => bail!("trying to truthy a suspended param lookup"),
            MarkedForm::SuspendedEnvEval { .. }      => bail!("trying to truthy a suspended env eval"),
            MarkedForm::SuspendedIf      { .. }      => bail!("trying to truthy a suspended if"),
            MarkedForm::SuspendedPair{ ids, .. }     => bail!("trying to truthy a suspended pair"),
            MarkedForm::PrimComb { .. }              => Ok(true),
            MarkedForm::DeriComb { .. }              => Ok(true),
        }
    }
    pub fn is_sym(&self) -> bool {
        match self {
            MarkedForm::Symbol(s)                    => true,
            _                                        => false,
        }
    }
    pub fn sym(&self) -> Result<&str> {
        match self {
            MarkedForm::Symbol(s)                    => Ok(s), 
            _                                        => bail!("not a symbol"),
        }
    }
    pub fn int(&self) -> Result<i32> {
        match self {
            MarkedForm::Int(i)                       => Ok(*i), 
            _                                        => bail!("not a int"),
        }
    }
    pub fn car(&self) -> Result<Rc<MarkedForm>> {
        match self {
            MarkedForm::Pair(h,ids,car,cdr)                                       => Ok(Rc::clone(car)),
            MarkedForm::SuspendedParamLookup { hash, name, id, cdr_num, car, evaled } if !car && !evaled => Ok(MarkedForm::new_suspended_param_lookup(name.clone(), id.clone(),
                                                                                                                                                *cdr_num, true, false)),
            _                                                                   => bail!("not a pair for car: {}", self),
        }
    }
    pub fn cdr(&self) -> Result<Rc<MarkedForm>> {
        match self {
            MarkedForm::Pair(h,ids,car,cdr)                                       => Ok(Rc::clone(cdr)),
            MarkedForm::SuspendedParamLookup { hash, name, id, cdr_num, car, evaled } if !car && !evaled => Ok(MarkedForm::new_suspended_param_lookup(name.clone(), id.clone(),
                                                                                                                                                *cdr_num+1, *car, false)),
            _                                                                   => bail!("not a pair for cdr: {}", self),
        }
    }
}
fn make_eval_prim(wrap_level: i32) -> Rc<MarkedForm> {
    Rc::new(MarkedForm::PrimComb { name: "eval".to_owned(), nonval_ok: true, takes_de: false, wrap_level, f: eval_func })
}
fn eval_func(bctx: BCtx, dctx: DCtx, p: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> {
    //println!("Ok, this is inside eval looking at {}", p);
    let x = p.car()?;
    let e = p.cdr()?.car()?;
    if !x.is_value() {
        //println!("Checking compatability");
        if let (MarkedForm::SuspendedParamLookup { name, id, cdr_num, car, evaled: false, .. }, MarkedForm::SuspendedEnvLookup { name: oname, id: oid, .. }) = (&*x, &*e) {
            if id == oid {
                return Ok((bctx, MarkedForm::new_suspended_param_lookup(name.clone(), id.clone(), *cdr_num, *car, true)));
            }
        }
        Ok((bctx, MarkedForm::new_suspended_pair( None, Some(x.ids()), make_eval_prim(0), p, None )))
    } else {
        //println!("Ok, returning new suspended env eval with");
        //println!("\t{} {}", p.car()?.unval()?, p.cdr()?.car()?);
        Ok((bctx, MarkedForm::new_suspended_env_eval(x.unval()?, e)))
    }
}


// Implement the suspended param / suspended env traversal
fn make_cons_prim(wrap_level: i32) -> Rc<MarkedForm> {
    Rc::new(MarkedForm::PrimComb { name: "cons".to_owned(), nonval_ok: true, takes_de: false, wrap_level, f: cons_func})
}
fn cons_func(bctx: BCtx, dctx: DCtx, p: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> {
    // the non-vals we should allow are
    // Also funcs with outstanding ids probs
    // (value . SuspendedParam)
    // ! maybe not!(value . SuspendedEnv)
    // (value . pair)
    // (pair  . pair)
    let h =  p.car()?;
    let t =  p.cdr()?.car()?;
    if !(h.is_value() || h.is_pair()) {
        Ok((bctx, MarkedForm::new_suspended_pair(None, Some(h.ids()), make_cons_prim(0), p, None)))
    } else if !(t.is_value() || t.is_pair() || t.is_suspended_param() || t.is_suspended_env()) {
        Ok((bctx, MarkedForm::new_suspended_pair(None, Some(t.ids()), make_cons_prim(0), p, None)))
    } else {
        Ok((bctx, MarkedForm::new_pair(h, t)))
    }
}
fn make_car_prim(wrap_level: i32)-> Rc<MarkedForm> {
    Rc::new(MarkedForm::PrimComb { name: "car".to_owned(), nonval_ok: true, takes_de: false, wrap_level, f: car_func})
}
fn car_func(bctx: BCtx, dctx: DCtx, p: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> {
    let maybe_pair = p.car()?;
    match maybe_pair.car() {
        Ok(x)  => Ok((bctx, x)),
        Err(_) => Ok((bctx, MarkedForm::new_suspended_pair(None, Some(maybe_pair.ids()), make_car_prim(0), p, None))),
    }
}
fn make_cdr_prim(wrap_level: i32) -> Rc<MarkedForm> {
    Rc::new(MarkedForm::PrimComb { name: "cdr".to_owned(), nonval_ok: true, takes_de: false, wrap_level, f: cdr_func})
}
fn cdr_func(bctx: BCtx, dctx: DCtx, p: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> {
    let maybe_pair = p.car()?;
    match maybe_pair.cdr() {
        Ok(x)  => Ok((bctx, x)),
        Err(_) => Ok((bctx, MarkedForm::new_suspended_pair(None, Some(maybe_pair.ids()), make_cdr_prim(0), p, None))),
    }
}


fn make_debug_prim() -> Rc<MarkedForm> {
    Rc::new(MarkedForm::PrimComb { name: "debug".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: debug_func})
}
fn debug_func(bctx: BCtx, dctx: DCtx, p: Rc<MarkedForm>) -> Result<(BCtx,Rc<MarkedForm>)> {
    // This one is a bit weird - we put the wrap level at 1 so both sides are pe'd,
    // and then return runtime
    // Hmm, I do wonder if it should capture ENV for debugging purposes
    Ok((bctx, MarkedForm::new_suspended_pair( None, Some(NeededIds::new_single(runtime_id.clone())), make_debug_prim(), p, None )))
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
                "vau" => Rc::new(MarkedForm::PrimComb { name: "vau".to_owned(), nonval_ok: false, takes_de: true, wrap_level: 0, f: |bctx, dctx, p| {
                    let de     = p.car()?.sym().map(|s| s.to_owned()).ok();
                    let params = p.cdr()?.car()?.sym()?.to_owned();
                    let body   = p.cdr()?.cdr()?.car()?.unval()?;
                    let se = Rc::clone(&dctx.e);
                    let bctx = bctx.set_uses_env(true);
                    let (bctx, id) = bctx.new_id();
                    let wrap_level = 0;
                    let sequence_params = vec![];
                    let rest_params = Some(params);
                    //println!("vau, making a new func {:?} - {}", id, p);
                    Ok((bctx, MarkedForm::new_deri_comb( se, None, de, id, wrap_level, sequence_params, rest_params, body, None )))
                }}),
                "eval"  => make_eval_prim(1),
                "cons"  => make_cons_prim(1),
                "car"   => make_car_prim(1),
                "cdr"   => make_cdr_prim(1),
                "debug" => make_debug_prim(),
                // Like Debug, listed as wrap_level 1 so bothe sides are pe'd, even though it would
                // be sequencing at runtime
                "assert" => Rc::new(MarkedForm::PrimComb { name: "assert".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let cond = p.car()?;
                    if !cond.truthy()? {
                        bail!("Assertion failed {}", cond)
                    }
                    Ok((bctx, p.cdr()?.car()?))
                }}),
                "if" => Rc::new(MarkedForm::PrimComb { name: "if".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 0, f: |bctx, dctx, p| {
                    let c = p.car()?.unval()?;
                    let t = p.cdr()?.car()?.unval()?;
                    let e = p.cdr()?.cdr()?.car()?.unval()?;
                    Ok((bctx, MarkedForm::new_suspended_if(None, c, t, e)))
                }}),
                // Non val is actually fine
                "quote" => Rc::new(MarkedForm::PrimComb { name: "quote".to_owned(), nonval_ok: true, takes_de: false, wrap_level: 0, f: |bctx, dctx, p| {
                    Ok((bctx, p.car()?))
                }}),
                "=" => Rc::new(MarkedForm::PrimComb { name: "=".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    let a =  p.car()?;
                    let b =  p.cdr()?.car()?;
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
                // This could allow nonval with fallback
                "comb?" => Rc::new(MarkedForm::PrimComb { name: "comb?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::PrimComb { .. }  => true,
                        MarkedForm::DeriComb { .. }  => true,
                        _                            => false,
                    }))))
                }}),
                // This could allow nonval with fallback
                "pair?" => Rc::new(MarkedForm::PrimComb { name: "pair?".to_owned(), nonval_ok: false, takes_de: false, wrap_level: 1, f: |bctx, dctx, p| {
                    Ok((bctx, Rc::new(MarkedForm::Bool(match &* p.car()? {
                        MarkedForm::Pair(_h, _i, _a,_b) => true,
                        _                           => false,
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
    let ids = x.ids();
    !ids.may_contain_id(&true_id) && check_id.map(|check_id| !ids.may_contain_id(&check_id)).unwrap_or(true)
}

pub fn partial_eval(bctx_in: BCtx, dctx: DCtx, form: Rc<MarkedForm>, use_memo: bool) -> Result<(BCtx,Rc<MarkedForm>)> {
    let mut bctx = bctx_in.push_used_ids();
    let mut next_form = Some(Rc::clone(&form));
    let mut skipped_from: Option<(BCtx, DCtx, Rc<MarkedForm>)> = None;
    let mut one04s = 0;
    let mut last: Option<Rc<MarkedForm>> = None;
    let mut doublings = 0;
    loop {
        let x = next_form.take().unwrap();
        println!("{:ident$}({})PE:", "", dctx.ident*4, ident=dctx.ident*4);
        if dctx.ident*4 == 104 {
            one04s += 1;
            if one04s == 100_000 {
                println!("{:ident$}({})PE: {}", "", dctx.ident*4, one04s, ident=dctx.ident*4);
                println!("{:ident$}PE: {}", "", x, ident=dctx.ident*4);
                assert!(false);
            }
            //println!("{:ident$}PE: {}", "", x, ident=dctx.ident*4);
        }
        if let Some(l) = last {
            if x == l {
                doublings += 1;
            } else {
                doublings = 0;
            }
        } else {
            doublings = 0;
        }
        if doublings == 100 {
            println!("100 doublings of {}", x);
            println!("(because of {:?} with {:?}/{:?})", x.ids(), dctx.real_set, dctx.fake_set);
        }
        assert!(doublings < 100);
        last = Some(Rc::clone(&x));
        //println!("{:ident$}PE: {}", "", x, ident=dctx.ident*4);
        //if !dctx.can_progress(x.ids()) {
        if !dctx.can_progress(&x) {
            //println!("{:ident$}Shouldn't go! (because of {:?} with {:?}/{:?})", "", x.ids(), dctx.real_set, dctx.fake_set, ident=dctx.ident*4);
            if !(x.is_value() || !dctx.fake_set.is_empty()) {
                println!("Hmm what's wrong here - it's not a value, but our fake set is empty...");
                println!("{:ident$}{}", "", x, ident=dctx.ident*4);
                println!("{:ident$}Shouldn't go! (because of {:?} with {:?}/{:?})", "", x.ids(), dctx.real_set, dctx.fake_set, ident=dctx.ident*4);
            }
            assert!(x.is_value() || !dctx.fake_set.is_empty());
            let (mut bctx, (uses_env, used_ids)) = bctx.pop_used_ids();
            //if form.is_legal_env_chain().unwrap_or(false) && x.is_legal_env_chain().unwrap_or(false) && x != form {
            // if we open it to more then we also need to track usage of current env
            if use_memo && x != form {
            //if false {
                //let form_ids = form.ids();
                //assert!(!form_ids.may_contain_id(&true_id));
                //if !x.is_legal_env_chain().unwrap_or(false) {
                    //println!("Went from legal hash chain {} to {}", form, x);
                    //println!("That is, from {}", form);
                    //println!("That is,   to {}", x);
                    //AHAH! Ok, how it happens is for things like SuspendedParamLookup(offset,eval=true) because it will do a lookup
                    // that is a legal thing to sub in, but then eval will cause it to become suspended and maybe not a legal environment
                    //
                    // Hmm, how do we deal with that?
                //}
                //assert!(x.is_legal_env_chain().unwrap_or(false));
                //println!("Inserting skip from {} to {} blocked on {:?}-{:?} and {}-{:?} ", form, x, used_ids, dctx.real_hash_set(Some(used_ids.clone())), uses_env, if uses_env { Some(calculate_hash(&dctx.e)) } else { None });
                bctx.memo.insert(form, (dctx.real_hash_set(Some(used_ids)), if uses_env { Some(calculate_hash(&dctx.e)) } else { None }, Rc::clone(&x)));
                //bctx.memo.insert(form, (dctx.real_hash_set(), if form_ids.may_contain_id(&true_id) { Some(calculate_hash(&dctx.e)) } else { None }, Rc::clone(&x)));
                //bctx.memo.insert(form, (dctx.real_hash_set(),  Some(calculate_hash(&dctx.e)),  Rc::clone(&x)));
            }
            //if let Some((obctx, odctx, ox)) = skipped_from {
            //    println!();
            //    println!();
            //    println!();
            //    println!("STARTING REPATH");
            //    println!();
            //    println!();
            //    let (nobctx, nox) = partial_eval(obctx, odctx, ox, false)?;
            //    println!();
            //    println!();
            //    println!();
            //    println!("REPATH DONE");
            //    println!();
            //    println!();
            //    if nox != x {
            //        println!();
            //        println!("x  : {}", x);
            //        println!("nox: {}", nox);
            //    }
            //    assert!(nox == x);
            //}
            return Ok((bctx, x));
        }
        let got = bctx.memo.get(&x);
        // ah crap it's not the same ids it's the same ids with the same hashes
        //if false {
        if use_memo && got.map(|(ids,maybe_e_hash,it)| maybe_e_hash.map(|h| h == calculate_hash(&dctx.e)).unwrap_or(true) && dctx.real_hash_set(None).is_superset(ids)).unwrap_or(false) {
            let skip = Rc::clone(&got.unwrap().2);
            println!("{:ident$}({}) SKIPPING PE ", "", dctx.ident, ident=dctx.ident*4);
            //println!("{:ident$}({}) PE {} skip forwards to {}", "", dctx.ident, x, skip, ident=dctx.ident*4);
            //println!("{:ident$}({}) PE {} skip forwards to {} inside {} - got was {:?} and our hash is {}", "", dctx.ident, x, skip, dctx.e, got, calculate_hash(&dctx.e), ident=dctx.ident*4);
            //skipped_from = Some((bctx.clone(), dctx.clone(), x));
            //HERE
            let gots = got.unwrap().clone();
            if gots.1.is_some() {
                bctx = bctx.set_uses_env(true);
            }
            for (_,_,id) in gots.0 {
                bctx = bctx.add_id(id);
            }
            //THERE

            next_form = Some(skip);
        } else {
            //println!("{:ident$}({}) PE {} (because of {:?} with {:?}/{:?})", "", dctx.ident, x, x.ids(), dctx.real_set, dctx.fake_set, ident=dctx.ident*4);
            let (new_bctx, new_form) =  partial_eval_step(&x, bctx.clone(), &dctx, use_memo)?;
            bctx = new_bctx;
            // basic Drop redundent veval
            // Old one was recursive over parameters to combs, which we might need, since the redundent veval isn't captured by
            // ids. TODO!
            //
            // Nowadays, dropping EnvEval is legal always *unless*
            //  - True is in ids
            //  because we have the env attached to suspended lookups, if, and call
            if let MarkedForm::SuspendedEnvEval { x, e, .. } = &*new_form {
                if !x.ids().may_contain_id(&true_id) && e.is_legal_env_chain()? {
                    //println!("{:ident$}Dropping redundent eval: {} from {}", "", x, e, ident=dctx.ident*4);
                    //println!("{:ident$}Dropping redundent eval: {}", "", x, ident=dctx.ident*4);
                    println!("{:ident$}Dropping redundent eval:", "", ident=dctx.ident*4);
                    next_form = Some(Rc::clone(x));
                    // do we still need force for drop redundent veval?
                    // Not while it's not recursive, at elaset
                } else {
                    next_form = Some(new_form);
                }
            } else {
                next_form = Some(new_form);
            }
        }
    }
}
enum MapUnvalPEvalResult {
    Ok(BCtx,Rc<MarkedForm>),
    NotYet(anyhow::Error),
    BadError(anyhow::Error),
}
fn map_unval_peval(bctx: BCtx, dctx: DCtx, x: Rc<MarkedForm>, use_memo: bool) -> MapUnvalPEvalResult {
    match &*x {
        MarkedForm::Nil                     => MapUnvalPEvalResult::Ok(bctx,x),
        MarkedForm::Pair(_h, ids, x_car, x_cdr) => {
            match x_car.unval() {
                Ok(unval) => {
                    match partial_eval(bctx, dctx.clone(), unval, use_memo) {
                        Ok((bctx, new_x_car)) => {
                            match map_unval_peval(bctx, dctx.clone(), Rc::clone(x_cdr), use_memo) {
                                MapUnvalPEvalResult::Ok(bctx, new_x_cdr) => MapUnvalPEvalResult::Ok(bctx, MarkedForm::new_pair(new_x_car, new_x_cdr)),
                                e => e,
                            }
                        }
                        Err(e) => MapUnvalPEvalResult::BadError(e),
                    }
                },
                Err(e) => MapUnvalPEvalResult::NotYet(e),
            }
        },

        MarkedForm::Int(i)                          => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")),
        MarkedForm::Bool(b)                         => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")),
        MarkedForm::Symbol(s)                       => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")), 
        MarkedForm::PrimComb { .. }                 => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")),
        MarkedForm::Pair(h,ids,car,cdr)             => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")),
        MarkedForm::DeriComb { ids, .. }            => MapUnvalPEvalResult::BadError(anyhow!("map_unval_peval over not a list")),

        MarkedForm::SuspendedSymbol(h, sus, name)   => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),
        MarkedForm::SuspendedEnvLookup { id, .. }   => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),
        MarkedForm::SuspendedParamLookup { id, .. } => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),
        MarkedForm::SuspendedEnvEval { ids, ..}     => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),
        MarkedForm::SuspendedIf      { ids, ..}     => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),
        MarkedForm::SuspendedPair{ ids, .. }        => MapUnvalPEvalResult::NotYet(anyhow!("map_unval_peval over not (yet) a list")),

    }
}
fn partial_eval_step(x: &Rc<MarkedForm>, bctx: BCtx, dctx: &DCtx, use_memo: bool) -> Result<(BCtx,Rc<MarkedForm>)>  {
    //println!("{:ident$}({}) {}", "", dctx.ident, x, ident=dctx.ident*4);
    match &**x {
        MarkedForm::Pair(h,ids,car,cdr) => {
            //println!("{:ident$}pair ({}) {}", "", dctx.ident, x, ident=dctx.ident*4);
            let (bctx, car) = partial_eval(bctx, dctx.clone(), Rc::clone(car), use_memo)?;
            let (bctx, cdr) = partial_eval(bctx, dctx.clone(), Rc::clone(cdr), use_memo)?;
            Ok((bctx, MarkedForm::new_pair(car, cdr)))
        },
        MarkedForm::SuspendedSymbol(h, sus, name) => {
            // Have to account for the *weird* case that the env chain ends in a suspended param / suspended env
            //println!("Lookin up symbol {}", name);
            //println!("Lookin up symbol {} in {}", name, dctx.e);
            let (bctx, mut t) = if let Some(sus) = sus {
                partial_eval(bctx, dctx.clone(), Rc::clone(sus), use_memo)?
            } else {
                (bctx.set_uses_env(true), Rc::clone(&dctx.e))
            };
            loop {
                if let Ok(cmp) = t.car().and_then(|kv| kv.car()).and_then(|s| s.sym().map(|s| s.to_owned())) {
                    if *name == cmp {
                        //println!("\tgot for symbol {} {}", name, t.car()?.cdr()?.tag_name(name));
                        return Ok((bctx, t.car()?.cdr()?.tag_name(name)));
                    } else {
                        t = t.cdr()?;
                    }
                } else {
                    // bad env
                    match &*t {
                        MarkedForm::Nil                          => bail!("Lookup for {} not found!", name),
                        MarkedForm::SuspendedSymbol(h,sus, name) => break,
                        MarkedForm::SuspendedEnvLookup   { .. }  => break,
                        MarkedForm::SuspendedParamLookup { .. }  => break,
                        MarkedForm::SuspendedEnvEval     { .. }  => break,
                        MarkedForm::SuspendedIf          { .. }  => break,
                        MarkedForm::SuspendedPair        { .. }  => break,
                        MarkedForm::Pair(h,ids,car,cdr)          => break,
                        _                                        => bail!("Illegal lookup for {}", name),
                    }
                }
            }
            //println!("\tcouldn't find it, returning suspended");
            return Ok((bctx, MarkedForm::new_suspended_symbol(Some(t), name.clone())));
        },
        MarkedForm::SuspendedEnvLookup { hash, name, id } => {
            if let Some(v) = dctx.sus_env_stack.get(id) {
                //println!("SuspendedEnvLookup for {:?} got {}", name, v);
                let bctx = bctx.add_id(id.clone());
                Ok((bctx, if let Some(name) = name { v.tag_name(name) } else { Rc::clone(v) }))
            } else {
                panic!("failed env lookup (forced)");
            }
        },
        MarkedForm::SuspendedParamLookup { hash, name, id, cdr_num, car, evaled } => {
println!("SUSSUS param lookup");
            if let Some(v) = dctx.sus_prm_stack.get(id) {
                let bctx = bctx.add_id(id.clone());
                let mut translated_value = if let Some(name) = name { v.tag_name(name) } else { Rc::clone(v) };
                for i in 0..*cdr_num {
                    translated_value = translated_value.cdr()?;
                }
                if *car {
                    translated_value = translated_value.car()?;
                }
                if *evaled {
                    // but with this, we have to deal with unval failures
                    // actually, do we have to deal with unval failures?
                    translated_value = MarkedForm::new_suspended_env_eval(translated_value.unval().unwrap(), MarkedForm::new_suspended_env_lookup(None, id.clone()));
                }
                Ok((bctx, translated_value))
            } else {
                panic!("failed param lookup (forced)");
            }
        },
        MarkedForm::SuspendedEnvEval { x, e, .. } => {
            // this bit is a little tricky - we'd like to tail call, but we can't lose our env
            // if it fails.
            let (bctx, e) = partial_eval(bctx, dctx.clone(),          Rc::clone(e), use_memo)?;
            if !e.is_legal_env_chain()? {
                Ok((bctx, MarkedForm::new_suspended_env_eval(Rc::clone(x), e)))
            } else {
                // Reset uses env b/c we're not, we're using e...
                let uses_env = bctx.get_uses_env();
                let (bctx, x) = partial_eval(bctx, dctx.copy_set_env(&e), Rc::clone(x), use_memo)?;
                let bctx = bctx.set_uses_env(uses_env);
                if x.is_value() {
                    Ok((bctx, x))
                } else {
                    Ok((bctx, MarkedForm::new_suspended_env_eval(x, e)))
                }
            }
            // Note also that we drop redundent vevals at the bottom of the loop tail-call loop
            // with force
        },
        MarkedForm::SuspendedIf { c, id_env, t, e, ids, .. } => {
            let (e_override, bctx, dctx) = if let Some((id, env)) = id_env {
                let (bctx, nenv) = partial_eval(bctx, dctx.clone(), Rc::clone(env), use_memo)?;
                if !nenv.is_legal_env_chain()? {
                    return Ok((bctx, MarkedForm::new_suspended_if(Some((id.clone(), nenv)), Rc::clone(c), Rc::clone(t), Rc::clone(e))));
                }
                (true, bctx, dctx.copy_set_env(&nenv))
            } else {
                (false, bctx, dctx.clone())
            };
            let (bctx, c) = partial_eval(bctx, dctx.clone(), Rc::clone(c), use_memo)?;
            if let Ok(b) = c.truthy() {
                if b {
                    Ok((bctx, Rc::clone(t)))
                } else {
                    Ok((bctx, Rc::clone(e)))
                }
            } else {
                let (could_stop, dctx) = dctx.copy_push_fake_if();
                let (    bctx, t) = partial_eval(bctx, dctx.clone(), Rc::clone(t), use_memo)?;
                let (mut bctx, e) = partial_eval(bctx, dctx.clone(), Rc::clone(e), use_memo)?;
                if let Some(cs) = could_stop {
                    if !e_override {
                        bctx = bctx.set_uses_env(true);
                    }
                    Ok((bctx, MarkedForm::new_suspended_if(Some((cs, Rc::clone(&dctx.e))), c, t, e)))
                } else {
                    Ok((bctx, MarkedForm::new_suspended_if(None, c, t, e)))
                }
            }
        },
        MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
println!("SUSSUS deri comb");
            // TODO: figure out wrap level, sequence params, etc
            // the current env is our new se
            
            // wat
            //let se = Rc::clone(&dctx.e);
            let (bctx, se) = partial_eval(bctx, dctx.clone(), Rc::clone(se), use_memo)?;
            if !se.is_legal_env_chain()? {
                return Ok((bctx, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), Rc::clone(body), None)));
            }
            let ident_amount = dctx.ident*4;

            match dctx.copy_push_frame(id.clone(), &se, &de, None, &rest_params, None, body) {
                PushFrameResult::Ok(inner_dctx) => {
                    //println!("{:ident$}Doing a body deri for {:?} which is {}", "", lookup_name, x, ident=ident_amount);
                    //println!("{:ident$}and also body ids is {:?}", "", body.ids(), ident=ident_amount);
                    println!("{:ident$}pushing DeriComb for {:?}", "", id, ident=ident_amount);
                    // inner use doesn't count since through se
                    let uses_env = bctx.get_uses_env();
                    let (bctx, body) = partial_eval(bctx, inner_dctx, Rc::clone(&body), use_memo)?;
                    let bctx = bctx.set_uses_env(uses_env);
                    let bctx = bctx.pop_id_frame(id);
                    println!("{:ident$}popping DeriComb for {:?}", "", id, ident=ident_amount);
                    //println!("{:ident$}result was {}", "", body, ident=ident_amount);
                    Ok((bctx, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), body, None)))
                },
                PushFrameResult::UnderBody(rec_stop_under) => {
                    //println!("{:ident$}call of {:?} failed b/c rec_stop_under b/c BODY", "", lookup_name, ident=dctx.ident*4);
                    Ok((bctx, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), Rc::clone(body), Some(rec_stop_under))))
                },
                PushFrameResult::UnderIf(rec_stop_under) => {
                    //println!("{:ident$}call of {:?} failed b/c rec_stop_under b/c IF", "", lookup_name, ident=dctx.ident*4);
                    Ok((bctx, MarkedForm::new_deri_comb(se, lookup_name.clone(), de.clone(), id.clone(), *wrap_level, sequence_params.clone(), rest_params.clone(), Rc::clone(body), Some(rec_stop_under))))
                },
            }
        },
        MarkedForm::SuspendedPair { hash, ids, env, car, cdr, attempted, under_if } => {
println!("SUSSUS suspended pair");
            let (e_override, bctx, env) = if let Some(env) = env {
                let (bctx, nenv) = partial_eval(bctx, dctx.clone(), Rc::clone(env), use_memo)?;
                if !nenv.is_legal_env_chain()? {
                    return Ok((bctx, MarkedForm::new_suspended_pair( Some(nenv), attempted.clone(), Rc::clone(car), Rc::clone(cdr), under_if.clone())));
                }
                (true, bctx, nenv)
            } else {
                (false, bctx, Rc::clone(&dctx.e))
            };
            let mut need_denv = true;
            let (    bctx, mut car) = partial_eval(bctx, dctx.clone(), Rc::clone(car), use_memo)?;
            let (mut bctx, mut cdr) = partial_eval(bctx, dctx.clone(), Rc::clone(cdr), use_memo)?;
            while let Some(wrap_level) = car.wrap_level() {
                if wrap_level > 0 {
                    // two types of errors here - real ones, and ones that just prevent evaluating
                    // the entire parameter list right now due to suspended
                    match map_unval_peval(bctx.clone(), dctx.clone(), Rc::clone(&cdr), use_memo) {
                        MapUnvalPEvalResult::Ok(new_bctx, new_cdr) => {
                            car = car.decrement_wrap_level().unwrap();
                            cdr = new_cdr;
                            bctx = new_bctx;
                        },
                        MapUnvalPEvalResult::NotYet(e) => {
                            //println!("{:ident$} evaling parameters failed (for now) b/c {:?}", "", e, ident=dctx.ident*4);
                            break;
                        },
                        MapUnvalPEvalResult::BadError(e) => {
                            //println!("{:ident$} evaling parameters failed (FOREVER) b/c {:?}", "", e, ident=dctx.ident*4);
                            return Err(e);
                        },
                    }
                } else {
                    // check to see if can do call
                    // We might want to enable not pure values for cons/car/cdr?
                    match &*car {
                        MarkedForm::PrimComb { name, nonval_ok, takes_de, wrap_level, f} => {
                            need_denv = *takes_de;
                            if !nonval_ok && !cdr.is_value() {
                                break;
                            }
                            //println!("{:ident$}doing a call eval of {}", "", name, ident=dctx.ident*4);
                            //println!("{:ident$}parameters {} are? a val because {:?}", "", cdr, cdr.ids(), ident=dctx.ident*4);
                            //return f(bctx.clone(), dctx.clone(), Rc::clone(&cdr)); 
                            // If it's either already set, or if we're not overriding, keep sub-result
                            let e_mask = !e_override || bctx.get_uses_env();
                            let (bctx,result) = f(bctx.clone(), dctx.copy_set_env(&env), Rc::clone(&cdr))?;
                            let newue = e_mask && bctx.get_uses_env();
                            let bctx = bctx.set_uses_env(newue);
                            //println!("{:ident$}successful result is {}", "", result, ident=dctx.ident*4);
                            //println!("{:ident$}successful result", "", ident=dctx.ident*4);
                            return Ok((bctx,result));
                        }
                        MarkedForm::DeriComb { hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
                            need_denv = de.is_some();
                            if !cdr.is_value() {
                                break;
                            }
                            let saved_env = if need_denv { Some(Rc::clone(&env)) } else { None };
                            //new_attempted = Attempted::True(if de.is_some() { Some(dctx.e.ids()) } else { None });
                            if de.is_some() && dctx.e.ids().may_contain_id(id) {
                                // The current environment may contain a reference to our ID, which
                                // means if we take that environment, if we then PE that
                                // environment we will replace it with our real environment that
                                // still has a dynamic reference to the current environment, which
                                // will be an infinate loop
                                if need_denv && !e_override {
                                    bctx = bctx.set_uses_env(true);
                                }
                                return Ok((bctx, MarkedForm::new_suspended_pair( saved_env, Some(NeededIds::new_single(id.clone())), car, cdr, None)));
                            }
                            // not yet supporting sequence params
                            match dctx.copy_push_frame(id.clone(), &se, &de, Some(Rc::clone(&env)), &rest_params, Some(Rc::clone(&cdr)), body) {
                                PushFrameResult::Ok(inner_dctx) => {
                                    let ident_amount = inner_dctx.ident*4;
                                    //println!("{:ident$}doing a call eval of {} in {}", "", body, inner_dctx.e, ident=inner_dctx.ident*4);
                                    //println!("{:ident$}doing a call eval of {:?}", "", lookup_name, ident=ident_amount);
                                    //println!("{:ident$}with_parameters {}", "", cdr, ident=ident_amount);

                                    //Here is where we could do a tail call instead, but there
                                    //would be no recovery back into the call-form...
                                    println!("{:ident$}pushing true derived call for for {:?}", "", id, ident=ident_amount);
                                    let e_mask = (de.is_some() && !e_override) || bctx.get_uses_env();
                                    let (bctx, r) =  partial_eval(bctx.clone(), inner_dctx, Rc::clone(body), use_memo)?;
                                    println!("{:ident$}popping true derived call for for {:?}", "", id, ident=ident_amount);
                                    let newue = e_mask && bctx.get_uses_env();
                                    let bctx = bctx.set_uses_env(newue);

                                    let mut bctx = bctx.pop_id_frame(id);
                                    if combiner_return_ok(&r, Some(id.clone())) {
                                        println!("{:ident$}return ok {:?} - {:?}", "", id, r.ids(), ident=ident_amount);
                                        return Ok((bctx, r));
                                    } else {
                                        if need_denv && !e_override {
                                            bctx = bctx.set_uses_env(true);
                                        }
                                        let id = id.clone();
                                        let car_ids = car.ids();
                                        let cdr_ids = cdr.ids();
                                        let sus_pair = MarkedForm::new_suspended_pair( saved_env, Some(r.ids()), car, cdr, None);
                                        println!("{:ident$}return not ok, doing sus pair {:?} - {:?} (car_ids {:?}, cdr_ids {:?})", "", id, sus_pair.ids(), car_ids, cdr_ids, ident=ident_amount);
                                        if r.ids().may_contain_id(&id) {
                                            println!("Need self to be real but we were - {}", r);
                                            //ok, so the not progressing when se isn't a legal env is preventing progress that could be made with a real env
                                            // which makes sense
                                        }
                                        assert!(!r.ids().may_contain_id(&id));
                                        return Ok((bctx, sus_pair));
                                    }
                                },
                                PushFrameResult::UnderBody(rec_stop_under) => { unreachable!() },
                                PushFrameResult::UnderIf(rec_stop_under) => {
                                    //println!("{:ident$}call of {:?} failed b/c rec_stop_under of if", "", lookup_name, ident=dctx.ident*4);
                                    if need_denv && !e_override {
                                        bctx = bctx.set_uses_env(true);
                                    }
                                    return Ok((bctx, MarkedForm::new_suspended_pair( saved_env, None, car, cdr, Some(rec_stop_under))));
                                },
                            }
                        },
                        // These are illegal values
                        MarkedForm::Nil                             => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::Pair(h, ids, x_car, x_cdr)      => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::Int(i)                          => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::Bool(b)                         => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::Symbol(s)                       => bail!("tried to call a bad value {:?}", car), 
                        MarkedForm::PrimComb { .. }                 => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::Pair(h,ids,car,cdr)             => bail!("tried to call a bad value {:?}", car),
                        MarkedForm::DeriComb { ids, .. }            => bail!("tried to call a bad value {:?}", car),
                        _ => {}, // suspended, so reform
                    }
                    break;
                }
            }
            // Didn't manage to call
            if need_denv && !e_override {
                bctx = bctx.set_uses_env(true);
            }
            Ok((bctx, MarkedForm::new_suspended_pair( if need_denv { Some(env) } else { None }, None, car, cdr, None)))
        },
        // Values should never get here b/c ids UNLESS FORCE HAH
        _ => panic!("value evaled! {}", x),
    }
}

impl fmt::Display for MarkedForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkedForm::Nil                   => write!(f, "nil"),
            MarkedForm::Int(i)                => write!(f, "{}", i),
            MarkedForm::Bool(b)               => write!(f, "{}", b),
            MarkedForm::Symbol(s)             => write!(f, "{}", s),
            MarkedForm::Pair( h, ids, car, cdr)   => {
                //write!(f, "{:?}#({}", ids, car)?;
                write!(f, "({}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair( ref h, ref ids, ref carp, ref cdrp) => {
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
            MarkedForm::SuspendedEnvEval {  hash, ids, x, e }            => write!(f, "({}){{Sveval {} {}}}", ids, x, e),
            MarkedForm::SuspendedIf      {  hash, ids, id_env, c, t, e }    => {
                if id_env.is_some() {
                    write!(f, "({})#HasEnv{{Sif {} {} {}}}", ids, c, t, e)
                } else {
                    write!(f, "({}){{Sif {} {} {}}}", ids, c, t, e)
                }
            },
            MarkedForm::SuspendedSymbol( hash,sus,name)                  => if let Some(sus) = sus { write!(f, "({}){}", sus, name) } else { write!(f, "(){}", name) },
            MarkedForm::SuspendedEnvLookup { hash, name, id }            => write!(f, "{:?}({:?}env)", name, id),
            MarkedForm::SuspendedParamLookup { hash, name, id, cdr_num, car, evaled } => write!(f, "{:?}({:?}{}{}{})", name, id, cdr_num, car, evaled),
            MarkedForm::PrimComb { name, wrap_level, .. }          => write!(f, "<{}{}>", name, wrap_level),

            MarkedForm::DeriComb {  hash, lookup_name, ids, se, de, id, wrap_level, sequence_params, rest_params, body } => {
                //let env_form = format!("{}", se);
                write!(f, "{}#[{:?}/{:?}/{:?}/{}/{:?}/{:?}/{}]", ids, lookup_name, de, id, wrap_level, sequence_params, rest_params, body)
            },

            MarkedForm::SuspendedPair{ hash, ids, env, car, cdr, .. } => {
                if env.is_some() {
                    write!(f, "{}#HasEnv{{{}", ids, car)?;
                } else {
                    write!(f, "{}#{{{}", ids, car)?;
                }
                //write!(f, "{{{}", car)?;
                let mut traverse: Rc<MarkedForm> = Rc::clone(cdr);
                loop {
                    match &*traverse {
                        MarkedForm::Pair( ref h, ref ids, ref carp, ref cdrp) => {
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
