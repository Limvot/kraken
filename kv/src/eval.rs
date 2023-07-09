use std::fmt;
use std::boxed::Box;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;

pub trait FormT: std::fmt::Debug {
    fn sym(&self) -> Option<&str>;
    fn pair(&self) -> Option<(Rc<Self>,Rc<Self>)>;
    fn car(&self) -> Option<Rc<Self>>;
    fn cdr(&self) -> Option<Rc<Self>>;
    fn is_nil(&self) -> bool;
    fn call(&self, p: Rc<Self>, e: Rc<Self>, nc: Box<Cont<Self>>, metac: Cont<Self>) -> Cursor<Self>;
    fn impl_prim(ins: PrimCombI, e: Rc<Self>, ps: Vec<Rc<Self>>, c: Cont<Self>, metac: Cont<Self>) -> Cursor<Self>;
}

#[derive(Debug, Eq, PartialEq)]
pub enum Cont<F: FormT + ?Sized> {
    Exit,
    MetaRet,
    CatchRet { nc: Box<Cont<F>>,  restore_meta: Box<Cont<F>> },
    Eval     {                    e: Rc<F>, nc: Box<Cont<F>> },
    Call     { p: Rc<F>,          e: Rc<F>, nc: Box<Cont<F>> },
    PramEval { eval_limit: i32, to_eval: Rc<F>, collected: Option<Vec<Rc<F>>>, e: Rc<F>, ins: PrimCombI, nc: Box<Cont<F>> },
}
impl<F: FormT> Clone for Cont<F> {
    fn clone(&self) -> Self {
        match self {
            Cont::Exit                                                   => Cont::Exit,
            Cont::MetaRet                                                => Cont::MetaRet,
            Cont::CatchRet { nc,  restore_meta     }                     => Cont::CatchRet { nc: nc.clone(),  restore_meta: restore_meta.clone() },
            Cont::Eval     {                 e, nc }                     => Cont::Eval     {                             e: Rc::clone(e),        nc: nc.clone() },
            Cont::Call     { p,              e, nc }                     => Cont::Call     { p: Rc::clone(p),            e: Rc::clone(e),        nc: nc.clone() },
            Cont::PramEval { eval_limit, to_eval, collected, e, ins, nc} => Cont::PramEval { eval_limit: *eval_limit, to_eval: Rc::clone(to_eval),
                                                                                             collected: collected.as_ref().map(|x| x.iter().map(|x| Rc::clone(x)).collect()),
                                                                                             e: Rc::clone(e), ins: ins.clone(), nc: nc.clone() },
        }
    }
}
pub struct Cursor<F: FormT + ?Sized> { pub f: Rc<F>, pub c: Cont<F>, pub metac: Cont<F> }

pub fn eval<F: FormT>(e: Rc<F>, f: Rc<F>) -> Rc<F> {
    let mut cursor = Cursor::<F> { f, c: Cont::Eval { e, nc: Box::new(Cont::MetaRet) }, metac: Cont::Exit };
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
            Cont::Eval { e, nc } => {
                if let Some((comb, p)) = f.pair() {
                   cursor = Cursor { f: comb, c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::Call { p, e, nc }) }, metac }
                } else if let Some(s) = f.sym() {
                   let mut t = Rc::clone(&e);
                   while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                        t = t.cdr().unwrap();
                    }
                    cursor = Cursor { f: t.car().unwrap().cdr().unwrap(), c: *nc, metac };
                } else {
                    cursor = Cursor { f: Rc::clone(&f), c: *nc, metac };
                }
            },
            Cont::PramEval { eval_limit, to_eval, collected, e, ins, nc } => {
                let mut next_collected = if let Some(mut collected) = collected {
                    collected.push(f); collected
                } else { vec![] };
                if eval_limit == 0 || to_eval.is_nil() {
                    let mut traverse = to_eval;
                    while !traverse.is_nil() {
                        next_collected.push(traverse.car().unwrap());
                        traverse = traverse.cdr().unwrap();
                    }
                    cursor = F::impl_prim(ins, e, next_collected, *nc, metac);
                } else {
                    cursor = Cursor { f: to_eval.car().unwrap(), c: Cont::Eval { e: Rc::clone(&e), nc: Box::new(Cont::PramEval { eval_limit: eval_limit - 1,
                                                                                                                                 to_eval: to_eval.cdr().unwrap(),
                                                                                                                                 collected: Some(next_collected),
                                                                                                                                 e, ins, nc }) }, metac };
                }
            },
            Cont::Call { p, e, nc } => {
                cursor = f.call(p, e, nc, metac);
            },
        }
    }
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

