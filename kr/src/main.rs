#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::{Form,PossibleTailCall};

fn eval(e: Rc<Form>, f: Rc<Form>) -> Rc<Form> {
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
                        if let Some(params) = params.sym() {
                            new_e = assoc(params, Rc::clone(p), new_e);
                        }
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

fn root_env() -> Rc<Form> {
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
            let params = p.cdr().unwrap().car().unwrap();
            let body   = p.cdr().unwrap().cdr().unwrap().car().unwrap();

            PossibleTailCall::Result(Rc::new(Form::DeriComb { se: e, de, params, body }))
        }))),
        ("=", Rc::new(Form::PrimComb("=".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            PossibleTailCall::Result(Rc::new(Form::Bool(a == b)))
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
            println!("Debug: {:?}", eval(Rc::clone(&e), p.car().unwrap()));
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

fn main() {
    let input = "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))";
    let parsed_input = grammar::TermParser::new().parse(input).unwrap();
    println!("Parsed input is {:?}", parsed_input);
    let result = eval(root_env(), Rc::new(parsed_input));
    println!("Result is {:?}", result);
}

#[test]
fn parse_test() {
    let g = grammar::TermParser::new();
    for test in [
    "22", "(22)", "(((22)))",
    "(22 )", "()", "( )", "( 44)", "(44 )",
    "(22 44 (1) 33 (4 5 (6) 6))", "hello",
    "-", "+", "(+ 1 ;hi
        3)", "'13", "hello-world", "_",
    ] {
        assert!(g.parse(test).is_ok());
    }
    assert!(g.parse("((22)").is_err());
}
fn eval_test<T: Into<Form>>(gram: &grammar::TermParser, e: &Rc<Form>, code: &str, expected: T) {
    assert_eq!(*eval(Rc::clone(e), Rc::new(gram.parse(code).unwrap())), expected.into());
}

#[test]
fn basic_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, "(+ 2 (car (cons 4 '(1 2))))", 6);
    eval_test(&g, &e, "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))", true);
    eval_test(&g, &e, "(if (= 2 2) (+ 1 2) (+ 3 4))", 3);
    eval_test(&g, &e, "(quote a)", "a");
    eval_test(&g, &e, "'a", "a");
    eval_test(&g, &e, "'(1 . a)", (1, "a"));
    eval_test(&g, &e, "'(1 a)", (1, ("a", Form::Nil)));
    eval_test(&g, &e, "true", true);
    eval_test(&g, &e, "false", false);
    eval_test(&g, &e, "nil", Form::Nil);

    eval_test(&g, &e, "(+ 1 2)",  3);
    eval_test(&g, &e, "(- 1 2)", -1);
    eval_test(&g, &e, "(* 1 2)",  2);
    eval_test(&g, &e, "(/ 4 2)",  2);
    eval_test(&g, &e, "(% 3 2)",  1);
    eval_test(&g, &e, "(& 3 2)",  2);
    eval_test(&g, &e, "(| 2 1)",  3);
    eval_test(&g, &e, "(^ 2 1)",  3);
    eval_test(&g, &e, "(^ 3 1)",  2);

    eval_test(&g, &e, "(comb? +)", true);
    eval_test(&g, &e, "(comb? (vau d p 1))", true);
    eval_test(&g, &e, "(comb? 1)", false);
    eval_test(&g, &e, "(pair? '(a))", true);
    //eval_test(&g, &e, "(pair? '())",  true);
    eval_test(&g, &e, "(nil? nil)", true);
    eval_test(&g, &e, "(nil? 1)", false);
    eval_test(&g, &e, "(pair? 1)", false);
    eval_test(&g, &e, "(symbol? 'a)", true);
    eval_test(&g, &e, "(symbol? 1)", false);
    eval_test(&g, &e, "(int? 1)", true);
    eval_test(&g, &e, "(int? true)", false);
    eval_test(&g, &e, "(bool? true)", true);
    eval_test(&g, &e, "(bool? 1)", false);

    eval_test(&g, &e, "!(bool?) 1",    false);
    eval_test(&g, &e, "!(bool?) true", true);

    eval_test(&g, &e, "((vau root_env _ (eval 'a (cons (cons 'a 2) root_env))))", 2);
}


use once_cell::sync::Lazy;
static LET: Lazy<String> = Lazy::new(|| {
    "!((vau root_env p (eval (car p)
      (cons (cons 'let1
     (vau de p (eval (car (cdr (cdr p))) (cons (cons (car p) (eval (car (cdr p)) de)) de)))
    ) root_env))))".to_owned()
});

#[test]
fn fib_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (let1 x 10 (+ x 7))", *LET), 17);
    let def_fib = "
    !(let1 fib (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 0)
       !(if (= 1 n) 1)
       (+ (self self (- n 1)) (self self (- n 2)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fib fib 6)", *LET, def_fib), 8);
}
#[test]
fn fact_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    let def_fact = "
    !(let1 fact (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 1)
       (* n (self self (- n 1)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fact fact 6)", *LET, def_fact), 720);
}
static VAPPLY: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vapply (vau de p
       !(let1 f    (eval (car p) de))
       !(let1 ip   (eval (car (cdr p)) de))
       !(let1 nde  (eval (car (cdr (cdr p))) de))
       (eval (cons f ip) nde)
    ))", *LET)
});
#[test]
fn vapply_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // need the vapply to keep env in check because otherwise the env keeps growing
    // and the Rc::drop will overflow the stack lol
    let def_badid = format!("
    {}
    !(let1 badid (vau de p
       !(let1 inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 n      (car (cdr ip)))
           !(let1 acc    (car (cdr (cdr ip))))
           !(if (= 0 n) acc)
           (vapply self (cons self (cons (- n 1) (cons (+ acc 1) nil))) de)
       ))
       (vapply inner (cons inner (cons (eval (car p) de) (cons 0 nil))) de)
    ))", *VAPPLY);
    // Won't work unless tail calls work
    eval_test(&g, &e, &format!("{} (badid 1000)", def_badid), 1000);
}

static VMAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vmap (vau de p
       !(let1 vmap_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 f      (car (cdr ip)))
           !(let1 l      (car (cdr (cdr ip))))
           !(if (= nil l) l)
           (cons (vapply f (cons (car l) nil) de) (vapply self (cons self (cons f (cons (cdr l) nil))) de))
       ))
       (vapply vmap_inner (cons vmap_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *VAPPLY)
});
#[test]
fn vmap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Maybe define in terms of a right fold?
    eval_test(&g, &e, &format!("{} (vmap (vau de p (+ 1 (car p))) '(1 2 3))", *VMAP), (2, (3, (4, Form::Nil))));
}

static WRAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 wrap (vau de p
       !(let1 f (eval (car p) de))
       (vau ide p (vapply f (vmap (vau _ xp (eval (car xp) ide)) p) ide))
    ))", *VMAP)
});
#[test]
fn wrap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Make sure (wrap (vau ...)) and internal style are optimized the same
    eval_test(&g, &e, &format!("{} ((wrap (vau _ p (+ (car p) 1))) (+ 1 2))", *WRAP), 4);
}

static UNWRAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 unwrap (vau de p
       !(let1 f (eval (car p) de))
       (vau ide p (vapply f (vmap (vau _ xp (cons quote (cons (car xp) nil))) p) ide))
    ))", *WRAP)
});
#[test]
fn unwrap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Can't represent prims in tests :( - they do work though, uncommenting and checking the
    // failed assert verifies
    //eval_test(&g, &e, &format!("{} ((unwrap (vau de p             (car p)))         (+ 1 2))", def_unwrap), ("quote", (("+", (1, (2, Form::Nil))), Form::Nil)));
    //eval_test(&g, &e, &format!("{} ((unwrap (vau de p       (eval (car p) de)))     (+ 1 2))", def_unwrap), (("+", (1, (2, Form::Nil))), Form::Nil));
    eval_test(&g, &e, &format!("{} ((unwrap (vau de p (eval (eval (car p) de) de))) (+ 1 2))",       *UNWRAP), 3);
    eval_test(&g, &e, &format!("{} ((unwrap (vau de p (+ (eval (eval (car p) de) de) 1))) (+ 1 2))", *UNWRAP), 4);
}

static LAPPLY: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 lapply (vau de p
       !(let1 f    (eval (car p) de))
       !(let1 ip   (eval (car (cdr p)) de))
       !(let1 nde  (eval (car (cdr (cdr p))) de))
       (eval (cons (unwrap f) ip) nde)
    ))", *UNWRAP)
});
#[test]
fn lapply_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Should this allow envs at all? It technically can, but I feel like it kinda goes against the
    // sensible deriviation
    let def_lbadid = format!("
    {}
    !(let1 lbadid (vau de p
       !(let1 inner (wrap (vau ide ip
           !(let1 self   (car ip))
           !(let1 n      (car (cdr ip)))
           !(let1 acc    (car (cdr (cdr ip))))
           !(if (= 0 n) acc)
           (lapply self (cons self (cons (- n 1) (cons (+ acc 1) nil))) de)
       )))
       (lapply inner (cons inner (cons (eval (car p) de) (cons 0 nil))) de)
    ))", *LAPPLY);
    // Won't work unless tail calls work
    // takes a while though
    eval_test(&g, &e, &format!("{} (lbadid 1000)", def_lbadid), 1000);
}

static VFOLDL: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vfoldl (vau de p
       !(let1 vfoldl_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 f      (car (cdr ip)))
           !(let1 a      (car (cdr (cdr ip))))
           !(let1 l      (car (cdr (cdr (cdr ip)))))
           !(if (= nil l) a)
           (vapply self (cons self (cons f (cons (vapply f (cons a (cons (car l) nil)) de) (cons (cdr l) nil)))) de)
       ))
       (vapply vfoldl_inner (cons vfoldl_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) (cons (eval (car (cdr (cdr p))) de) nil)))) de)
    ))", *LAPPLY)
});
#[test]
fn vfoldl_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (vfoldl (vau de p (+ (car p) (car (cdr p)))) 0 '(1 2 3))", *VFOLDL), 6);
}
static ZIPD: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 zipd (vau de p
       !(let1 zipd_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 a      (car (cdr ip)))
           !(let1 b      (car (cdr (cdr ip))))
           !(if (= nil a) a)
           !(if (= nil b) b)
           (cons (cons (car a) (car b)) (vapply self (cons self (cons (cdr a) (cons (cdr b) nil))) de))
       ))
       (vapply zipd_inner (cons zipd_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *VFOLDL)
});
#[test]
fn zipd_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (zipd '(1 2 3) '(4 5 6))", *ZIPD), ((1,4), ((2,5), ((3,6), Form::Nil))));
}
static CONCAT: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 concat (vau de p
       !(let1 concat_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 a      (car (cdr ip)))
           !(let1 b      (car (cdr (cdr ip))))
           !(if (= nil a) b)
           (cons (car a) (vapply self (cons self (cons (cdr a) (cons b nil))) de))
       ))
       (vapply concat_inner (cons concat_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *ZIPD)
});

#[test]
fn concat_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (concat '(1 2 3) '(4 5 6))", *CONCAT), (1, (2, (3, (4, (5, (6, Form::Nil)))))));
}

static BVAU: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 match_params (wrap (vau 0 p
         !(let1 self (car                p))
         !(let1 p_ls (car (cdr           p)))
         !(let1 dp   (car (cdr (cdr      p))))
         !(let1 e    (car (cdr (cdr (cdr p)))))
         !(if (= nil   p_ls) (assert (= nil dp) e))
         !(if (symbol? p_ls) (cons (cons p_ls dp) e))
         (self self (cdr p_ls) (cdr dp) (self self (car p_ls) (car dp) e))
    )))
    !(let1 bvau (vau se p
       (if (= nil (cdr (cdr p)))
           ; No de case
           !(let1 p_ls (car p))
           !(let1 b_v  (car (cdr p)))
           (vau 0 dp
               (eval b_v (match_params match_params p_ls dp se))
           )

           ; de case
           !(let1 de_s (car p))
           !(let1 p_ls (car (cdr p)))
           !(let1 b_v  (car (cdr (cdr p))))
           (vau dde dp
               (eval b_v (match_params match_params p_ls dp (cons (cons de_s dde) se)))
           )
       )
    ))", *CONCAT)
});
#[test]
fn bvau_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((bvau _ (a b c) (+ a (- b c))) 10 2 3)", *BVAU), 9);
    eval_test(&g, &e, &format!("{} ((bvau (a b c) (+ a (- b c))) 10 2 3)", *BVAU), 9);

    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2 3)", *BVAU), (3, Form::Nil));
    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2)", *BVAU), Form::Nil);
    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau c c) 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau c c))", *BVAU), Form::Nil);
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) c) (10 2) 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) a) (10 2) 3 4 5)", *BVAU), 10);
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) b) (10 2) 3 4 5)", *BVAU), 2);

    eval_test(&g, &e, &format!("{} ((wrap (bvau _ (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", *BVAU), 7);
    eval_test(&g, &e, &format!("{} ((wrap (bvau (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", *BVAU), 7);
}

static LAMBDA: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 lambda (vau de p
       (wrap (vapply bvau p de))
    ))", *BVAU)
});
#[test]
fn lambda_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((lambda (a b c) (+ a (- b c))) (+ 10 1) (+ 2 2) (+ 5 3))", *LAMBDA), 7);

    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2 3)", *LAMBDA), (3, Form::Nil));
    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2)", *LAMBDA), Form::Nil);
    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda c c) 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda c c))", *LAMBDA), Form::Nil);
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) c) '(10 2) 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) a) '(10 2) 3 4 5)", *LAMBDA), 10);
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) b) '(10 2) 3 4 5)", *LAMBDA), 2);
    eval_test(&g, &e, &format!("{} ((lambda ((a b . c)  d) b) '(10 2 3 4) 3)", *LAMBDA), 2);
    eval_test(&g, &e, &format!("{} ((lambda ((a b . c)  d) c) '(10 2 3 4) 3)", *LAMBDA), (3, (4, Form::Nil)));
    // should fail
    //eval_test(&g, &e, &format!("{} ((lambda (a b c) c) 10 2 3 4)", *LAMBDA), 3);
}

static LET2: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 let1 (bvau dp (s v b)
        (eval b (match_params match_params s (eval v dp) dp))
    ))
    ", *LAMBDA)
});

#[test]
fn let2_eval_test() { let g = grammar::TermParser::new(); let e = root_env();

    eval_test(&g, &e, &format!("{} (let1 x               (+ 10 1) (+ x 1))", *LET2), 12);
    eval_test(&g, &e, &format!("{} (let1 x               '(10 1) x)", *LET2), (10, (1, Form::Nil)));
    eval_test(&g, &e, &format!("{} (let1 (a b)           '(10 1) a)", *LET2), 10);
    eval_test(&g, &e, &format!("{} (let1 (a b)           '(10 1) b)", *LET2),  1);
    eval_test(&g, &e, &format!("{} (let1 (a b . c)       '(10 1) c)", *LET2),  Form::Nil);
    eval_test(&g, &e, &format!("{} (let1 (a b . c)       '(10 1 2 3) c)", *LET2),  (2, (3, Form::Nil)));
    eval_test(&g, &e, &format!("{} (let1 ((a . b) . c)   '((10 1) 2 3) a)", *LET2),  10);
    eval_test(&g, &e, &format!("{} (let1 ((a . b) . c)   '((10 1) 2 3) b)", *LET2),  (1, Form::Nil));
    // should fail
    //eval_test(&g, &e, &format!("{} (let1 (a b c) '(10 2 3 4) a)", *LET2), 10);
}

static LIST: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 list (lambda args args))
    ", *LET2)
});

#[test]
fn list_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (list 1 2 (+ 3 4))", *LIST), (1, (2, (7, Form::Nil))));
}

static Y: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 Y (lambda (f3)
        ((lambda (x1) (x1 x1))
         (lambda (x2) (f3 (wrap (vau app_env y (lapply (x2 x2) y app_env)))))))
    )
    ", *LIST)
});

#[test]
fn y_eval_test() { let g = grammar::TermParser::new(); let e = root_env();

    eval_test(&g, &e, &format!("{} ((Y (lambda (recurse) (lambda (n) (if (= 0 n) 1 (* n (recurse (- n 1))))))) 5)", *Y), 120);

}

static RLAMBDA: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 rlambda (bvau se (n p b)
        (eval (list Y (list lambda (list n) (list lambda p b))) se)
    ))
    ", *Y)
});

#[test]
fn rlambda_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((rlambda recurse (n) (if (= 0 n) 1 (* n (recurse (- n 1))))) 5)", *RLAMBDA), 120);
}
static AND_OR: Lazy<String> = Lazy::new(|| {
    // need to extend for varidac
    format!("
    {}
    !(let1 and (bvau se (a b)
        !(let1 ae (eval a se))
        (if ae (eval b se) ae)
    ))
    !(let1 or (bvau se (a b)
        !(let1 ae (eval a se))
        (if ae ae (eval b se))
    ))
    ", *RLAMBDA)
});

#[test]
fn and_or_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (and true  true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (and false true)",  *AND_OR), false);
    eval_test(&g, &e, &format!("{} (and true  false)", *AND_OR), false);
    eval_test(&g, &e, &format!("{} (and false false)", *AND_OR), false);

    eval_test(&g, &e, &format!("{} (or true  true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or false true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or true  false)", *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or false false)", *AND_OR), false);
}
static MATCH: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 match (vau de (x . cases)
        !(let1 evaluate_case (rlambda evaluate_case (access c)
                   !(if (symbol? c)                                            (array true                       (lambda (b) (array let (array c access) b))))
                   !(if (and (array? c) (= 2 (len c)) (= 'unquote (idx c 0)))  (array (array = access (idx c 1)) (lambda (b) b)))
                   !(if (and (array? c) (= 2 (len c)) (= 'quote   (idx c 0)))  (array (array = access c)         (lambda (b) b)))
                   !(if (array?  c)
                             !(let1 tests             (array and (array array? access) (array = (len c) (array len access))))
                             !(let1 (tests body_func) ((lambda recurse (tests body_func i) (if (= i (len c))
                                                                                                     (array tests body_func)
                                                                                                     ; else
                                                                                                     !(let1 (inner_test inner_body_func) (evaluate_case (array idx access i) (idx c i)) )
                                                                                                     (recurse (concat tests (array inner_test))
                                                                                                              (lambda (b) (body_func (inner_body_func b)))
                                                                                                              (+ i 1))))
                                                            tests (lambda (b) b) 0))
                             (array tests body_func))
                   (array (array = access c)         (lambda (b) b))
        ))
        !(let1 helper (rlambda helper (x_sym cases i) (if (< i (- (len cases) 1))  (let1 (test body_func) (evaluate_case x_sym (idx cases i))
                                                                                             (concat (array test (body_func (idx cases (+ i 1)))) (helper x_sym cases (+ i 2))))

                                                                                   (array true (array assert false)))))

        (eval (array let (array '___MATCH_SYM x) (concat (array cond) (helper '___MATCH_SYM cases 0))) de)
    ))
    ", *AND_OR)
});
//#[test]
//fn match_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
//    eval_test(&g, &e, &format!("{} (match (+ 1 2) 1 2 2 3 3 4 true 0)", *MATCH), 120);
//}

