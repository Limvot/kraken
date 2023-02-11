#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::{Form,PossibleTailCall};

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

    let e = root_env();

    fn eval_test<T: Into<Form>>(gram: &grammar::TermParser, e: &Rc<Form>, code: &str, expected: T) {
        assert_eq!(*eval(Rc::clone(e), Rc::new(gram.parse(code).unwrap())), expected.into());
    }

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
    let LET = "!((vau root_env p (eval (car p)
      (cons (cons 'let1
     (vau de p (eval (car (cdr (cdr p))) (cons (cons (car p) (eval (car (cdr p)) de)) de)))
    ) root_env))))";

    eval_test(&g, &e, &format!("{} (let1 x 10 (+ x 7))", LET), 17);
    let FIB = "
    !(let1 fib (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 0)
       !(if (= 1 n) 1)
       (+ (self self (- n 1)) (self self (- n 2)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fib fib 6)", LET, FIB), 8);

    let FACT = "
    !(let1 fact (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 1)
       (* n (self self (- n 1)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fact fact 6)", LET, FACT), 720);
    let VAPPLY = format!("
    {}
    !(let1 vapply (vau de p
       !(let1 f    (eval (car p) de))
       !(let1 ip   (eval (car (cdr p)) de))
       !(let1 nde  (eval (car (cdr (cdr p))) de))
       (eval (cons f ip) nde)
    ))", LET);

    // need the vapply to keep env in check because otherwise the env keeps growing
    // and the Rc::drop will overflow the stack lol
    let BADID = format!("
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
    ))", VAPPLY);
    // Won't work unless tail calls work
    eval_test(&g, &e, &format!("{} (badid 1000)", BADID), 1000);

    // Maybe define in terms of a right fold?
    let VMAP = format!("
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
    ))", VAPPLY);
    eval_test(&g, &e, &format!("{} (vmap (vau de p (+ 1 (car p))) '(1 2 3))", VMAP), (2, (3, (4, Form::Nil))));

    // Make sure (wrap (vau ...)) and internal style are optimized the same
    let WRAP = format!("
    {}
    !(let1 wrap (vau de p
       !(let1 f (eval (car p) de))
       (vau ide p (vapply f (vmap (vau _ xp (eval (car xp) ide)) p) ide))
    ))", VMAP);
    eval_test(&g, &e, &format!("{} ((wrap (vau _ p (+ (car p) 1))) (+ 1 2))", WRAP), 4);
    // TODO: unwrap

    let VFOLDL = format!("
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
    ))", WRAP);
    eval_test(&g, &e, &format!("{} (vfoldl (vau de p (+ (car p) (car (cdr p)))) 0 '(1 2 3))", VFOLDL), 6);

    let ZIPD = format!("
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
    ))", VFOLDL);
    eval_test(&g, &e, &format!("{} (zipd '(1 2 3) '(4 5 6))", ZIPD), ((1,4), ((2,5), ((3,6), Form::Nil))));

    let CONCAT = format!("
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
    ))", ZIPD);
    eval_test(&g, &e, &format!("{} (concat '(1 2 3) '(4 5 6))", CONCAT), (1, (2, (3, (4, (5, (6, Form::Nil)))))));

    // Should do some error checking, I suppose?
    // Also, extend for variadic (no zip, but back to fold I think)
    let BVAU = format!("
    {}
    !(let1 bvau (vau se p
       (if (= nil (cdr (cdr p)))
           ; No de case
           !(let1 p_ls (car p))
           !(let1 b_v  (car (cdr p)))
           (vau _ dp
               !(let1 zipped_env (concat (zipd p_ls dp) se))
               (eval b_v zipped_env)
           )

           ; de case
           !(let1 de_s (car p))
           !(let1 p_ls (car (cdr p)))
           !(let1 b_v  (car (cdr (cdr p))))
           (vau dde dp
               !(let1 dde_se (cons (cons de_s dde) se))
               !(let1 zipped_env (concat (zipd p_ls dp) dde_se))
               (eval b_v zipped_env)
           )
       )
    ))", CONCAT);
    eval_test(&g, &e, &format!("{} ((bvau _ (a b c) (+ a (- b c))) 10 2 3)", BVAU), 9);
    eval_test(&g, &e, &format!("{} ((bvau (a b c) (+ a (- b c))) 10 2 3)", BVAU), 9);
    eval_test(&g, &e, &format!("{} ((wrap (bvau _ (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", BVAU), 7);
    eval_test(&g, &e, &format!("{} ((wrap (bvau (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", BVAU), 7);

    let LAMBDA = format!("
    {}
    !(let1 lambda (vau de p
       (wrap (vapply bvau p de))
    ))", BVAU);
    eval_test(&g, &e, &format!("{} ((lambda (a b c) (+ a (- b c))) (+ 10 1) (+ 2 2) (+ 5 3))", LAMBDA), 7);
}

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
                    Form::PrimComb(ref n,  ref f) => match f(e, Rc::clone(p)) {
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
        ("quote", Rc::new(Form::PrimComb("quote".to_owned(), |e, p| {
            PossibleTailCall::Result(p.car().unwrap())
        }))),

        ("debug", Rc::new(Form::PrimComb("debug".to_owned(), |e, p| {
            println!("Debug: {:?}", eval(Rc::clone(&e), p.car().unwrap()));
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
                Form::PrimComb(n, f)  => true,
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
    //let input = "(+ 2 (car (cons 4 '(1 2))))";
    let input = "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))";
    //let input = "(if (= 2 2) (+ 1 2) (+ 3 4))";
    //let input = "(quote a)";
    //let input = "'a";
    let parsed_input = grammar::TermParser::new().parse(input).unwrap();
    println!("Parsed input is {:?}", parsed_input);
    let result = eval(root_env(), Rc::new(parsed_input));
    println!("Result is {:?}", result);
}
