#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::Form;

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

    eval_test(&g, &e, "((vau root_env _ (eval 'a (cons (cons 'a 2) root_env))))", 2);
    let LET = "
    ((vau root_env _ (eval '(let1 a 8 (+ a 9))
      (cons (cons 'let1

     (vau de p (eval (car (cdr (cdr p))) (cons (cons (car p) (eval (car (cdr p)) de)) de))) 
    ) root_env))))
    ";

    eval_test(&g, &e, LET, 17);

    // TODO, finish lambda
    let LAMBDA = "
    ((vau root_env _ (eval '((lambda x 2))
      (cons (cons 'lambda

        (vau de p  (eval (cons vau (cons '_ (cons (car p) (cons (car (cdr p)) nil)))) de))

      ) root_env))))
    ";

    eval_test(&g, &e, LAMBDA, 2);
}

fn eval(e: Rc<Form>, f: Rc<Form>) -> Rc<Form> {
    let mut e = e;
    let mut x = Option::Some(f);
    loop {
        let cur = x.take().unwrap();
        println!("Evaluating {:?} in {:?}", cur, e);
        match *cur {
            Form::Symbol(ref s) => {
                let mut t = e;
                println!("Looking up {} in {:?}", s, t);
                while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                    t = t.cdr().unwrap();
                }
                return t.car().unwrap().cdr().unwrap();
            },
            Form::Pair(ref c, ref p) => {
                let comb = eval(Rc::clone(&e), Rc::clone(c));
                match *comb {
                    Form::PrimComb(ref n,  ref f) => return f(e, Rc::clone(p)),
                    Form::DeriComb{ref se, ref de, ref params, ref body } => {
                        let mut new_e = Rc::clone(se);
                        if let Some(de) = de {
                            new_e = assoc(de, Rc::clone(&e), new_e);
                        }
                        if let Some(params) = params.sym() {
                            new_e = assoc(params, Rc::clone(p), new_e);
                        }
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
            println!("To get eval body, evaluating {:?} in {:?}", p.car(), e);
            let b = eval(Rc::clone(&e), p.car().unwrap());
            let e = if let Some(ne) = p.cdr().unwrap().car() {
                println!("To get eval env, evaluating {:?} in {:?}", ne, e);
                eval(e, ne)
            } else { e };
            println!("Evaling {:?} in {:?}", b, e);
            eval(e, b)
        }))),
        // (vau de params body)
        ("vau", Rc::new(Form::PrimComb("vau".to_owned(), |e, p| {
            let de     = p.car().unwrap().sym().map(|s| s.to_owned());
            let params = p.cdr().unwrap().car().unwrap();
            let body   = p.cdr().unwrap().cdr().unwrap().car().unwrap();

            Rc::new(Form::DeriComb { se: e, de, params, body })
        }))),
        ("=", Rc::new(Form::PrimComb("=".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap());
            let b = eval(e, p.cdr().unwrap().car().unwrap());
            Rc::new(Form::Bool(a == b))
        }))),
        ("if", Rc::new(Form::PrimComb("if".to_owned(), |e, p| {
            if eval(Rc::clone(&e), p.car().unwrap()).truthy() {
                eval(e, p.cdr().unwrap().car().unwrap())
            } else if let Some(els) = p.cdr().unwrap().cdr().and_then(|x| x.car()) {
                eval(e, els)
            } else {
                // should we really allow this? (2 arg if with no else)
                Rc::new(Form::Nil)
            }
        }))),
        ("cons", Rc::new(Form::PrimComb("cons".to_owned(), |e, p| {
            let h = eval(Rc::clone(&e), p.car().unwrap());
            let t = eval(e, p.cdr().unwrap().car().unwrap());
            Rc::new(Form::Pair(h, t))
        }))),
        ("car", Rc::new(Form::PrimComb("car".to_owned(), |e, p| {
            eval(Rc::clone(&e), p.car().unwrap()).car().unwrap()
        }))),
        ("cdr", Rc::new(Form::PrimComb("cdr".to_owned(), |e, p| {
            eval(Rc::clone(&e), p.car().unwrap()).cdr().unwrap()
        }))),
        ("quote", Rc::new(Form::PrimComb("quote".to_owned(), |e, p| {
            p.car().unwrap()
        }))),

        ("+", Rc::new(Form::PrimComb("+".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a + b))
        }))),
        ("-", Rc::new(Form::PrimComb("-".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a - b))
        }))),
        ("*", Rc::new(Form::PrimComb("*".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a * b))
        }))),
        ("/", Rc::new(Form::PrimComb("/".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a / b))
        }))),
        ("%", Rc::new(Form::PrimComb("%".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a % b))
        }))),
        ("&", Rc::new(Form::PrimComb("&".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a & b))
        }))),
        ("|", Rc::new(Form::PrimComb("|".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a | b))
        }))),
        ("^", Rc::new(Form::PrimComb("^".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a ^ b))
        }))),

        ("comb?", Rc::new(Form::PrimComb("comb?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::PrimComb(n, f)  => true,
                Form::DeriComb { .. } => true,
                _                     => false,
            }))
        }))),
        ("pair?", Rc::new(Form::PrimComb("pair?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Pair(_a,_b) => true,
                _                 => false,
            }))
        }))),
        ("symbol?", Rc::new(Form::PrimComb("symbol?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Symbol(_) => true,
                _               => false,
            }))
        }))),
        ("int?", Rc::new(Form::PrimComb("int?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Int(_) => true,
                _            => false,
            }))
        }))),
        // maybe bool? but also could be derived. Nil def
        ("bool?", Rc::new(Form::PrimComb("bool?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Bool(_) => true,
                _             => false,
            }))
        }))),
        ("nil?", Rc::new(Form::PrimComb("nil?".to_owned(), |e, p| {
            Rc::new(Form::Bool(match &*eval(e, p.car().unwrap()) {
                Form::Nil => true,
                _         => false,
            }))
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
