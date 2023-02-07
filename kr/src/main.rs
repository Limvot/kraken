#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::Form;

#[test]
fn parse_test() {
    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("(22)").is_ok());
    assert!(grammar::TermParser::new().parse("(((22)))").is_ok());

    assert!(grammar::TermParser::new().parse("((22)").is_err());

    assert!(grammar::TermParser::new().parse("22").is_ok());
    assert!(grammar::TermParser::new().parse("(22)").is_ok());
    assert!(grammar::TermParser::new().parse("(22 )").is_ok());
    assert!(grammar::TermParser::new().parse("()").is_ok());
    assert!(grammar::TermParser::new().parse("( )").is_ok());
    assert!(grammar::TermParser::new().parse("( 44)").is_ok());
    assert!(grammar::TermParser::new().parse("(44 )").is_ok());
    assert!(grammar::TermParser::new().parse("(22 44 (1) 33 (4 5 (6) 6))").is_ok());
    assert!(grammar::TermParser::new().parse("hello").is_ok());
    assert!(grammar::TermParser::new().parse("-").is_ok());
    assert!(grammar::TermParser::new().parse("+").is_ok());
    assert!(grammar::TermParser::new().parse("(+ 1 ;hi
        3)").is_ok());
    assert!(grammar::TermParser::new().parse("'13").is_ok());
    assert!(grammar::TermParser::new().parse("hello-world").is_ok());
    assert!(grammar::TermParser::new().parse("_").is_ok());
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

fn main() {
    let env = assoc_vec(vec![
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
        ("quote", Rc::new(Form::PrimComb("quote".to_owned(), |e, p| {
            p
        }))),
        ("+", Rc::new(Form::PrimComb("+".to_owned(), |e, p| {
            let a = eval(Rc::clone(&e), p.car().unwrap()).int().unwrap();
            let b = eval(e, p.cdr().unwrap().car().unwrap()).int().unwrap();
            Rc::new(Form::Int(a + b))
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
    ]);
    //let input = "(+ 2 (car (cons 4 '(1 2))))";
    let input = "((vau d p (+ (eval (car p) d) 13)) (+ 1 3))";
    let result = eval(env, Rc::new(grammar::TermParser::new().parse(input).unwrap()));
    println!("Result is {:?}", result);
}
