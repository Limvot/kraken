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
        match *cur {
            Form::Symbol(ref s) => {
                let mut t = e;
                while s != t.car().unwrap().car().unwrap().sym().unwrap() {
                    t = t.cdr().unwrap();
                }
                return t.car().unwrap().cdr().unwrap();
            },
            Form::Pair(ref c, ref p) => {
                let comb = eval(Rc::clone(&e), Rc::clone(c));
                match *comb {
                    Form::PrimComb(ref n, ref f) => return f(e, Rc::clone(p)),
                    _ => panic!("Tried to call not a PrimComb {:?}", comb),
                }
            },
            _ => return cur,
        }
    }
}

fn main() {
    fn assoc(kvs: Vec<(&str, Rc<Form>)>) -> Rc<Form> {
        let mut to_ret = Rc::new(Form::Nil);
        for (k, v) in kvs {
            to_ret = Rc::new(Form::Pair(
                        Rc::new(Form::Pair(
                                Rc::new(Form::Symbol(k.to_owned())),
                                v)),
                        to_ret));
        }
        to_ret
    }
    let env = assoc(vec![
        ("+", Rc::new(Form::PrimComb("+".to_owned(), |e, p| {
            Rc::new(Form::Int(p.car().unwrap().int().unwrap() + p.cdr().unwrap().car().unwrap().int().unwrap()))
        })))
    ]);
    let input = "(+ 2 2)";
    let result = eval(env, Rc::new(grammar::TermParser::new().parse(input).unwrap()));
    println!("Result is {:?}", result);
}
