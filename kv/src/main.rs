#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod basic;
use crate::basic::{root_env,Form};
mod eval;
use crate::eval::{eval};
mod opt;
use crate::opt::{OptForm};

#[cfg(test)]
mod test;

fn main() {
    //let input = "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))";
    //let input = "(+ 1 3)";
    let input = "(= (+ 1 3) (* 2 2))";
    let parsed_input = grammar::TermParser::new().parse(input).unwrap();
    //println!("Parsed input is {} - {:?}", parsed_input, parsed_input);
    let root = root_env();
    let     result =     eval(Rc::clone(&root), Rc::clone(&parsed_input));
    let opt_root:  Rc<OptForm> = (&*root).into();
    let opt_input: Rc<OptForm> = (&*parsed_input).into();
    let opt_result =     eval(opt_root, opt_input);
    println!("Result is {} - {:?}", result, result);
    println!("Opt Result is {} - {:?}", opt_result, opt_result);
    assert!(opt_result.congruent(&*result));
}

