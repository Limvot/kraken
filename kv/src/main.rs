#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::{eval,root_env};

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
    //let opt_result = opt_eval(root, parsed_input);
    println!("Result is {} - {:?}", result, result);
    //println!("Opt Result is {} - {:?}", opt_result, opt_result);
    //assert!(opt_result.congruent(result));
}

