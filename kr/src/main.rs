#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;

mod ast;
use crate::ast::{eval,root_env};
mod pe_ast;
use crate::pe_ast::{mark,partial_eval,new_base_ctxs};

mod test;


fn main() {
    let input = "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))";
    let parsed_input = Rc::new(grammar::TermParser::new().parse(input).unwrap());
    println!("Parsed input is {} - {:?}", parsed_input, parsed_input);
    let (bctx, dctx) = new_base_ctxs();
    let (bctx, marked) = mark(Rc::clone(&parsed_input),bctx);
    let unvaled = marked.unval().unwrap();
    println!("Parsed unvaled that is    {}", unvaled);
    let (bctx, ped) =  partial_eval(bctx, dctx, unvaled);
    let result = eval(root_env(), parsed_input);
    println!("Result is {} - {:?}", result, result);
}

