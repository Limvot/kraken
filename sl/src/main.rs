#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;
use anyhow::Result;

use sl::{tree_walker_eval, Env};

fn main() -> Result<()> {
    let input = "
    (begin
        (debug 1)
        (debug (= 1 2))
        (debug (+ 2 3))
        (define a (+ 1 (* 3 4)))
        (define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
        (debug 'gonna_fact_it)
        (debug fact)
        (debug (fact 5))
        (debug a)
        (define b (cons 1 (cons 2 (cons 3 nil))))
        (debug b)
        (debug (car b))
        (debug (cdr b))
        (if (= 1 2) (+ 2 3) (* 2 2))
    )
        ";
    let parsed_input = Rc::new(grammar::TermParser::new().parse(input)?);
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    let e = Env::root_env();
    let tree_walker_evaled = tree_walker_eval(Rc::clone(&parsed_input), e)?;
    println!("tree walker evaled: {tree_walker_evaled}");
    Ok(())
}
