#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;
use anyhow::Result;

use sl::{tree_walker_eval, Env};

fn main() -> Result<()> {
    let input = "(if (= 1 2) (+ 2 3) (* 2 2))";
    let parsed_input = Rc::new(grammar::TermParser::new().parse(input)?);
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    let mut e = Env::root_env();
    let tree_walker_evaled = tree_walker_eval(Rc::clone(&parsed_input), &mut e)?;
    println!("tree walker evaled: {tree_walker_evaled}");
    Ok(())
}
