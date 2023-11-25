#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::rc::Rc;
use anyhow::Result;

use sl::eval;

fn main() -> Result<()> {
    let input = "
    (begin
        (debug 1)
        ;(debug (= 1 2))
        ;(debug (+ 2 3))
        ;(define a (+ 1 (* 3 4)))

        ;(define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
        ;(debug 'gonna_fact_it)
        ;(debug fact)
        ;(debug (fact 400))

        ;(define fact2 (lambda (n a) (if (= n 1) a (fact2 (- n 1) (* n a)))))
        ;(debug 'gonna_fact2_it)
        ;(debug fact2)
        ;(debug (fact2 400 1))





        ;(define faft (lambda (n) (if (= n 1) 1 (+ n (faft (- n 1))))))
        ;(debug 'gonna_faft_it)
        ;(debug faft)
        ;(debug (faft 400))

        (define faft2 (lambda (n a) (if (= n 1) a (faft2 (- n 1) (+ n a)))))
        (debug 'gonna_faft2_it)
        (debug faft2)
        (debug (faft2 400 1))





        ;(define fib (lambda (n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))))
        ;(debug 'gonna_fib_it)
        ;(debug fib)
        ;(debug (fib 10))

        ;(debug a)
        ;(define b (cons 1 (cons 2 (cons 3 nil))))
        ;(debug b)
        ;(debug (car b))
        ;(debug (cdr b))
        (if (= 1 2) (+ 2 3) (* 2 2))
    )
        ";
    let parsed_input = Rc::new(grammar::TermParser::new().parse(input)?);
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    let evaled = eval(Rc::clone(&parsed_input))?;
    println!("evaled: {evaled}");
    Ok(())
}
