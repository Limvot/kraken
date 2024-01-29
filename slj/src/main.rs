#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use anyhow::Result;

use sl::{eval,Form,Crc,Cvec,Prim,ID};

fn alias(a: Crc<u64>, b: Crc<u64>) {
    println!("a: {}, b: {}", *a, *b);
}

fn main() -> Result<()> {
    // our Form shennigins will only work on 64 bit platforms
    assert!(std::mem::size_of::<usize>() == 8);

    let x = Crc::new(1);
    alias(Crc::clone(&x), x);
    let rc_u64_size = std::mem::size_of::<Crc<u64>>();
    assert!(rc_u64_size == 8);
    println!("for our Crc, we have size {}", rc_u64_size);

    let begn = Form::new_symbol("begin");
    println!("this should be begin {begn}");

    let i  = Form::new_int(23);
    let n  = Form::new_nil();
    let bf = Form::new_bool(false);
    let bt = Form::new_bool(true);

    let p  = Form::new_pair(Form::new_int(50), Form::new_nil());

    let pra  = Form::new_prim(Prim::Add);
    let pre  = Form::new_prim(Prim::Eq);

    let s = Form::new_symbol("woopwpp");


    let mut params = Cvec::new();
    params.push("a".to_owned());
    params.push("b".to_owned());

    println!("{i} {n} {bf} {bt} {p} {pra} {pre} {s}");

    let mut my_vec: Cvec<Form> = Cvec::new();
    my_vec.push(i);
    my_vec.push(n);
    my_vec.push(bf);
    my_vec.push(bt);
    my_vec.push(p);
    my_vec.push(pra);
    my_vec.push(pre);
    my_vec.push(s);
    my_vec.push(begn);


    println!(" from vec {}", my_vec[3]);
    for i in my_vec.iter() {
        println!(" from vec {}", i);
    }
    println!("{my_vec}");



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





        (define faft_h (lambda (faft_h n) (if (= n 1) (debug 1) (+ n (faft_h faft_h (- n 1))))))
        (define faft (lambda (n) (faft_h faft_h n)))

        (debug 'gonna_faft_it)
        (debug faft)
        (debug (faft 6))
        (debug 'gonna_faft_it2)
        (debug (faft 10))
        ;(debug (faft 400))

        ;(define faft2 (lambda (n a) (if (= n 1) a (faft2 (- n 1) (+ n a)))))
        ;(debug 'gonna_faft2_it)
        ;(debug faft2)
        ;(debug (faft2 6 1))
        ;(debug (faft2 400 1))





        ;(define fib (lambda (n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2))))))
        ;(debug 'gonna_fib_it)
        ;(debug fib)
        ;(debug (fib 10))

        ;(debug a)
        ;(define b (cons 1 (cons 2 (cons 3 nil))))
        ;(debug b)
        ;(debug (car b))
        ;(debug (cdr b))
        ;(if (= 1 2) (+ 2 3) (* 2 2))
        (or false false )
    )
        ";
    let parsed_input = grammar::TermParser::new().parse(input)?;
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    println!("Yep that was all?");
    let evaled = eval(parsed_input.clone())?;
    println!("evaled: {evaled}");
    Ok(())
}
