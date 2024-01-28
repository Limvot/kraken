#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use anyhow::Result;
use std::cell::Cell;
//use std::rc::Rc;

use sl::eval;

use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::mem::ManuallyDrop;

use std::fmt;

#[repr(C)]
pub struct Rc<T> {
    ptr: NonNull<RcInner<T>>,
    phantom: PhantomData<RcInner<T>>
}
#[repr(C)]
pub struct RcInner<T> {
    rc: Cell<usize>,
    data: T,
}
impl<T> Rc<T> {
    pub fn new(data: T) -> Rc<T> {
        let boxed = Box::new(RcInner { rc: Cell::new(1), data });
        Rc {
            ptr: NonNull::new(Box::into_raw(boxed)).unwrap(),
            phantom: PhantomData,
        }
    }
    pub fn into_ptr(self) -> *mut RcInner<T> {
        ManuallyDrop::new(self).ptr.as_ptr() as *mut RcInner<T>
    }
    pub fn from_ptr(ptr: *mut RcInner<T>) -> Self {
        Rc {
            ptr: NonNull::new(ptr).unwrap(),
            phantom: PhantomData,
        }
    }
}
unsafe impl<T: Sync+Send> Send for Rc<T> {}
unsafe impl<T: Sync+Send> Sync for Rc<T> {}
impl<T> Deref for Rc<T> {
    type Target = T;
    fn deref(&self) -> &T {
        let inner = unsafe { self.ptr.as_ref() };
        &inner.data
    }
}
impl<T> Clone for Rc<T> {
    fn clone(&self) -> Rc<T> {
        let inner = unsafe { self.ptr.as_ref() };
        let old = inner.rc.get();
        inner.rc.set(old + 1);
        if old > isize::MAX as usize {
            std::process::abort();
        }
        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}
impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        let inner = unsafe { self.ptr.as_mut() };
        let old = inner.rc.get();
        inner.rc.set(old - 1);
        if old != 1 {
            return;
        }
        unsafe { drop(Box::from_raw(self.ptr.as_ptr())); }
    }
}


/*
pub enum Form {
    Nil,
    Int(i32),
    Bool(bool),
    Symbol(String),
    Pair(Rc<Form>, Rc<Form>),
    Closure(Vec<String>, Rc<Form>, Rc<Form>, ID),
    Prim(Prim),
}
*/
#[repr(C)]
struct Form {
    data: *const Form,
    phantom: PhantomData<Form>
}
#[repr(C)]
struct FormPair {
    car: Form,
    cdr: Form,
}
/*
 * this better be a 64 bit platform
 * huh, if we only support i32s, then we have a lot more room for tags
 * 8 byte alignment gets us 3 bits, or uh 8 options
 * we'll choose 000 for ints to make math easy
 *
 *        000 - Int
 *        001 - Nil
 *        010 - Bool(false) // this is needlessly wasteful of the bits but hay
 *        011 - Bool(true)
 *        100 - Symbol - will want to convert into an Rc around a StringRawParts struct
 *        101 - Pair   - an Rc around a Pair struct
 *        110 - Closure- eek: Closure(Vec<String>, Rc<Form>, Rc<Form>, ID),
 * xxxx   111 - Prim (xxxx for which one)
 *
 * I don't actually think we need our own repr(C) Vec implementation, at least not for now - we can
 * make do with a VecRawParts struct (without implementations)
 *
 * in both cases, StringRawParts and VecRawParts, we can rebuild slices from the raw parts for
 * read-only access, which is all we need (until Drop, at which point we should re-constitute them
 * from their raw parts, which is stable)
 */
impl Form {
    fn new_int(x: isize) -> Self {
        Self { data: (x << 3) as *const Form, phantom: PhantomData }
    }
    fn new_nil() -> Self {
        Self { data: 0b001 as *const Form, phantom: PhantomData }
    }
    fn new_bool(b: bool) -> Self {
        Self { data: (if b { 0b011 } else { 0b010 }) as *const Form, phantom: PhantomData }
    }
    fn new_pair(car: Form, cdr: Form) -> Self {
        let p = Rc::new(FormPair { car, cdr }).into_ptr() as usize;
        assert!(p & 0b111 == 0);
        Self { data: (p | 0b101) as *const Form, phantom: PhantomData }
    }
}
impl Drop for Form {
    fn drop(&mut self) {
        match self.data as usize & 0b111 {
            0b000 | 0b001 | 0b010 | 0b011 => { println!("dropping simple {self}"); }, // int, nil, false, true
            0b101 => {
                // pair
                let _ = Rc::<FormPair>::from_ptr( (self.data as usize & !0b111) as *mut RcInner<FormPair> );
            },
            _ => unreachable!(),
        }
    }
}
impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.data as usize & 0b111 {
            0b000 => {
                write!(f, "{}", self.data as isize >> 3)?;
            },
            0b001 => {
                write!(f, "nil")?;
            },
            0b010 => {
                write!(f, "false")?;
            },
            0b011 => {
                write!(f, "true")?;
            },
            0b101 => {
                write!(f, "pair")?;
            },
            _ => unreachable!(),
        }
        Ok(())
    }
}



fn alias(a: Rc<u64>, b: Rc<u64>) {
    println!("a: {}, b: {}", *a, *b);
}

fn main() -> Result<()> {
    // our Form shennigins will only work on 64 bit platforms
    assert!(std::mem::size_of::<usize>() == 8);

    let x = Rc::new(1);
    alias(Rc::clone(&x), x);
    let rc_u64_size = std::mem::size_of::<Rc<u64>>();
    assert!(rc_u64_size == 8);
    println!("for our Rc, we have size {}", rc_u64_size);

    let i  = Form::new_int(23);
    let n  = Form::new_nil();
    let bf = Form::new_bool(false);
    let bt = Form::new_bool(true);

    let p  = Form::new_pair(Form::new_int(50), Form::new_nil());
    println!("{i} {n} {bf} {bt} {p}");


    /*
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
    let parsed_input = Rc::new(grammar::TermParser::new().parse(input)?);
    //println!("Hello, world: {parsed_input:?}");
    println!("Hello, world: {parsed_input}");
    let evaled = eval(Rc::clone(&parsed_input))?;
    println!("evaled: {evaled}");
    */
    Ok(())
}
