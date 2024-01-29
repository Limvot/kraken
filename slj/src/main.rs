#[macro_use] extern crate lalrpop_util;
lalrpop_mod!(pub grammar);

use std::fmt;
use std::collections::BTreeMap;
use std::sync::Mutex;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::{self, NonNull};
use std::mem::{self, ManuallyDrop};
use std::alloc::{self, Layout};
use std::cell::Cell;
//use std::rc::Rc;

use once_cell::sync::Lazy;
use anyhow::Result;

use sl::eval;

#[repr(C)]
pub struct Vec<T> {
    ptr: NonNull<T>,
    cap: usize,
    len: usize,
}
unsafe impl<T: Send> Send for Vec<T> {}
unsafe impl<T: Sync> Sync for Vec<T> {}
impl<T> Vec<T> {
    pub fn new() -> Self {
        assert!(mem::size_of::<T>() != 0, "no ZST");
        Vec {
            ptr: NonNull::dangling(),
            len: 0,
            cap: 0,
        }
    }
    fn grow(&mut self) {
        let (new_cap, new_layout) = if self.cap == 0 {
            (1, Layout::array::<T>(1).unwrap())
        } else {
            let new_cap = 2 * self.cap;
            let new_layout = Layout::array::<T>(new_cap).unwrap();
            (new_cap, new_layout)
        };
        assert!(new_layout.size() <= isize::MAX as usize, "allocation too large");
        let new_ptr = if self.cap == 0 {
            unsafe { alloc::alloc(new_layout) }
        } else {
            let old_layout = Layout::array::<T>(self.cap).unwrap();
            let old_ptr = self.ptr.as_ptr() as *mut u8;
            unsafe { alloc::realloc(old_ptr, old_layout, new_layout.size()) }
        };
        self.ptr = match NonNull::new(new_ptr as *mut T) {
            Some(p) => p,
            None => alloc::handle_alloc_error(new_layout),
        };
        self.cap = new_cap;
    }
    pub fn push(&mut self, elem: T) {
        if self.len == self.cap { self.grow(); }
        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.len), elem);
        }
        self.len += 1;
    }
    pub fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            None
        } else {
            self.len -= 1;
            unsafe {
                Some(ptr::read(self.ptr.as_ptr().add(self.len)))
            }
        }
    }
}
impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        if self.cap != 0 {
            while let Some(_) = self.pop() {}
            let layout = Layout::array::<T>(self.cap).unwrap();
            unsafe {
                alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}
impl<T> Deref for Vec<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        unsafe {
            std::slice::from_raw_parts(self.ptr.as_ptr(), self.len)
        }
    }
}
// insert, remove, into_iter, and drain all missing
impl<T: fmt::Display> fmt::Display for Vec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for x in self.iter() {
            write!(f, " {}", x)?;
        }
        write!(f, " ]")?;
        Ok(())
    }
}


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

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
#[repr(transparent)]
pub struct ID {
    id: i64
}
impl fmt::Display for ID {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[repr(C)]
struct Closure {
    params: Vec<String>,
    e:      Form,
    body:   Form,
    id:     ID,
}

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
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
#[repr(usize)]
pub enum Prim {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Cons,
    Car,
    Cdr,
}
impl Prim {
    fn two_params(self) -> bool {
        match self {
            Prim::Car | Prim::Cdr => false,
            _                     => true,
        }
    }
}
/*
 * this better be a 64 bit platform
 * huh, if we only support i32s, then we have a lot more room for tags
 * 8 byte alignment gets us 3 bits, or uh 8 options
 * we'll choose 000 for ints to make math easy
 *
 *        000 - Int
 *        001 - Nil
 *        010 - Bool(false) // this is needlessly wasteful of the bits but hay - should take one of them over as a String probs
 *        011 - Bool(true)
 *        100 - Symbol - will want to convert into an Rc around a StringRawParts struct
 *        101 - Pair   - an Rc around a Pair struct
 *        110 - Closure- eek: Closure(Vec<String>, Rc<Form>, Rc<Form>, ID),
 * xxxx   111 - Prim (xxxx for which one)
 *
 * I don't actually think we need our own repr(C) Vec implementation, at least not for now - we can
 * make do with a VecRawParts struct (without implementations)
 *  Hay I did it anyway
 *
 * in both cases, StringRawParts and VecRawParts, we can rebuild slices from the raw parts for
 * read-only access, which is all we need (until Drop, at which point we should re-constitute them
 * from their raw parts, which is stable)
 *
 * For symbols, it would actually make sense to create the String, then leak it so it lasts for the
 * program, then deduplicate to it and pass the static const slice around
 * Could even fit entirely in the Form if the max length of a symbol is 2^16
 */
const TAG_OFFSET:     usize = 3;

const SYM_LEN_OFFSET: usize = 3;
const SYM_LEN_MASK:   usize = 0xFF; // could be bigger
const SYM_PTR_OFFSET: usize = 11;

const TAG_MASK:       usize = 0b111;
const TAG_INT:        usize = 0b000;
const TAG_NIL:        usize = 0b001;
const TAG_BOOL_FALSE: usize = 0b010;
const TAG_BOOL_TRUE:  usize = 0b011;
const TAG_SYMBOL:     usize = 0b100;
const TAG_PAIR:       usize = 0b101;
const TAG_CLOSURE:    usize = 0b110;
const TAG_PRIM:       usize = 0b111;


static SYMBOLS: Lazy<Mutex<BTreeMap<String,&'static str>>> = Lazy::new(Mutex::default);

impl Form {
    fn new_int(x: isize) -> Self {
        Self { data: (x << TAG_OFFSET) as *const Form, phantom: PhantomData }
    }
    fn new_nil() -> Self {
        Self { data: TAG_NIL as *const Form, phantom: PhantomData }
    }
    fn new_bool(b: bool) -> Self {
        Self { data: (if b { TAG_BOOL_TRUE } else { TAG_BOOL_FALSE }) as *const Form, phantom: PhantomData }
    }
    fn new_pair(car: Form, cdr: Form) -> Self {
        let p = Rc::new(FormPair { car, cdr }).into_ptr() as usize;
        assert!(p & TAG_MASK == 0);
        Self { data: (p | TAG_PAIR) as *const Form, phantom: PhantomData }
    }
    fn new_closure(params: Vec<String>, e: Form, body: Form, id: ID) -> Self {
        let p = Rc::new(Closure { params, e, body, id }).into_ptr() as usize;
        assert!(p & TAG_MASK == 0);
        Self { data: (p | TAG_CLOSURE) as *const Form, phantom: PhantomData }
    }
    fn new_prim(p: Prim) -> Self {
        Self { data: (((p as usize) << TAG_OFFSET) | TAG_PRIM) as *const Form, phantom: PhantomData }
    }
    fn new_symbol(s: &str) -> Form {
        assert!(s.len() < SYM_LEN_MASK);
        let mut symbols = SYMBOLS.lock().unwrap();
        let ds = if let Some(ds) = symbols.get(s) {
            ds
        } else {
            // here we leak the memory of a new owned copy of s,
            // and then transmute it into an &'static str that we keep in our global
            // map for deduplication. Spicy stuff.
            let mut value = ManuallyDrop::new(s.to_owned());
            value.shrink_to_fit();
            let slice = unsafe { std::mem::transmute(value.as_str()) };
            symbols.insert(s.to_owned(), slice);
            slice
        };
        Self { data: (((ds.as_ptr() as usize) << SYM_PTR_OFFSET) | (ds.len() << SYM_LEN_OFFSET) | TAG_SYMBOL) as *const Form, phantom: PhantomData }
    }

    fn car(&self) -> &Form {
        assert!(self.data as usize & TAG_MASK == TAG_PAIR);
        unsafe { &(*((self.data as usize & !TAG_MASK) as *mut RcInner<FormPair>)).data.car }
    }
    fn cdr(&self) -> &Form {
        assert!(self.data as usize & TAG_MASK == TAG_PAIR);
        unsafe { &(*((self.data as usize & !TAG_MASK) as *mut RcInner<FormPair>)).data.cdr }
    }
    fn closure(&self) -> &Closure {
        assert!(self.data as usize & TAG_MASK == TAG_CLOSURE);
        unsafe { &(*((self.data as usize & !TAG_MASK) as *mut RcInner<Closure>)).data }
    }
    fn prim(&self) -> Prim {
        assert!(self.data as usize & TAG_MASK == TAG_PRIM);
        unsafe { *(&((self.data as usize) >> TAG_OFFSET) as *const usize as *const Prim) }
    }
    fn str(&self) -> &str {
        assert!(self.data as usize & TAG_MASK == TAG_SYMBOL);
        let len = ((self.data as usize) >> SYM_LEN_OFFSET) & SYM_LEN_OFFSET;
        let ptr = ((self.data as usize) >> SYM_PTR_OFFSET) as *const u8;
        std::str::from_utf8(unsafe { std::slice::from_raw_parts(ptr, len) }).unwrap()
    }
}
impl Drop for Form {
    fn drop(&mut self) {
        match self.data as usize & TAG_MASK {
            TAG_INT | TAG_NIL | TAG_BOOL_FALSE | TAG_BOOL_TRUE | TAG_PRIM | TAG_SYMBOL => { println!("dropping simple {self}"); }, // doing nothing for symbol is fine
                                                                                                                                   // since it's deduplicated
            TAG_PAIR => {
                let _ = Rc::<FormPair>::from_ptr( (self.data as usize & !TAG_MASK) as *mut RcInner<FormPair> );
            },
            TAG_CLOSURE => {
                let _ = Rc::<Closure>::from_ptr( (self.data as usize & !TAG_MASK) as *mut RcInner<Closure> );
            },
            _ => unreachable!(),
        }
    }
}
impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.data as usize & TAG_MASK {
            TAG_INT => {
                write!(f, "{}", self.data as isize >> 3)?;
            },
            TAG_NIL => {
                write!(f, "nil")?;
            },
            TAG_BOOL_FALSE => {
                write!(f, "false")?;
            },
            TAG_BOOL_TRUE => {
                write!(f, "true")?;
            },
            TAG_PAIR => {
                write!(f, "({} . {}", self.car(), self.cdr())?;
            },
            TAG_PRIM => {
                write!(f, "{:?}", self.prim())?;
            },
            TAG_SYMBOL => {
                write!(f, "{}", self.str())?;
            },
            TAG_CLOSURE => {
                let Closure { params, e, body, id, } = self.closure();
                write!(f, "<{params} {e} {body} {id}>")?;
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

    let pra  = Form::new_prim(Prim::Add);
    let pre  = Form::new_prim(Prim::Eq);

    let s = Form::new_symbol("woopwpp");


    let mut params = Vec::new();
    params.push("a".to_owned());
    params.push("b".to_owned());
    let c = Form::new_closure(params, Form::new_nil(), Form::new_nil(), ID { id: 9 });

    println!("{i} {n} {bf} {bt} {p} {pra} {pre} {s} {c}");

    let mut my_vec: Vec<Form> = Vec::new();
    my_vec.push(i);
    my_vec.push(n);
    my_vec.push(bf);
    my_vec.push(bt);
    my_vec.push(p);
    my_vec.push(pra);
    my_vec.push(pre);
    my_vec.push(s);
    my_vec.push(c);


    println!(" from vec {}", my_vec[3]);
    for i in my_vec.iter() {
        println!(" from vec {}", i);
    }
    println!("{my_vec}");



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
