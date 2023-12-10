use std::rc::Rc;

use crate::grammar;
use ki::{root_env,Form,eval};

#[test]
fn parse_test() {
    let g = grammar::TermParser::new();
    for test in [
    "22", "(22)", "(((22)))",
    "(22 )", "()", "( )", "( 44)", "(44 )",
    "(22 44 (1) 33 (4 5 (6) 6))", "hello",
    "-", "+", "(+ 1 ;hi
        3)", "'13", "hello-world", "_",
    ] {
        assert!(g.parse(test).is_ok());
    }
    assert!(g.parse("((22)").is_err());
}

fn eval_test<T: Into<Form>>(gram: &grammar::TermParser, e: &Rc<Form>, code: &str, expected: T) {
    println!("Doing test {}", code);
    let parsed = gram.parse(code).unwrap();
    let basic_result = eval(Rc::clone(e), Rc::clone(&parsed));
    assert_eq!(*basic_result, expected.into());
}

#[test]
fn basic_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, "(+ 2 (car (cons 4 '(1 2))))", 6);
    eval_test(&g, &e, "(= 17 ((vau d p (+ (eval (car p) d) 13)) (+ 1 3)))", true);
    eval_test(&g, &e, "(if (= 2 2) (+ 1 2) (+ 3 4))", 3);
    eval_test(&g, &e, "(quote a)", "a");
    eval_test(&g, &e, "'a", "a");
    eval_test(&g, &e, "'(1 . a)", (1, "a"));
    eval_test(&g, &e, "'(1 a)", (1, ("a", Form::Nil)));
    eval_test(&g, &e, "true", true);
    eval_test(&g, &e, "false", false);
    eval_test(&g, &e, "nil", Form::Nil);

    eval_test(&g, &e, "(+ 1 2)",  3);
    eval_test(&g, &e, "(- 1 2)", -1);
    eval_test(&g, &e, "(* 1 2)",  2);
    eval_test(&g, &e, "(/ 4 2)",  2);
    eval_test(&g, &e, "(% 3 2)",  1);
    eval_test(&g, &e, "(& 3 2)",  2);
    eval_test(&g, &e, "(| 2 1)",  3);
    eval_test(&g, &e, "(^ 2 1)",  3);
    eval_test(&g, &e, "(^ 3 1)",  2);

    eval_test(&g, &e, "(< 3 1)",  false);
    eval_test(&g, &e, "(<= 3 1)",  false);
    eval_test(&g, &e, "(> 3 1)",  true);
    eval_test(&g, &e, "(>= 3 1)",  true);

    eval_test(&g, &e, "(comb? +)", true);
    eval_test(&g, &e, "(comb? (vau d p 1))", true);
    eval_test(&g, &e, "(comb? 1)", false);
    eval_test(&g, &e, "(pair? '(a))", true);
    //eval_test(&g, &e, "(pair? '())",  true);
    eval_test(&g, &e, "(nil? nil)", true);
    eval_test(&g, &e, "(nil? 1)", false);
    eval_test(&g, &e, "(pair? 1)", false);
    eval_test(&g, &e, "(symbol? 'a)", true);
    eval_test(&g, &e, "(symbol? 1)", false);
    eval_test(&g, &e, "(int? 1)", true);
    eval_test(&g, &e, "(int? true)", false);
    eval_test(&g, &e, "(bool? true)", true);
    eval_test(&g, &e, "(bool? 1)", false);

    eval_test(&g, &e, "!(bool?) 1",    false);
    eval_test(&g, &e, "!(bool?) true", true);

    eval_test(&g, &e, "((vau root_env _ (eval 'a (cons (cons 'a 2) root_env))))", 2);
    eval_test(&g, &e, "'name-dash", "name-dash");

    eval_test(&g, &e, "(get (cell 1))", 1);
    eval_test(&g, &e, "(set (cell 1) 2)", 1);
    eval_test(&g, &e, "(cell? (cell 1))", true);
    eval_test(&g, &e, "((vau de p (eval (quote (cons (set a 2) (get a))) (cons (cons (quote a) (cell 1)) de))))", (1,2));

    eval_test(&g, &e, "(reset 1)", 1);
    eval_test(&g, &e, "(reset (+ 1 2))", 3);
    eval_test(&g, &e, "(reset (+ 1 (shift (vau de p 2))))", 2);
    eval_test(&g, &e, "(reset (+ 1 (shift (vau de p ((car p) 2)))))", 3);
    eval_test(&g, &e, "(reset (+ 1 (shift (vau de p ((car p) ((car p) 2)) ))))", 4);
    eval_test(&g, &e, "(reset (+ 1 (shift (vau de p (+ ((car p) 3) ((car p) ((car p) 2))) ))))", 8);
    eval_test(&g, &e, "((reset (+ 1 (shift (vau de p (car p))))) 2)", 3);
}


use once_cell::sync::Lazy;
static LET: Lazy<String> = Lazy::new(|| {
    "!((vau root_env p (eval (car p)
      (cons (cons 'let1
     (vau de p (eval (car (cdr (cdr p))) (cons (cons (car p) (eval (car (cdr p)) de)) de)))
    ) root_env))))".to_owned()
});


#[test]
fn fib_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (let1 x 10 (+ x 7))", *LET), 17);
    let def_fib = "
    !(let1 fib (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 0)
       !(if (= 1 n) 1)
       (+ (self self (- n 1)) (self self (- n 2)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fib fib 6)", *LET, def_fib), 8);
}
#[test]
fn fact_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    let def_fact = "
    !(let1 fact (vau de p
       !(let1 self (eval (car p) de))
       !(let1 n    (eval (car (cdr p)) de))
       !(if (= 0 n) 1)
       (* n (self self (- n 1)))
    ))";
    eval_test(&g, &e, &format!("{} {} (fact fact 6)", *LET, def_fact), 720);
}
static VAPPLY: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vapply (vau de p
       !(let1 f    (eval (car p) de))
       !(let1 ip   (eval (car (cdr p)) de))
       !(let1 nde  (eval (car (cdr (cdr p))) de))
       (eval (cons f ip) nde)
    ))", *LET)
});
#[test]
fn vapply_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // need the vapply to keep env in check because otherwise the env keeps growing
    // and the Rc::drop will overflow the stack lol
    let def_badid = format!("
    {}
    !(let1 badid (vau de p
       !(let1 inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 n      (car (cdr ip)))
           !(let1 acc    (car (cdr (cdr ip))))
           !(if (= 0 n) acc)
           (vapply self (cons self (cons (- n 1) (cons (+ acc 1) nil))) de)
       ))
       (vapply inner (cons inner (cons (eval (car p) de) (cons 0 nil))) de)
    ))", *VAPPLY);
    // Won't work unless tail calls work
    // so no PE?
    eval_test(&g, &e, &format!("{} (badid 1000)", def_badid), 1000);
}

static VMAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vmap (vau de p
       !(let1 vmap_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 f      (car (cdr ip)))
           !(let1 l      (car (cdr (cdr ip))))
           !(if (= nil l) l)
           (cons (vapply f (cons (car l) nil) de) (vapply self (cons self (cons f (cons (cdr l) nil))) de))
       ))
       (vapply vmap_inner (cons vmap_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *VAPPLY)
});
#[test]
fn vmap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Maybe define in terms of a right fold?
    //eval_test(&g, &e, &format!("{} (vmap (vau de p (+ 1 (car p))) '(1 2 3))", *VMAP), (2, (3, (4, Form::Nil))));
    eval_test(&g, &e, &format!("{} (vmap (vau de p (+ 1 (car p))) '(1))", *VMAP), (2, Form::Nil));
}

static WRAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 wrap (vau de p
       !(let1 f (eval (car p) de))
       (vau ide p (vapply f (vmap (vau _ xp (eval (car xp) ide)) p) ide))
    ))", *VMAP)
});
#[test]
fn wrap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Make sure (wrap (vau ...)) and internal style are optimized the same
    eval_test(&g, &e, &format!("{} ((wrap (vau _ p (+ (car p) 1))) (+ 1 2))", *WRAP), 4);
}

static UNWRAP: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 unwrap (vau de p
       !(let1 f (eval (car p) de))
       (vau ide p (vapply f (vmap (vau _ xp (cons quote (cons (car xp) nil))) p) ide))
    ))", *WRAP)
});
#[test]
fn unwrap_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Can't represent prims in tests :( - they do work though, uncommenting and checking the
    // failed assert verifies
    //eval_test(&g, &e, &format!("{} ((unwrap (vau de p             (car p)))         (+ 1 2))", def_unwrap), ("quote", (("+", (1, (2, Form::Nil))), Form::Nil)));
    //eval_test(&g, &e, &format!("{} ((unwrap (vau de p       (eval (car p) de)))     (+ 1 2))", def_unwrap), (("+", (1, (2, Form::Nil))), Form::Nil));
    eval_test(&g, &e, &format!("{} ((unwrap (vau de p (eval (eval (car p) de) de))) (+ 1 2))",       *UNWRAP), 3);
    eval_test(&g, &e, &format!("{} ((unwrap (vau de p (+ (eval (eval (car p) de) de) 1))) (+ 1 2))", *UNWRAP), 4);
}

static LAPPLY: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 lapply (vau de p
       !(let1 f    (eval (car p) de))
       !(let1 ip   (eval (car (cdr p)) de))
       !(let1 nde  (eval (car (cdr (cdr p))) de))
       (eval (cons (unwrap f) ip) nde)
    ))", *UNWRAP)
});
#[test]
fn lapply_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    // Should this allow envs at all? It technically can, but I feel like it kinda goes against the
    // sensible deriviation
    let def_lbadid = format!("
    {}
    !(let1 lbadid (vau de p
       !(let1 inner (wrap (vau ide ip
           !(let1 self   (car ip))
           !(let1 n      (car (cdr ip)))
           !(let1 acc    (car (cdr (cdr ip))))
           !(if (= 0 n) acc)
           (lapply self (cons self (cons (- n 1) (cons (+ acc 1) nil))) de)
       )))
       (lapply inner (cons inner (cons (eval (car p) de) (cons 0 nil))) de)
    ))", *LAPPLY);
    // Won't work unless tail calls work
    // takes a while though
    eval_test(&g, &e, &format!("{} (lbadid 1000)", def_lbadid), 1000);
}

static VFOLDL: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 vfoldl (vau de p
       !(let1 vfoldl_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 f      (car (cdr ip)))
           !(let1 a      (car (cdr (cdr ip))))
           !(let1 l      (car (cdr (cdr (cdr ip)))))
           !(if (= nil l) a)
           (vapply self (cons self (cons f (cons (vapply f (cons a (cons (car l) nil)) de) (cons (cdr l) nil)))) de)
       ))
       (vapply vfoldl_inner (cons vfoldl_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) (cons (eval (car (cdr (cdr p))) de) nil)))) de)
    ))", *LAPPLY)
});
#[test]
fn vfoldl_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (vfoldl (vau de p (+ (car p) (car (cdr p)))) 0 '(1 2 3))", *VFOLDL), 6);
}
static ZIPD: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 zipd (vau de p
       !(let1 zipd_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 a      (car (cdr ip)))
           !(let1 b      (car (cdr (cdr ip))))
           !(if (= nil a) a)
           !(if (= nil b) b)
           (cons (cons (car a) (car b)) (vapply self (cons self (cons (cdr a) (cons (cdr b) nil))) de))
       ))
       (vapply zipd_inner (cons zipd_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *VFOLDL)
});
#[test]
fn zipd_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (zipd '(1 2 3) '(4 5 6))", *ZIPD), ((1,4), ((2,5), ((3,6), Form::Nil))));
}
static CONCAT: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 concat (vau de p
       !(let1 concat_inner (vau ide ip
           !(let1 self   (car ip))
           !(let1 a      (car (cdr ip)))
           !(let1 b      (car (cdr (cdr ip))))
           !(if (= nil a) b)
           (cons (car a) (vapply self (cons self (cons (cdr a) (cons b nil))) de))
       ))
       (vapply concat_inner (cons concat_inner (cons (eval (car p) de) (cons (eval (car (cdr p)) de) nil))) de)
    ))", *ZIPD)
});

#[test]
fn concat_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (concat '(1 2 3) '(4 5 6))", *CONCAT), (1, (2, (3, (4, (5, (6, Form::Nil)))))));
}

static BVAU: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 match_params (wrap (vau 0 p
         !(let1 self (car                p))
         !(let1 p_ls (car (cdr           p)))
         !(let1 dp   (car (cdr (cdr      p))))
         !(let1 e    (car (cdr (cdr (cdr p)))))
         !(if (= nil   p_ls) (assert (= nil dp) e))
         !(if (symbol? p_ls) (cons (cons p_ls dp) e))
         (self self (cdr p_ls) (cdr dp) (self self (car p_ls) (car dp) e))
    )))
    !(let1 bvau (vau se p
       (if (= nil (cdr (cdr p)))
           ; No de case
           !(let1 p_ls (car p))
           !(let1 b_v  (car (cdr p)))
           (vau 0 dp
               (eval b_v (match_params match_params p_ls dp se))
           )

           ; de case
           !(let1 de_s (car p))
           !(let1 p_ls (car (cdr p)))
           !(let1 b_v  (car (cdr (cdr p))))
           (vau dde dp
               (eval b_v (match_params match_params p_ls dp (cons (cons de_s dde) se)))
           )
       )
    ))", *CONCAT)
});
#[test]
fn bvau_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((bvau _ (a b c) (+ a (- b c))) 10 2 3)", *BVAU), 9);
    eval_test(&g, &e, &format!("{} ((bvau (a b c) (+ a (- b c))) 10 2 3)", *BVAU), 9);

    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2 3)", *BVAU), (3, Form::Nil));
    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2)", *BVAU), Form::Nil);
    eval_test(&g, &e, &format!("{} ((bvau (a b . c) c) 10 2 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau c c) 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau c c))", *BVAU), Form::Nil);
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) c) (10 2) 3 4 5)", *BVAU), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) a) (10 2) 3 4 5)", *BVAU), 10);
    eval_test(&g, &e, &format!("{} ((bvau ((a b) . c) b) (10 2) 3 4 5)", *BVAU), 2);

    eval_test(&g, &e, &format!("{} ((wrap (bvau _ (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", *BVAU), 7);
    eval_test(&g, &e, &format!("{} ((wrap (bvau (a b c) (+ a (- b c)))) (+ 10 1) (+ 2 2) (+ 5 3))", *BVAU), 7);
}

static LAMBDA: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 lambda (vau de p
       (wrap (vapply bvau p de))
    ))", *BVAU)
});
#[test]
fn lambda_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((lambda (a b c) (+ a (- b c))) (+ 10 1) (+ 2 2) (+ 5 3))", *LAMBDA), 7);
    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2 3)", *LAMBDA), (3, Form::Nil));
    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2)", *LAMBDA), Form::Nil);
    eval_test(&g, &e, &format!("{} ((lambda (a b . c) c) 10 2 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda c c) 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda c c))", *LAMBDA), Form::Nil);
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) c) '(10 2) 3 4 5)", *LAMBDA), (3, (4, (5, Form::Nil))));
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) a) '(10 2) 3 4 5)", *LAMBDA), 10);
    eval_test(&g, &e, &format!("{} ((lambda ((a b) . c) b) '(10 2) 3 4 5)", *LAMBDA), 2);
    eval_test(&g, &e, &format!("{} ((lambda ((a b . c)  d) b) '(10 2 3 4) 3)", *LAMBDA), 2);
    eval_test(&g, &e, &format!("{} ((lambda ((a b . c)  d) c) '(10 2 3 4) 3)", *LAMBDA), (3, (4, Form::Nil)));
    // should fail
    //eval_test(&g, &e, &format!("{} ((lambda (a b c) c) 10 2 3 4)", *LAMBDA), 3);
}

static LET2: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 let1 (bvau dp (s v b)
        (eval b (match_params match_params s (eval v dp) dp))
    ))
    ", *LAMBDA)
});

#[test]
fn let2_eval_test() { let g = grammar::TermParser::new(); let e = root_env();

    eval_test( &g, &e, &format!("{} (let1 x               (+ 10 1) (+ x 1))", *LET2), 12);
    eval_test( &g, &e, &format!("{} (let1 x               '(10 1) x)", *LET2), (10, (1, Form::Nil)));
    eval_test( &g, &e, &format!("{} (let1 (a b)           '(10 1) a)", *LET2), 10);
    eval_test( &g, &e, &format!("{} (let1 (a b)           '(10 1) b)", *LET2),  1);
    eval_test( &g, &e, &format!("{} (let1 (a b . c)       '(10 1) c)", *LET2),  Form::Nil);
    eval_test( &g, &e, &format!("{} (let1 (a b . c)       '(10 1 2 3) c)", *LET2),  (2, (3, Form::Nil)));
    eval_test( &g, &e, &format!("{} (let1 ((a . b) . c)   '((10 1) 2 3) a)", *LET2),  10);
    eval_test( &g, &e, &format!("{} (let1 ((a . b) . c)   '((10 1) 2 3) b)", *LET2),  (1, Form::Nil));
    // should fail
    //eval_test(&g, &e, &format!("{} (let1 (a b c) '(10 2 3 4) a)", *LET2), 10);
}

static LIST: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 list (lambda args args))
    ", *LET2)
});

#[test]
fn list_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (list 1 2 (+ 3 4))", *LIST), (1, (2, (7, Form::Nil))));
}

static Y: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 Y (lambda (f3)
        ((lambda (x1) (x1 x1))
         (lambda (x2) (f3 (wrap (vau app_env y (lapply (x2 x2) y app_env)))))))
    )
    ", *LIST)
});

#[test]
fn y_eval_test() { let g = grammar::TermParser::new(); let e = root_env();

    eval_test(&g, &e, &format!("{} ((Y (lambda (recurse) (lambda (n) (if (= 0 n) 1 (* n (recurse (- n 1))))))) 5)", *Y), 120);

}

static RLAMBDA: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 rlambda (bvau se (n p b)
        (eval (list Y (list lambda (list n) (list lambda p b))) se)
    ))
    ", *Y)
});

#[test]
fn rlambda_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} ((rlambda recurse (n) (if (= 0 n) 1 (* n (recurse (- n 1))))) 5)", *RLAMBDA), 120);
}
static AND_OR: Lazy<String> = Lazy::new(|| {
    // need to extend for varidac
    format!("
    {}
    !(let1 and (bvau se (a b)
        !(let1 ae (eval a se))
        (if ae (eval b se) ae)
    ))
    !(let1 or (bvau se (a b)
        !(let1 ae (eval a se))
        (if ae ae (eval b se))
    ))
    ", *RLAMBDA)
});

#[test]
fn and_or_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (and true  true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (and false true)",  *AND_OR), false);
    eval_test(&g, &e, &format!("{} (and true  false)", *AND_OR), false);
    eval_test(&g, &e, &format!("{} (and false false)", *AND_OR), false);

    eval_test(&g, &e, &format!("{} (or true  true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or false true)",  *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or true  false)", *AND_OR), true);
    eval_test(&g, &e, &format!("{} (or false false)", *AND_OR), false);
}
static LEN: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 len (lambda (l)
        !(let1 len_helper (rlambda len_helper (l a)
            (if (pair? l) (len_helper (cdr l) (+ 1 a))
                          a)
        ))
        (len_helper l 0)
    ))
    ", *AND_OR)
});

#[test]
fn len_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (len '())",       *LEN), 0);
    eval_test(&g, &e, &format!("{} (len '(1))",      *LEN), 1);
    eval_test(&g, &e, &format!("{} (len '(1 2))",    *LEN), 2);
    eval_test(&g, &e, &format!("{} (len '(1 2 3))",  *LEN), 3);
}
static MATCH: Lazy<String> = Lazy::new(|| {
    format!("
    {}
    !(let1 match (bvau de (x . cases)
        !(let1 evaluate_case (rlambda evaluate_case (access c)
                   !(if (symbol? c)                              (list true                          (lambda (b) (list let1 c access b))))
                   !(if (and (pair? c) (= 'unquote (car c)))     (list (list = access (car (cdr c))) (lambda (b) b)))
                   !(if (and (pair? c) (= 'quote   (car c)))     (list (list = access c)             (lambda (b) b)))
                   !(if (pair? c)
                             !(let1 tests             (list and (list pair? access) (list = (len c) (list len access))))
                             !(let1 (tests body_func) ((rlambda recurse (c tests access body_func) (if (pair? c)
                                                                                                     !(let1 (inner_test inner_body_func) (evaluate_case (list car access) (car c)))
                                                                                                     (recurse (cdr c)
                                                                                                              (list and tests inner_test)
                                                                                                              (list cdr access)
                                                                                                              (lambda (b) (body_func (inner_body_func b))))
                                                                                                     ; else
                                                                                                     (list tests body_func)
                                                                                                     ))
                                                            c tests access (lambda (b) b)))
                             (list tests body_func))
                   (list (list = access c)         (lambda (b) b))
        ))
        !(let1 helper (rlambda helper (x_sym cases) (if (= nil cases) (list assert false) 
                                                                      (let1 (test body_func) (evaluate_case x_sym (car cases))
                                                                                             (concat (list if test (body_func (car (cdr cases)))) (list (helper x_sym (cdr (cdr cases)))))))))

        (eval (list let1 '___MATCH_SYM x (helper '___MATCH_SYM cases)) de)
        ;!(let1 expanded (list let1 '___MATCH_SYM x (helper '___MATCH_SYM cases)))
        ;(debug expanded (eval expanded de))
    ))
    ", *LEN)
});
#[test]
fn match_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (match (+ 1 2) 1 2 2 3 3 4 _ 0)", *MATCH), 4);
    eval_test(&g, &e, &format!("{} (match '(1 2)  1 2 2 3 3 4 _ 0)", *MATCH), 0);
    eval_test(&g, &e, &format!("{} (match '(1 2) 1 2 2 3 (a b) (+ a (+ 2 b)) _ 0)", *MATCH), 5);
    eval_test(&g, &e, &format!("{} (match '(1 2) 1 2 2 3 '(1 2) 7 _ 0)", *MATCH), 7);
    eval_test(&g, &e, &format!("{} (let1 a 70 (match (+ 60 10) (unquote a) 100 2 3 _ 0))", *MATCH), 100);
}
static RBTREE: Lazy<String> = Lazy::new(|| {
    format!("
    {}
 !(let1 empty (list 'B  nil nil nil))
 !(let1 E     empty)
 !(let1 EE    (list 'BB nil nil nil))

 !(let1 generic-foldl (rlambda generic-foldl (f z t) (match t
                                                  (unquote E)         z

                                                  (c a x b)  !(let1 new_left_result (generic-foldl f z a))
                                                             !(let1 folded (f new_left_result x))
                                                             (generic-foldl f folded b))))

 !(let1 blacken (lambda (t) (match t
                         ('R a x b) (list 'B a x b)
                         t          t)))
 !(let1 balance (lambda (t) (match t
                         ; figures 1 and 2
                         ('B ('R ('R a x b) y c) z d)    (list 'R (list 'B a x b) y (list 'B c z d))
                         ('B ('R a x ('R b y c)) z d)    (list 'R (list 'B a x b) y (list 'B c z d))
                         ('B a x ('R ('R b y c) z d))    (list 'R (list 'B a x b) y (list 'B c z d))
                         ('B a x ('R b y ('R c z d)))    (list 'R (list 'B a x b) y (list 'B c z d))
                         ; figure 8, double black cases
                         ('BB ('R a x ('R b y c)) z d)   (list 'B (list 'B a x b) y (list 'B c z d))
                         ('BB a x ('R ('R b y c) z d))   (list 'B (list 'B a x b) y (list 'B c z d))
                         ; already balenced
                         t                               t)))

 !(let1 map-insert !(let1 ins (rlambda ins (t k v) (match t
                                  (unquote E)                    (list 'R t (list k v) t)
                                  (c a x b) !(if   (< k (car x)) (balance (list c (ins a k v) x b)))
                                            !(if   (= k (car x)) (list c a (list k v) b))
                                            (balance (list c a x (ins b k v))))))
                   (lambda (t k v) (blacken (ins t k v))))

 !(let1 map-empty            empty)

 !(let1 make-test-tree (rlambda make-test-tree (n t) (if (<= n 0) t
                                                                  (make-test-tree (- n 1) (map-insert t n (= 0 (% n 10)))))))
 !(let1 reduce-test-tree (lambda (tree) (generic-foldl (lambda (a x) (if (car (cdr x)) (+ a 1) a)) 0 tree)))
    ", *MATCH)
});
#[test]
fn rbtree_eval_test() { let g = grammar::TermParser::new(); let e = root_env();
    eval_test(&g, &e, &format!("{} (reduce-test-tree (make-test-tree 10 map-empty))", *RBTREE), 1);
    //eval_test(&g, &e, &format!("{} (reduce-test-tree (make-test-tree 20 map-empty))", *RBTREE), 2);
}
