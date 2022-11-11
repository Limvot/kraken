#!/usr/bin/env newlisp

(define cont list)
(define cont? list?)
;(define cont (lambda (l) (array (length l) l)))
;(define cont? array?)

(define (evaluate_case access c) (cond
          ((and (list? c) (= 2 (length c)) (= 'unquote (c 0)))  (list (list = access (c 1)) (lambda (b) b)))
          ((symbol? c)                                          (list true                  (expand (lambda (b) (list let (list 'c 'access) b)) 'c 'access)))
          ((list?  c)                                           (letn (
                                 tests            (list and (list list? access) (list = (length c) (list length access)))
                                 tests__body_func (local (recurse) (setq recurse (lambda (tests body_func i) (if (= i (length c))
                                                                                            (list tests body_func)
                                                                                            (letn ( inner_test__inner_body_func (evaluate_case (list access i) (c i))
                                                                                                    inner_test      (inner_test__inner_body_func 0)
                                                                                                    inner_body_func (inner_test__inner_body_func 1)
                                                                                                  )
                                                                                         (recurse (append tests (list inner_test))
                                                                                                       (expand (lambda (b) (body_func (inner_body_func b))) 'body_func 'inner_body_func)
                                                                                                       (+ i 1))))))
                                                                   (recurse tests (lambda (b) b) 0))
                          ) tests__body_func))
          (true                                                 (list (list = access c)     (lambda (b) b)))
      ))

(define (my-match-helper x_sym cases i)  (cond ((< i (- (length cases) 1)) (let (test__body_func (evaluate_case x_sym (cases i)))
                                                                                       (append (list (list (test__body_func 0) ((test__body_func 1) (cases (+ i 1))))) (my-match-helper x_sym cases (+ i 2)))))
                                               (true                     '((true ("none matched"))))))

(macro (my-match X) X)
(constant 'my-match (lambda-macro (X) (expand (list let (list '__MATCH_SYM 'X) (cons cond (my-match-helper '__MATCH_SYM (args) 0))) 'X)))


(define (addD nI mI) (my-match (list nI mI)
                           (('VL n x) ('VL m y))        (list 'VL (+ n m) x)
                           (('VL 0 x) f)                f
                           (f ('VL 0 y))                f
                           (f ('VL n y))                (addD (list 'VL n y) f)
                           (('VL n x) ('A ('VL m y) f)) (addD (list 'VL (+ n m) y) f)
                           (f ('A ('VL m y) g))         (addD (list 'VL m y) (addD f g))
                           (('A f g) h)                 (addD f (addD g h))
                           (f g)                        (list 'A f g)))

(define (mulD nI mI) (my-match (list nI mI)
                           (('VL n x) ('VL m y))        (list 'VL (* n m) x)
                           (('VL 0 x) f)                (list 'VL 0 x)
                           (f ('VL 0 y))                (list 'VL 0 y)
                           (('VL 1 x) f)                f
                           (f ('VL 1 y))                f
                           (f ('VL n y))                (mulD (list 'VL n y) f)
                           (('VL n x) ('M ('VL m y) f)) (mulD (list 'VL (* n m) y) f)
                           (f ('M ('VL m y) g))         (mulD (list 'VL m y) (mulD f g))
                           (('M f g) h)                 (mulD f (mulD g h))
                           (f g)                        (list 'M f g)))

(define (powD nI mI) (my-match (list nI mI)
                           (('VL n x) ('VL m y))        (list 'VL (pow n m) x)
                           (f ('VL 0 y))                (list 'VL 1 y)
                           (f ('VL 1 y))                f
                           (('VL 0 y) f)                (list 'VL 1 y)
                           (f g)                        (list 'P f g)))

(define (lnD nI) (my-match nI
                           ('VL 1 x)        (list 'VL 0 x) 
                           (f)              (list 'L f 0)))

(define (derv x e) (my-match e
                    ('VL a b) ('VL 0 b)
                    ('VR y b) (cond ((= x y) ('VL 1 b))
                                    (true    ('VL 0 b)))
                    ('A f g)  (addD (derv x f) (derv x g))
                    ('M f g)  (addD (mulD f (derv x g)) (mulD g (derv x f)))
                    ('P f g)  (mulD (powD f g) (addD (mul (mul g (derv x f)) (powd f (list 'VL -1 0))) (mulD (lnD f) (derv x g))))
                    ('L f)    (mulD (derv x f) (powD f (list 'VL -1 0)))

))

(define (countD nI) (my-match (nI)
                           ('VL 1 x)        1
                           ('VR 1 x)        1
                           ('A f g)         (+ (countD f) (countD g))
                           ('M f g)         (+ (countD f) (countD g))
                           ('P f g)         (+ (countD f) (countD g))
                           ('L f g)         (countD f)))

(define (nest-aux s f n x) (cond ((= n 0) x)
                                 (true    (nest-aux s f (- n 1) (f (- s n) x)))))

(define (nest f n e) (nest-aux n f n e))

(define (deriv i f) d)

(println (nest deriv (integer (main-args 2)) (powr (list 'VR "x" 0) (list 'VR "x" 0)))))))

(exit)
