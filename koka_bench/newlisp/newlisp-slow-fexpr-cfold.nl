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
(define-macro (my-match x) (eval (list let (list '__MATCH_SYM x) (cons cond (my-match-helper '__MATCH_SYM (args) 0)))))


(define (mk-expr n v) (cond ((= n 0) (cond ((= v 0) (list 'VR 1 0))
                                           (true    (list 'VL v 0))))
                            (true    (list 'A (mk-expr (- n 1) (+ v 1)) (mk-expr (- n 1) (max (- v 1) 0))))))

(define (append-add a b) (my-match a 
                           ('A c d) (list 'A c (append-add d b))
                           a        (list 'A a b)))

(define (append-mul a b) (my-match a 
                           ('M c d) (list 'M c (append-mul d b))
                           a        (list 'M a b)))

(define (reassoc e) (my-match e
                           ('A a b) (append-add (reassoc a) (reassoc b))
                           ('M c d) (append-mul (reassoc a) (reassoc b))
                           e        e))

(define (cfoldD e) (my-match e
                  ('A a b) (letn (ap (cfoldD a)
                                  bp (cfoldD b))
                                 (my-match ap
                                 ('VL s t) (my-match bp
                                           ('VL m n) (list 'VL (+ s m) 0)
                                           ('A m ('VL n p)) (list 'A (list 'VL (+ s n) 0) m)
                                           ('A ('VL m n) p) (list 'A (list 'VL (+ s m) 0) p)
                                           ep (list 'A ap bp)
                                           )
                                 ep (list 'A ap bp)
                                 )
                            )
                  ('M c d) (letn (cp (cfoldD c)
                                  dp (cfoldD dec))
                                 (my-match cp
                                 ('VL s t) (my-match dp
                                           ('VL m n) (list 'VL (* s m) 0)
                                           ('M m ('VL n p)) (list 'M (list 'VL (* s n) 0) m)
                                           ('M ('VL m n) p) (list 'M (list 'VL (* s m) 0) p)
                                           ep (list 'M cp dp)
                                           )
                                 ep (list 'M ap bp)
                                 )
                            )
                  e        e))

(define (evalD e) (my-match e
                  ('VR a b) 0
                  ('VL c d) c
                  ('A e f)  (+ (evalD e) (evalD f))
                  ('M l r)  (* (evalD l) (evalD r))))


(println (evalD (cfoldD (reassoc (mk-expr (integer (main-args 2)) 1)))))

(exit)
