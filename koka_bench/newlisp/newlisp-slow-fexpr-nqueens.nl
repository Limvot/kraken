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

(define (safe q d xs) ())

(define (extendS q acc xss) (my-match xss
                              ))

(define (findS n q) (cond ((= q 0) (list nil))
                              true (extendS n nil (findS n (- q 1)))))

(define (nqueens n) (length (findS n n)))

(println (nqueens (integer (main-args 2))))

(exit)
