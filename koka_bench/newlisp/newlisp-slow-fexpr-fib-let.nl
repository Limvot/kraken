#!/usr/bin/env newlisp
;;
;; fibonacci series
;; mostly from http://www.newlisp.org/syntax.cgi?benchmarks/fibo.newlisp.txt
;; 	modified slightly to match others
;; 

(define (fib n)
	(my-match (0	1)
		  (1  1)
		  (n		(let (a (fib (- n 1))
		                  b (fib (- n 2))
						  ) (+ a b)))))

(define (my-match-helper x_sym cases i)  (cond ((< i (- (length cases) 1)) (let (test__body_func (evaluate_case x_sym (cases i)))
                                                                                       (append (list (list (test__body_func 0) ((test__body_func 1) (cases (+ i 1))))) (my-match-helper x_sym cases (+ i 2)))))
                                               (true                     '((true ("none matched"))))))
(define-macro (my-match x) (eval (list let (list '__MATCH_SYM x) (cons cond (my-match-helper '__MATCH_SYM (args) 0)))))

(println (fib (integer (main-args 2))))

(exit)
