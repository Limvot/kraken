#!/usr/bin/env newlisp
;;
;; fibonacci series
;; mostly from http://www.newlisp.org/syntax.cgi?benchmarks/fibo.newlisp.txt
;; 	modified slightly to match others
;; 

(define (fib n)
	(cond ((= 0 n)	1)
		  ((= 1 n)  1)
		  (true		(+ (fib (- n 1)) (fib (- n 2))))))

(println (fib (integer (main-args 2))))

(exit)
