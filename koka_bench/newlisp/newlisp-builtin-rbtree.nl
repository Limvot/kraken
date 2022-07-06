#!/usr/bin/env newlisp

(new Tree 'Foo)

(define (make-test-tree n t) (cond ((<= n 0)	t)
								   (true		(make-test-tree (- n 1) (begin (t n (= 0 (% n 10))) t)))))
(define (reduce-test-tree t) (let ((sum 0)) (dolist (item (t)) (if (item 1) (setq sum (+ sum 1))))))


(println (reduce-test-tree (make-test-tree (integer (main-args 2)) Foo)))

(exit)
