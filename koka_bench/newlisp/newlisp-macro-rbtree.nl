#!/usr/bin/env newlisp

(define cont list)
(define cont? list?)
;(define cont (lambda (l) (array (length l) l)))
;(define cont? array?)

; Sigh, newLisp doesn't seem to be expand 'a to (quote a) so we can't look for it, and
; it doesn't support unquoting or splice-unquoting at all. As a hack, we instead
; do some string manipulation on symbols starting with the special characters ~ or @

; OH WAIT NO WE DON'T
; we just write it out explicitly
; ugly, but fair

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
;(define-macro (my-match x) (eval (list let (list '__MATCH_SYM x) (cons cond (my-match-helper '__MATCH_SYM (args) 0)))))
;(define-macro (my-match x)       (list let (list '__MATCH_SYM x) (cons cond (my-match-helper '__MATCH_SYM (args) 0))))

;(println "Hodwy!")
;(define myvar1 (list 'ASDF 1 2 3))
;(define myvar myvar1)
;(define searche (list 'ASDF 1 2 3))
;(println "match result " (my-match searche
;                   1                2
;                   (1 a)            (string "list!" a)
;                   (unquote myvar)  (list searche "oooh fancy" searche)
;                   'a               "haha"
;                   2                3))
;(println "blacken test " (my-match (list 'R 1 2 3) 
;                   (unquote myvar)  "oooh fancy"
;                                   (c a x b) (list 'B c a x b)
;                                   t          t))
;(println "done")

(define empty (list 'B  nil nil nil))
(define E     empty)
(define EE    (list 'BB nil nil nil))

(define (map-foldl f z t) (my-match t
                                     (unquote E)         z

                                     (c a x b)  (letn (new_left_result (map-foldl f z a)
                                                       folded (f new_left_result x)
                                                      ) (map-foldl f folded b))))
                                     ;(c a x b)  (map-foldl f (f (map-foldl f z a) x) b)))

(define (blacken t) (my-match t
                              ('R a x b) (list 'B a x b)
                              t          t))
(define (balance t) (my-match t
                        ; figures 1 and 2
                        ('B ('R ('R a x b) y c) z d)    (list 'R (list 'B a x b) y (list 'B c z d))
                        ('B ('R a x ('R b y c)) z d)    (list 'R (list 'B a x b) y (list 'B c z d))
                        ('B a x ('R ('R b y c) z d))    (list 'R (list 'B a x b) y (list 'B c z d))
                        ('B a x ('R b y ('R c z d)))    (list 'R (list 'B a x b) y (list 'B c z d))
                        ; figure 8, double black cases
                        ('BB ('R a x ('R b y c)) z d)   (list 'B (list 'B a x b) y (list 'B c z d))
                        ('BB a x ('R ('R b y c) z d))   (list 'B (list 'B a x b) y (list 'B c z d))
                        ; already balenced
                        t                               t))

(define (map-insert-helper t k v) (my-match t
                                 (unquote E)   (list 'R t (list k v) t)
                                 (c a x b)     (cond ((< k (x 0)) (balance (list c (map-insert-helper a k v) x b)))
                                                     ((= k (x 0)) (list c a (list k v) b))
                                                     (true            (balance (list c a x (map-insert-helper b k v)))))))
(define (map-insert t k v) (blacken (map-insert-helper t k v)))

(define map-empty            empty)

(define (make-test-tree n t) (cond ((<= n 0)	t)
								   (true		(make-test-tree (- n 1) (map-insert t n (= 0 (% n 10)))))))
(define (reduce-test-tree t) (map-foldl (lambda (a x) (if (x 1) (+ a 1) a)) 0 t))


(println (reduce-test-tree (make-test-tree (integer (main-args 2)) map-empty)))

(exit)
