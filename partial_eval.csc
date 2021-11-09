(import (chicken process-context))
(import (chicken port))
(import (r5rs))
(define-syntax rec-lambda
    (er-macro-transformer
         (lambda (x r c)
                (let (
                         (name (car (cdr x)))
                         (params (car (cdr (cdr x))))
                         (body (car (cdr (cdr (cdr x)))))
                     )
                     `(rec ,name (lambda ,params ,body))))))
(let* (
       (array list)
       (concat append)
       (len length)
       (idx list-ref)

       (= equal?)
       (drop (rec-lambda recurse (x i) (if (= 0 i) x (recurse (cdr x) (- i 1)))))
       (take (rec-lambda recurse (x i) (if (= 0 i) '() (cons (car x) (recurse (cdr x) (- i 1))))))
       (slice (lambda (x s e) (let* ( (l (len x))
                                      (s (if (< s 0) (+ s l 1) s))
                                      (e (if (< e 0) (+ e l 1) e))
                                      (t (- e s)) )
                               (take (drop x s) t))))
      )

(let* (

    (val?                   (lambda (x) (= 'val (idx x 0))))
    (.val                   (lambda (x) (idx x 1)))
    (marked_array?          (lambda (x) (= 'marked_array (idx x 0))))
    (.marked_array_is_val   (lambda (x) (idx x 1)))
    (.marked_array_values   (lambda (x) (idx x 2)))
    (marked_symbol?         (lambda (x) (= 'marked_symbol (idx x 0))))
    (.marked_symbol_is_val  (lambda (x) (idx x 1)))
    (.marked_symbol_value   (lambda (x) (idx x 2)))
    (comb?                  (lambda (x) (= 'comb (idx x 0))))
    (.comb                  (lambda (x) (slice x 1 -1)))
    (prim_comb?             (lambda (x) (= 'prim_comb (idx x 0))))
    (.prim_comb             (lambda (x) (idx x 1)))
    (marked_env?            (lambda (x) (= 'env (idx x 0))))
    (marked_env_real?       (lambda (x) (idx x 1)))
    (.marked_env_idx        (lambda (x) (idx x 2)))
    (.env_marked            (lambda (x) (idx x 3)))

    (test-all (lambda () (begin
        (print (val? '(val)))
        (print "take 3" (take '(1 2 3 4 5 6 7 8 9 10) 3))
        (print "drop 3" (drop '(1 2 3 4 5 6 7 8 9 10) 3))
        (print (slice '(1 2 3) 1 2))
        (print (slice '(1 2 3) 1 -1))
        (print (slice '(1 2 3) -1 -1))
        (print (slice '(1 2 3) -2 -1))

        (print (array 1 2 3))
        (print (command-line-arguments))

        (print (call-with-input-string "'(1 2)" (lambda (p) (read p))))
        (print (read (open-input-string "'(3 4)")))
    )))

) (test-all))
)
