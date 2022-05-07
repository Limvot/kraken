(pretty-print ((letrec ((fib (lambda (n) (cond ((equal? n 0) 1)
                                               ((equal? n 1) 1)
                                               (#t (+ (fib (- n 1)) (fib (- n 2))))))))
                        fib) (read (open-input-string (list-ref (command-line) 1)))))
