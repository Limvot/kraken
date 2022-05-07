(pretty-print ((letrec ((fib (lambda (n) (cond ((equal? n 0) 1)
                                               ((equal? n 1) 1)
                                               (#t (let (
                                                        (r1 (fib (- n 1)))
                                                        (r2 (fib (- n 2)))
                                                    ) (+ r1 r2)))))))
                        fib) (read (open-input-string (list-ref (command-line) 1)))))
