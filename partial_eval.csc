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

(define-syntax dlet
    (er-macro-transformer
        (lambda (x r c)
            (let* (
                    (items (list-ref x 1))
                    (body (list-ref x 2))
                    (flat_map_i (lambda (f l) ((rec recurse (lambda (f l i) (cond
                                                                                ((equal? '() l) '())
                                                                                (#t      (append (f i (car l)) (recurse f (cdr l) (+ i 1)))))
                                                            )) f l 0)))
                    (flatten-helper (rec recurse (lambda (items)
                                                    (cond
                                                        ((equal? '() items) '())
                                                        (#t           (let* (
                                                                            (clause (car items))
                                                                            (result (cond
                                                                                        ((list? (car clause)) (let ((s (gensym)))
                                                                                                                    (cons `(,s ,(car (cdr clause)))
                                                                                                                        (flat_map_i (lambda (i x)
                                                                                                                                   (recurse `((,x (list-ref ,s ,i))))
                                                                                                                               )
                                                                                                                               (car clause)))))
                                                                                        (#t                (list clause))))
                                                                          ) (append result (recurse (cdr items)))))
                                                    ))))

                    (flat_items (flatten-helper items))
                    (_ (print items " flattened " flat_items))
                  ) `(let* ,flat_items ,body)
             ))))
(define-syntax dlambda
    (er-macro-transformer
         (lambda (x r c)
                (let (
                         (params (list-ref x 1))
                         (param_sym (gensym))
                         (body (list-ref x 2))
                     )
                     `(lambda ,param_sym (dlet ( (,params ,param_sym) ) ,body))))))

(let* (
       (array list)
       (concat append)
       (len length)
       (idx list-ref)
       (false #f)
       (true #t)
       (nil '())

       (= equal?)
       (drop (rec-lambda recurse (x i) (if (= 0 i) x (recurse (cdr x) (- i 1)))))
       (take (rec-lambda recurse (x i) (if (= 0 i) '() (cons (car x) (recurse (cdr x) (- i 1))))))
       (slice (lambda (x s e) (let* ( (l (len x))
                                      (s (if (< s 0) (+ s l 1) s))
                                      (e (if (< e 0) (+ e l 1) e))
                                      (t (- e s)) )
                               (take (drop x s) t))))

       (str (lambda args (begin
                            (define mp (open-output-string))
                            (display args mp)
                            (get-output-string mp))))

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

    (mark_symbol (lambda (is_val x) (array 'marked_symbol is_val x)))
    (mark_array  (lambda (is_val x) (array 'marked_array is_val x)))
    (mark_val    (lambda (x)        (array 'val x)))



    (later? (rec-lambda recurse (x) (or (and (marked_array? x)  (or (= false (.marked_array_is_val x)) (foldl (lambda (a x) (or a (recurse x))) false (.marked_array_values x))))
                                        (and (marked_symbol? x) (= false (.marked_symbol_is_val x)))
                                   )))

    (false? (lambda (x) (cond ((and (marked_array? x)  (= false (.marked_array_is_val x)))    (error "got a later marked_array passed to false? " x))
                              ((and (marked_symbol? x) (= false (.marked_symbol_is_val x)))   (error "got a later marked_symbol passed to false? " x))
                              ((val? x)                                                       (not (.val x)))
                              (true                                                           false))))

    (env-lookup-helper (rec-lambda recurse (dict key i fail success) (cond ((and (= i (- (len dict) 1)) (= nil (idx dict i)))  (fail))
                                                                           ((= i (- (len dict) 1))                             (recurse (.env_marked (idx dict i)) key 0 fail success))
                                                                           ((= key (idx (idx dict i) 0))                       (success (idx (idx dict i) 1)))
                                                                           (true                                               (recurse dict key (+ i 1) fail success)))))
    (env-lookup (lambda (env key) (env-lookup-helper (.env_marked env) key 0 (lambda () (error key " not found in env " (.env_marked env))) (lambda (x) x))))

    (mark (rec-lambda recurse (x) (cond   ((env? x)        (error "called mark with an env " x))
                                          ((combiner? x)   (error "called mark with a combiner " x))
                                          ((symbol? x)     (mark_symbol false x))
                                          ((array? x)      (mark__array false (map recurse x)))
                                          (true            (mark_val x)))))

    (indent_str (rec-lambda recurse (i) (if (= i 0) ""
                                                   (str "   " (recurse (- i 1))))))

    (str_strip (lambda args (apply str (concat (slice args 0 -2) (array ((rec-lambda recurse (x)
        (cond ((val? x)           (.val x))
              ((marked_array? x)  (let ((stripped_values (map recurse (.marked_array_values x))))
                                        (if (.marked_array_is_val x) (cons array stripped_values)
                                                                     stripped_values)))
              ((marked_symbol? x) (if (.marked_symbol_is_val x) (array 'quote (.marked_symbol_value x))
                                                                (.marked_symbol_value x)))
              ((comb? x)          (dlet (((wrap_level de? se variadic params body) (.comb x)))
                                      (str "<comb " wrap_level " " de? " <se " (recurse se) "> " params " " (recurse body) ">")))
              ((prim_comb? x)     (idx x 2))
              ((marked_env? x)    (let ((e (.env_marked x))
                                        (index (.marked_env_idx x))
                                        (u (idx e -1))
                                       ) (if u (str "<" (if (marked_env_real? x) "real" "fake") " ENV idx: " (str index) ", " (map (dlambda ((k v)) (array k (recurse v))) (slice e 0 -2)) " upper: " (recurse u)  ">")
                                               "<no_upper_likely_root_env>")))
              (true               (error (str "some other str_strip? |" x "|")))
        )
    ) (idx args -1)))))))
    (print_strip (lambda args (println (apply str_strip args))))

    (strip (let ((helper (rec-lambda recurse (x need_value)
        (cond ((val? x)           (.val x))
              ((marked_array? x)  (let ((stripped_values (map (lambda (x) (recurse x need_value)) (.marked_array_values x))))
                                      (if (.marked_array_is_val x) (if need_value (error (str "needed value for this strip but got" x)) (cons array stripped_values))
                                                                   stripped_values)))
              ((marked_symbol? x) (if (.marked_symbol_is_val x) (if need_value (error (str "needed value for this strip but got" x)) (array quote (.marked_symbol_value x)))
                                                               (.marked_symbol_value x)))
              ((comb? x)          (dlet (((wrap_level de? se variadic params body) (.comb x))
                                         (de_entry (if de? (array de?) (array)))
                                         (final_params (if variadic (concat (slice params 0 -2) '& (array (idx params -1))) params))
                                         ; Honestly, could trim down the env to match what could be evaluated in the comb
                                         ; Also if this isn't real, lower to a call to vau
                                         (se_env (if (marked_env_real? se) (recurse se true) nil))
                                         (body_v (recurse body false))
                                         (ve (concat (array vau) de_entry (array final_params) (array body_v)))
                                         (fe ((rec-lambda recurse (x i) (if (= i 0) x (recurse (array wrap x) (- i 1)))) ve wrap_level))
                                  ) (if se_env (eval fe se_env) fe)))
              ((prim_comb? x)     (idx x 2))
                                         ; env emitting doesn't pay attention to real value right now, not sure if that makes sense
                                         ; TODO: properly handle de Bruijn indexed envs
              ((marked_env? x)    (cond ((and (not need_value) (= 0 (.marked_env_idx x))) (array current-env))
                                        (true                                             (let ((_ (if (not (marked_env_real? x)) (error (str_strip "trying to emit fake env!" x)))))
                                                                                                (upper (idx (.env_marked x) -1))
                                                                                                (upper_env (if upper (recurse upper true) empty_env))
                                                                                                (just_entries (slice (.env_marked x) 0 -2))
                                                                                                (vdict (map (dlambda ((k v)) (array k (recurse v true))) just_entries))
                                                                                        ) (add-dict-to-env upper_env vdict))))
              (true               (error (str "some other strip? " x)))
        )
    )))  (lambda (x) (let ((_ (print_strip "stripping: " x)) (r (helper x false)) (_ (println "result of strip " r))) r))))

    (test-all (lambda () (begin
        (print (val? '(val)))
        (print "take 3" (take '(1 2 3 4 5 6 7 8 9 10) 3))
        (print "drop 3" (drop '(1 2 3 4 5 6 7 8 9 10) 3))
        (print (slice '(1 2 3) 1 2))
        (print (slice '(1 2 3) 1 -1))
        (print (slice '(1 2 3) -1 -1))
        (print (slice '(1 2 3) -2 -1))

        (print "ASWDF")
        (print ( (dlambda ((a b)) a) '(1337 1338)))
        (print ( (dlambda ((a b)) b) '(1337 1338)))

        (print (str 1 2 3 (array 1 23 4) "a" "B"))

        (print (dlet ( (x 2) ((a b) '(1 2)) (((i i2) i3) '((5 6) 7)) ) (+  x a b i i2 i3)))

        (print (array 1 2 3))
        (print (command-line-arguments))

        (print (call-with-input-string "'(1 2)" (lambda (p) (read p))))
        (print (read (open-input-string "'(3 4)")))
    )))

) (test-all))
)
