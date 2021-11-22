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

(define-syntax needs_params_val_lambda
    (er-macro-transformer
         (lambda (x r c)
                (let ((f_sym (list-ref x 1)))
                     `(needs_params_val_lambda_inner ',f_sym ,f_sym)))))

(let* (
       (array list)
       (array? list?)
       (concat append)
       (len length)
       (idx (lambda (x i) (list-ref x (if (< i 0) (+ i 1 (len x)) i))))
       (false #f)
       (true #t)
       (nil '())

       (println print)

       (= equal?)
       (!= (lambda (a b) (not (= a b))))
       (% modulo)
       (int? integer?)
       (env? (lambda (x) false))
       (combiner? (lambda (x) false))
       (drop (rec-lambda recurse (x i) (if (= 0 i) x (recurse (cdr x) (- i 1)))))
       (take (rec-lambda recurse (x i) (if (= 0 i) (array) (cons (car x) (recurse (cdr x) (- i 1))))))
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
                                          ((array? x)      (mark_array false (map recurse x)))
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

    ; A bit wild, but what if instead of is_value we had an evaluation level integer, kinda like wrap?
    ; when lowering, it could just turn into multiple evals or somesuch, though we'd have to be careful of envs...
    (try_unval (rec-lambda recurse (x fail_f)
        (cond ((marked_array? x) (if (not (.marked_array_is_val x)) (array false (fail_f x))
                                                                    (dlet (((sub_ok subs) (foldl (dlambda ((ok a) x) (dlet (((nok p) (recurse x fail_f)))
                                                                                                                       (array (and ok nok) (concat a (array p)))))
                                                                                               (array true (array))
                                                                                               (.marked_array_values x))))
                                                                         (array sub_ok (array 'marked_array false subs)))))
              ((marked_symbol? x) (if (.marked_symbol_is_val x) (array true (array 'marked_symbol false (.marked_symbol_value x)))
                                                                (array false (fail_f x))))
              (true               (array true x))
        )
    ))
    (try_unval_array (lambda (x) (foldl (dlambda ((ok a) x) (dlet (((nok p) (try_unval x (lambda (_) nil))))
                                                                   (array (and ok nok) (concat a (array p)))))
                                        (array true (array))
                                        x)))

    (ensure_val (rec-lambda recurse (x)
        (cond ((marked_array? x)  (array 'marked_array true (map recurse (.marked_array_values x))))
              ((marked_symbol? x) (array 'marked_symbol true (.marked_symbol_value x)))
              (true               x)
        )
    ))
    ; This is a conservative analysis, since we can't always tell what constructs introduce
    ; a new binding scope & would be shadowing... we should at least be able to implement it for
    ; vau/lambda, but we won't at first
    (in_array (let ((helper (rec-lambda recurse (x a i) (cond ((= i (len a))   false)
                                                              ((= x (idx a i)) true)
                                                              (true            (recurse x a (+ i 1)))))))
                   (lambda (x a) (helper x a 0))))

    (is_all_values (lambda (evaled_params) (foldl (lambda (a x) (and a (not (later? x)))) true evaled_params)))

    ; * TODO: allowing envs to be shead if they're not used.
    (shift_envs (rec-lambda recurse (cutoff d x) (cond
        ((val? x)            (array true x))
        ((marked_env? x)     (dlet (((_env is_real dbi meat) x)
                                    ((nmeat_ok nmeat) (foldl (dlambda ((ok r) (k v)) (dlet (((tok tv) (recurse cutoff d v))) (array (and ok tok) (concat r (array (array k tv)))))) (array true (array)) (slice meat 0 -2)))
                                    ((nupper_ok nupper) (if (idx meat -1) (recurse cutoff d (idx meat -1)) (array true nil)))
                                    (ndbi (cond ((nil? dbi)      nil)
                                                ((>= dbi cutoff) (+ dbi d))
                                                (true            dbi)))
                              ) (array (and nmeat_ok nupper_ok (or is_real (and ndbi (>= ndbi 0)))) (array 'env is_real ndbi (concat nmeat (array nupper))))))
        ((comb? x)           (dlet (((wrap_level de? se variadic params body) (.comb x))
                                    ((se_ok nse) (recurse cutoff d se))
                                    ((body_ok nbody) (recurse (+ cutoff 1) d body))
                                ) (array (and se_ok body_ok) (array 'comb wrap_level de? nse variadic params nbody))))
        ((prim_comb? x)      (array true x))
        ((marked_symbol? x)  (array true x))
        ((marked_array? x)   (dlet (((insides_ok insides) (foldl (dlambda ((ok r) tx) (dlet (((tok tr) (recurse cutoff d tx))) (array (and ok tok) (concat r (array tr))))) (array true (array)) (.marked_array_values x))))
                                   (array insides_ok (array 'marked_array (.marked_array_is_val x) insides))))
        (true                (error (str "impossible shift_envs value " x)))
    )))
    (increment_envs (lambda (x) (idx (shift_envs 0  1 x) 1)))
    (decrement_envs (lambda (x)      (shift_envs 0 -1 x)))

    ; TODO: instead of returning the later symbols, we could create a new value of a new type
    ; ['ref de_bruijn_index_of_env index_into_env] or somesuch. Could really simplify
    ; compiling, and I think make partial-eval more efficient. More accurate closes_over analysis too, I think
    (make_tmp_inner_env (lambda (params de? de)
            (array 'env false 0 (concat (map (lambda (p) (array p (array 'marked_symbol false p))) params) (if (= nil de?) (array) (array (array de? (array 'marked_symbol false de?)) )) (array (increment_envs de))))))


    (partial_eval_helper (rec-lambda recurse (x env env_stack indent)
        (cond   ((val? x)            x)
                ((marked_env? x)     (let ((dbi (.marked_env_idx x)))
                                          (if dbi (let* ((new_env (idx env_stack dbi))
                                                         (ndbi (.marked_env_idx new_env))
                                                         (_ (if (!= 0 ndbi) (error (str_strip "new env with non-zero dbis " x))))
                                                         (_ (println (str_strip "replacing " x) (str_strip " with " new_env)))
                                                        )
                                                        (if (= 0 dbi) new_env (idx (shift_envs 0 dbi new_env) 1)))
                                                  x)))

                ((comb? x)           (dlet (((wrap_level de? se variadic params body) (.comb x)))
                                        (if (or (and (not (marked_env_real? env)) (not (marked_env_real? se)))   ; both aren't real, re-evaluation of creation site
                                                (and      (marked_env_real? env)  (not (marked_env_real? se))))  ; new env real, but se isn't - creation!
                                             (let ((inner_env (make_tmp_inner_env params de? env)))
                                                  (array 'comb wrap_level de? env variadic params (recurse body inner_env (cons inner_env env_stack) (+ indent 1))))
                                             x)))
                ((prim_comb? x)      x)
                ((marked_symbol? x)  (if (.marked_symbol_is_val x) x
                                                                  (env-lookup env (.marked_symbol_value x))))
                ((marked_array? x)   (cond ((.marked_array_is_val x) x)
                                           ((= 0 (len (.marked_array_values x))) (error "Partial eval on empty array"))
                                           (true (let* ((values (.marked_array_values x))
                                                        (_ (print_strip (indent_str indent) "partial_evaling comb " (idx values 0)))
                                                        (comb (recurse (idx values 0) env env_stack (+ 1 indent)))
                                                        (literal_params (slice values 1 -1))
                                                        (_ (println (indent_str indent) "Going to do an array call!"))
                                                        (_ (print_strip (indent_str indent) "     total is " x))
                                                        (_ (print_strip (indent_str indent) "     evaled comb is  " comb))
                                                        (ident (+ 1 indent))
                                                     )
                                                     (cond ((prim_comb? comb) ((.prim_comb comb) env env_stack literal_params (+ 1 indent)))
                                                           ((comb? comb)      (dlet (
                                                                                 (rp_eval (lambda (p) (recurse p env env_stack (+ 1 indent))))
                                                                                 ((wrap_level de? se variadic params body) (.comb comb))
                                                                                 (ensure_val_params (map ensure_val literal_params))
                                                                                 ((ok appropriatly_evaled_params) ((rec-lambda param-recurse (wrap cparams)
                                                                                        (if (!= 0 wrap)
                                                                                            (dlet ((pre_evaled (map rp_eval cparams))
                                                                                                   ((ok unval_params) (try_unval_array pre_evaled)))
                                                                                                 (if (not ok) (array ok nil)
                                                                                                    (let* ((evaled_params (map rp_eval unval_params)))
                                                                                                          (param-recurse (- wrap 1) evaled_params))))
                                                                                            (array true cparams))
                                                                                    ) wrap_level ensure_val_params))
                                                                                 (ok_and_non_later (and ok (is_all_values appropriatly_evaled_params)))
                                                                                 ) (if (not ok_and_non_later) (array 'marked_array false (cons comb (if (> wrap_level 0) (map rp_eval literal_params)
                                                                                                                                                     literal_params)))
                                                                                 (dlet (
                                                                                 (final_params (if variadic (concat (slice appropriatly_evaled_params 0 (- (len params) 1))
                                                                                                                   (array (array 'marked_array true (slice appropriatly_evaled_params (- (len params) 1) -1))))
                                                                                                           appropriatly_evaled_params))
                                                                                 ((array de_real de_entry) (if (!= nil de?) (array (marked_env_real? env) (array (array de? (increment_envs env) ) ) )
                                                                                                                     (array true (array))))
                                                                                 (inner_env (array 'env (and de_real (marked_env_real? se)) 0 (concat (zip params (map (lambda (x) (increment_envs x)) final_params)) de_entry (array (increment_envs se)))))
                                                                                 (_ (print_strip (indent_str indent) " with inner_env is " inner_env))
                                                                                 (_ (print_strip (indent_str indent) "going to eval " body))

                                                                                 (tmp_func_result (recurse body inner_env (cons inner_env env_stack) (+ 1 indent)))
                                                                                 (_ (print_strip (indent_str indent) "evaled result of function call  is " tmp_func_result))
                                                                                 ((able_to_sub_env func_result) (decrement_envs tmp_func_result))
                                                                                 (result_is_later (later? func_result))
                                                                                 (_ (print_strip (indent_str indent) "success? " able_to_sub_env " decremented result of function call  is " tmp_func_result))
                                                                                 (stop_envs ((rec-lambda ser (a e) (if e (ser (cons e a) (idx (.env_marked e) -1)) a)) (array) se))
                                                                                 (result_closes_over (contains_symbols stop_envs (concat params (if de? (array de?) (array))) func_result))
                                                                                 (_ (println (indent_str indent) "func call able_to_sub: " able_to_sub_env " result is later? " result_is_later " and result_closes_over " result_closes_over))
                                                                                 ; This could be improved to a specialized version of the function
                                                                                 ; just by re-wrapping it in a comb instead if we wanted.
                                                                                 ; Something to think about!
                                                                                 (result (if (or (not able_to_sub_env) (and result_is_later result_closes_over))
                                                                                                (array 'marked_array false (cons comb (if (> wrap_level 0) (map rp_eval literal_params)
                                                                                                                                                     literal_params)))
                                                                                                func_result))
                                                                             ) result))))
                                                           ((later? comb)    (array 'marked_array false (cons comb literal_params)))
                                                           (true             (error (str "impossible comb value " x))))))))
                (true                (error (str "impossible partial_eval value " x)))
        )
    ))

    ; !!!!!!
    ; ! I think needs_params_val_lambda should be combined with parameters_evaled_proxy
    ; !!!!!!
    (parameters_evaled_proxy (rec-lambda recurse (pasthr_ie inner_f) (lambda (de env_stack params indent) (dlet (
        (_ (println "partial_evaling params in parameters_evaled_proxy is " params))
        ((evaled_params l) (foldl (dlambda ((ac i) p) (let ((p (partial_eval_helper p de env_stack (+ 1 indent))))
                                                         (array (concat ac (array p)) (+ i 1))))
                                 (array (array) 0)
                                 params))
    ) (inner_f (lambda args (apply (recurse pasthr_ie inner_f) args)) de env_stack evaled_params indent)))))

    (needs_params_val_lambda_inner (lambda (f_sym actual_function) (let* (
        (handler (rec-lambda recurse (de env_stack params indent) (let (
                ;_ (println "partial_evaling params in need_params_val_lambda for " f_sym " is " params)
                (evaled_params (map (lambda (p) (partial_eval_helper p de env_stack (+ 1 indent))) params))
            )
            (if (is_all_values evaled_params) (mark (apply actual_function (map strip evaled_params)))
                                              (array 'marked_array false (cons (array 'prim_comb recurse actual_function) evaled_params))))))
        ) (array f_sym (array 'prim_comb handler actual_function)))))

    (root_marked_env (array 'env true nil (array

        ; Ok, so for combinators, it should partial eval the body.
        ; It should then check to see if the partial-evaled body has closed over
        ; any 'later values from above the combinator. If so, the combinator should
        ; evaluate to a ['later [vau de? params (strip partially_evaled_body)]], otherwise it can evaluate to a 'comb.
        ; Note that this 'later may be re-evaluated later if the parent function is called.
        (array 'vau (array 'prim_comb (rec-lambda recurse (de env_stack params indent) (dlet (
            (mde?         (if (= 3 (len params)) (idx params 0) nil))
            (vau_mde?     (if (= nil mde?) (array) (array mde?)))
            (de?          (if mde? (.marked_symbol_value mde?) nil))
            (vau_de?      (if (= nil de?) (array) (array de?)))
            (raw_marked_params (if (= nil de?) (idx params 0) (idx params 1)))
            (raw_params (map (lambda (x) (if (not (marked_symbol? x)) (error (str "not a marked symbol " x))
                                            (.marked_symbol_value x))) (.marked_array_values raw_marked_params)))

            ((variadic vau_params)  (foldl (dlambda ((v a) x) (if (= x '&) (array true a) (array v (concat a (array x))))) (array false (array)) raw_params))
            (body        (if (= nil de?) (idx params 1) (idx params 2)))
            (inner_env (make_tmp_inner_env vau_params de? de))
            (_ (print_strip (indent_str indent) "in vau, evaluating body with 'later params - " body))
            (pe_body (partial_eval_helper body inner_env (cons inner_env env_stack) (+ 1 indent)))
            (_ (print_strip (indent_str indent) "in vau, result of evaluating body was " pe_body))
        ) (array 'comb 0 de? de variadic vau_params pe_body)
        )) 'vau_fake_real))

        (array 'wrap (array 'prim_comb (parameters_evaled_proxy 0 (dlambda (recurse de env_stack (evaled) indent)
              (if (comb? evaled) (dlet (((wrap_level de? se variadic params body) (.comb evaled))
                                        (wrapped_marked_fun (array 'comb (+ 1 wrap_level) de? se variadic params body))
                                       ) wrapped_marked_fun)
                                 (array 'marked_array false (array (array 'prim_comb recurse wrap) evaled))))
        ) 'wrap_fake_real))

        (array 'unwrap (array 'prim_comb (parameters_evaled_proxy 0 (dlambda (recurse de env_stack (evaled) indent)
             (if (comb? evaled) (dlet (((wrap_level de? se variadic params body) (.comb evaled))
                                       (unwrapped_marked_fun (array 'comb (- wrap_level 1) de? se variadic params body))
                                      ) unwrapped_marked_fun)
                                 (array 'marked_array false (array (array 'prim_comb recurse wrap) evaled))))
        ) 'unwrap_fake_real))

        (array 'eval (array 'prim_comb (rec-lambda recurse (de env_stack params indent) (dlet (
            (self (array 'prim_comb recurse eval))
            (eval_env (if (= 2 (len params)) (partial_eval_helper (idx params 1) de env_stack (+ 1 indent))
                                             de))
            (eval_env_v (if (= 2 (len params)) (array eval_env) (array)))
         ) (if (not (marked_env? eval_env)) (array 'marked_array false (cons self params))
                                            (dlet (
            (_ (print_strip (indent_str indent) " partial_evaling_body the first time " (idx params 0)))
            (body1 (partial_eval_helper (idx params 0) de env_stack (+ 1 indent)))
            (_ (print_strip (indent_str indent) "after first eval of param " body1))

            ; With this, we don't actually fail as this is always a legitimate uneval
            (fail_handler (lambda (failed) (array 'marked_array false (concat (array self failed) eval_env_v))))
            ((ok unval_body) (try_unval body1 fail_handler))
            (self_fallback (fail_handler body1))
            (_ (print_strip (indent_str indent) "partial_evaling body for the second time in eval " unval_body))
            (body2 (if (= self_fallback unval_body) self_fallback (partial_eval_helper unval_body eval_env env_stack (+ 1 indent))))
            (_ (print_strip (indent_str indent) "and body2 is " body2))
            ) body2))
        )) 'eval_fake_real))

        ;TODO: This could go a lot farther, not stopping after the first 'later, etc
        ; Also, GAH on odd params - but only one by one - a later odd param can't be imm_eval cuz it will
        ; be frozen if an earlier cond is 'later....
        (array 'cond (array 'prim_comb (parameters_evaled_proxy nil (lambda (recurse de env_stack evaled_params indent)
            (if (!= 0 (% (len evaled_params) 2)) (error (str "partial eval cond with odd evaled_params " evaled_params))
                ((rec-lambda recurse_inner (i)
                                          (cond ((later? (idx evaled_params i))  (array 'marked_array false (cons (array 'prim_comb recurse cond) (slice evaled_params i -1))))
                                                ((false? (idx evaled_params i))  (recurse_inner (+ 2 i)))
                                                (true                            (idx evaled_params (+ 1 i)))) ; we could partially_eval again passing in immediate
                                                                                                               ; eval if it was true, to partially counteract the above GAH
                 ) 0)
            )
        )) 'cond_fake_real))

        (needs_params_val_lambda symbol?)
        (needs_params_val_lambda int?)
        (needs_params_val_lambda string?)

        ;; RESUME with combiner?

        (needs_params_val_lambda +)
        (needs_params_val_lambda -)
        (needs_params_val_lambda *)
        (needs_params_val_lambda /)
        (needs_params_val_lambda %)
        ;(needs_params_val_lambda &)
        ;(needs_params_val_lambda |)
        ;(needs_params_val_lambda <<)
        ;(needs_params_val_lambda >>)
        (needs_params_val_lambda =)
        (needs_params_val_lambda !=)
        (needs_params_val_lambda <)
        (needs_params_val_lambda <=)
        (needs_params_val_lambda >)
        (needs_params_val_lambda >=)


    )))


    (partial_eval (lambda (x) (partial_eval_helper (mark x) root_marked_env (array) 0)))


    (test-all (lambda () (let* (
            (run_test (lambda (s) (print "result of test \"" s "\" => " (str_strip (partial_eval (read (open-input-string s)))))))
        ) (begin
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

        (print (run_test "(+ 1 2)"))

    ))))

) (test-all))
)
