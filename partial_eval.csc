(import (chicken process-context))
(import (chicken port))
(import (chicken io))
(import (chicken bitwise))
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


(define-syntax give_up_eval_params
    (er-macro-transformer
         (lambda (x r c)
                (let ((f_sym (list-ref x 1)))
                     `(give_up_eval_params_inner ',f_sym ,f_sym)))))


(define-syntax mif
    (er-macro-transformer
         (lambda (x r c)
                (let (
                        (cond (list-ref x 1))
                        (v (gensym))
                        (then (list-ref x 2))
                        (else (if (equal? 4 (length x)) (list-ref x 3) ''()))
                     )
                     `(let ((,v ,cond)) (if (and (not (equal? (array) ,v)) ,v) ,then ,else))))))


(let* (
       (= equal?)
       (!= (lambda (a b) (not (= a b))))
       (array list)
       (array? list?)
       (concat append)
       (len length)
       (idx (lambda (x i) (list-ref x (mif (< i 0) (+ i (len x)) i))))
       (false #f)
       (true #t)
       (nil '())
       (str-to-symbol string->symbol)
       (get-text symbol->string)

       (nil? (lambda (x) (= nil x)))
       (bool? (lambda (x) (or (= #t x) (= #f x))))
       (println print)

       (read-string (lambda (s) (read (open-input-string s))))

       (zip (lambda args (apply map list args)))

       (empty_dict (array))
       (put (lambda (m k v) (cons (array k v) m)))
       (get-value (lambda (d k) (let ((result (alist-ref k d)))
                                     (if (array? result) (idx result 0)
                                                         (error (str "could not find " k " in " d))))))

       (% modulo)
       (int? integer?)
       (env? (lambda (x) false))
       (combiner? (lambda (x) false))
       (drop (rec-lambda recurse (x i) (mif (= 0 i) x (recurse (cdr x) (- i 1)))))
       (take (rec-lambda recurse (x i) (mif (= 0 i) (array) (cons (car x) (recurse (cdr x) (- i 1))))))
       (slice (lambda (x s e) (let* ( (l (len x))
                                      (s (mif (< s 0) (+ s l 1) s))
                                      (e (mif (< e 0) (+ e l 1) e))
                                      (t (- e s)) )
                               (take (drop x s) t))))

       (filter (rec-lambda recurse (f l) (cond ((nil? l)    nil)
                                               ((f (car l)) (cons (car l) (recurse f (cdr l))))
                                               (true        (recurse f (cdr l))))))

       (flat_map (lambda (f l) ((rec recurse (lambda (f l) (cond
                                                                ((equal? '() l) '())
                                                                (#t      (append (f (car l)) (recurse f (cdr l)))))
                                               )) f l)))
       (str (lambda args (begin
                            (define mp (open-output-string))
                            (display args mp)
                            (get-output-string mp))))

       (write_file (lambda (file bytes) (call-with-output-file file (lambda (out) (foldl (lambda (_ o) (write-byte o out)) (void) bytes)))))
      )
(let* (

    (val?                   (lambda (x) (= 'val (idx x 0))))
    (marked_array?          (lambda (x) (= 'marked_array (idx x 0))))
    (marked_symbol?         (lambda (x) (= 'marked_symbol (idx x 0))))
    (comb?                  (lambda (x) (= 'comb (idx x 0))))
    (prim_comb?             (lambda (x) (= 'prim_comb (idx x 0))))
    (marked_env?            (lambda (x) (= 'env (idx x 0))))

    (marked_env_real?       (lambda (x) (idx x 2)))
    (.val                   (lambda (x) (idx x 2)))
    (.marked_array_is_val   (lambda (x) (idx x 2)))
    (.marked_array_values   (lambda (x) (idx x 3)))
    (.marked_symbol_is_val  (lambda (x) (idx x 2)))
    (.marked_symbol_value   (lambda (x) (idx x 3)))
    (.comb                  (lambda (x) (slice x 2 -1)))
    (.prim_comb             (lambda (x) (idx x 2)))
    (.marked_env            (lambda (x) (slice x 2 -1)))
    (.marked_env_idx        (lambda (x) (idx x 3)))
    (.env_marked            (lambda (x) (idx x 4)))

    (.hash                  (lambda (x) (idx x 1)))

    (combine_hash     (lambda (a b) (+ (* 37 a) b)))
    (hash_bool        (lambda (b) (if b 2 3)))
    (hash_num         (lambda (n) (combine_hash 5 n)))
    (hash_string      (lambda (s) (foldl combine_hash 7  (map char->integer (string->list s)))))
    (hash_symbol      (lambda (is_val s) (combine_hash (if is_val 11 13) (hash_string (symbol->string s)))))

    (hash_array       (lambda (is_val a) (foldl combine_hash (if is_val 17 19) (map .hash a))))
    (hash_env         (lambda (is_real dbi arrs) (combine_hash (mif dbi (hash_num dbi) 59) (let* (
                                                                (inner_hash (foldl (dlambda (c (s v)) (combine_hash c (combine_hash (hash_symbol false s) (.hash v))))
                                                                                   (if is_real 23 29)
                                                                                   (slice arrs 0 -2)))
                                                                (end (idx arrs -1))
                                                                (end_hash (mif end (.hash end) 31))
                                                                                                         ) (combine_hash inner_hash end_hash)))))
    (hash_comb        (lambda (wrap_level de? se variadic params body) (combine_hash 41
                                                                       (combine_hash (mif de? (hash_symbol false de?) 43)
                                                                       (combine_hash (.hash se)
                                                                       (combine_hash (hash_bool variadic)
                                                                       (combine_hash (foldl (lambda (c x) (combine_hash c (hash_symbol false x))) 47 params)
                                                                       (.hash body))))))))
    (hash_prim_comb   (lambda (handler_fun real_or_name) (combine_hash 53 (hash_symbol false real_or_name))))
    (hash_val         (lambda (x) (cond ((bool? x)     (hash_bool x))
                                        ((string? x)   (hash_string x))
                                        ((int? x)      (hash_num x))
                                        (true          (error (str "bad thing to hash_val " x))))))
    ; 41 43 47 53 59 61 67 71

    (marked_symbol    (lambda (is_val x)                               (array 'marked_symbol    (hash_symbol is_val x)                             is_val x)))
    (marked_array     (lambda (is_val x)                               (array 'marked_array     (hash_array is_val x)                              is_val x)))
    (marked_val       (lambda (x)                                      (array 'val              (hash_val x)                                       x)))
    (marked_env       (lambda (is_real dbi arrs)                       (array 'env              (hash_env is_real dbi arrs)                        is_real dbi arrs)))
    (marked_comb      (lambda (wrap_level de? se variadic params body) (array 'comb             (hash_comb wrap_level de? se variadic params body) wrap_level de? se variadic params body)))
    (marked_prim_comb (lambda (handler_fun real_or_name)               (array 'prim_comb        (hash_prim_comb handler_fun real_or_name)          handler_fun real_or_name)))



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
                                          ((symbol? x)     (cond ((= 'true x)  (marked_val #t))
                                                                 ((= 'false x) (marked_val #f))
                                                                 (#t           (marked_symbol false x))))
                                          ((array? x)      (marked_array false (map recurse x)))
                                          (true            (marked_val x)))))

    (indent_str (rec-lambda recurse (i) (mif (= i 0) ""
                                                   (str "   " (recurse (- i 1))))))

    (str_strip (lambda args (apply str (concat (slice args 0 -2) (array ((rec-lambda recurse (x)
        (cond ((val? x)           (.val x))
              ((marked_array? x)  (let ((stripped_values (map recurse (.marked_array_values x))))
                                        (mif (.marked_array_is_val x) (cons array stripped_values)
                                                                     stripped_values)))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (array 'quote (.marked_symbol_value x))
                                                                (.marked_symbol_value x)))
              ((comb? x)          (dlet (((wrap_level de? se variadic params body) (.comb x)))
                                      (str "<comb " wrap_level " " de? " <se " (recurse se) "> " params " " (recurse body) ">")))
              ((prim_comb? x)     (idx x 2))
              ((marked_env? x)    (let* ((e (.env_marked x))
                                         (index (.marked_env_idx x))
                                         (u (idx e -1))
                                       ) (mif u (str "<" (mif (marked_env_real? x) "real" "fake") " ENV idx: " (str index) ", " (map (dlambda ((k v)) (array k (recurse v))) (slice e 0 -2)) " upper: " (recurse u)  ">")
                                               "<no_upper_likely_root_env>")))
              (true               (error (str "some other str_strip? |" x "|")))
        )
    ) (idx args -1)))))))
    (print_strip (lambda args (println (apply str_strip args))))

    (strip (let ((helper (rec-lambda recurse (x need_value)
        (cond ((val? x)           (.val x))
              ((marked_array? x)  (let ((stripped_values (map (lambda (x) (recurse x need_value)) (.marked_array_values x))))
                                      (mif (.marked_array_is_val x) (mif need_value (error (str "needed value for this strip but got" x)) (cons array stripped_values))
                                                                   stripped_values)))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (mif need_value (error (str "needed value for this strip but got" x)) (array quote (.marked_symbol_value x)))
                                                               (.marked_symbol_value x)))
              ((comb? x)          (dlet (((wrap_level de? se variadic params body) (.comb x))
                                         (de_entry (mif de? (array de?) (array)))
                                         (final_params (mif variadic (concat (slice params 0 -2) '& (array (idx params -1))) params))
                                         ; Honestly, could trim down the env to match what could be evaluated in the comb
                                         ; Also mif this isn't real, lower to a call to vau
                                         (se_env (mif (marked_env_real? se) (recurse se true) nil))
                                         (body_v (recurse body false))
                                         (ve (concat (array vau) de_entry (array final_params) (array body_v)))
                                         (fe ((rec-lambda recurse (x i) (mif (= i 0) x (recurse (array wrap x) (- i 1)))) ve wrap_level))
                                  ) (mif se_env (eval fe se_env) fe)))
              ((prim_comb? x)     (idx x 2))
                                         ; env emitting doesn't pay attention to real value right now, not sure mif that makes sense
                                         ; TODO: properly handle de Bruijn indexed envs
              ((marked_env? x)    (cond ((and (not need_value) (= 0 (.marked_env_idx x))) (array current-env))
                                        (true                                             (let ((_ (mif (not (marked_env_real? x)) (error (str_strip "trying to emit fake env!" x)))))
                                                                                                (upper (idx (.env_marked x) -1))
                                                                                                (upper_env (mif upper (recurse upper true) empty_env))
                                                                                                (just_entries (slice (.env_marked x) 0 -2))
                                                                                                (vdict (map (dlambda ((k v)) (array k (recurse v true))) just_entries))
                                                                                        ) (add-dict-to-env upper_env vdict))))
              (true               (error (str "some other strip? " x)))
        )
    )))  (lambda (x) (let* ((_ (print_strip "stripping: " x)) (r (helper x false)) (_ (println "result of strip " r))) r))))

    ; A bit wild, but what mif instead of is_value we had an evaluation level integer, kinda like wrap?
    ; when lowering, it could just turn into multiple evals or somesuch, though we'd have to be careful of envs...
    (try_unval (rec-lambda recurse (x fail_f)
        (cond ((marked_array? x) (mif (not (.marked_array_is_val x)) (array false (fail_f x))
                                                                    (dlet (((sub_ok subs) (foldl (dlambda ((ok a) x) (dlet (((nok p) (recurse x fail_f)))
                                                                                                                       (array (and ok nok) (concat a (array p)))))
                                                                                               (array true (array))
                                                                                               (.marked_array_values x))))
                                                                         (array sub_ok (marked_array false subs)))))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (array true (marked_symbol false (.marked_symbol_value x)))
                                                                (array false (fail_f x))))
              (true               (array true x))
        )
    ))
    (try_unval_array (lambda (x) (foldl (dlambda ((ok a) x) (dlet (((nok p) (try_unval x (lambda (_) nil))))
                                                                   (array (and ok nok) (concat a (array p)))))
                                        (array true (array))
                                        x)))

    (ensure_val (rec-lambda recurse (x)
        (cond ((marked_array? x)  (marked_array true (map recurse (.marked_array_values x))))
              ((marked_symbol? x) (marked_symbol true (.marked_symbol_value x)))
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

    ; TODO: make this check for stop envs using de Bruijn indicies
    (contains_symbols (rec-lambda recurse (stop_envs symbols x) (cond
              ((val? x)           false)
              ((marked_symbol? x) (let* ((r (in_array (.marked_symbol_value x) symbols))
                                         (_ (if r (println "!!! contains symbols found " x " in symbols " symbols))))
                                        r))
              ((marked_array? x)  (foldl (lambda (a x) (or a (recurse stop_envs symbols x))) false (.marked_array_values x)))
              ((comb? x)          (dlet (((wrap_level de? se variadic params body) (.comb x)))
                                      (or (recurse stop_envs symbols se) (recurse stop_envs (filter (lambda (y) (not (or (= de? y) (in_array y params)))) symbols) body))))

              ((prim_comb? x)    false)
              ((marked_env? x)   (let ((inner (.env_marked x)))
                                     (cond ((in_array x stop_envs)                                                                       false)
                                           ((foldl (lambda (a x) (or a (recurse stop_envs symbols (idx x 1)))) false (slice inner 0 -2)) true)
                                           ((idx inner -1)                                                                               (recurse stop_envs symbols (idx inner -1)))
                                           (true                                                                                         false))))
              (true              (error (str "Something odd passed to contains_symbols " x)))
    )))

    (is_all_values (lambda (evaled_params) (foldl (lambda (a x) (and a (not (later? x)))) true evaled_params)))

    ; * TODO: allowing envs to be shead mif they're not used.
    (shift_envs (rec-lambda recurse (cutoff d x) (cond
        ((val? x)            (array true x))
        ((marked_env? x)     (dlet (((is_real dbi meat) (.marked_env x))
                                    ((nmeat_ok nmeat) (foldl (dlambda ((ok r) (k v)) (dlet (((tok tv) (recurse cutoff d v))) (array (and ok tok) (concat r (array (array k tv)))))) (array true (array)) (slice meat 0 -2)))
                                    ((nupper_ok nupper) (mif (idx meat -1) (recurse cutoff d (idx meat -1)) (array true nil)))
                                    (ndbi (cond ((nil? dbi)      nil)
                                                ((>= dbi cutoff) (+ dbi d))
                                                (true            dbi)))
                              ) (array (and nmeat_ok nupper_ok (or is_real (and ndbi (>= ndbi 0)))) (marked_env is_real ndbi (concat nmeat (array nupper))))))
        ((comb? x)           (dlet (((wrap_level de? se variadic params body) (.comb x))
                                    ((se_ok nse) (recurse cutoff d se))
                                    ((body_ok nbody) (recurse (+ cutoff 1) d body))
                                ) (array (and se_ok body_ok) (marked_comb wrap_level de? nse variadic params nbody))))
        ((prim_comb? x)      (array true x))
        ((marked_symbol? x)  (array true x))
        ((marked_array? x)   (dlet (((insides_ok insides) (foldl (dlambda ((ok r) tx) (dlet (((tok tr) (recurse cutoff d tx))) (array (and ok tok) (concat r (array tr))))) (array true (array)) (.marked_array_values x))))
                                   (array insides_ok (marked_array (.marked_array_is_val x) insides))))
        (true                (error (str "impossible shift_envs value " x)))
    )))
    (increment_envs (lambda (x) (idx (shift_envs 0  1 x) 1)))
    (decrement_envs (lambda (x)      (shift_envs 0 -1 x)))

    ; TODO: instead of returning the later symbols, we could create a new value of a new type
    ; ['ref de_bruijn_index_of_env index_into_env] or somesuch. Could really simplify
    ; compiling, and I think make partial-eval more efficient. More accurate closes_over analysis too, I think
    (make_tmp_inner_env (lambda (params de? de)
            (marked_env false 0 (concat (map (lambda (p) (array p (marked_symbol false p))) params) (mif (= nil de?) (array) (array (array de? (marked_symbol false de?)) )) (array (increment_envs de))))))


    (partial_eval_helper (rec-lambda recurse (x env env_stack indent)
        (cond   ((val? x)            x)
                ((marked_env? x)     (let ((dbi (.marked_env_idx x)))
                                          (mif dbi (let* ((new_env (idx env_stack dbi))
                                                         (ndbi (.marked_env_idx new_env))
                                                         (_ (mif (!= 0 ndbi) (error (str_strip "new env with non-zero dbis " x))))
                                                         (_ (println (str_strip "replacing " x) (str_strip " with " new_env)))
                                                        )
                                                        (mif (= 0 dbi) new_env (idx (shift_envs 0 dbi new_env) 1)))
                                                  x)))

                ((comb? x)           (dlet (((wrap_level de? se variadic params body) (.comb x)))
                                        (mif (or (and (not (marked_env_real? env)) (not (marked_env_real? se)))   ; both aren't real, re-evaluation of creation site
                                                 (and      (marked_env_real? env)  (not (marked_env_real? se))))  ; new env real, but se isn't - creation!
                                             (let ((inner_env (make_tmp_inner_env params de? env)))
                                                  (marked_comb wrap_level de? env variadic params (recurse body inner_env (cons inner_env env_stack) (+ indent 1))))
                                             x)))
                ((prim_comb? x)      x)
                ((marked_symbol? x)  (mif (.marked_symbol_is_val x) x
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
                                                                                        (mif (!= 0 wrap)
                                                                                            (dlet ((pre_evaled (map rp_eval cparams))
                                                                                                   ((ok unval_params) (try_unval_array pre_evaled)))
                                                                                                 (mif (not ok) (array ok nil)
                                                                                                    (let* ((evaled_params (map rp_eval unval_params)))
                                                                                                          (param-recurse (- wrap 1) evaled_params))))
                                                                                            (array true cparams))
                                                                                    ) wrap_level ensure_val_params))
                                                                                 (ok_and_non_later (and ok (is_all_values appropriatly_evaled_params)))
                                                                                 ) (mif (not ok_and_non_later) (marked_array false (cons comb (mif (> wrap_level 0) (map rp_eval literal_params)
                                                                                                                                                     literal_params)))
                                                                                 (dlet (
                                                                                 (final_params (mif variadic (concat (slice appropriatly_evaled_params 0 (- (len params) 1))
                                                                                                                   (array (marked_array true (slice appropriatly_evaled_params (- (len params) 1) -1))))
                                                                                                           appropriatly_evaled_params))
                                                                                 ((de_real de_entry) (mif (!= nil de?) (array (marked_env_real? env) (array (array de? (increment_envs env) ) ) )
                                                                                                                     (array true (array))))
                                                                                 (inner_env (marked_env (and de_real (marked_env_real? se)) 0 (concat (zip params (map (lambda (x) (increment_envs x)) final_params)) de_entry (array (increment_envs se)))))
                                                                                 (_ (print_strip (indent_str indent) " with inner_env is " inner_env))
                                                                                 (_ (print_strip (indent_str indent) "going to eval " body))

                                                                                 (tmp_func_result (recurse body inner_env (cons inner_env env_stack) (+ 1 indent)))
                                                                                 (_ (print_strip (indent_str indent) "evaled result of function call  is " tmp_func_result))
                                                                                 ((able_to_sub_env func_result) (decrement_envs tmp_func_result))
                                                                                 (result_is_later (later? func_result))
                                                                                 (_ (print_strip (indent_str indent) "success? " able_to_sub_env " decremented result of function call  is " tmp_func_result))
                                                                                 (stop_envs ((rec-lambda ser (a e) (mif e (ser (cons e a) (idx (.env_marked e) -1)) a)) (array) se))
                                                                                 (result_closes_over (contains_symbols stop_envs (concat params (mif de? (array de?) (array))) func_result))
                                                                                 (_ (println (indent_str indent) "func call able_to_sub: " able_to_sub_env " result is later? " result_is_later " and result_closes_over " result_closes_over))
                                                                                 ; This could be improved to a specialized version of the function
                                                                                 ; just by re-wrapping it in a comb instead mif we wanted.
                                                                                 ; Something to think about!
                                                                                 (result (mif (or (not able_to_sub_env) (and result_is_later result_closes_over))
                                                                                                (marked_array false (cons comb (mif (> wrap_level 0) (map rp_eval literal_params)
                                                                                                                                                     literal_params)))
                                                                                                func_result))
                                                                             ) result))))
                                                           ((later? comb)    (marked_array false (cons comb literal_params)))
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
            (mif (is_all_values evaled_params) (mark (apply actual_function (map strip evaled_params)))
                                              (marked_array false (cons (marked_prim_comb recurse f_sym) evaled_params))))))
        ) (array f_sym (marked_prim_comb handler f_sym)))))

    (give_up_eval_params_inner (lambda (f_sym actual_function) (let* (
        (handler (rec-lambda recurse (de env_stack params indent) (let (
                ;_ (println "partial_evaling params in give_up_eval_params for " f_sym " is " params)
                (evaled_params (map (lambda (p) (partial_eval_helper p de env_stack (+ 1 indent))) params))
            )
            (marked_array false (cons (marked_prim_comb recurse f_sym) evaled_params)))))
        ) (array f_sym (marked_prim_comb handler f_sym)))))


    (root_marked_env (marked_env true nil (array

        (array 'vau (marked_prim_comb (rec-lambda recurse (de env_stack params indent) (dlet (
            (mde?         (mif (= 3 (len params)) (idx params 0) nil))
            (vau_mde?     (mif (= nil mde?) (array) (array mde?)))
            (_ (print "mde? is " mde?))
            (_ (print "\tmde? if " (mif mde? #t #f)))
            (de?          (mif mde? (.marked_symbol_value mde?) nil))
            (_ (print "de? is " de?))
            (vau_de?      (mif (= nil de?) (array) (array de?)))
            (raw_marked_params (mif (= nil de?) (idx params 0) (idx params 1)))
            (raw_params (map (lambda (x) (mif (not (marked_symbol? x)) (error (str "not a marked symbol " x))
                                            (.marked_symbol_value x))) (.marked_array_values raw_marked_params)))

            ((variadic vau_params)  (foldl (dlambda ((v a) x) (mif (= x '&) (array true a) (array v (concat a (array x))))) (array false (array)) raw_params))
            (body        (mif (= nil de?) (idx params 1) (idx params 2)))
            (inner_env (make_tmp_inner_env vau_params de? de))
            (_ (print_strip (indent_str indent) "in vau, evaluating body with 'later params - " body))
            (pe_body (partial_eval_helper body inner_env (cons inner_env env_stack) (+ 1 indent)))
            (_ (print_strip (indent_str indent) "in vau, result of evaluating body was " pe_body))
        ) (marked_comb 0 de? de variadic vau_params pe_body)
        )) 'vau_fake_real))

        (array 'wrap (marked_prim_comb (parameters_evaled_proxy 0 (dlambda (recurse de env_stack (evaled) indent)
              (mif (comb? evaled) (dlet (((wrap_level de? se variadic params body) (.comb evaled))
                                        (wrapped_marked_fun (marked_comb (+ 1 wrap_level) de? se variadic params body))
                                       ) wrapped_marked_fun)
                                 (marked_array false (array (marked_prim_comb recurse 'wrap_fake_real) evaled))))
        ) 'wrap_fake_real))

        (array 'unwrap (marked_prim_comb (parameters_evaled_proxy 0 (dlambda (recurse de env_stack (evaled) indent)
             (mif (comb? evaled) (dlet (((wrap_level de? se variadic params body) (.comb evaled))
                                       (unwrapped_marked_fun (marked_comb (- wrap_level 1) de? se variadic params body))
                                      ) unwrapped_marked_fun)
                                 (marked_array false (array (marked_prim_comb recurse 'unwrap_fake_real) evaled))))
        ) 'unwrap_fake_real))

        (array 'eval (marked_prim_comb (rec-lambda recurse (de env_stack params indent) (dlet (
            (self (marked_prim_comb recurse 'eval_fake_real))
            (eval_env (mif (= 2 (len params)) (partial_eval_helper (idx params 1) de env_stack (+ 1 indent))
                                             de))
            (eval_env_v (mif (= 2 (len params)) (array eval_env) (array)))
         ) (mif (not (marked_env? eval_env)) (marked_array false (cons self params))
                                            (dlet (
            (_ (print_strip (indent_str indent) " partial_evaling_body the first time " (idx params 0)))
            (body1 (partial_eval_helper (idx params 0) de env_stack (+ 1 indent)))
            (_ (print_strip (indent_str indent) "after first eval of param " body1))

            ; With this, we don't actually fail as this is always a legitimate uneval
            (fail_handler (lambda (failed) (marked_array false (concat (array self failed) eval_env_v))))
            ((ok unval_body) (try_unval body1 fail_handler))
            (self_fallback (fail_handler body1))
            (_ (print_strip (indent_str indent) "partial_evaling body for the second time in eval " unval_body))
            (body2 (mif (= self_fallback unval_body) self_fallback (partial_eval_helper unval_body eval_env env_stack (+ 1 indent))))
            (_ (print_strip (indent_str indent) "and body2 is " body2))
            ) body2))
        )) 'eval_fake_real))

        (array 'cond (marked_prim_comb (rec-lambda recurse (de env_stack params indent)
            (mif (!= 0 (% (len params) 2)) (error (str "partial eval cond with odd params " params))
                ((rec-lambda recurse_inner (i so_far)
                                          (let* ((evaled_cond (partial_eval_helper (idx params i) de env_stack (+ 1 indent)))
                                                 (_ (print (indent_str indent) "in cond cond " (idx params i) " evaluated to " evaled_cond)))
                                          (cond ((later? evaled_cond)            (recurse_inner (+ 2 i) (concat so_far (array evaled_cond
                                                                                                                              (partial_eval_helper (idx params (+ i 1)) de env_stack (+ 1 indent))))))
                                                ((false? evaled_cond)            (recurse_inner (+ 2 i) so_far))
                                                ((= (len params) i)              (marked_array false (cons (marked_prim_comb recurse 'cond_fake_real) so_far)))
                                                (true                            (let ((evaled_body (partial_eval_helper (idx params (+ 1 i)) de env_stack (+ 1 indent))))
                                                                                      (mif (!= (len so_far) 0) (marked_array false (cons (marked_prim_comb recurse 'cond_fake_real) (concat so_far (array evaled_cond evaled_body))))
                                                                                                                evaled_body)))
                 ))) 0 (array))
            )
        ) 'cond_fake_real))

        (needs_params_val_lambda symbol?)
        (needs_params_val_lambda int?)
        (needs_params_val_lambda string?)

        (array 'combiner? (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_param) indent)
            (cond ((comb? evaled_param)          (marked_val true))
                  ((prim_comb? evaled_param)     (marked_val true))
                  ((later? evaled_param)         (marked_array false (array (marked_prim_comb recurse 'combinerp_fake_real) evaled_param)))
                  (true                          (marked_val false))
            )
        )) 'combinerp_fake_real))
        (array 'env? (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_param) indent)
            (cond ((marked_env? evaled_param)    (marked_val true))
                  ((later? evaled_param)         (marked_array false (array (marked_prim_comb recurse 'envp_fake_real) evaled_param)))
                  (true                          (marked_val false))
            )
        )) 'envp_fake_real))
        (needs_params_val_lambda nil?)
        (needs_params_val_lambda bool?)
        (needs_params_val_lambda str-to-symbol)
        (needs_params_val_lambda get-text)

        (array 'array? (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_param) indent)
            (cond
                  ((later? evaled_param)         (marked_array false (array (marked_prim_comb recurse 'arrayp_fake_real) evaled_param)))
                  ((marked_array? evaled_param)  (marked_val true))
                  (true                          (marked_val false))
            )
        )) 'arrayp_fake_real))

        ; This one's sad, might need to come back to it.
        ; We need to be able to differentiate between half-and-half arrays
        ; for when we ensure_params_values or whatever, because that's super wrong
        (array 'array (marked_prim_comb (parameters_evaled_proxy nil (lambda (recurse de env_stack evaled_params indent)
                                                (mif (is_all_values evaled_params) (marked_array true evaled_params)
                                                                                   (marked_array false (cons (marked_prim_comb recurse 'array_fake_real) evaled_params)))
        )) 'array_fake_real))
        (array 'len (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_param) indent)
            (cond ((later? evaled_param)         (marked_array false (array (marked_prim_comb recurse 'len_fake_real) evaled_param)))
                  ((marked_array? evaled_param)  (marked_val (len (.marked_array_values evaled_param))))
                  (true                          (error (str "bad type to len " evaled_param)))
            )
        )) 'len_fake_real))
        (array 'idx (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_array evaled_idx) indent)
            (cond ((and (val? evaled_idx) (marked_array? evaled_array) (.marked_array_is_val evaled_array)) (idx (.marked_array_values evaled_array) (.val evaled_idx)))
                  (true                                                                                     (marked_array false (array (marked_prim_comb recurse 'idx_fake_real) evaled_array evaled_idx)))
            )
        )) 'idx_fake_real))
        (array 'slice (marked_prim_comb (parameters_evaled_proxy nil (dlambda (recurse de env_stack (evaled_array evaled_begin evaled_end) indent)
            (cond ((and (val? evaled_begin) (val? evaled_end) (marked_array? evaled_array) (.marked_array_is_val evaled_array))
                            (marked_array true (slice (.marked_array_values evaled_array) (.val evaled_begin) (.val evaled_end))))
                  (true     (marked_array false (array (marked_prim_comb recurse 'slice_fake_real) evaled_array evaled_idx evaled_begin evaled_end)))
            )
        )) 'slice_fake_real))
        (array 'concat (marked_prim_comb (parameters_evaled_proxy nil (lambda (recurse de env_stack evaled_params indent)
            (cond ((foldl (lambda (a x) (and a (and (marked_array? x) (.marked_array_is_val x)))) true evaled_params) (marked_array true (lapply concat (map (lambda (x)
                                                                                                                                                             (.marked_array_values x))
                                                                                                                                                            evaled_params))))
                  (true                                                                                               (marked_array false (cons (marked_prim_comb recurse 'concat_fake_real) evaled_params)))
            )
        )) 'concat_fake_real))

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

        ; these could both be extended to eliminate other known true values except for the end and vice-versa
        (array 'and (marked_prim_comb (parameters_evaled_proxy nil (lambda (recurse de env_stack evaled_params indent)
            ((rec-lambda inner_recurse (i)
                                      (cond ((= i (- (len evaled_params) 1))          (idx evaled_params i))
                                            ((later? (idx evaled_params i))           (marked_array false (cons (marked_prim_comb recurse 'and_fake_real) (slice evaled_params i -1))))
                                            ((false? (idx evaled_params i))           (idx evaled_params i))
                                            (true                                     (inner_recurse (+ 1 i))))
             ) 0)
        )) 'and_fake_real))
        ; see above for improvement
        (array 'or (marked_prim_comb (parameters_evaled_proxy nil (lambda (recurse de env_stack evaled_params indent)
            ((rec-lambda inner_recurse (i)
                                      (cond ((= i (- (len evaled_params) 1))          (idx evaled_params i))
                                            ((later? (idx evaled_params i))           (marked_array false (cons (marked_prim_comb recurse 'or_fake_real) (slice evaled_params i -1))))
                                            ((false? (idx evaled_params i))           (recurse (+ 1 i)))
                                            (true                                     (idx evaled_params i)))
             ) 0)
        )) 'or_fake_real))
        ; should make not a built in and then do here
        ; OR not - I think it will actually lower correctly partially evaled

        ;(needs_params_val_lambda pr-str)
        (needs_params_val_lambda str)
        ;(needs_params_val_lambda prn)
        (give_up_eval_params println)
        ; really do need to figure out mif we want to keep meta, and add it mif so
        ;(give_up_eval_params meta)
        ;(give_up_eval_params with-meta)
        ; mif we want to get fancy, we could do error/recover too
        ;(give_up_eval_params error)
        ;(give_up_eval_params recover)
        (needs_params_val_lambda read-string)
        ;(give_up_eval_params slurp)
        ;(give_up_eval_params get_line)
        ;(give_up_eval_params write_file)
        (array 'empty_env (marked_env true nil (array nil)))

        nil
    )))


    (partial_eval (lambda (x) (partial_eval_helper (mark x) root_marked_env (array) 0)))

    ;; WASM

    (bor  bitwise-ior)
    (band bitwise-and)
    (<<   arithmetic-shift)
    (>>   (lambda (a b) (arithmetic-shift a (- b))))

    ; Vectors and Values
    ; Bytes encode themselves
    (encode_LEB128_helper (rec-lambda recurse (allow_neg x)
        (cond ((and allow_neg (< x #x80)) (array x))
                             ((< x #x40)  (array x))
              (true       (cons (bor (band x #x7F) #x80) (recurse true (>> x 7)))))
    ))
    (encode_u_LEB128    (lambda (x) (encode_LEB128_helper true x)))
    (encode_s8_LEB128   (lambda (x) (encode_LEB128_helper (< x 0) (band x #xFF))))
    (encode_s32_LEB128  (lambda (x) (encode_LEB128_helper (< x 0) (band x #xFFFFFFFF))))
    (encode_s33_LEB128  (lambda (x) (encode_LEB128_helper (< x 0) (band x #x1FFFFFFFF))))
    (encode_s64_LEB128  (lambda (x) (encode_LEB128_helper (< x 0) (band x #xFFFFFFFFFFFFFFFF))))
    (encode_vector (lambda (enc v)
        (concat (encode_u_LEB128 (len v)) (flat_map enc v) )
    ))
    (encode_floating_point (lambda (x) (error "unimplemented")))
    (encode_name (lambda (name)
        (encode_vector (lambda (x) (array x)) (map char->integer (string->list name)))
    ))
    (encode_bytes encode_name)

    (encode_limits (lambda (x)
       (cond ((= 1 (len x)) (concat (array #x00) (encode_u_LEB128 (idx x 0))))
             ((= 2 (len x)) (concat (array #x01) (encode_u_LEB128 (idx x 0)) (encode_u_LEB128 (idx x 1))))
             (true          (error "trying to encode bad limits")))
    ))
    (encode_number_type (lambda (x)
        (cond  ((= x 'i32) (array #x7F))
               ((= x 'i64) (array #x7E))
               ((= x 'f32) (array #x7D))
               ((= x 'f64) (array #x7C))
               (true       (error (str "bad number type " x))))
    ))
    (encode_valtype (lambda (x)
        ; we don't handle reference types yet
        (encode_number_type x)
    ))
    (encode_result_type (lambda (x)
        (encode_vector encode_valtype x)
    ))
    (encode_function_type (lambda (x)
        (concat (array #x60) (encode_result_type (idx x 0))
                             (encode_result_type (idx x 1)))
    ))
    (encode_ref_type (lambda (t) (cond ((= t 'funcref)   (array #x70))
                                       ((= t 'externref) (array #x6F))
                                       (true             (error (str "Bad ref type " t))))))
    (encode_type_section (lambda (x)
        (let (
            (encoded (encode_vector encode_function_type x))
        ) (concat (array #x01) (encode_u_LEB128 (len encoded)) encoded ))
    ))
    (encode_import (lambda (import)
        (dlet (
            ((mod_name name type idx) import)
        ) (concat (encode_name mod_name)
                  (encode_name name)
                  (cond ((= type 'func)   (concat (array #x00) (encode_u_LEB128 idx)))
                        ((= type 'table)  (concat (array #x01) (error "can't encode table type")))
                        ((= type 'memory) (concat (array #x02) (error "can't encode memory type")))
                        ((= type 'global) (concat (array #x03) (error "can't encode global type")))
                        (true                                  (error (str "bad import type" type)))))
        )
    ))
    (encode_import_section (lambda (x)
        (let (
            (encoded (encode_vector encode_import x))
        ) (concat (array #x02) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    (encode_table_type (lambda (t) (concat (encode_ref_type (idx t 0)) (encode_limits (idx t 1)))))

    (encode_table_section (lambda (x)
        (let (
            (encoded (encode_vector encode_table_type x))
        ) (concat (array #x04) (encode_u_LEB128 (len encoded)) encoded ))
    ))
    (encode_memory_section (lambda (x)
        (let (
            (encoded (encode_vector encode_limits x))
        ) (concat (array #x05) (encode_u_LEB128 (len encoded)) encoded ))
    ))
    (encode_export (lambda (export)
        (dlet (
            ((name type idx) export)
        ) (concat (encode_name name)
                  (cond ((= type 'func)   (array #x00))
                        ((= type 'table)  (array #x01))
                        ((= type 'memory) (array #x02))
                        ((= type 'global) (array #x03))
                        (true             (error "bad export type")))
                  (encode_u_LEB128 idx)
        ))
    ))
    (encode_export_section (lambda (x)
        (let (
            (_ (print "encoding element " x))
            (encoded (encode_vector encode_export x))
            (_ (print "donex"))
        ) (concat (array #x07) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    (encode_start_section (lambda (x)
        (cond ((= 0 (len x)) (array))
              ((= 1 (len x)) (let ((encoded (encode_u_LEB128 (idx x 0)))) (concat (array #x08) (encode_u_LEB128 (len encoded)) encoded )))
              (true          (error (str "bad lenbgth for start section " (len x) " was " x))))
    ))

    (encode_function_section (lambda (x)
        (let* (                                     ; nil functions are placeholders for improted functions
            (_ (println "encoding function section " x))
            (filtered (filter (lambda (i) (!= nil i)) x))
            (_ (println "post filtered " filtered))
            (encoded (encode_vector encode_u_LEB128 filtered))
        ) (concat (array #x03) (encode_u_LEB128 (len encoded)) encoded ))
    ))
    (encode_blocktype (lambda (type) (cond ((symbol? type)  (encode_valtype type))
                                           ((= (array) type)     (array #x40)) ; empty type
                                           (true            (encode_s33_LEB128 typ))
                                   )))

    (encode_ins (rec-lambda recurse (ins)
        (let (
            (op (idx ins 0))
        ) (cond ((= op 'unreachable)            (array #x00))
                ((= op 'nop)                    (array #x01))
                ((= op 'block)          (concat (array #x02) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (array #x0B)))
                ((= op 'loop)           (concat (array #x03) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (array #x0B)))
                ((= op 'if)             (concat (array #x04) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (if (!= 3 (len ins)) (concat (array #x05) (flat_map recurse (idx ins 3)))
                                                                                                                                          (array ))     (array #x0B)))
                ((= op 'br)             (concat (array #x0C) (encode_u_LEB128 (idx ins 1))))
                ((= op 'br_if)          (concat (array #x0D) (encode_u_LEB128 (idx ins 1))))
                ;...
                ((= op 'return)                 (array #x0F))
                ((= op 'call)           (concat (array #x10) (encode_u_LEB128 (idx ins 1))))
                ; call_indirect
                ; skipping a bunch
                ; Parametric Instructions
                ((= op 'drop)                   (array #x1A))
                ; skip
                ; Variable Instructions
                ((= op 'local.get)      (concat (array #x20) (encode_u_LEB128 (idx ins 1))))
                ((= op 'local.set)      (concat (array #x21) (encode_u_LEB128 (idx ins 1))))
                ((= op 'local.tee)      (concat (array #x22) (encode_u_LEB128 (idx ins 1))))
                ((= op 'global.get)     (concat (array #x23) (encode_u_LEB128 (idx ins 1))))
                ((= op 'global.set)     (concat (array #x24) (encode_u_LEB128 (idx ins 1))))
                ; table
                ; memory
                ((= op 'i32.load)       (concat (array #x28) (encode_u_LEB128 (idx ins 1)) (encode_u_LEB128 (idx ins 2))))
                ((= op 'i64.load)       (concat (array #x29) (encode_u_LEB128 (idx ins 1)) (encode_u_LEB128 (idx ins 2))))
                ((= op 'i32.store)      (concat (array #x36) (encode_u_LEB128 (idx ins 1)) (encode_u_LEB128 (idx ins 2))))
                ((= op 'i64.store)      (concat (array #x37) (encode_u_LEB128 (idx ins 1)) (encode_u_LEB128 (idx ins 2))))
                ; Numeric Instructions
                ((= op 'i32.const)      (concat (array #x41) (encode_s32_LEB128 (idx ins 1))))
                ((= op 'i64.const)      (concat (array #x42) (encode_s64_LEB128 (idx ins 1))))
                ; skip
                ((= op 'i32.add)                (array #x6A))
        ))
    ))

    (encode_expr (lambda (expr) (concat (flat_map encode_ins expr) (array #x0B))))
    (encode_code (lambda (x)
        (dlet (
            ((locals body) x)
            (enc_locals (encode_vector (lambda (loc)
                                    (concat (encode_u_LEB128 (idx loc 0)) (encode_valtype (idx loc 1)))) locals))
            (enc_expr (encode_expr body))
            (code_bytes (concat enc_locals enc_expr))
        ) (concat (encode_u_LEB128 (len code_bytes)) code_bytes))
    ))
    (encode_code_section (lambda (x)
        (let (
            (encoded (encode_vector encode_code x))
        ) (concat (array #x0A) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    (encode_global_type (lambda (t) (concat (encode_valtype (idx t 0)) (cond ((= (idx t 1) 'const) (array #x00))
                                                                             ((= (idx t 1) 'mut)   (array #x01))
                                                                             (true                 (error (str "bad mutablity " (idx t 1))))))))
    (encode_global_section (lambda (global_section)
        (let (
            (encoded (encode_vector (lambda (x) (concat (encode_global_type (idx x 0)) (encode_expr (idx x 1)))) global_section))
        ) (concat (array #x06) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    ; only supporting one type of element section for now, active funcrefs with offset
    (encode_element (lambda (x) (concat (array #x00) (encode_expr (idx x 0)) (encode_vector encode_u_LEB128 (idx x 1)))))
    (encode_element_section (lambda (x)
        (let (
            (_ (print "encoding element " x))
            (encoded (encode_vector encode_element x))
            (_ (print "donex"))
        ) (concat (array #x09) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    (encode_data (lambda (data) (cond ((= 2 (len data)) (concat (array #x00) (encode_expr (idx data 0)) (encode_bytes (idx data 1))))
                                      ((= 1 (len data)) (concat (array #x01) (encode_bytes (idx data 0))))
                                      ((= 3 (len data)) (concat (array #x02) (encode_u_LEB128 (idx data 0)) (encode_expr (idx data 1)) (encode_bytes (idx data 2))))
                                      (true             (error (str "bad data" data))))))
    (encode_data_section (lambda (x)
        (let (
            (encoded (encode_vector encode_data x))
        ) (concat (array #x0B) (encode_u_LEB128 (len encoded)) encoded ))
    ))

    (wasm_to_binary (lambda (wasm_code)
        (dlet (
            ((type_section import_section function_section table_section memory_section global_section export_section start_section element_section code_section data_section) wasm_code)
            (_ (println "type_section" type_section "import_section" import_section "function_section" function_section "memory_section" memory_section "global_section" global_section "export_section" export_section "start_section" start_section "element_section" element_section "code_section" code_section "data_section" data_section))
            (magic    (array #x00 #x61 #x73 #x6D ))
            (version  (array #x01 #x00 #x00 #x00 ))
            (type     (encode_type_section type_section))
            (import   (encode_import_section import_section))
            (function (encode_function_section function_section))
            (table    (encode_table_section table_section))
            (memory   (encode_memory_section memory_section))
            (global   (encode_global_section global_section))
            (export   (encode_export_section export_section))
            (start    (encode_start_section start_section))
            (elem     (encode_element_section element_section))
            (code     (encode_code_section code_section))
            (data     (encode_data_section data_section))
            ;data_count (let (body (encode_u_LEB128 (len data_section))) (concat (array #x0C) (encode_u_LEB128 (len body)) body))
            (data_count (array))
        ) (concat magic version type import function table memory global export data_count start elem code data))
    ))

    (module (lambda args (let (
        (helper (rec-lambda recurse (entries i name_dict type import function table memory global export start elem code data)
            (if (= i (len entries)) (array  type import function table memory global export start elem code data)
                (dlet (
                    ((n_d t im f ta m g e s elm c d) ((idx entries i) name_dict type import function table memory global export start elem code data))
                 ) (recurse entries (+ i 1) n_d t im f ta m g e s elm c d)))))
    ) (helper args 0 empty_dict (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array )))))

    (table (lambda (idx_name . limits_type) (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len table)) type import function (concat table (array (array (idx limits_type -1) (slice limits_type 0 -2) ))) memory global export start elem code data ))))

    (memory (lambda (idx_name . limits) (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len memory)) type import function table (concat memory (array limits)) global export start elem code data ))))

    (func (lambda (name . inside) (lambda (name_dict type import function table memory global export start elem code data)
        (dlet (
            (_ (print "ok, doing a func: " name " with inside " inside))
            ((params result locals body) ((rec-lambda recurse (i pe re)
                                            (cond ((and (= nil pe) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'param (idx (idx inside i) 0)))
                                                             (recurse (+ i 1) pe re))
                                                  ((and (= nil pe) (= nil re) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'result (idx (idx inside i) 0)))
                                                            ; only one result possible
                                                             (recurse (+ i 1) i (+ i 1)))
                                                  ((and (= nil re) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'result (idx (idx inside i) 0)))
                                                            ; only one result possible
                                                             (recurse (+ i 1) pe (+ i 1)))
                                                  ((and            (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'local (idx (idx inside i) 0)))
                                                             (recurse (+ i 1) pe re))
                                                  (true      (array (slice inside 0 (or (!= nil pe) 0)) (slice inside (or (!= nil pe) 0) (or (!= nil re) (!= nil pe) 0)) (slice inside (or (!= nil re) (!= nil pe) 0) i) (slice inside i -1) ))
                                            )
                                        ) 0 nil nil))
            (result (if (!= 0 (len result)) (idx result 0)
                                           result))
            (_ (println "params " params " result " result " locals " locals " body " body))
            (outer_name_dict (put name_dict name (len function)))
            ((num_params inner_name_dict) (foldl (lambda (a x) (array (+ (idx a 0) 1) (put (idx a 1) (idx x 1) (idx a 0)))) (array 0 outer_name_dict ) params))
            ((num_locals inner_name_dict) (foldl (lambda (a x) (array (+ (idx a 0) 1) (put (idx a 1) (idx x 1) (idx a 0)))) (array num_params inner_name_dict ) locals))
            (_ (println "inner name dict" inner_name_dict))
            (compressed_locals ((rec-lambda recurse (cur_list cur_typ cur_num i)
                (cond ((and (= i (len locals)) (= 0 cur_num)) cur_list)
                           ((= i (len locals))                (concat cur_list (array (array cur_num cur_typ) )))
                      ((= cur_typ (idx (idx locals i) 2))     (recurse cur_list                              cur_typ                (+ 1 cur_num) (+ 1 i)))
                      ((= nil cur_typ)                        (recurse cur_list                              (idx (idx locals i) 2) 1             (+ 1 i)))
                      (true                                   (recurse (concat cur_list (array (array cur_num cur_typ))) (idx (idx locals i) 2) 1             (+ 1 i))))
               ) (array) nil 0 0))
            ;(inner_env (add-dict-to-env de (put inner_name_dict 'depth 0)))
            (_ (println "params: " params " result: " result))
            (our_type (array (map (lambda (x) (idx x 2)) params) result))
            (_ (println "about to get our_code"))
            (our_code (flat_map (lambda (ins) (cond ((array? ins) ins)
                                                    (true        (ins)) ; un-evaled function, bare WAT
                                              ))
                                body))
            (_ (println "resulting code " our_code))
        ) (array
            outer_name_dict
            ; type
            (concat type (array our_type ))
            ; import
            import
            ; function
            (concat function (array (len function) ))
            ; table
            table
            ; memory
            memory
            ; global
            global
            ; export
            export
            ; start
            start
            ; element
            elem
            ; code
            (concat code (array (array compressed_locals our_code ) ))
            ; data
            data
        ))
    )))

    (drop      (lambda ()                                                        (array (array 'drop))))
    (i32.const (lambda (const)                                                   (array (array 'i32.const const))))
    (i64.const (lambda (const)                                                   (array (array 'i64.const const))))
    (local.get (lambda (const)                                                   (array (array 'local.get const))))
    (i32.add   (lambda flatten  (concat (flat_map (lambda (x) x) flatten)        (array (array 'i32.add)))))
    (i32.load  (lambda flatten  (concat (flat_map (lambda (x) x) flatten)        (array (array 'i32.load 2 0)))))
    (i64.load  (lambda flatten  (concat (flat_map (lambda (x) x) flatten)        (array (array 'i64.load 3 0)))))
    (i32.store (lambda flatten  (concat (flat_map (lambda (x) x) flatten)        (array (array 'i32.store 2 0)))))
    (i64.store (lambda flatten  (concat (flat_map (lambda (x) x) flatten)        (array (array 'i64.store 3 0)))))
    (flat_eval_ins (lambda (instructions) (flat_map (lambda (ins) (cond ((array? ins) ins)
                                                                        (true        (ins)))) instructions)))

    (block_like_body (lambda (name inner) (flat_eval_ins inner)))
    (block     (lambda (name . inner)                                            (array (array 'block (array) (block_like_body name inner)))))
    (_loop     (lambda (name . inner)                                            (array (array 'loop  (array) (block_like_body name inner)))))
    (_if       (lambda (name . inner) (dlet (
                                            ((end_idx else_section) (if (= 'else (idx (idx inner -1) 0))      (array -2            (slice (idx inner -1) 1 -1) )
                                                                                                              (array -1             nil )))
                                            ((end_idx then_section) (if (= 'then (idx (idx inner end_idx) 0)) (array (- end_idx 1) (slice (idx inner end_idx) 1 -1) )
                                                                                                              (array (- end_idx 1) (array (idx inner end_idx) ) )))
                                            (flattened (flat_eval_ins (slice inner 0 end_idx)))
                                            (_ (println "flattened " flattened " then_section " then_section " else_section " else_section))
                                            (then_block (block_like_body name then_section))
                                            (else_block (if (!= nil else_section) (array (block_like_body name else_section))
                                                                                  (array)))
                                           ) (concat flattened                  (array (concat (array 'if (array) then_block) else_block))))))

    (br        (lambda (block) (if (int? block)  (array (array 'br block)))))
    (br_if     (lambda (block . flatten)  (let ((rest (flat_eval_ins flatten)))
                                                (concat rest  (array (array 'br_if block))))))
    (call      (lambda (f . flatten)  (concat (flat_map (lambda (x) x) flatten)  (array (array 'call f)))))

    (import (lambda (mod_name name t_idx_typ) (lambda (name_dict type import function table memory global export start elem code data) (dlet (
            (_ (print "t_idx_type " t_idx_typ))
            (_ (if (!= 'func (idx t_idx_typ 0)) (error "only supporting importing functions rn")))
            ((import_type idx_name param_type result_type) t_idx_typ)
            (actual_type_idx (len type))
            (actual_type (array (slice param_type 1 -1) (slice result_type 1 -1) ))
        )
        (array (put name_dict idx_name (len function)) (concat type (array actual_type)) (concat import (array (array mod_name name import_type actual_type_idx) )) (concat function (array nil)) table memory global export start elem code data ))
    )))

    (global (lambda (idx_name global_type expr) (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len global))
          type import function table memory
          (concat global (array (array (if (array? global_type) (reverse global_type) (array global_type 'const)) expr )))
          export start elem code data )
    )))

    (export (lambda (name t_v) (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global
               (concat export (array (array name (idx t_v 0) (get-value name_dict (idx t_v 1)) ) ))
               start elem code data )
    )))

    (start (lambda (name) (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global export (concat start (array (get-value name_dict name))) elem code data )
    )))

    (elem (lambda (offset . entries) (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global export start (concat elem (array (array offset (map (lambda (x) (get-value name_dict x)) entries)))) code data )
    )))

    (data (lambda it (lambda (name_dict type import function table memory global export start elem code data)
                       (array name_dict type import function table memory global export start elem code (concat data (array it))))))

    ;(i32 'i32)
    ;(i64 'i32)
    ;(param  (lambda it (cons 'param  it)))
    ;(result (lambda it (cons 'result it)))

    (then (lambda rest (cons 'then rest)))
    (else (lambda rest (cons 'else rest)))


    (test-all (lambda () (let* (
            (run_test (lambda (s) (let* (
                                    (_ (print "\n\ngoing to partial eval " s))
                                    (result (partial_eval (read-string s)))
                                    (_ (print "result of test \"" s "\" => " (str_strip result)))
                                    (_ (print "with a hash of " (.hash result)))
                                    ) nil)))
        ) (begin
        (print (val? '(val)))
        (print "take 3" (take '(1 2 3 4 5 6 7 8 9 10) 3))
        ; shadowed by wasm
        ;(print "drop 3" (drop '(1 2 3 4 5 6 7 8 9 10) 3))
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

        (print "mif tests")
        (print (mif true 1 2))
        (print (mif false 1 2))
        (print (mif true 1))
        (print (mif false 1))
        (print "mif tests end")

        (print "zip " (zip '(1 2 3) '(4 5 6) '(7 8 9)))

        (print (run_test "(+ 1 2)"))
        (print) (print)
        (print (run_test "(cond false 1 true 2)"))
        (print (run_test "(println 1)"))
        (print (run_test "((vau (x) (+ x 1)) 2)"))


        (print (run_test "(+ 1 2)"))
        (print (run_test "(vau (y) (+ 1 2))"))
        (print (run_test "((vau (y) (+ 1 2)) 4)"))
        (print (run_test "((vau (y) y) 4)"))
        (print (run_test "((vau (y) (+ 13 2 y)) 4)"))
        (print (run_test "((wrap (vau (y) (+ 13 2 y))) (+ 3 4))"))
        (print (run_test "(vau de (y) (+ (eval y de) (+ 1 2)))"))
        (print (run_test "((vau de (y) ((vau dde (z) (+ 1 (eval z dde))) y)) 17)"))

        (print (run_test "(cond false 1 false 2 (+ 1 2) 3 true 1337)"))
        (print (run_test "(vau de (x) (cond false 1 false 2 x 3 true 42))"))
        (print (run_test "(vau de (x) (cond false 1 false 2 3 x true 42))"))

        (print (run_test "(combiner? true)"))
        (print (run_test "(combiner? (vau de (x) x))"))
        (print (run_test "(vau de (x) (combiner? x))"))

        (print (run_test "((vau (x) x) a)"))

        (print (run_test "(env? true)"))
        ; this doesn't partially eval, but it could with a more percise if the marked values were more percise
        (print (run_test "(vau de (x) (env? de))"))
        (print (run_test "(vau de (x) (env? x))"))
        (print (run_test "((vau de (x) (env? de)) 1)"))

        (print (run_test "((wrap (vau (let1) (let1 a 12 (+ a 1)))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_test "((wrap (vau (let1) (let1 a 12 (vau (x) (+ a 1))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_test "((wrap (vau (let1) (let1 a 12 (wrap (vau (x) (+ x a 1)))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_test "((wrap (vau (let1) (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a))))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        (print "\n\nlet 4.3\n\n")
        (print (run_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))
        (print "\n\nlet 4.7\n\n")
        (print (run_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ x a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))

        (print "\n\nlet 5\n\n")
        (print (run_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ x a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        (print "\n\nlambda 1\n\n")
        (print (run_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (lambda (x) x)
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print "\n\nlambda 2\n\n")
        (print (run_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (let1 a 12
                                         (lambda (x) (+ a x)))
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print "\n\nlambda 3\n\n")
        (print (run_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (let1 a 12
                                         (lambda (x) (let1 b (+ a x)
                                                             (+ a x b))))
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        (print (run_test "(array 1 2 3 4 5)"))
        (print (run_test "((wrap (vau (a & rest) rest)) 1 2 3 4 5)"))

        (print "\n\nrecursion test\n\n")
        (print (run_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
                                                                                     true     1                               )) 5)
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        (print "\n\nlambda recursion test\n\n")
        (print (run_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (lambda (n) ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
                                                                                     true     1                               )) n))
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (let* (
            ;(output (wasm_to_binary (module)))
            (output (wasm_to_binary (module
                  (import "wasi_unstable" "path_open"
                          '(func $path_open  (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                             (result i32)))
                  (import "wasi_unstable" "fd_prestat_dir_name"
                          '(func $fd_prestat_dir_name  (param i32 i32 i32)
                                           (result i32)))
                  (import "wasi_unstable" "fd_read"
                          '(func $fd_read  (param i32 i32 i32 i32)
                                           (result i32)))
                  (import "wasi_unstable" "fd_write"
                          '(func $fd_write (param i32 i32 i32 i32)
                                           (result i32)))
                  (memory '$mem 1)
                  (global '$gi 'i32       (i32.const 8))
                  (global '$gb '(mut i64) (i64.const 9))
                  (table  '$tab 2 'funcref)
                  (data (i32.const 16) "HellH") ;; adder to put, then data


                  (func '$start
                        (i32.store (i32.const 8) (i32.const 16))  ;; adder of data
                        (i32.store (i32.const 12) (i32.const 5)) ;; len of data
                        ;; open file
                        (call 0 ;$path_open
                              (i32.const 3) ;; file descriptor
                              (i32.const 0) ;; lookup flags
                              (i32.const 16) ;; path string  *
                              (i32.load (i32.const 12)) ;; path string  len
                              (i32.const 1) ;; o flags
                              (i64.const 66) ;; base rights
                              (i64.const 66) ;; inheriting rights
                              (i32.const 0) ;; fdflags
                              (i32.const 4) ;; opened fd out ptr
                         )
                         drop
                        (block '$a ; 1
                            (block '$b ; 2
                                (br 1 ;$a
                                )
                                (br_if  2 ;$b
                                        (i32.const 3))
                                (_loop '$l ; 3
                                    (br ;$a
                                        1
                                    )
                                    (br ;$l
                                        3
                                    )
                                )
                                (_if '$myif  ; 3
                                    (i32.const 1)
                                    (then
                                        (i32.const 1)
                                        drop
                                        (br ;$b
                                            2
                                        )
                                    )
                                    (else
                                        (br ;$myif
                                            3
                                        )
                                    )
                                )
                                (_if '$another ; 3
                                    (i32.const 1)
                                    (br 2 ;$b
                                    ))
                                (i32.const 1)
                                (_if '$third ; 3
                                    (br ;$b
                                        2
                                    ))
                                (_if '$fourth ; 3
                                    (br 3 ;$fourth
                                    ))
                            )
                        )
                        (call 2; $fd_read
                              (i32.const 0) ;; file descriptor
                              (i32.const 8) ;; *iovs
                              (i32.const 1) ;; iovs_len
                              (i32.const 12) ;; nwritten, overwrite buf len with it
                        )
                        drop

                        ;; print name
                        (call 3; $fd_write
                              (i32.load (i32.const 4)) ;; file descriptor
                              (i32.const 8) ;; *iovs
                              (i32.const 1) ;; iovs_len
                              (i32.const 4) ;; nwritten
                        )
                        drop
                  )

                  (elem (i32.const 0) '$start '$start)
                  (export "memory" '(memory $mem))
                  (export "_start" '(func   $start))
            )))
            (_ (print "to out " output))
            (_ (write_file "./csc_out.wasm" output))
        ) (void))

    ))))

) (test-all))
)
