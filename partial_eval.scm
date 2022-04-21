
; both Gambit and Chez define pretty-print. Chicken doesn't obv
; In Chez, arithmetic-shift is bitwise-arithmetic-shift

; Chicken
;(import (chicken process-context)) (import (chicken port)) (import (chicken io)) (import (chicken bitwise)) (import (chicken string)) (import (r5rs)) (define write_file (lambda (file bytes) (call-with-output-file file (lambda (out) (foldl (lambda (_ o) (write-byte o out)) (void) bytes))))) (define args (command-line-arguments))

; Chez
(define print pretty-print) (define arithmetic-shift bitwise-arithmetic-shift) (define foldl fold-left) (define foldr fold-right) (define write_file (lambda (file bytes) (let* ( (port (open-file-output-port file)) (_ (foldl (lambda (_ o) (put-u8 port o)) (void) bytes)) (_ (close-port port))) '()))) (define args (cdr (command-line)))
;(compile-profile 'source)

; Gambit - Gambit also has a problem with the dlet definition (somehow recursing and making (cdr nil) for (cdr ls)?), even if using the unstable one that didn't break syntax-rules
;(define print pretty-print)

(define-syntax rec-lambda
  (syntax-rules ()
    ((_ name params body) (letrec ((name (lambda params body))) name))))


; Based off of http://www.phyast.pitt.edu/~micheles/scheme/scheme15.html
; many thanks!
(define-syntax dlet
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((() bad)) expr) expr)
    ((_ (((arg1 arg2 ...) lst)) expr)
            (let ((ls lst))
              (dlet ((arg1 (car ls)))
                    (dlet (((arg2 ...) (cdr ls))) expr))))
   ((_ ((name value)) expr) (let ((name value)) expr))
   ((_ ((name value) (n v) ...) expr) (dlet ((name value)) (dlet ((n v) ...) expr)))
))

(define-syntax dlambda
  (syntax-rules ()
    ((_ params body) (lambda fullparams (dlet ((params fullparams)) body)))))

(define-syntax mif
  (syntax-rules ()
    ((_ con then     ) (if (let ((x con)) (and (not (equal? (list) x)) x)) then '()))
    ((_ con then else) (if (let ((x con)) (and (not (equal? (list) x)) x)) then else))))



(define str (lambda args (begin
                     (define mp (open-output-string))
                     ((rec-lambda recurse (x) (if (and x (not (equal? '() x))) (begin (display (car x) mp) (recurse (cdr x))) '())) args)
                     (get-output-string mp))))

(define true_error error)
(define error (lambda args (begin (print "ERROR! About to Error! args are\n") (print (str args)) (apply true_error args))))

; Adapted from https://stackoverflow.com/questions/16335454/reading-from-file-using-scheme WTH
(define (slurp path)
 (list->string (call-with-input-file path
    (lambda (input-port)
         (let loop ((x (read-char input-port)))
                (cond
                        ((eof-object? x) '())
                                (#t (begin (cons x (loop (read-char input-port)))))))))))

(define speed_hack #t)
;(define GLOBAL_MAX 0)

(let* (
      (lapply apply)
      (= equal?)
      (!= (lambda (a b) (not (= a b))))
      (array list)
      (array? list?)
      (concat (lambda args (cond ((equal?  (length args) 0)   (list))
                                 ((list?   (list-ref args 0)) (apply append args))
                                 ((string? (list-ref args 0)) (apply string-append args))
                                 (#t          (error "bad value to concat " (list-ref args 0))))))
      (len    (lambda (x)  (cond ((list? x)   (length x))
                                 ((string? x) (string-length x))
                                 (#t          (error "bad value to len")))))
      (idx (lambda (x i) (cond ((list? x)                  (list-ref x                (if (< i 0) (+ i (len x)) i)))
                               ((string? x) (char->integer (list-ref (string->list x) (if (< i 0) (+ i (len x)) i)))))))
      (false #f)
      (true #t)
      (nil '())
      (str-to-symbol string->symbol)
      (get-text symbol->string)

      (bor  bitwise-ior)
      (band bitwise-and)
      (bxor bitwise-xor)
      (bnot bitwise-not)
      (<<   arithmetic-shift)
      (>>   (lambda (a b) (arithmetic-shift a (- b))))

      (print (lambda args (print (apply str args))))
      (true_str str)
      ;(str (if speed_hack (lambda args "") str))
      (true_print print)
      (print (if speed_hack (lambda x 0) print))
      ;(true_print print)
      (println print)


      (nil? (lambda (x) (= nil x)))
      (bool? (lambda (x) (or (= #t x) (= #f x))))

      (read-string (lambda (s) (read (open-input-string s))))

      (zip (lambda args (apply map list args)))

      (% modulo)
      (int? integer?)
      (str? string?)
      (env? (lambda (x) false))
      (combiner? (lambda (x) false))

      ;; For chicken and Chez
      (drop (rec-lambda recurse (x i) (if (= 0 i) x (recurse (cdr x) (- i 1)))))
      (take (rec-lambda recurse (x i) (if (= 0 i) (array) (cons (car x) (recurse (cdr x) (- i 1))))))
      (slice (lambda (x s e) (let* ( (l (len x))
                                     (s (if (< s 0) (+ s l 1) s))
                                     (e (if (< e 0) (+ e l 1) e))
                                     (t (- e s)) )
                              (if (list? x) (take (drop x s) t)
                                            (list->string (take (drop (string->list x) s) t))))))
      (range   (rec-lambda recurse (a b)
                   (cond ((= a b) nil)
                         ((< a b) (cons a (recurse (+ a 1) b)))
                         (true    (cons a (recurse (- a 1) b)))
      )))
      (filter (rec-lambda recurse (f l) (cond ((nil? l)    nil)
                                              ((f (car l)) (cons (car l) (recurse f (cdr l))))
                                              (true        (recurse f (cdr l))))))

      (flat_map (lambda (f l) ((rec recurse (lambda (f l) (cond
                                                               ((equal? '() l) '())
                                                               (#t      (append (f (car l)) (recurse f (cdr l)))))
                                              )) f l)))

      ;;;;;;;;;;;;;;;;;;
      ; End kludges
      ;;;;;;;;;;;;;;;;;;


      (empty_dict-list (array))
      (put-list (lambda (m k v) (cons (array k v) m)))
      (get-list (lambda (d k) ((rec-lambda recurse (k d len_d i) (cond ((= len_d i)             false)
                                                                       ((= k (idx (idx d i) 0)) (idx d i))
                                                                       (true                    (recurse k d len_d (+ 1 i)))))
                                k d (len d) 0)))

      ;(combine_hash     (lambda (a b) (+ (* 37 a) b)))
      (combine_hash     (lambda (a b) (band #xFFFFFFFFFFFFFF (+ (* 37 a) b))))
      (hash_bool        (lambda (b) (if b 2 3)))
      (hash_num         (lambda (n) (combine_hash 5 n)))
      (hash_string      (lambda (s) (foldl combine_hash 7  (map char->integer (string->list s)))))
      ;(hash_string      (lambda (s) (foldl combine_hash 7  s)))
      ;(hash_string      (lambda (s) (foldl combine_hash 102233  (map char->integer (string->list s)))))

      (empty_dict-tree nil)
      ;(trans-key (lambda (k) (cond ((string? k) (cons (hash_string k) k))
      ;                             ((symbol? k) (cons (hash_string (symbol->string k)) k))
      ;                             (true        (cons k k)))))
      ;(put-helper (rec-lambda put-helper (m k v) (cond ((nil? m)        (cons (list k v) (cons nil                       nil)))
      ;                                                 ((and (= (car k) (caaar m))
      ;                                                       (= (cdr k) (cdaar m)))      (cons (list k v) (cons (cadr m)                  (cddr m))))
      ;                                                 ((< (car k) (caaar m))            (cons (car m)    (cons (put-helper (cadr m) k v) (cddr m))))
      ;                                                 (true                             (cons (car m)    (cons (cadr m)                  (put-helper (cddr m) k v)))))))
      ;(put-tree (lambda (m k v) (put-helper m (trans-key k) v)))
      ;(get-helper (rec-lambda get-helper (m k) (cond ((nil? m)                     false)
      ;                                               ((and (= (car k) (caaar m))
      ;                                                     (= (cdr k) (cdaar m)))  (car m))
      ;                                               ((< (car k) (caaar m))        (get-helper (cadr m) k))
      ;                                               (true                         (get-helper (cddr m) k)))))
      (trans-key (lambda (k) (cond ((string? k) (hash_string k))
                                   ((symbol? k) (hash_string (get-text k)))
                                   (true        k))))
      (put-helper (rec-lambda put-helper (m hk k v) (cond ((nil? m)                (array hk        k         v          nil                           nil))
                                                           ((and (= hk (idx m 0))
                                                                 (= k  (idx m 1))) (array hk        k         v         (idx m 3)                     (idx m 4)))
                                                           ((< hk (idx m 0))       (array (idx m 0) (idx m 1) (idx m 2) (put-helper (idx m 3) hk k v) (idx m 4)))
                                                           (true                   (array (idx m 0) (idx m 1) (idx m 2) (idx m 3)                     (put-helper (idx m 4) hk k v))))))
      (put-tree (lambda (m k v) (put-helper m (trans-key k) k v)))
      (get-helper (rec-lambda get-helper (m hk k) (cond ((nil? m)                     false)
                                                        ((and (= hk (idx m 0))
                                                              (= k  (idx m 1)))       (array k (idx m 2)))
                                                        ((< hk (idx m 0))             (get-helper (idx m 3) hk k))
                                                        (true                         (get-helper (idx m 4) hk k)))))
      (get-tree (lambda (m k) (get-helper m (trans-key k) k)))

      ;(empty_dict empty_dict-list)
      ;(put put-list)
      ;(get get-list)
      (empty_dict empty_dict-tree)
      (put put-tree)
      (get get-tree)

      ;(empty_dict (list empty_dict-list empty_dict-tree))
      ;(put (lambda (m k v) (list (put-list (idx m 0) k v) (put-tree (idx m 1) k v))))
      ;(get (lambda (m k) (dlet (  ;(_ (true_print "doing a get " m " " k))
      ;                            (list-result (get-list (idx m 0) k))
      ;                            (tree-result (get-tree (idx m 1) k))
      ;                            (_ (if (and (!= list-result tree-result) (!= (idx list-result 1) (idx tree-result 1))) (error "BAD GET " list-result " vs " tree-result)))
      ;                            ) tree-result)))

      (get-value (lambda (d k) (dlet ((result (get d k)))
                                    (if (array? result) (idx result 1)
                                                       (error (str "could not find " k " in " d))))))
      (get-value-or-false (lambda (d k) (dlet ((result (get d k)))
                                    (if (array? result) (idx result 1)
                                                       false))))

    (in_array (dlet ((helper (rec-lambda recurse (x a len_a i) (cond ((= i len_a)     false)
                                                                     ((= x (idx a i)) true)
                                                                     (true            (recurse x a len_a (+ i 1)))))))
                   (lambda (x a) (helper x a (len a) 0))))
    (array_item_union (lambda (a bi) (if (in_array bi a) a (cons bi a))))
    (array_union (lambda (a b) (foldl array_item_union a b)))


    (intset_word_size 64)
    (in_intset (rec-lambda in_intset (x a) (cond ((nil? a) false)
                                                 ((>= x intset_word_size) (in_intset (- x intset_word_size) (cdr a)))
                                                 (true                    (!= (band (>> (car a) x) 1) 0)))))

    (intset_item_union (rec-lambda intset_item_union   (a bi) (cond ((nil? a)                 (intset_item_union (array 0) bi))
                                                                    ((>= bi intset_word_size) (cons (car a) (intset_item_union (cdr a) (- bi intset_word_size))))
                                                                    (true                     (cons (bor (car a) (<< 1 bi)) (cdr a))))))

    (intset_item_remove (rec-lambda intset_item_remove (a bi) (cond ((nil? a)                 nil)
                                                                    ((>= bi intset_word_size) (dlet ((new_tail (intset_item_remove (cdr a) (- bi intset_word_size))))
                                                                                                    (if (and (nil? new_tail) (= 0 (car a))) nil
                                                                                                                                            (cons (car a) new_tail))))
                                                                    (true                     (dlet ((new_int (band (car a) (bnot (<< 1 bi)))))
                                                                                                    (if (and (nil? (cdr a)) (= 0 new_int)) nil
                                                                                                                                           (cons new_int (cdr a))))))))
    (intset_union (rec-lambda intset_union (a b) (cond ((and (nil? a) (nil? b)) nil)
                                                       ((nil? a)                b)
                                                       ((nil? b)                a)
                                                       (true                    (cons (bor (car a) (car b)) (intset_union (cdr a) (cdr b)))))))

    (intset_intersection_nonempty (rec-lambda intset_intersection_nonempty (a b) (cond ((nil? a)  false)
                                                                                       ((nil? b)  false)
                                                                                       (true      (or (!= 0 (band (car a) (car b))) (intset_intersection_nonempty (cdr a) (cdr b)))))))

    ;(_ (true_print "of 1 " (intset_item_union nil 1)))
    ;(_ (true_print "of 1 and 2 " (intset_item_union (intset_item_union nil 1) 2)))
    ;(_ (true_print "of 1 and 2 union 3 4" (intset_union (intset_item_union (intset_item_union nil 1) 2) (intset_item_union (intset_item_union nil 3) 4))))

    ;(_ (true_print "of 100 " (intset_item_union nil 100)))
    ;(_ (true_print "of 100 and 200 " (intset_item_union (intset_item_union nil 100) 200)))
    ;(_ (true_print "of 100 and 200 union 300 400" (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400))))

    ;(_ (true_print "1 in 1 " (in_intset 1 (intset_item_union nil 1))))
    ;(_ (true_print "1 in 1 and 2 " (in_intset 1 (intset_item_union (intset_item_union nil 1) 2))))
    ;(_ (true_print "1 in 1 and 2 union 3 4" (in_intset 1 (intset_union (intset_item_union (intset_item_union nil 1) 2) (intset_item_union (intset_item_union nil 3) 4)))))

    ;(_ (true_print "1 in 1 "                (in_intset 1 (intset_item_union nil 1))))
    ;(_ (true_print "1 in 1 and 2 "          (in_intset 1 (intset_item_union (intset_item_union nil 1) 2))))
    ;(_ (true_print "1 in 1 and 2 union 3 4" (in_intset 1 (intset_union (intset_item_union (intset_item_union nil 1) 2) (intset_item_union (intset_item_union nil 3) 4)))))

    ;(_ (true_print "5 in 1 "                (in_intset 5 (intset_item_union nil 1))))
    ;(_ (true_print "5 in 1 and 2 "          (in_intset 5 (intset_item_union (intset_item_union nil 1) 2))))
    ;(_ (true_print "5 in 1 and 2 union 3 4" (in_intset 5 (intset_union (intset_item_union (intset_item_union nil 1) 2) (intset_item_union (intset_item_union nil 3) 4)))))

    ;(_ (true_print "1 in 100 "                      (in_intset 1 (intset_item_union nil 100))))
    ;(_ (true_print "1 in 100 and 200 "              (in_intset 1 (intset_item_union (intset_item_union nil 100) 200))))
    ;(_ (true_print "1 in 100 and 200 union 300 400" (in_intset 1 (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400)))))
    ;(_ (true_print "5 in 100 "                      (in_intset 5 (intset_item_union nil 100))))
    ;(_ (true_print "5 in 100 and 200 "              (in_intset 5 (intset_item_union (intset_item_union nil 100) 200))))
    ;(_ (true_print "5 in 100 and 200 union 300 400" (in_intset 5 (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400)))))
    ;(_ (true_print "100 in 100 "                      (in_intset 100 (intset_item_union nil 100))))
    ;(_ (true_print "100 in 100 and 200 "              (in_intset 100 (intset_item_union (intset_item_union nil 100) 200))))
    ;(_ (true_print "100 in 100 and 200 union 300 400" (in_intset 100 (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400)))))
    ;(_ (true_print "500 in 100 "                      (in_intset 500 (intset_item_union nil 100))))
    ;(_ (true_print "500 in 100 and 200 "              (in_intset 500 (intset_item_union (intset_item_union nil 100) 200))))
    ;(_ (true_print "500 in 100 and 200 union 300 400" (in_intset 500 (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400)))))

    ;(_ (true_print "all removed in 100 and 200 union 300 400" (intset_item_remove (intset_item_remove (intset_item_remove (intset_item_remove (intset_union (intset_item_union (intset_item_union nil 100) 200) (intset_item_union (intset_item_union nil 300) 400)) 100) 200) 300) 400)))

    (intset_union_without (lambda (wo a b) (intset_item_remove (intset_union a b) wo)))

    (val?                               (lambda (x) (= 'val (idx x 0))))
    (marked_array?                      (lambda (x) (= 'marked_array (idx x 0))))
    (marked_symbol?                     (lambda (x) (= 'marked_symbol (idx x 0))))
    (comb?                              (lambda (x) (= 'comb (idx x 0))))
    (prim_comb?                         (lambda (x) (= 'prim_comb (idx x 0))))
    (marked_env?                        (lambda (x) (= 'env (idx x 0))))

    (.hash                              (lambda (x) (idx x 1)))

    (.val                               (lambda (x) (idx x 2)))

    (.marked_array_is_val               (lambda (x) (idx x 2)))
    (.marked_array_is_attempted         (lambda (x) (idx x 3)))
    (.marked_array_needed_for_progress  (lambda (x) (idx x 4)))
    (.marked_array_values               (lambda (x) (idx x 5)))
    (.marked_array_source               (lambda (x) (if (= true (idx x 6)) x (idx x 6))))
    (.marked_array_this_rec_stop        (lambda (x) (idx x 7)))

    (.marked_symbol_needed_for_progress (lambda (x) (idx x 2)))
    (.marked_symbol_is_val              (lambda (x) (= nil (.marked_symbol_needed_for_progress x))))
    (.marked_symbol_value               (lambda (x) (idx x 3)))

    (.comb                              (lambda (x) (slice x 2 -1)))
    (.comb_id                           (lambda (x) (idx x 3)))
    (.comb_des                          (lambda (x) (idx x 4)))
    (.comb_env                          (lambda (x) (idx x 5)))
    (.comb_body                         (lambda (x) (idx x 8)))
    (.comb_wrap_level                   (lambda (x) (idx x 2)))
    (.comb_rec_hashes                   (lambda (x) (idx x 9)))

    (.prim_comb_sym                     (lambda (x) (idx x 3)))
    (.prim_comb_handler                 (lambda (x) (idx x 2)))
    (.prim_comb_wrap_level              (lambda (x) (idx x 4)))
    (.prim_comb_val_head_ok             (lambda (x) (idx x 5)))
    (.prim_comb                         (lambda (x) (slice x 2 -1)))

    (.marked_env                        (lambda (x) (slice x 2 -1)))
    (.marked_env_has_vals               (lambda (x) (idx x 2)))
    (.marked_env_needed_for_progress    (lambda (x) (idx x 3)))
    (.marked_env_idx                    (lambda (x) (idx x 4)))
    (.marked_env_upper                  (lambda (x) (idx (idx x 5) -1)))
    (.env_marked                        (lambda (x) (idx x 5)))
    (marked_env_real?                   (lambda (x) (= nil (idx (.marked_env_needed_for_progress x) 0))))
    (.any_comb_wrap_level               (lambda (x) (cond  ((prim_comb? x)   (.prim_comb_wrap_level x))
                                                           ((comb? x)        (.comb_wrap_level x))
                                                           (true             (error "bad .any_comb_level")))))


    ; The actual needed_for_progress values are either
    ; #t            - any eval will do something
    ; nil           - is a value, no eval will do anything
    ; (3 4 1...)    - list of env ids that would allow forward progress
    ; But these are paired with another list of hashes that if you're not inside
    ; of an evaluation of, then it could progress futher. These are all caused by
    ; the infinite recursion stopper.
    (needed_for_progress (rec-lambda needed_for_progress (x) (cond ((marked_array? x)       (.marked_array_needed_for_progress  x))
                                                                   ((marked_symbol? x)      (dlet ((n (.marked_symbol_needed_for_progress x))) (array (if (int? n) (intset_item_union nil n) n) nil nil)))
                                                                   ((marked_env? x)         (.marked_env_needed_for_progress    x))
                                                                   ((comb? x)               (dlet ((id (.comb_id x))
                                                                                                   ((body_needed _hashes extra1) (needed_for_progress (.comb_body x)))
                                                                                                   ((se_needed   _hashes extra2) (needed_for_progress (.comb_env  x))))
                                                                                                (if (or (= true body_needed) (= true se_needed)) (array true nil nil)
                                                                                                        (array (intset_union_without id body_needed se_needed)
                                                                                                               nil (intset_union_without id extra1 extra2))
                                                                                                )))
                                                                   ((prim_comb? x)          (array nil nil nil))
                                                                   ((val? x)                (array nil nil nil))
                                                                   (true                    (error (str "what is this? in need for progress" x))))))
    (needed_for_progress_slim (lambda (x) (idx (needed_for_progress x) 0)))
    (hash_symbol      (lambda (progress_idx s) (combine_hash (cond ((= true progress_idx) 11)
                                                                   ((int? progress_idx)   (combine_hash 13 progress_idx))
                                                                   (true                  113)) (hash_string (get-text s)))))

    (hash_array       (lambda (is_val attempted a) (foldl combine_hash (if is_val 17 (cond ((int? attempted) (combine_hash attempted 19))
                                                                                            (attempted       61)
                                                                                            (true            107))) (map .hash a))))
    (hash_env         (lambda (has_vals progress_idxs dbi arrs) (combine_hash (if has_vals 107 109)
                                                                (combine_hash (mif dbi (hash_num dbi) 59) (dlet (
                                                                ;(_ (begin (true_print "pre slice " (slice arrs 0 -2)) 0))
                                                                ;(_ (begin (true_print "about to do a fold " progress_idxs " and " (slice arrs 0 -2)) 0))
                                                                (inner_hash (foldl (dlambda (c (s v)) (combine_hash c (combine_hash (hash_symbol true s) (.hash v))))
                                                                                   (cond ((= nil progress_idxs)     23)
                                                                                         ((= true progress_idxs)    29)
                                                                                         (true                      (foldl combine_hash 31 progress_idxs)))
                                                                                   (slice arrs 0 -2)))
                                                                (end (idx arrs -1))
                                                                (end_hash (mif end (.hash end) 41))
                                                                                                         ) (combine_hash inner_hash end_hash))))))
    (hash_comb        (lambda (wrap_level env_id de? se variadic params body)
                                                                             (combine_hash 43
                                                                             (combine_hash wrap_level
                                                                             (combine_hash env_id
                                                                             (combine_hash (mif de? (hash_symbol true de?) 47)
                                                                             (combine_hash (.hash se)
                                                                             (combine_hash (hash_bool variadic)
                                                                             (combine_hash (foldl (lambda (c x) (combine_hash c (hash_symbol true x))) 53 params)
                                                                             (.hash body))))))))))
    (hash_prim_comb   (lambda (handler_fun real_or_name wrap_level val_head_ok) (combine_hash (combine_hash 59 (hash_symbol true real_or_name))
                                                                                              (combine_hash (if val_head_ok 89 97) wrap_level))))
    (hash_val         (lambda (x) (cond ((bool? x)     (hash_bool x))
                                        ((string? x)   (hash_string x))
                                        ((int? x)      (hash_num x))
                                        (true          (error (str "bad thing to hash_val " x))))))
    ; 127 131 137 139 149 151 157 163 167 173

    (marked_symbol    (lambda (progress_idx x)                        (array 'marked_symbol    (hash_symbol progress_idx x)                      progress_idx x)))
    (marked_array     (lambda (is_val attempted resume_hashes x source)       (dlet (
                                                                            ((sub_progress_idxs hashes extra) (foldl (dlambda ((a ahs aeei) (x xhs x_extra_env_ids))
                                                                                                    (array (cond ((or (= true a) (= true x)) true)
                                                                                                                 (true           (intset_union a x)))
                                                                                                           (array_union ahs xhs)
                                                                                                           (intset_union aeei x_extra_env_ids))
                                                                                                 ) (array (array) resume_hashes (array)) (map needed_for_progress x)))
                                                                            (progress_idxs (cond ((and (= nil sub_progress_idxs) (not is_val) (= true attempted))  nil)
                                                                                                 ((and (= nil sub_progress_idxs) (not is_val) (= false attempted)) true)
                                                                                                 ((and (= nil sub_progress_idxs) (not is_val) (int? attempted))    (array attempted))
                                                                                                 (true                                                             (if (int? attempted)
                                                                                                                                                                       (intset_item_union sub_progress_idxs attempted)
                                                                                                                                                                       sub_progress_idxs))))
                                                                        ) (array 'marked_array  (hash_array is_val attempted x) is_val attempted (array progress_idxs hashes extra) x source resume_hashes))))


    (marked_env       (lambda (has_vals de? de ue dbi arrs) (dlet (
                                  (de_entry (mif de? (array (array de? de)) (array)))
                                  (full_arrs (concat arrs de_entry (array ue)))
                                  ((progress_idxs1 _hashes extra1) (mif ue  (needed_for_progress ue) (array nil nil nil)))
                                  ((progress_idxs2 _hashes extra2) (mif de? (needed_for_progress de) (array nil nil nil)))
                                  (progress_idxs (intset_union progress_idxs1 progress_idxs2))
                                  (extra (intset_union extra1 extra2))
                                  (progress_idxs (if (not has_vals) (intset_item_union progress_idxs dbi) progress_idxs))
                                  (extra         (if (!= nil progress_idxs) (intset_item_union extra dbi) extra))
                                ) (array 'env              (hash_env has_vals progress_idxs dbi full_arrs)                  has_vals (array progress_idxs nil extra) dbi full_arrs))))


    (marked_val       (lambda (x)                                             (array 'val       (hash_val x)                                              x)))
    (marked_comb      (lambda (wrap_level env_id de? se variadic params body rec_hash) (array 'comb  (hash_comb wrap_level env_id de? se variadic params body) wrap_level env_id de? se variadic params body rec_hash)))
    (marked_prim_comb (lambda (handler_fun real_or_name wrap_level val_head_ok)    (array 'prim_comb        (hash_prim_comb handler_fun real_or_name wrap_level val_head_ok) handler_fun real_or_name wrap_level val_head_ok)))

    (with_wrap_level                    (lambda (x new_wrap) (cond  ((prim_comb? x)   (dlet (((handler_fun real_or_name wrap_level val_head_ok) (.prim_comb x)))
                                                                                             (marked_prim_comb handler_fun real_or_name new_wrap val_head_ok)))
                                                                    ((comb? x)        (dlet (((wrap_level env_id de? se variadic params body rec_hash) (.comb x)))
                                                                                             (marked_comb new_wrap env_id de? se variadic params body rec_hash)))
                                                                    (true             (error "bad with_wrap_level")))))

    (add_hash_if_comb (lambda (new_hash x) (cond ((comb? x)        (dlet (
                                                                     ((wrap_level env_id de? se variadic params body rec_hash) (.comb x))
                                                                    ) (marked_comb wrap_level env_id de? se variadic params body (cons (array new_hash wrap_level) rec_hash))))
                                                 (true             x))))




    (later_head? (rec-lambda recurse (x) (or (and (marked_array? x)  (or (= false (.marked_array_is_val x)) (foldl (lambda (a x) (or a (recurse x))) false (.marked_array_values x))))
                                             (and (marked_symbol? x) (= false (.marked_symbol_is_val x)))
                                         )))


    ; array and comb are the ones wherewhere (= nil (needed_for_progress_slim x)) == total_value? isn't true.
    ; Right now we only call functions when all parameters are values, which means you can't
    ; create a true_value array with non-value memebers (*right now* anyway), but it does mean that
    ; you can create a nil needed for progress array that isn't a value, namely for the give_up_*
    ; primitive functions (extra namely, log and error, which are our two main sources of non-purity besides implicit runtime errors).
    ; OR, currently, having your code stopped because of infinite recursion checker. This comes up with the Y combiner
    ; For combs, being a value is having your env-chain be real?
    (total_value?  (lambda (x) (if (marked_array? x) (.marked_array_is_val x)
                                                     (= nil (needed_for_progress_slim x)))))


    (is_all_values      (lambda (evaled_params) (foldl (lambda (a x) (and a      (total_value? x))) true evaled_params)))
    (is_all_head_values (lambda (evaled_params) (foldl (lambda (a x) (and a (not (later_head? x)))) true evaled_params)))

    (false? (lambda (x) (cond ((and (marked_array? x)  (= false (.marked_array_is_val x)))    (error "got a later marked_array passed to false? " x))
                              ((and (marked_symbol? x) (= false (.marked_symbol_is_val x)))   (error "got a later marked_symbol passed to false? " x))
                              ((val? x)                                                       (not (.val x)))
                              (true                                                           false))))


    (mark (rec-lambda recurse (x) (cond   ((env? x)        (error "called mark with an env " x))
                                          ((combiner? x)   (error "called mark with a combiner " x))
                                          ((symbol? x)     (cond ((= 'true x)  (marked_val #t))
                                                                 ((= 'false x) (marked_val #f))
                                                                 (#t           (marked_symbol nil x))))
                                          ((array? x)      (marked_array true false nil (map recurse x) true))
                                          (true            (marked_val x)))))

    (indent_str (rec-lambda recurse (i) (mif (= i 0) ""
                                                   (str "   " (recurse (- i 1))))))
    (indent_str (if speed_hack (lambda (i) "") indent_str))

    (str_strip (lambda args (apply true_str (concat (slice args 0 -2) (array (idx ((rec-lambda recurse (x done_envs)
        (cond ((= nil x)          (array "<nil>" done_envs))
              ((string? x)        (array (true_str "<raw string " x ">") done_envs))
              ((val? x)           (array (true_str (.val x)) done_envs))
              ((marked_array? x)  (dlet (((stripped_values done_envs) (foldl (dlambda ((vs de) x) (dlet (((v de) (recurse x de))) (array (concat vs (array v)) de)))
                                                                             (array (array) done_envs) (.marked_array_values x))))
                                        (mif (.marked_array_is_val x) (array (true_str "[" stripped_values "]") done_envs)
                                                                      ;(array (true_str stripped_values) done_envs))))
                                                                      (array (true_str "<a" (.marked_array_is_attempted x) ",r" (needed_for_progress x) "~" (.marked_array_this_rec_stop x) "~*" (.hash x) "*>" stripped_values) done_envs))))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (array (true_str "'" (.marked_symbol_value x)) done_envs)
                                                                 (array (true_str (.marked_symbol_needed_for_progress x) "#" (.marked_symbol_value x)) done_envs)))
              ((comb? x)          (dlet (((wrap_level env_id de? se variadic params body rec_hash) (.comb x))
                                         ((se_s done_envs)   (recurse se done_envs))
                                         ((body_s done_envs) (recurse body done_envs)))
                                      (array (true_str "<n " (needed_for_progress x) " (comb " wrap_level " " env_id " " rec_hash " " se_s " " de? " " params " " body_s ")>") done_envs)))
              ((prim_comb? x)     (array (true_str "<wl=" (.prim_comb_wrap_level x) " " (.prim_comb_sym x) ">") done_envs))
              ((marked_env? x)    (dlet ((e (.env_marked x))
                                         (index (.marked_env_idx x))
                                         (u (idx e -1))
                                         (already (in_array index done_envs))
                                         (opening (true_str "{" (mif (marked_env_real? x) "real" "fake") (mif (.marked_env_has_vals x) " real vals" " fake vals")  " ENV idx: " (true_str index) ", "))
                                         ((middle done_envs) (if already (array "" done_envs) (foldl (dlambda ((vs de) (k v)) (dlet (((x de) (recurse v de))) (array (concat vs (array (array k x))) de)))
                                                                                                     (array (array) done_envs)
                                                                                                     (slice e 0 -2))))
                                         ((upper  done_envs) (if already (array "" done_envs) (mif u (recurse u done_envs) (array "no_upper_likely_root_env" done_envs))))
                                         (done_envs (if already done_envs (cons index done_envs)))
                                       ) (array (if already (true_str opening "omitted}")
                                                            (if (> (len e) 30) (true_str "{" (len e) "env}")
                                                                               (true_str opening middle " upper: " upper "}"))) done_envs)
                                               ))
              (true               (error (true_str "some other str_strip? |" x "|")))
        )
    ) (idx args -1) (array)) 0))))))
    (true_str_strip str_strip)
    (str_strip (if speed_hack (lambda args 0) str_strip))
    ;(true_str_strip str_strip)
    (print_strip (lambda args (println (apply str_strip args))))

    (env-lookup-helper (rec-lambda recurse (dict key i fail success) (cond ((and (= i (- (len dict) 1)) (= nil (idx dict i)))  (fail))
                                                                           ((= i (- (len dict) 1))                             (recurse (.env_marked (idx dict i)) key 0 fail success))
                                                                           ((= key (idx (idx dict i) 0))                       (success (idx (idx dict i) 1)))
                                                                           (true                                               (recurse dict key (+ i 1) fail success)))))
    (env-lookup (lambda (env key) (env-lookup-helper (.env_marked env) key 0 (lambda () (error (str key " not found in env " (str_strip env)))) (lambda (x) x))))

    (strip (dlet ((helper (rec-lambda recurse (x need_value)
        (cond ((val? x)           (.val x))
              ((marked_array? x)  (dlet ((stripped_values (map (lambda (x) (recurse x need_value)) (.marked_array_values x))))
                                      (mif (.marked_array_is_val x) stripped_values
                                                                    (error (str "needed value for this strip but got" x)))))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (.marked_symbol_value x)
                                                                 (error (str "needed value for this strip but got" x))))
              ((comb? x)          (error "got comb for strip, won't work"))
              ((prim_comb? x)     (idx x 2))
                                         ; env emitting doesn't pay attention to real value right now, not sure mif that makes sense
                                         ; TODO: properly handle de Bruijn indexed envs
              ((marked_env? x)    (error "got env for strip, won't work"))
              (true               (error (str "some other strip? " x)))
        )
    )))  (lambda (x) (dlet (
                            ;(_ (print_strip "stripping: " x))
                            (r (helper x true))
                            ;(_ (println "result of strip " r))
                            ) r))))

    (try_unval (rec-lambda recurse (x fail_f)
        (cond ((marked_array? x) (mif (not (.marked_array_is_val x)) (array false (fail_f x))
                                                                    (if (!= 0 (len (.marked_array_values x)))
                                                                        (dlet ((values (.marked_array_values x))
                                                                               ((ok f) (recurse (idx values 0) fail_f))
                                                                        ) (array ok (marked_array false false nil (cons f (slice values 1 -1)) (.marked_array_source x))))
                                                                        (array true (marked_array false false nil (array) (.marked_array_source x))))))
              ((marked_symbol? x) (mif (.marked_symbol_is_val x) (array true (marked_symbol true (.marked_symbol_value x)))
                                                                 (array false (fail_f x))))
              (true               (array true x))
        )
    ))
    (try_unval_array (lambda (x) (foldl (dlambda ((ok a) x) (dlet (((nok p) (try_unval x (lambda (_) nil))))
                                                                   (array (and ok nok) (concat a (array p)))))
                                        (array true (array))
                                        x)))

    (check_for_env_id_in_result (lambda (s_env_id x) (idx ((rec-lambda check_for_env_id_in_result (memo s_env_id x)
        (dlet (
            ((need _hashes extra) (needed_for_progress x))
            (in_need (if (!= true need) (in_intset s_env_id need) false))
            (in_extra (in_intset s_env_id extra))
              ;(or in_need in_extra) (array memo true)
              ;(!= true need)        (array memo false)
        ) (cond ((or in_need in_extra) (array memo true))
                ((!= true need)        (array memo false))
                (true                  (dlet (

            (old_way (dlet (
                    (hash (.hash x))
                    ;(result (if (or (comb? x) (marked_env? x)) (alist-ref hash memo) false))
                    ;(result (if (or (marked_array? x) (marked_env? x)) (alist-ref hash memo) false))
                    ;(result (if (marked_env? x) (my-alist-ref hash memo) false))
                    (result (if (marked_env? x) (get memo hash) false))
                ) (if (array? result) (array memo (idx result 1)) (cond
                      ((marked_symbol? x) (array memo false))
                      ((marked_array? x)  (dlet (
                                            (values (.marked_array_values x))
                                            ((memo result) ((rec-lambda recurse (memo i) (if (= (len values) i) (array memo false)
                                                                                              (dlet (((memo r) (check_for_env_id_in_result memo s_env_id (idx values i))))
                                                                                                  (if r (array memo true)
                                                                                                        (recurse memo (+ i 1))))))
                                                            memo 0))
                                            ;(memo (put memo hash result))
                                          ) (array memo result)))
                      ((prim_comb? x)    (array memo false))
                      ((val? x)           (array memo false))
                      ((comb? x)          (dlet (
                                            ((wrap_level i_env_id de? se variadic params body rec_hash) (.comb x))
                                            ((memo in_se) (check_for_env_id_in_result memo s_env_id se))
                                            ((memo total) (if (and (not in_se) (!= s_env_id i_env_id)) (check_for_env_id_in_result memo s_env_id body)
                                                                                                       (array memo in_se)))
                                            ;(memo (put memo hash total))
                                          ) (array memo total)))

                      ((marked_env? x)   (if (and (not (marked_env_real? x)) (= s_env_id (.marked_env_idx x))) (array memo true)
                                                 (dlet (
                                                    (values (slice (.env_marked x) 0 -2))
                                                    (upper  (idx (.env_marked x) -1))
                                                    ((memo result) ((rec-lambda recurse (memo i) (if (= (len values) i) (array memo false)
                                                                                                      (dlet (((memo r) (check_for_env_id_in_result memo s_env_id (idx (idx values i) 1))))
                                                                                                          (if r (array memo true)
                                                                                                                (recurse memo (+ i 1))))))
                                                                    memo 0))
                                                    ((memo result) (if (or result (= nil upper)) (array memo result)
                                                                                                 (check_for_env_id_in_result memo s_env_id upper)))
                                                    (memo (put memo hash result))
                                                 ) (array memo result))))
                      (true              (error (str "Something odd passed to check_for_env_id_in_result " x)))
            ))))

            (new_if_working (or in_need in_extra))
            (_ (if (and (!= true need) (!= new_if_working (idx old_way 1))) (error "GAH looking for " s_env_id " - " need " - " extra " - " new_if_working " " (idx old_way 1))))
        ) old_way))))) (array) s_env_id x) 1)))

    (comb_takes_de? (lambda (x l) (cond
        ((comb? x)      (!= nil (.comb_des x)))
        ((prim_comb? x) (cond ((= (.prim_comb_sym x) 'vau)    true)
                              ((= (.prim_comb_sym x) 'eval)   (= 1 l))
                              ((= (.prim_comb_sym x) 'veval)  (= 1 l))
                              ((= (.prim_comb_sym x) 'lapply) (= 1 l))
                              ((= (.prim_comb_sym x) 'vapply) (= 1 l))
                              ((= (.prim_comb_sym x) 'cond)   true) ; but not vcond
                              (true                           false)))
        ((and (marked_array? x)  (not (.marked_array_is_val  x)))          true)
        ((and (marked_symbol? x) (not (.marked_symbol_is_val x)))          true)
        (true           (error (str "illegal comb_takes_de? param " x)))
    )))

    ; Handles let 4.3 through macro level leaving it as (<comb wraplevel=1 (y) (+ y x 12)> 13)
    ; need handling of symbols (which is illegal for eval but ok for calls) to push it farther
    (combiner_return_ok (rec-lambda combiner_return_ok (func_result env_id)
        (cond   ((not (later_head? func_result)) (not (check_for_env_id_in_result env_id func_result)))
        ; special cases now
        ;   *(veval body {env}) => (combiner_return_ok {env})
        ;       The reason we don't have to check body is that this form is only creatable in ways that body was origionally a value and only need {env}
        ;           Either it's created by eval, in which case it's fine, or it's created by something like (eval (array veval x de) de2) and the array has checked it,
        ;           or it's created via literal vau invocation, in which case the body is a value.
                ((and (marked_array? func_result)
                      (prim_comb? (idx (.marked_array_values func_result) 0))
                      (= 'veval (.prim_comb_sym (idx (.marked_array_values func_result) 0)))
                      (= 3 (len (.marked_array_values func_result)))
                      (combiner_return_ok (idx (.marked_array_values func_result) 2) env_id))                                       true)
        ;   (func ...params) => (and (doesn't take de func) (foldl combiner_return_ok (cons func params)))
        ;
                ((and (marked_array? func_result)
                      (not (comb_takes_de? (idx (.marked_array_values func_result) 0) (len (.marked_array_values func_result))))
                      (foldl (lambda (a x) (and a (combiner_return_ok x env_id))) true (.marked_array_values func_result)))         true)

        ;   So that's enough for macro like, but we would like to take it farther
        ;       For like (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a)))))
        ;           we get to (+ 13 x 12) not being a value, and it reconstructs
        ;           (<comb wraplevel=1 (y) (+ y x 12)> 13)
        ;           and that's what eval gets, and eval then gives up as well.

        ;           That will get caught by the above cases to remain the expansion (<comb wraplevel=1 (y) (+ y x 12)> 13),
        ;           but ideally we really want another case to allow (+ 13 x 12) to bubble up
        ;           I think it would be covered by the (func ...params) case if a case is added to allow symbols to be bubbled up if their
        ;           needed for progress wasn't true or the current environment, BUT this doesn't work for eval, just for functions,
        ;           since eval changes the entire env chain (but that goes back to case 1, and might be eliminated at compile if it's an env reachable from the func).
        ;
        ;
        ;   Do note a key thing to be avoided is allowing any non-val inside a comb, since that can cause a fake env's ID to
        ;   reference the wrong env/comb in the chain.
        ;   We do allow calling eval with a fake env, but since it's only callable withbody value and is strict (by calling this)
        ;   about it's return conditions, and the env it's called with must be ok in the chain, and eval doesn't introduce a new scope, it works ok.
        ;   We do have to be careful about allowing returned later symbols from it though, since it could be an entirely different env chain.

                (true false)
        )
    ))

    (drop_redundent_veval (rec-lambda drop_redundent_veval (partial_eval_helper x de env_stack pectx indent) (dlet (
                                                    (env_id (.marked_env_idx de))
                                                    (r (if
                                                      (and (marked_array? x)
                                                           (not (.marked_array_is_val x)))
                                                           (if (and (prim_comb? (idx (.marked_array_values x) 0))
                                                                    (= 'veval (.prim_comb_sym (idx (.marked_array_values x) 0)))
                                                                    (= 3 (len (.marked_array_values x)))
                                                                    (not (marked_env_real? (idx (.marked_array_values x) 2)))
                                                                    (= env_id (.marked_env_idx (idx (.marked_array_values x) 2)))) (drop_redundent_veval partial_eval_helper (idx (.marked_array_values x) 1) de env_stack pectx (+ 1 indent))
                                                                                                                                   ; wait, can it do this? will this mess with eval?

                                                            ; basically making sure that this comb's params are still good to eval
                                                            (if (and (or (prim_comb? (idx (.marked_array_values x) 0)) (comb? (idx (.marked_array_values x) 0)))
                                                                     (!= -1 (.any_comb_wrap_level (idx (.marked_array_values x) 0))))
                                                                (dlet (((pectx err ress changed) (foldl (dlambda ((c er ds changed) p) (dlet (
                                                                                                      (pre_hash (.hash p))
                                                                                                      ((c e d) (drop_redundent_veval partial_eval_helper p de env_stack c (+ 1 indent)))
                                                                                                      (err (mif er er e))
                                                                                                      (changed (mif err false (or (!= pre_hash (.hash d)) changed)))
                                                                                                    ) (array c err (concat ds (array d)) changed)))
                                                                                                 (array pectx nil (array) false)
                                                                                                 (.marked_array_values x)))
                                                                        ((pectx err new_array) (if (or (!= nil err) (not changed))
                                                                                                   (array pectx err x)
                                                                                                   (partial_eval_helper (marked_array false (.marked_array_is_attempted x) nil ress (.marked_array_source x))
                                                                                                                        false de env_stack pectx (+ indent 1) true)))

                                                                       ) (array pectx err new_array))
                                                                (array pectx nil x))
                                                            ) (array pectx nil x))))

                                                    r)))

    (make_tmp_inner_env (lambda (params de? ue env_id)
        (dlet ((param_entries       (map (lambda (p) (array p (marked_symbol env_id p))) params))
               (possible_de         (mif (= nil de?) (array) (marked_symbol  env_id de?)))
            ) (marked_env false de? possible_de ue env_id param_entries))))


    (partial_eval_helper (rec-lambda partial_eval_helper (x only_head env env_stack pectx indent force)
        (dlet (((for_progress for_progress_hashes extra_env_ids) (needed_for_progress x))
               (_ (print_strip (indent_str indent) "for_progress " for_progress ", for_progress_hashes " for_progress_hashes " for " x))
               ((env_counter memo) pectx)
               (hashes_now (foldl (lambda (a hash) (or a (= false (get-value-or-false memo hash)))) false for_progress_hashes))
              )
        (if (or force hashes_now (= for_progress true) (intset_intersection_nonempty for_progress (idx env_stack 0)))
        (cond   ((val? x)            (array pectx nil x))
                ((marked_env? x)     (dlet ((dbi (.marked_env_idx x)))
                                          ; compiler calls with empty env stack
                                          (mif dbi (dlet ( (new_env ((rec-lambda rec (i len_env_stack) (cond ((= i len_env_stack)                           nil)
                                                                                               ((= dbi (.marked_env_idx (idx (idx env_stack 1) i)))     (idx (idx env_stack 1) i))
                                                                                               (true                                            (rec (+ i 1) len_env_stack))))
                                                                     0 (len (idx env_stack 1))))
                                                         (_ (println (str_strip "replacing " x) (str_strip " with (if nonnil) " new_env)))
                                                        )
                                                        (array pectx nil (if (!= nil new_env) new_env x)))
                                                  (array pectx nil x))))

                ((comb? x)           (dlet (((wrap_level env_id de? se variadic params body rec_hash) (.comb x)))
                                        (mif (or (and (not (marked_env_real? env)) (not (marked_env_real? se)))   ; both aren't real, re-evaluation of creation site
                                                 (and      (marked_env_real? env)  (not (marked_env_real? se))))  ; new env real, but se isn't - creation!
                                             (dlet ((inner_env (make_tmp_inner_env params de? env env_id))
                                                    ((pectx err evaled_body) (partial_eval_helper body false inner_env (array (idx env_stack 0) (cons inner_env (idx env_stack 1))) pectx (+ indent 1) false)))
                                                   (array pectx err (mif err nil (marked_comb wrap_level env_id de? env variadic params evaled_body rec_hash))))
                                             (array pectx nil x))))
                ((prim_comb? x)      (array pectx nil x))
                ((marked_symbol? x)  (mif (.marked_symbol_is_val x) x
                                                                    (env-lookup-helper (.env_marked env) (.marked_symbol_value x) 0
                                                                                       (lambda ()  (array pectx (str "could't find " (str_strip x) " in " (str_strip env))  nil))
                                                                                       (lambda (x) (array pectx nil x)))))
                                                                     ; Does this ever happen? non-fully-value arrays?
                ((marked_array? x)   (cond ((.marked_array_is_val x) (dlet ( ((pectx err inner_arr) (foldl (dlambda ((c er ds) p) (dlet (((c e d) (partial_eval_helper p false env env_stack c (+ 1 indent) false))) (array c (mif er er e) (concat ds (array d)))))
                                                                                                             (array pectx nil (array))
                                                                                                             (.marked_array_values x)))
                                                                           ) (array pectx err (mif err nil (marked_array true false nil inner_arr (.marked_array_source x))))))
                                           ((= 0 (len (.marked_array_values x))) (array pectx "Partial eval on empty array" nil))
                                           (true (dlet ((values (.marked_array_values x))
                                                        (_ (print_strip (indent_str indent) "partial_evaling comb " (idx values 0)))

                                                        (literal_params (slice values 1 -1))
                                                        ((pectx err comb) (partial_eval_helper (idx values 0) true env env_stack pectx (+ 1 indent) false))
                                                       ) (cond ((!= nil err) (array pectx err nil))
                                                               ((later_head? comb)                               (array pectx nil (marked_array false true nil (cons comb literal_params) (.marked_array_source x))))
                                                               ((not (or (comb? comb) (prim_comb? comb))) (array pectx (str "impossible comb value " x) nil))
                                                               (true    (dlet (
                                                                            ; If we haven't evaluated the function before at all, we would like to partially evaluate it so we know
                                                                            ; what it needs. We'll see if this re-introduces exponentail (I think this should limit it to twice?)
                                                                            ((pectx comb_err comb) (if (and (= nil err) (= true (needed_for_progress_slim comb)))
                                                                                                        (partial_eval_helper comb false env env_stack pectx (+ 1 indent) false)
                                                                                                        (array pectx err comb)))
                                                                            (_ (println (indent_str indent) "Going to do an array call!"))
                                                                            (indent (+ 1 indent))
                                                                            (_ (print_strip (indent_str indent) "total (in env " (.marked_env_idx env) ") is (proceeding err " err ") " x))
                                                                            (map_rp_eval (lambda (pectx ps) (foldl (dlambda ((c er ds) p) (dlet ((_ (print_strip (indent_str indent) "rp_evaling " p)) ((c e d) (partial_eval_helper p false env env_stack c (+ 1 indent) false)) (_ (print_strip (indent_str indent) "result of rp_eval was err " e " and value " d))) (array c (mif er er e) (concat ds (array d)))))
                                                                                                        (array pectx nil (array))
                                                                                                        ps)))
                                                                            (wrap_level (.any_comb_wrap_level comb))
                                                                                                                                ; -1 is a minor hack for veval to prevent re-eval
                                                                                                                                ; in the wrong env and vcond to prevent guarded
                                                                                                                                ; infinate recursion
                                                                            ((remaining_wrap param_err evaled_params pectx) (if (= -1 wrap_level)
                                                                                                                                (array -1 nil literal_params pectx)
                                                                                                                                ((rec-lambda param-recurse (wrap cparams pectx)
                                                                                   (dlet (
                                                                                          (_ (print (indent_str indent) "For initial rp_eval:"))
                                                                                          (_ (map (lambda (x) (print_strip (indent_str indent) "item " x)) cparams))
                                                                                          ((pectx er pre_evaled) (map_rp_eval pectx cparams))
                                                                                          (_ (print (indent_str indent) "er for intial  rp_eval: " er))
                                                                                         )
                                                                                       (mif er (array wrap er nil pectx)
                                                                                       (mif (!= 0 wrap)
                                                                                           (dlet (((ok unval_params) (try_unval_array pre_evaled)))
                                                                                                (mif (not ok) (array wrap nil pre_evaled pectx)
                                                                                                              (param-recurse (- wrap 1) unval_params pectx)))
                                                                                           (array wrap nil pre_evaled pectx)))))
                                                                                                                        wrap_level literal_params pectx)))
                                                                            (_ (println (indent_str indent) "Done evaluating parameters"))

                                                                            (l_later_call_array (lambda () (marked_array false true nil (cons (with_wrap_level comb remaining_wrap) evaled_params) (.marked_array_source x))))
                                                                            (ok_and_non_later (or (= -1 remaining_wrap)
                                                                                                  (and (= 0 remaining_wrap) (if (and (prim_comb? comb) (.prim_comb_val_head_ok comb))
                                                                                                                                (is_all_head_values evaled_params)
                                                                                                                                (is_all_values evaled_params)))))
                                                                            (_ (println (indent_str indent) "ok_and_non_later " ok_and_non_later))
                                                                        ) (cond ((!= nil comb_err)      (array pectx comb_err nil))
                                                                                ((!= nil param_err)     (array pectx param_err nil))
                                                                                ((not ok_and_non_later) (array pectx nil (l_later_call_array)))
                                                                                ((prim_comb? comb) (dlet (
                                                                                                    ;(_ (println (indent_str indent) "Calling prim comb " (.prim_comb_sym comb)))
                                                                                                    ;(_ (if (= '!= (.prim_comb_sym comb)) (true_print (indent_str indent) "Calling prim comb " (.prim_comb_sym comb) " with params " evaled_params)))
                                                                                                    ((pectx err result) ((.prim_comb_handler comb) only_head env env_stack pectx evaled_params (+ 1 indent)))
                                                                                                    ) (if (= 'LATER err) (array pectx nil (l_later_call_array))
                                                                                                                         (array pectx err result))))
                                                                                ((comb? comb)      (dlet (
                                                                                     ((wrap_level env_id de? se variadic params body rec_hash) (.comb comb))


                                                                                     (final_params (mif variadic (concat (slice evaled_params 0 (- (len params) 1))
                                                                                                                       (array (marked_array true false nil (slice evaled_params (- (len params) 1) -1) nil)))
                                                                                                                 evaled_params))
                                                                                     (de_env (mif (!= nil de?) env nil))
                                                                                     (inner_env (marked_env true de? de_env se env_id (zip params final_params)))
                                                                                     (_ (print_strip (indent_str indent) " with inner_env is " inner_env))
                                                                                     (_ (print_strip (indent_str indent) "going to eval " body))

                                                                                     ; prevent infinite recursion
                                                                                     (hash (combine_hash (.hash body) (.hash inner_env)))
                                                                                     ((env_counter memo) pectx)
                                                                                     ((pectx func_err func_result rec_stop) (if (!= false (get-value-or-false memo hash))
                                                                                                                                (array pectx nil "stopping for infinite recursion" true)
                                                                                                                    (dlet (
                                                                                                                        (new_memo (put memo hash nil))
                                                                                                                        (pectx (array env_counter new_memo))
                                                                                                                        ((pectx func_err func_result) (partial_eval_helper body only_head inner_env
                                                                                                                                                            (array (intset_item_union (idx env_stack 0) env_id)
                                                                                                                                                                   (cons inner_env (idx env_stack 1)))
                                                                                                                                                                           pectx (+ 1 indent) false))
                                                                                                                        ((env_counter new_memo) pectx)
                                                                                                                        (pectx (array env_counter memo))
                                                                                                                    ) (array pectx func_err func_result false))))

                                                                                     (_ (print_strip (indent_str indent) "evaled result of function call (in env " (.marked_env_idx env) ", with inner " env_id ") and err " func_err "  is " func_result))
                                                                                     ;(_ (mif (= 6008223282910300 hash) (true_print "yep it's this call, and we got " (true_str_strip func_result))))
                                                                                     (must_stop_maybe_id (and (= nil func_err)
                                                                                                              (or rec_stop (if (not (combiner_return_ok func_result env_id))
                                                                                                                               (if (!= nil de?) (.marked_env_idx env) true)
                                                                                                                               false))))
                                                                                     ) (if (!= nil func_err) (array pectx func_err nil)
                                                                                             (if must_stop_maybe_id
                                                                                                 (array pectx nil (marked_array false must_stop_maybe_id (if rec_stop (array hash) nil) (cons (with_wrap_level comb remaining_wrap) evaled_params) (.marked_array_source x)))
                                                                                                 (dlet (((pectx err x) (drop_redundent_veval partial_eval_helper func_result env env_stack pectx indent)))
                                                                                                        (array pectx err (add_hash_if_comb hash x)))))))
                                                                        )))
                                                               )))))

                (true                (array pectx (str "impossible partial_eval value " x) nil))
        )
        ; otherwise, we can't make progress yet
        (begin (print_strip (indent_str indent) "Not evaluating " x)
               ;(print (indent_str indent) "comparing to env stack " env_stack)
               (drop_redundent_veval partial_eval_helper x env env_stack pectx indent))))
    ))

    (needs_params_val_lambda (lambda (f_sym actual_function) (dlet (
        (handler (rec-lambda recurse (only_head de env_stack pectx params indent)
                                     (array pectx nil (mark (apply actual_function (map strip params))))))
        ) (array f_sym (marked_prim_comb handler f_sym 1 false)))))

    (give_up_eval_params (lambda (f_sym) (dlet (
        (handler (lambda (only_head de env_stack pectx params indent) (array pectx 'LATER nil)))
        ) (array f_sym (marked_prim_comb handler f_sym 1 false)))))

    (veval_inner (rec-lambda recurse (only_head de env_stack pectx params indent) (dlet (
        (body (idx params 0))
        (implicit_env (!= 2 (len params)))
        (eval_env (if implicit_env de (idx params 1)))
        ((pectx err eval_env)  (if implicit_env (array pectx nil de)
                                                (partial_eval_helper (idx params 1) only_head de env_stack pectx (+ 1 indent) false)))
        ((pectx err ebody) (if (or (!= nil err) (not (marked_env? eval_env)))
                               (array pectx err body)
                               (partial_eval_helper body only_head eval_env env_stack pectx (+ 1 indent) false)))
    ) (cond
        ((!= nil err)                                          (begin (print (indent_str indent) "got err " err) (array pectx err nil)))
        ; If our env was implicit, then our unval'd code can be inlined directly in our caller
        (implicit_env                                          (drop_redundent_veval partial_eval_helper ebody de env_stack pectx indent))
        ((combiner_return_ok ebody (.marked_env_idx eval_env)) (drop_redundent_veval partial_eval_helper ebody de env_stack pectx indent))
        (true                                                  (drop_redundent_veval partial_eval_helper (marked_array false true nil (array (marked_prim_comb recurse 'veval -1 true) ebody eval_env) nil) de env_stack pectx indent))
    ))))

    (root_marked_env (marked_env true nil nil nil nil (array

        (array 'eval (marked_prim_comb (rec-lambda recurse (only_head de env_stack pectx evaled_params indent)
            (if (not (total_value? (idx evaled_params 0)))                                (array pectx nil (marked_array false true nil (cons (marked_prim_comb recurse 'eval 0 true) evaled_params) nil))
            (if (and (= 2 (len evaled_params)) (not (marked_env? (idx evaled_params 1)))) (array pectx nil (marked_array false true nil (cons (marked_prim_comb recurse 'eval 0 true) evaled_params) nil))
            (dlet (
                (body (idx evaled_params 0))
                (implicit_env (!= 2 (len evaled_params)))
                (eval_env (if implicit_env de (idx evaled_params 1)))
                ((ok unval_body) (try_unval body (lambda (_) nil)))
                (_ (if (not ok) (error "actually impossible eval unval")))


            ) (veval_inner only_head de env_stack pectx (if implicit_env (array unval_body) (array unval_body eval_env)) indent))))
        ) 'eval 1 true))

        (array 'vapply (marked_prim_comb (dlambda (only_head de env_stack pectx args indent)
            (veval_inner only_head de env_stack pectx (array (marked_array false false nil (cons (idx args 0) (.marked_array_values (idx args 1))) nil) (mif (= 3 (len args)) (idx args 2) de)) (+ 1 indent))
        ) 'vapply 1 true))
        (array 'lapply (marked_prim_comb (dlambda (only_head de env_stack pectx args indent)
            (veval_inner only_head de env_stack pectx (array (marked_array false false nil (cons (with_wrap_level (idx args 0) (- (.any_comb_wrap_level (idx args 0)) 1)) (.marked_array_values (idx args 1))) nil) (mif (= 3 (len args)) (idx args 2) de)) (+ 1 indent))
        ) 'lapply 1 true))

        (array 'vau (marked_prim_comb (lambda (only_head de env_stack pectx params indent) (dlet (
            (mde?         (mif (= 3 (len params)) (idx params 0) nil))
            (vau_mde?     (mif (= nil mde?) (array) (array mde?)))
            (_ (print (indent_str indent) "mde? is " mde?))
            (_ (print (indent_str indent) "\tmde? if " (mif mde? #t #f)))
            (de?          (mif mde? (.marked_symbol_value mde?) nil))
            (_ (print (indent_str indent) "de? is " de?))
            (vau_de?      (mif (= nil de?) (array) (array de?)))
            (raw_marked_params (mif (= nil de?) (idx params 0) (idx params 1)))
            (raw_params (map (lambda (x) (mif (not (marked_symbol? x)) (error (str "not a marked symbol " x))
                                            (.marked_symbol_value x))) (.marked_array_values raw_marked_params)))

            ((variadic vau_params)  (foldl (dlambda ((v a) x) (mif (= x '&) (array true a) (array v (concat a (array x))))) (array false (array)) raw_params))
            ((ok body) (try_unval (mif (= nil de?) (idx params 1) (idx params 2)) (lambda (_) nil)))
            (_ (if (not ok) (error "actually impossible vau unval")))
            ((env_counter memo) pectx)
            (new_id env_counter)
            (env_counter (+ 1 env_counter))
            (pectx (array env_counter memo))
            ((pectx err pe_body) (if only_head (begin (print "skipping inner eval cuz only_head") (array pectx nil body))
                                       (dlet (
                                            (inner_env (make_tmp_inner_env vau_params de? de new_id))
                                            (_ (print_strip (indent_str indent) "in vau, evaluating body with 'later params - " body))
                                            ((pectx err pe_body) (partial_eval_helper body false inner_env (array (idx env_stack 0)
                                                                                                                  (cons inner_env (idx env_stack 1))) pectx (+ 1 indent) false))
                                            (_ (print_strip (indent_str indent) "in vau, result of evaluating body was " pe_body))
                                        ) (array pectx err pe_body))))
        ) (mif err (array pectx err nil) (array pectx nil (marked_comb 0 new_id de? de variadic vau_params pe_body nil)))
        )) 'vau 0 true))

        (array 'wrap (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled) indent)
              (if (comb? evaled) (array pectx nil (with_wrap_level evaled (+ (.any_comb_wrap_level evaled) 1)))
                                 (array pectx "bad passed to wrap" nil))
        ) 'wrap 1 true))

        (array 'unwrap (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled) indent)
              ; TODO should support prim comb like runtime
              (if (comb? evaled) (array pectx nil (with_wrap_level evaled (- (.any_comb_wrap_level evaled) 1)))
                                 (array pectx "bad passed to unwrap" nil))
        ) 'unwrap 1 true))

        (array 'cond (marked_prim_comb ((rec-lambda recurse (already_stripped) (lambda (only_head de env_stack pectx params indent)
            (mif (!= 0 (% (len params) 2)) (array pectx (str "partial eval cond with odd params " params) nil)
                (dlet (
                    (eval_helper    (lambda (to_eval pectx)
                                       (dlet (((ok unvald) (if already_stripped (array true to_eval)
                                                                                (try_unval to_eval (lambda (_) nil)))))
                                             (mif (not ok)
                                                  (array pectx "bad unval in cond" nil)
                                                  (partial_eval_helper unvald false de env_stack pectx (+ 1 indent) false)))))
                )
                ((rec-lambda recurse_inner (i so_far pectx)
                                          (dlet (((pectx err pred)                              (eval_helper (idx params i) pectx)))
                                          (cond ((!= nil err)                                   (array pectx err nil))
                                                ((later_head? pred)                             (dlet (
                                                                                            (sliced_params (slice params (+ i 1) -1))
                                                                                            (this (marked_array false true nil (concat (array (marked_prim_comb (recurse false) 'cond 0 true)
                                                                                                                                                         pred)
                                                                                                                                                  sliced_params) nil))
                                                                                            (hash (combine_hash (combine_hash 101 (.hash this)) (+ 103 (.marked_env_idx de))))
                                                                                            ((env_counter memo) pectx)
                                                                                            (already_in (!= false (get-value-or-false memo hash)))
                                                                                            (_ (if already_in (print_strip "ALREADY IN " this)
                                                                                                              (print_strip "NOT ALREADY IN, CONTINUING with " this)))
                                                                                            ; WE SHOULDN'T DIE ON ERROR, since these errors may be guarded by conditions we
                                                                                            ; can't evaluate. We'll keep branches that error as un-valed only
                                                                                            ((pectx _err evaled_params later_hash) (if already_in
                                                                                                        (array pectx nil (map (lambda (x) (dlet (((ok ux) (try_unval x (lambda (_) nil)))
                                                                                                                                                 (_ (if (not ok) (error "BAD cond un"))))
                                                                                                                                                 ux))
                                                                                                                              sliced_params) hash)
                                                                                                        (foldl (dlambda ((pectx _err as later_hash) x)
                                                                                                            (dlet (((pectx er a) (eval_helper x pectx)))
                                                                                                                (mif er (dlet (((ok ux) (if already_stripped (array true x) (try_unval x (lambda (_) nil))))
                                                                                                                                                 (_ (if (not ok) (error (str "BAD cond un" x)))))
                                                                                                                                                 (array pectx nil (concat as (array ux)) later_hash))
                                                                                                                        (array pectx nil (concat as (array a)) later_hash)))
                                                                                                        ) (array (array env_counter (put memo hash nil)) nil (array) nil) sliced_params)))
                                                                                            ((env_counter omemo) pectx)
                                                                                            (pectx (array env_counter memo))
                                                                                                ) (array pectx nil (marked_array false true later_hash (concat (array (marked_prim_comb (recurse true) 'vcond -1 true) pred) evaled_params) nil))))
                                                ((and (< (+ 2 i) (len params)) (false? pred))   (recurse_inner (+ 2 i) so_far pectx))
                                                (                              (false? pred)    (array pectx "comb reached end with no true" nil))
                                                (true                                           (eval_helper (idx params (+ i 1)) pectx))
                 ))) 0 (array) pectx))
            )
        )) false) 'cond 0 true))

        (needs_params_val_lambda 'symbol? symbol?)
        (needs_params_val_lambda 'int? int?)
        (needs_params_val_lambda 'string? string?)

        (array 'combiner? (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_param) indent)
            (array pectx nil (cond
                  ((comb? evaled_param)          (marked_val true))
                  ((prim_comb? evaled_param)     (marked_val true))
                  (true                          (marked_val false))
            ))
        ) 'combiner? 1 true))
        (array 'env? (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_param) indent)
            (array pectx nil (cond
                  ((marked_env? evaled_param)    (marked_val true))
                  (true                          (marked_val false))
            ))
        ) 'env? 1 true))
        (needs_params_val_lambda 'nil? nil?)
        (needs_params_val_lambda 'bool? bool?)
        (needs_params_val_lambda 'str-to-symbol str-to-symbol)
        (needs_params_val_lambda 'get-text get-text)

        (array 'array? (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_param) indent)
            (array pectx nil (cond
                  ((marked_array? evaled_param)  (marked_val true))
                  (true                          (marked_val false))
            ))
        ) 'array? 1 true))

        ; Look into eventually allowing some non values, perhaps, when we look at combiner non all value params
        (array 'array (marked_prim_comb (lambda (only_head de env_stack pectx evaled_params indent)
                                                (array pectx nil (marked_array true false nil evaled_params nil))
        ) 'array 1 false))
        (array 'len (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_param) indent)
            (cond
                  ((marked_array? evaled_param)         (array pectx nil (marked_val (len (.marked_array_values evaled_param)))))
                  ((and (val? evaled_param)
                        (string? (.val evaled_param)))  (array pectx nil (marked_val (len (.val evaled_param)))))
                  (true                                 (array pectx (str "bad type to len " evaled_param) nil))
            )
        ) 'len 1 true))
        (array 'idx (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_array evaled_idx) indent)
            (cond
                  ((and (val? evaled_idx) (marked_array? evaled_array))                       (if (< (.val evaled_idx) (len (.marked_array_values evaled_array)))
                                                                                                  (array pectx nil (idx (.marked_array_values evaled_array) (.val evaled_idx)))
                                                                                                  (array pectx (true_str "idx out of bounds " evaled_array " " evaled_idx) nil)))
                  ((and (val? evaled_idx) (val? evaled_array) (string? (.val evaled_array)))  (array pectx nil (marked_val (idx (.val evaled_array) (.val evaled_idx)))))
                  (true                                                                       (array pectx (str "bad type to idx " evaled_idx " " evaled_array) nil))
            )
        ) 'idx 1 true))
        (array 'slice (marked_prim_comb (dlambda (only_head de env_stack pectx (evaled_array evaled_begin evaled_end) indent)
            (cond
                  ((and (val? evaled_begin) (val? evaled_end) (marked_array? evaled_array))
                            (array pectx nil (marked_array true false nil (slice (.marked_array_values evaled_array) (.val evaled_begin) (.val evaled_end)) nil)))
                  ((and (val? evaled_begin) (val? evaled_end) (val? evaled_array) (string? (.val evaled_array)))
                            (array pectx nil (marked_val (slice (.val evaled_array) (.val evaled_begin) (.val evaled_end)))))
                  (true     (array pectx (str "bad params to slice " evaled_begin " " evaled_end " " evaled_array) nil))
            )
        ) 'slice 1 true))
        (array 'concat (marked_prim_comb (lambda (only_head de env_stack pectx evaled_params indent)
            (cond
                  ((foldl (lambda (a x) (and a (marked_array? x))) true evaled_params)
                        (array pectx nil (marked_array true false nil (lapply concat (map (lambda (x) (.marked_array_values x)) evaled_params)) nil)))
                  ((foldl (lambda (a x) (and a (val? x) (string? (.val x)))) true evaled_params)
                        (array pectx nil (marked_val (lapply concat (map (lambda (x) (.val x)) evaled_params)))))
                  (true                                                                (array pectx (str "bad params to concat " evaled_params) nil))
            )
        ) 'concat 1 true))

        (needs_params_val_lambda '+ +)
        (needs_params_val_lambda '- -)
        (needs_params_val_lambda '* *)
        (needs_params_val_lambda '/ /)
        (needs_params_val_lambda '% %)
        (needs_params_val_lambda 'band band)
        (needs_params_val_lambda 'bor bor)
        (needs_params_val_lambda 'bnot bnot)
        (needs_params_val_lambda 'bxor bxor)
        (needs_params_val_lambda '<< <<)
        (needs_params_val_lambda '>> >>)
        (needs_params_val_lambda '= =)
        (needs_params_val_lambda '!= !=)
        (needs_params_val_lambda '< <)
        (needs_params_val_lambda '<= <=)
        (needs_params_val_lambda '> >)
        (needs_params_val_lambda '>= >=)
        (needs_params_val_lambda 'str true_str)
        ;(needs_params_val_lambda 'pr-str pr-str)
        ;(needs_params_val_lambda 'prn prn)
        (give_up_eval_params 'log)
        (give_up_eval_params 'debug)
        (give_up_eval_params 'builtin_fib)
        ; really do need to figure out mif we want to keep meta, and add it mif so
        ;(give_up_eval_params 'meta meta)
        ;(give_up_eval_params 'with-meta with-meta)
        ; mif we want to get fancy, we could do error/recover too
        (give_up_eval_params 'error)
        ;(give_up_eval_params 'recover)
        (needs_params_val_lambda 'read-string read-string)
        (array 'empty_env (marked_env true nil nil nil nil nil))
    )))

    (partial_eval (lambda (x) (partial_eval_helper (idx (try_unval (mark x) (lambda (_) nil)) 1) false root_marked_env (array nil nil) (array 0 empty_dict) 0 false)))


    ;; WASM

    ; Vectors and Values
    ; Bytes encode themselves

    ; Note that the shift must be arithmatic
    (encode_LEB128 (rec-lambda recurse (x)
        (dlet ((b (band #x7F x))
              (v (>> x 7)))

            (cond ((or (and (= v 0) (= (band b #x40) 0)) (and (= v -1) (!= (band b #x40) 0))) (array b))
                  (true                                                                       (cons (bor b #x80) (recurse v)))))
    ))
    (encode_vector (lambda (enc v)
        (concat (encode_LEB128 (len v)) (flat_map enc v) )
    ))
    (encode_floating_point (lambda (x) (error "unimplemented")))
    (encode_name (lambda (name)
        (encode_vector (lambda (x) (array x)) (map char->integer (string->list name)))
    ))
    (hex_digit (lambda (digit) (dlet ((d (char->integer digit)))
                                    (cond ((< d #x3A) (- d #x30))
                                          ((< d #x47) (- d #x37))
                                          (true       (- d #x57))))))
    (encode_bytes (lambda (str)
        (encode_vector (lambda (x) (array x)) ((rec-lambda recurse (s) (cond
                                                                          ((= nil s)       nil)
                                                                          ((= #\\ (car s)) (cons (+ (* 16 (hex_digit (car (cdr s))))
                                                                                                          (hex_digit (car (cdr (cdr s))))) (recurse (cdr (cdr (cdr s))))))
                                                                          (true            (cons (char->integer (car s)) (recurse (cdr s))))
                                               )) (string->list str)))
    ))

    (encode_limits (lambda (x)
       (cond ((= 1 (len x)) (concat (array #x00) (encode_LEB128 (idx x 0))))
             ((= 2 (len x)) (concat (array #x01) (encode_LEB128 (idx x 0)) (encode_LEB128 (idx x 1))))
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
        (dlet (
            (encoded (encode_vector encode_function_type x))
        ) (concat (array #x01) (encode_LEB128 (len encoded)) encoded ))
    ))
    (encode_import (lambda (import)
        (dlet (
            ((mod_name name type idx) import)
        ) (concat (encode_name mod_name)
                  (encode_name name)
                  (cond ((= type 'func)   (concat (array #x00) (encode_LEB128 idx)))
                        ((= type 'table)  (concat (array #x01) (error "can't encode table type")))
                        ((= type 'memory) (concat (array #x02) (error "can't encode memory type")))
                        ((= type 'global) (concat (array #x03) (error "can't encode global type")))
                        (true                                  (error (str "bad import type" type)))))
        )
    ))
    (encode_import_section (lambda (x)
        (dlet (
            (encoded (encode_vector encode_import x))
        ) (concat (array #x02) (encode_LEB128 (len encoded)) encoded ))
    ))

    (encode_table_type (lambda (t) (concat (encode_ref_type (idx t 0)) (encode_limits (idx t 1)))))

    (encode_table_section (lambda (x)
        (dlet (
            (encoded (encode_vector encode_table_type x))
        ) (concat (array #x04) (encode_LEB128 (len encoded)) encoded ))
    ))
    (encode_memory_section (lambda (x)
        (dlet (
            (encoded (encode_vector encode_limits x))
        ) (concat (array #x05) (encode_LEB128 (len encoded)) encoded ))
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
                  (encode_LEB128 idx)
        ))
    ))
    (encode_export_section (lambda (x)
        (dlet (
            ;(_ (print "encoding element " x))
            (encoded (encode_vector encode_export x))
            ;(_ (print "donex"))
        ) (concat (array #x07) (encode_LEB128 (len encoded)) encoded ))
    ))

    (encode_start_section (lambda (x)
        (cond ((= 0 (len x)) (array))
              ((= 1 (len x)) (dlet ((encoded (encode_LEB128 (idx x 0)))) (concat (array #x08) (encode_LEB128 (len encoded)) encoded )))
              (true          (error (str "bad lenbgth for start section " (len x) " was " x))))
    ))

    (encode_function_section (lambda (x)
        (dlet (                                     ; nil functions are placeholders for improted functions
            ;(_ (println "encoding function section " x))
            (filtered (filter (lambda (i) (!= nil i)) x))
            ;(_ (println "post filtered " filtered))
            (encoded (encode_vector encode_LEB128 filtered))
        ) (concat (array #x03) (encode_LEB128 (len encoded)) encoded ))
    ))
    (encode_blocktype (lambda (type) (cond ((symbol? type)   (encode_valtype type))
                                           ((= (array) type) (array #x40)) ; empty type
                                           (true             (encode_LEB128 type))
                                   )))

    (encode_ins (rec-lambda recurse (ins)
        (dlet (
            (op (idx ins 0))
        ) (cond ((= op 'unreachable)            (array #x00))
                ((= op 'nop)                    (array #x01))
                ((= op 'block)          (concat (array #x02) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (array #x0B)))
                ((= op 'loop)           (concat (array #x03) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (array #x0B)))
                ((= op 'if)             (concat (array #x04) (encode_blocktype (idx ins 1)) (flat_map recurse (idx ins 2)) (if (!= 3 (len ins)) (concat (array #x05) (flat_map recurse (idx ins 3)))
                                                                                                                                          (array ))     (array #x0B)))
                ((= op 'br)             (concat (array #x0C) (encode_LEB128 (idx ins 1))))
                ((= op 'br_if)          (concat (array #x0D) (encode_LEB128 (idx ins 1))))
                ;...
                ((= op 'return)                 (array #x0F))
                ((= op 'call)           (concat (array #x10) (encode_LEB128 (idx ins 1))))
                ((= op 'call_indirect)  (concat (array #x11) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ; skipping a bunch
                ; Parametric Instructions
                ((= op 'drop)                   (array #x1A))
                ; skip
                ; Variable Instructions
                ((= op 'local.get)      (concat (array #x20) (encode_LEB128 (idx ins 1))))
                ((= op 'local.set)      (concat (array #x21) (encode_LEB128 (idx ins 1))))
                ((= op 'local.tee)      (concat (array #x22) (encode_LEB128 (idx ins 1))))
                ((= op 'global.get)     (concat (array #x23) (encode_LEB128 (idx ins 1))))
                ((= op 'global.set)     (concat (array #x24) (encode_LEB128 (idx ins 1))))
                ; table
                ; memory
                ((= op 'i32.load)       (concat (array #x28) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load)       (concat (array #x29) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.load8_s)    (concat (array #x2C) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.load8_u)    (concat (array #x2D) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.load16_s)   (concat (array #x2E) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.load16_u)   (concat (array #x2F) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load8_s)    (concat (array #x30) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load8_u)    (concat (array #x31) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load16_s)   (concat (array #x32) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load16_u)   (concat (array #x33) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load32_s)   (concat (array #x34) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.load32_u)   (concat (array #x35) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.store)      (concat (array #x36) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.store)      (concat (array #x37) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.store8)     (concat (array #x3A) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i32.store16)    (concat (array #x3B) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.store8)     (concat (array #x3C) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'i64.store16)    (concat (array #x3D) (encode_LEB128 (idx ins 1)) (encode_LEB128 (idx ins 2))))
                ((= op 'memory.grow)            (array #x40 #x00))
                ; Numeric Instructions
                ((= op 'i32.const)      (concat (array #x41) (encode_LEB128 (idx ins 1))))
                ((= op 'i64.const)      (concat (array #x42) (encode_LEB128 (idx ins 1))))
                ((= op 'i32.eqz)                (array #x45))
                ((= op 'i32.eq)                 (array #x46))
                ((= op 'i32.ne)                 (array #x47))
                ((= op 'i32.lt_s)               (array #x48))
                ((= op 'i32.lt_u)               (array #x49))
                ((= op 'i32.gt_s)               (array #x4A))
                ((= op 'i32.gt_u)               (array #x4B))
                ((= op 'i32.le_s)               (array #x4C))
                ((= op 'i32.le_u)               (array #x4D))
                ((= op 'i32.ge_s)               (array #x4E))
                ((= op 'i32.ge_u)               (array #x4F))

                ((= op 'i64.eqz)                (array #x50))
                ((= op 'i64.eq)                 (array #x51))
                ((= op 'i64.ne)                 (array #x52))
                ((= op 'i64.lt_s)               (array #x53))
                ((= op 'i64.lt_u)               (array #x54))
                ((= op 'i64.gt_s)               (array #x55))
                ((= op 'i64.gt_u)               (array #x56))
                ((= op 'i64.le_s)               (array #x57))
                ((= op 'i64.le_u)               (array #x58))
                ((= op 'i64.ge_s)               (array #x59))
                ((= op 'i64.ge_u)               (array #x5A))

                ((= op 'i32.add)                (array #x6A))
                ((= op 'i32.sub)                (array #x6B))
                ((= op 'i32.mul)                (array #x6C))
                ((= op 'i32.div_s)              (array #x6D))
                ((= op 'i32.div_u)              (array #x6E))
                ((= op 'i32.rem_s)              (array #x6F))
                ((= op 'i32.rem_u)              (array #x70))
                ((= op 'i32.and)                (array #x71))
                ((= op 'i32.or)                 (array #x72))
                ((= op 'i32.shl)                (array #x74))
                ((= op 'i32.shr_s)              (array #x75))
                ((= op 'i32.shr_u)              (array #x76))
                ((= op 'i64.add)                (array #x7C))
                ((= op 'i64.sub)                (array #x7D))
                ((= op 'i64.mul)                (array #x7E))
                ((= op 'i64.div_s)              (array #x7F))
                ((= op 'i64.div_u)              (array #x80))
                ((= op 'i64.rem_s)              (array #x81))
                ((= op 'i64.rem_u)              (array #x82))
                ((= op 'i64.and)                (array #x83))
                ((= op 'i64.or)                 (array #x84))
                ((= op 'i64.xor)                (array #x85))
                ((= op 'i64.shl)                (array #x86))
                ((= op 'i64.shr_s)              (array #x87))
                ((= op 'i64.shr_u)              (array #x88))

                ((= op 'i32.wrap_i64)           (array #xA7))
                ((= op 'i64.extend_i32_s)       (array #xAC))
                ((= op 'i64.extend_i32_u)       (array #xAD))

                ((= op 'memory.copy)            (array #xFC #x0A #x00 #x00))
        ))
    ))

    (encode_expr (lambda (expr) (concat (flat_map encode_ins expr) (array #x0B))))
    (encode_code (lambda (x)
        (dlet (
            ((locals body) x)
            (enc_locals (encode_vector (lambda (loc)
                                    (concat (encode_LEB128 (idx loc 0)) (encode_valtype (idx loc 1)))) locals))
            (enc_expr (encode_expr body))
            (code_bytes (concat enc_locals enc_expr))
        ) (concat (encode_LEB128 (len code_bytes)) code_bytes))
    ))
    (encode_code_section (lambda (x)
        (dlet (
            (encoded (encode_vector encode_code x))
        ) (concat (array #x0A) (encode_LEB128 (len encoded)) encoded ))
    ))

    (encode_global_type (lambda (t) (concat (encode_valtype (idx t 0)) (cond ((= (idx t 1) 'const) (array #x00))
                                                                             ((= (idx t 1) 'mut)   (array #x01))
                                                                             (true                 (error (str "bad mutablity " (idx t 1))))))))
    (encode_global_section (lambda (global_section)
        (dlet (
            ;(_ (print "encoding exprs " global_section))
            (encoded (encode_vector (lambda (x) (concat (encode_global_type (idx x 0)) (encode_expr (idx x 1)))) global_section))
        ) (concat (array #x06) (encode_LEB128 (len encoded)) encoded ))
    ))

    ; only supporting one type of element section for now, active funcrefs with offset
    (encode_element (lambda (x) (concat (array #x00) (encode_expr (idx x 0)) (encode_vector encode_LEB128 (idx x 1)))))
    (encode_element_section (lambda (x)
        (dlet (
            ;(_ (print "encoding element " x))
            (encoded (encode_vector encode_element x))
            ;(_ (print "donex"))
        ) (concat (array #x09) (encode_LEB128 (len encoded)) encoded ))
    ))


    (encode_data (lambda (data) (cond ((= 2 (len data)) (concat (array #x00) (encode_expr (idx data 0)) (encode_bytes (idx data 1))))
                                      ((= 1 (len data)) (concat (array #x01) (encode_bytes (idx data 0))))
                                      ((= 3 (len data)) (concat (array #x02) (encode_LEB128 (idx data 0)) (encode_expr (idx data 1)) (encode_bytes (idx data 2))))
                                      (true             (error (str "bad data" data))))))
    (encode_data_section (lambda (x)
        (dlet (
            (encoded (encode_vector encode_data x))
        ) (concat (array #x0B) (encode_LEB128 (len encoded)) encoded ))
    ))


    (wasm_to_binary (lambda (wasm_code)
        (dlet (
            ((type_section import_section function_section table_section memory_section global_section export_section start_section element_section code_section data_section) wasm_code)
            ;(_ (println "type_section" type_section "import_section" import_section "function_section" function_section "memory_section" memory_section "global_section" global_section "export_section" export_section "start_section" start_section "element_section" element_section "code_section" code_section "data_section" data_section))
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
            ;data_count (dlet (body (encode_LEB128 (len data_section))) (concat (array #x0C) (encode_LEB128 (len body)) body))
            (data_count (array))
        ) (concat magic version type import function table memory global export data_count start elem code data))
    ))

    (module (lambda args (dlet (
        (helper (rec-lambda recurse (entries i name_dict type import function table memory global export start elem code data)
            (if (= i (len entries)) (array  type import function table memory global export start elem code data)
                (dlet (
                    ((n_d t im f ta m g e s elm c d) ((idx entries i) name_dict type import function table memory global export start elem code data))
                 ) (recurse entries (+ i 1) n_d t im f ta m g e s elm c d)))))
    ) (helper (apply concat args) 0 empty_dict (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array ) (array )))))

    (table (lambda (idx_name . limits_type) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len table)) type import function (concat table (array (array (idx limits_type -1) (slice limits_type 0 -2) ))) memory global export start elem code data )))))

    (memory (lambda (idx_name . limits) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len memory)) type import function table (concat memory (array limits)) global export start elem code data )))))

    (func (lambda (name . inside) (array (lambda (name_dict type import function table memory global export start elem code data)
        (dlet (
            ;(_ (print "ok, doing a func: " name " with inside " inside))
            ((params result locals body) ((rec-lambda recurse (i pe re)
                                            (cond ((and (= false pe) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'param (idx (idx inside i) 0)))
                                                             (recurse (+ i 1) pe re))
                                                  ((and (= false pe) (= false re) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'result (idx (idx inside i) 0)))
                                                            ; only one result possible
                                                             (recurse (+ i 1) i (+ i 1)))
                                                  ((and (= false re) (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'result (idx (idx inside i) 0)))
                                                            ; only one result possible
                                                             (recurse (+ i 1) pe (+ i 1)))
                                                  ((and              (< i (len inside)) (array? (idx inside i)) (< 0 (len (idx inside i))) (= 'local (idx (idx inside i) 0)))
                                                             (recurse (+ i 1) (or pe i) (or re i)))
                                                  (true      (array (slice inside 0 (or pe i)) (slice inside (or pe i) (or re pe i)) (slice inside (or re pe i) i) (slice inside i -1)))
                                            )
                                        ) 0 false false))
            (result (if (!= 0 (len result)) (array (idx (idx result 0) 1))
                                           result))
            ;(_ (println "params " params " result " result " locals " locals " body " body))
            (outer_name_dict (put name_dict name (len function)))
            ((num_params inner_name_dict) (foldl (lambda (a x) (array (+ (idx a 0) 1) (put (idx a 1) (idx x 1) (idx a 0)))) (array 0 outer_name_dict ) params))
            ((num_locals inner_name_dict) (foldl (lambda (a x) (array (+ (idx a 0) 1) (put (idx a 1) (idx x 1) (idx a 0)))) (array num_params inner_name_dict ) locals))
            ;(_ (println "inner name dict" inner_name_dict))
            (compressed_locals ((rec-lambda recurse (cur_list cur_typ cur_num i)
                (cond ((and (= i (len locals)) (= 0 cur_num)) cur_list)
                           ((= i (len locals))                (concat cur_list (array (array cur_num cur_typ) )))
                      ((= cur_typ (idx (idx locals i) 2))     (recurse cur_list                              cur_typ                (+ 1 cur_num) (+ 1 i)))
                      ((= nil cur_typ)                        (recurse cur_list                              (idx (idx locals i) 2) 1             (+ 1 i)))
                      (true                                   (recurse (concat cur_list (array (array cur_num cur_typ))) (idx (idx locals i) 2) 1             (+ 1 i))))
               ) (array) nil 0 0))
            ;(_ (println "params: " params " result: " result))
            (our_type (array (map (lambda (x) (idx x 2)) params) result))
            ;(inner_env (add-dict-to-env de (put inner_name_dict 'depth 0)))
            (inner_name_dict_with_depth (put inner_name_dict 'depth 0))
            ;(_ (println "about to get our_code: " body))
            (our_code (flat_map (lambda (inss) (map (lambda (ins) (ins inner_name_dict_with_depth)) inss))
                                body))
            ;(_ (println "resulting code " our_code))
            (final_code (concat code (array (array compressed_locals our_code ) )))
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
            final_code
            ; data
            data
        ))
    ))))


    ;;;;;;;;;;;;;;;
    ; Instructions
    ;;;;;;;;;;;;;;;
    (unreachable    (lambda ()                                           (array (lambda (name_dict) (array 'unreachable)))))
    (_drop           (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'drop))))))
    (i32.const      (lambda (const)                                      (array (lambda (name_dict) (array 'i32.const const)))))
    (i64.const      (lambda (const)                                      (array (lambda (name_dict) (array 'i64.const const)))))
    (local.get      (lambda (const)                                      (array (lambda (name_dict)     (array 'local.get (if (int? const) const (get-value name_dict const)))))))
    (local.set      (lambda (const . flatten) (concat (apply concat flatten) (array (lambda (name_dict) (array 'local.set (if (int? const) const (get-value name_dict const))))))))
    (local.tee      (lambda (const . flatten) (concat (apply concat flatten) (array (lambda (name_dict) (array 'local.tee (if (int? const) const (get-value name_dict const))))))))
    (global.get     (lambda (const)                                      (array (lambda (name_dict)     (array 'global.get (if (int? const) const (get-value name_dict const)))))))
    (global.set     (lambda (const . flatten) (concat (apply concat flatten) (array (lambda (name_dict) (array 'global.set (if (int? const) const (get-value name_dict const))))))))
    (i32.add        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.add))))))
    (i32.sub        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.sub))))))
    (i32.mul        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.mul))))))
    (i32.div_s      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.div_s))))))
    (i32.div_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.div_u))))))
    (i32.rem_s      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.rem_s))))))
    (i32.rem_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.rem_u))))))
    (i32.and        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.and))))))
    (i32.or         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.or))))))
    (i64.add        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.add))))))
    (i64.sub        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.sub))))))
    (i64.mul        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.mul))))))
    (i64.div_s      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.div_s))))))
    (i64.div_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.div_u))))))
    (i64.rem_s      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.rem_s))))))
    (i64.rem_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.rem_u))))))
    (i64.and        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.and))))))
    (i64.or         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.or))))))
    (i64.xor        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.xor))))))

    (i32.eqz        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.eqz))))))
    (i32.eq         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.eq))))))
    (i32.ne         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.ne))))))
    (i32.lt_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.lt_s))))))
    (i32.lt_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.lt_u))))))
    (i32.gt_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.gt_s))))))
    (i32.gt_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.gt_u))))))
    (i32.le_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.le_s))))))
    (i32.le_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.le_u))))))
    (i32.ge_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.ge_s))))))
    (i32.ge_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.ge_u))))))
    (i64.eqz        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.eqz))))))
    (i64.eq         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.eq))))))
    (i64.ne         (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.ne))))))
    (i64.lt_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.lt_s))))))
    (i64.lt_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.lt_u))))))
    (i64.gt_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.gt_s))))))
    (i64.gt_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.gt_u))))))
    (i64.le_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.le_s))))))
    (i64.le_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.le_u))))))
    (i64.ge_s       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.ge_s))))))
    (i64.ge_u       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.ge_u))))))

    (mem_load   (lambda (op align) (lambda flatten (dlet (
        (explicit_offset (int? (idx flatten 0)))
        (offset       (if explicit_offset (idx flatten 0)      0))
        (flatten_rest (if explicit_offset (slice flatten 1 -1) flatten))
    ) (concat (apply concat flatten_rest) (array (lambda (name_dict) (array op align offset))))))))

    (i32.load       (mem_load 'i32.load 2))
    (i64.load       (mem_load 'i64.load 3))
    (i32.store      (mem_load 'i32.store 2))
    (i64.store      (mem_load 'i64.store 3))
    (i32.store8     (mem_load 'i32.store8 0))
    (i32.store16    (mem_load 'i32.store16 1))
    (i64.store8     (mem_load 'i64.store8 0))
    (i64.store16    (mem_load 'i64.store16 1))

    (i32.load8_s    (mem_load 'i32.load8_s  0))
    (i32.load8_u    (mem_load 'i32.load8_u  0))
    (i32.load16_s   (mem_load 'i32.load16_s 1))
    (i32.load16_u   (mem_load 'i32.load16_u 1))
    (i64.load8_s    (mem_load 'i64.load8_s  0))
    (i64.load8_u    (mem_load 'i64.load8_u  0))
    (i64.load16_s   (mem_load 'i64.load16_s 1))
    (i64.load16_u   (mem_load 'i64.load16_u 1))
    (i64.load32_s   (mem_load 'i64.load32_s 2))
    (i64.load32_u   (mem_load 'i64.load32_u 2))

    (memory.grow    (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'memory.grow))))))
    (i32.shl        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.shl))))))
    (i32.shr_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.shr_u))))))
    (i64.shl        (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.shl))))))
    (i64.shr_s      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.shr_s))))))
    (i64.shr_u      (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.shr_u))))))

    (i32.wrap_i64       (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i32.wrap_i64))))))
    (i64.extend_i32_s   (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.extend_i32_s))))))
    (i64.extend_i32_u   (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'i64.extend_i32_u))))))

    (memory.copy    (lambda flatten  (concat (apply concat flatten)      (array (lambda (name_dict) (array 'memory.copy))))))

    (block_like_body (lambda (name_dict name inner) (dlet (
                                                    (new_depth (+ 1 (get-value name_dict 'depth)))
                                                    (inner_env (put (put name_dict name new_depth) 'depth new_depth))
                                                ) (flat_map (lambda (inss) (map (lambda (ins) (ins inner_env)) inss)) inner))))


    (block     (lambda (name . inner)                               (array (lambda (name_dict) (array 'block (array) (block_like_body name_dict name inner))))))
    (_loop     (lambda (name . inner)                               (array (lambda (name_dict) (array 'loop  (array) (block_like_body name_dict name inner))))))
    (_if       (lambda (name . inner) (dlet (
                                            ((end_idx else_section) (if (= 'else (idx (idx inner -1) 0))        (array -2            (slice (idx inner -1) 1 -1) )
                                                                                                                (array -1             nil )))
                                            ((end_idx then_section) (if (= 'then (idx (idx inner end_idx) 0))   (array (- end_idx 1) (slice (idx inner end_idx) 1 -1) )
                                                                                                                (array (- end_idx 1) (array (idx inner end_idx) ) )))
                                            ((start_idx result_t)   (if (= 'result (idx (idx inner 0) 0))       (array 1             (idx (idx inner 0) 1))
                                                                                                                (array 0             (array))))
                                            (flattened (apply concat (slice inner start_idx end_idx)))
                                            ;(_ (println "result_t " result_t " flattened " flattened " then_section " then_section " else_section " else_section))
                                           ) (concat flattened      (array (lambda (name_dict) (concat (array 'if result_t (block_like_body name_dict name then_section))
                                                                                                      (if (!= nil else_section) (array (block_like_body name_dict name else_section))
                                                                                  (array)))))))))

    (then (lambda rest (cons 'then rest)))
    (else (lambda rest (cons 'else rest)))

    (br        (lambda (block) (array (lambda (name_dict) (array 'br (if (int? block) block (- (get-value name_dict 'depth) (get-value name_dict block))))))))
    (br_if     (lambda (block . flatten) (concat (apply concat flatten)  (array (lambda (name_dict) (array 'br_if (if (int? block) block (- (get-value name_dict 'depth) (get-value name_dict block)))))))))
    (call      (lambda (f . flatten)  (concat (apply concat flatten)  (array (lambda (name_dict) (array 'call (if (int? f) f (get-value name_dict f))))))))
    (call_indirect (lambda (type_idx table_idx . flatten)  (concat (apply concat flatten)  (array (lambda (name_dict) (array 'call_indirect type_idx table_idx))))))

    ;;;;;;;;;;;;;;;;;;;
    ; End Instructions
    ;;;;;;;;;;;;;;;;;;;

    (import (lambda (mod_name name t_idx_typ) (array (lambda (name_dict type import function table memory global export start elem code data) (dlet (
            (_ (if (!= 'func (idx t_idx_typ 0)) (error "only supporting importing functions rn")))
            ((import_type idx_name param_type result_type) t_idx_typ)
            (actual_type_idx (len type))
            (actual_type (array (slice param_type 1 -1) (slice result_type 1 -1) ))
        )
        (array (put name_dict idx_name (len function)) (concat type (array actual_type)) (concat import (array (array mod_name name import_type actual_type_idx) )) (concat function (array nil)) table memory global export start elem code data ))
    ))))

    (global (lambda (idx_name global_type expr) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array (put name_dict idx_name (len global))
          type import function table memory
          (concat global (array (array (if (array? global_type) (reverse global_type) (array global_type 'const)) (map (lambda (x) (x empty_dict)) expr) )))
          export start elem code data )
    ))))

    (export (lambda (name t_v) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global
               (concat export (array (array name (idx t_v 0) (get-value name_dict (idx t_v 1)) ) ))
               start elem code data )
    ))))

    (start (lambda (name) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global export (concat start (array (get-value name_dict name))) elem code data )
    ))))

    (elem (lambda (offset . entries) (array (lambda (name_dict type import function table memory global export start elem code data)
        (array name_dict type import function table memory global export start (concat elem (array (array (map (lambda (x) (x empty_dict)) offset) (map (lambda (x) (if (int? x) x (get-value name_dict x))) entries)))) code data )
    ))))

    (data (lambda it (array (lambda (name_dict type import function table memory global export start elem code data)
                       (array name_dict type import function table memory global export start elem code
                              (concat data (array (map (lambda (x) (if (array? x) (map (lambda (y) (y empty_dict)) x) x)) it))))))))


    ; Everything is an i64, and we're on a 32 bit wasm system, so we have a good many bits to play with

    ; Int - should maximize int
    ;  xxxxx0

    ; String - should be close to array, bitpacked, just different ptr rep?
    ;  <string_size32><string_ptr29>011

    ; Symbol - ideally interned (but not yet) also probs small-symbol-opt (def not yet)
    ;  <symbol_size32><symbol_ptr29>111

    ; Array / Nil
    ;  <array_size32><array_ptr29>101 / 0..0 101

    ; Combiner - a double of func index and closure (which could just be the env, actually, even if we trim...)
    ;  <func_idx29>|<env_ptr29><usesde1><wrap1>0001

    ; Env
    ; 0..0<env_ptr32 but still aligned>01001
    ;   Env object is <key_array_value><value_array_value><upper_env_value>
    ;       each being the full 64 bit objects.
    ;       This lets key_array exist in constant mem, and value array to come directly from passed params.

    ; True          / False
    ;  0..0 1 11001  /  0..0 0 11001

    (to_hex_digit (lambda (x) (string (integer->char (if (< x 10) (+ x #x30)
                                                                  (+ x #x37))))))
    (le_hexify_helper (rec-lambda recurse (x i) (if (= i 0) ""
                                                                  (concat "\\" (to_hex_digit (remainder (quotient x 16) 16))
                                                                               (to_hex_digit (remainder x 16))
                                                                               (recurse (quotient x 256) (- i 1))))))
    (i64_le_hexify (lambda (x) (le_hexify_helper (bitwise-and x #xFFFFFFFFFFFFFFFF) 8)))
    (i32_le_hexify (lambda (x) (le_hexify_helper (bitwise-and x #xFFFFFFFF) 4)))

    (nil_val        #b0101)
    (emptystr_val   #b0011)
    (true_val  #b000111001)
    (false_val #b000011001)

    (compile (dlambda ((pectx partial_eval_err marked_code) needs_runtime_eval) (mif partial_eval_err (error partial_eval_err) (wasm_to_binary (module
          (import "wasi_unstable" "path_open"
                  '(func $path_open  (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                     (result i32)))
          (import "wasi_unstable" "fd_read"
                  '(func $fd_read  (param i32 i32 i32 i32)
                                   (result i32)))
          (import "wasi_unstable" "fd_write"
                  '(func $fd_write (param i32 i32 i32 i32)
                                   (result i32)))
          (global '$malloc_head  '(mut i32) (i32.const 0))
          ;(global '$debug_malloc_head  '(mut i32) (i32.const 0))
          (global '$phs          '(mut i32) (i32.const 0))
          (global '$phl          '(mut i32) (i32.const 0))

          (global '$stack_trace  '(mut i64) (i64.const nil_val))
          (global '$debug_depth  '(mut i32) (i32.const -1))
          (global '$debug_func_to_call   '(mut i64) (i64.const nil_val))
          (global '$debug_params_to_call '(mut i64) (i64.const nil_val))
          (global '$debug_env_to_call    '(mut i64) (i64.const nil_val))

          (global '$num_mallocs  '(mut i32) (i32.const 0))
          (global '$num_sbrks    '(mut i32) (i32.const 0))
          (global '$num_frees    '(mut i32) (i32.const 0))

          (dlet (
              (alloc_data (dlambda (d (watermark datas)) (cond ((str? d)      (dlet ((size (+ 8 (band (len d) -8))))
                                                                                   (array (+ watermark 8)
                                                                                          (len d)
                                                                                          (array (+ watermark 8 size)
                                                                                                 (concat datas
                                                                                                         (data (i32.const watermark)
                                                                                                               (concat (i32_le_hexify size) "\\00\\00\\00\\80" d)))))))
                                                               (true          (error (str "can't alloc_data for anything else besides strings yet" d)))
                                                         )
              ))
              ; We won't use 0 because some impls seem to consider that NULL and crash on reading/writing?
              (iov_tmp 8) ; <32bit len><32bit ptr> + <32bit numwitten>
              (datasi (array (+ iov_tmp 16) (array)))
              ((true_loc true_length datasi) (alloc_data "true" datasi))
              ((false_loc false_length datasi) (alloc_data "false" datasi))

              ((bad_params_number_loc bad_params_number_length datasi) (alloc_data "\nError: passed a bad number of parameters\n" datasi))
              (bad_params_number_msg_val (bor (<< bad_params_number_length 32) bad_params_number_loc #b011))

              ((bad_params_type_loc bad_params_type_length datasi) (alloc_data "\nError: passed a bad type of parameters\n" datasi))
              (bad_params_type_msg_val (bor (<< bad_params_type_length 32) bad_params_type_loc #b011))

              ((dropping_loc dropping_length datasi) (alloc_data "dropping " datasi))
              (dropping_msg_val (bor (<< dropping_length 32) dropping_loc #b011))

              ((duping_loc duping_length datasi) (alloc_data "duping " datasi))
              (duping_msg_val (bor (<< duping_length 32) duping_loc #b011))

              ((error_loc error_length datasi) (alloc_data "\nError: " datasi))
              (error_msg_val (bor (<< error_length 32) error_loc #b011))
              ((log_loc log_length datasi) (alloc_data "\nLog: " datasi))
              (log_msg_val (bor (<< log_length 32) log_loc #b011))

              ((call_ok_loc call_ok_length datasi) (alloc_data "call ok!" datasi))
              (call_ok_msg_val (bor (<< call_ok_length 32) call_ok_loc #b011))

              ((newline_loc newline_length datasi) (alloc_data "\n" datasi))
              (newline_msg_val (bor (<< newline_length 32) newline_loc #b011))

              ((space_loc space_length datasi) (alloc_data " " datasi))
              (space_msg_val (bor (<< space_length 32) space_loc #b011))

              ((remaining_eval_loc remaining_eval_length datasi) (alloc_data "\nError: trying to call remainin eval\n" datasi))
              (remaining_eval_msg_val (bor (<< remaining_eval_length 32) remaining_eval_loc #b011))

              ((hit_upper_in_eval_loc hit_upper_in_eval_length datasi) (alloc_data "\nError: hit nil upper env when looking up symbol in remaining eval: " datasi))
              (hit_upper_in_eval_msg_val (bor (<< hit_upper_in_eval_length 32) hit_upper_in_eval_loc #b011))

              ((remaining_vau_loc remaining_vau_length datasi) (alloc_data "\nError: trying to call remainin vau (primitive)\n" datasi))
              (remaining_vau_msg_val (bor (<< remaining_vau_length 32) remaining_vau_loc #b011))

              ((no_true_cond_loc no_true_cond_length datasi) (alloc_data "\nError: runtime cond had no true branch\n" datasi))
              (no_true_cond_msg_val (bor (<< no_true_cond_length 32) no_true_cond_loc #b011))

              ((weird_wrap_loc weird_wrap_length datasi) (alloc_data "\nError: trying to call a combiner with a weird wrap (not 0 or 1)\n" datasi))
              (weird_wrap_msg_val (bor (<< weird_wrap_length 32) weird_wrap_loc #b011))

              ((bad_not_vau_loc bad_not_vau_length datasi) (alloc_data "\nError: Trying to call a function (not vau) but the parameters caused a compile error\n" datasi))
              (bad_not_vau_msg_val (bor (<< bad_not_vau_length 32) bad_not_vau_loc #b011))

              ((going_up_loc going_up_length datasi) (alloc_data "going up" datasi))
              (going_up_msg_val (bor (<< going_up_length 32) going_up_loc #b011))

              ((starting_from_loc starting_from_length datasi) (alloc_data "starting from " datasi))
              (starting_from_msg_val (bor (<< starting_from_length 32) starting_from_loc #b011))

              ((got_it_loc got_it_length datasi) (alloc_data "got it" datasi))
              (got_it_msg_val (bor (<< got_it_length 32) got_it_loc #b011))

              ((couldnt_parse_1_loc              couldnt_parse_1_length datasi) (alloc_data "\nError: Couldn't parse:\n" datasi))
              ( couldnt_parse_1_msg_val (bor (<< couldnt_parse_1_length 32) couldnt_parse_1_loc #b011))
              ((couldnt_parse_2_loc              couldnt_parse_2_length datasi) (alloc_data "\nAt character:\n" datasi))
              ( couldnt_parse_2_msg_val (bor (<< couldnt_parse_2_length 32) couldnt_parse_2_loc #b011))
              ((parse_remaining_loc              parse_remaining_length datasi) (alloc_data "\nLeft over after parsing, starting at byte offset:\n" datasi))
              ( parse_remaining_msg_val (bor (<< parse_remaining_length 32) parse_remaining_loc #b011))

              ((quote_sym_loc quote_sym_length datasi) (alloc_data "quote" datasi))
              (quote_sym_val (bor (<< quote_sym_length 32) quote_sym_loc #b111))

              ; 0 is path_open, 1 is fd_read, 2 is fd_write
              ;(num_pre_functions 2)
              (num_pre_functions 3)
              ((func_idx funcs) (array num_pre_functions (array)))

              (type_int      (array #b1      #b0))
              (type_string   (array #b111    #b011))
              (type_symbol   (array #b111    #b111))
              (type_array    (array #b111    #b101))
              (type_combiner (array #b1111   #b0001))
              (type_env      (array #b11111  #b01001))
              (type_bool     (array #b11111  #b11001))

              (typecheck (dlambda (idx result_type op (mask value) then_branch else_branch)
                (apply _if (concat (array '$matches) result_type
                    (array (op (i64.const value) (i64.and (i64.const mask) (i64.load (* 8 idx) (local.get '$ptr)))))
                    then_branch
                    else_branch
                ))
              ))

              (type_assert (rec-lambda type_assert (i type_check name_msg_val)
                (typecheck i (array)
                    i64.ne (if (array? (idx type_check 0)) (idx type_check 0) type_check)
                    (array (then
                        (if (and (array? (idx type_check 0)) (> (len type_check) 1))
                              (type_assert i (slice type_check 1 -1) name_msg_val)
                              (concat
                                    (call '$print (i64.const bad_params_type_msg_val))
                                    (call '$print (i64.const (<< i 1)))
                                    (call '$print (i64.const space_msg_val))
                                    (call '$print (i64.const name_msg_val))
                                    (call '$print (i64.const space_msg_val))
                                    (call '$print (i64.load (* 8 i) (local.get '$ptr)))
                                    (unreachable)
                              )
                        )
                    ))
                    nil
                )
              ))

              ; malloc allocates with size and refcount in header
              ((k_malloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$malloc '(param $bytes i32) '(result i32) '(local $result i32) '(local $ptr i32) '(local $last i32) '(local $pages i32)
                (global.set '$num_mallocs (i32.add (i32.const 1) (global.get '$num_mallocs)))
                ;(local.set '$bytes (i32.add (i32.const 24) (local.get '$bytes))) ; MDEBUG

                (local.set '$bytes (i32.add (i32.const 8) (local.get '$bytes)))
                ; ROUND AND ALIGN to 8 byte boundries (1 word) NOT ALLOWED - we expect 16 byte boundries, seemingly?
                ; (though it doesn't seem like it from the ptr encoding :/) It crashes if only 8...
                ;(local.set '$bytes (i32.and (i32.const -16) (i32.add (i32.const 15) (local.get '$bytes))))
                ; or heck, to 4 word boundries
                ;(local.set '$bytes (i32.and (i32.const -32) (i32.add (i32.const 31) (local.get '$bytes))))
                ; or 8 word boundries!
                (local.set '$bytes (i32.and (i32.const -64) (i32.add (i32.const 63) (local.get '$bytes))))

                (local.set '$result (i32.const 0))
                (_if '$has_head
                    (i32.ne (i32.const 0) (global.get '$malloc_head))
                    (then
                        (local.set '$ptr (global.get '$malloc_head))
                        (local.set '$last (i32.const 0))
                        (_loop '$l
                            (_if '$fits
                                (i32.ge_u (i32.load 0 (local.get '$ptr)) (local.get '$bytes))
                                (then
                                    (local.set '$result (local.get '$ptr))
                                    (_if '$head
                                        (i32.eq (local.get '$result) (global.get '$malloc_head))
                                        (then
                                            (global.set '$malloc_head (i32.load 4 (global.get '$malloc_head)))
                                        )
                                        (else
                                            (i32.store 4 (local.get '$last) (i32.load 4 (local.get '$result)))
                                        )
                                    )
                                )
                                (else
                                    (local.set '$last (local.get '$ptr))
                                    (local.set '$ptr (i32.load 4 (local.get '$ptr)))
                                    (br_if '$l (i32.ne (i32.const 0) (local.get '$ptr)))
                                )
                            )
                        )
                    )
                )
                (_if '$result_0
                    (i32.eqz (local.get '$result))
                    (then
                        (global.set '$num_sbrks   (i32.add (i32.const 1) (global.get '$num_sbrks)))
                        (local.set '$pages (i32.add (i32.const 1) (i32.shr_u (local.get '$bytes) (i32.const 16))))
                        (local.set '$result (i32.shl (memory.grow (local.get '$pages)) (i32.const 16)))
                        (i32.store 0 (local.get '$result) (i32.shl (local.get '$pages) (i32.const 16)))
                    )
                )
                ; If too big (>= 2x needed), break off a chunk
                (_if '$too_big
                    (i32.ge_u (i32.load 0 (local.get '$result)) (i32.shl (local.get '$bytes) (i32.const 1)))
                    (then
                        (local.set '$ptr (i32.add (local.get '$result) (local.get '$bytes)))
                        (i32.store 0 (local.get '$ptr) (i32.sub (i32.load 0 (local.get '$result)) (local.get '$bytes)))
                        (i32.store 4 (local.get '$ptr) (global.get '$malloc_head))
                        (global.set '$malloc_head (local.get '$ptr))
                        (i32.store 0 (local.get '$result) (local.get '$bytes))
                    )
                )

                ;(i32.store (local.get '$result) (global.get '$debug_malloc_head)) ; MDEBUG
                ;(global.set '$debug_malloc_head (local.get '$result))             ; MDEBUG
                ;(local.set '$result (i32.add (i32.const 16) (local.get '$result))) ; MDEBUG

                (i32.store 4 (local.get '$result) (i32.const 1))
                (i32.add (local.get '$result) (i32.const 8))
              ))))

              ((k_free func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$free '(param $bytes i32)
                   (local.set '$bytes (i32.sub (local.get '$bytes) (i32.const 8)))
                   (global.set '$num_frees (i32.add (i32.const 1) (global.get '$num_frees)))
                   (_if '$properly_counted
                        (i32.ne (i32.const 1) (i32.load 4 (local.get '$bytes)))
                        (then
                          (unreachable)
                        )
                   )
                   ;(i32.store 4 (local.get '$bytes) (i32.sub (i32.load 4 (local.get '$bytes)) (i32.const 1)))
                   (i32.store 4 (local.get '$bytes) (global.get '$malloc_head))
                   (global.set '$malloc_head (local.get '$bytes))
              ))))

              ((k_get_ptr func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$get_ptr '(param $bytes i64) '(result i32)
                    (_if '$is_not_string_symbol_array_int '(result i32)
                         (i64.eq (i64.const #b001) (i64.and (i64.const #b111) (local.get '$bytes)))
                         (then
                            (_if '$is_true_false '(result i32)
                                 (i64.eq (i64.const #b11001) (i64.and (i64.const #b11111) (local.get '$bytes)))
                                 (then (i32.const 0))
                                 (else
                                    (_if '$is_env '(result i32)
                                        (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$bytes)))
                                        (then (i32.wrap_i64 (i64.shr_u (local.get '$bytes) (i64.const 5))))
                                        (else (i32.wrap_i64 (i64.and (i64.const #xFFFFFFF8) (i64.shr_u (local.get '$bytes) (i64.const 3))))) ; is comb
                                    )
                                 )
                             )
                         )
                         (else
                            (_if '$is_int '(result i32)
                                 (i64.eq (i64.const #b0) (i64.and (i64.const #b1) (local.get '$bytes)))
                                 (then (i32.const 0))
                                 (else (i32.wrap_i64 (i64.and (i64.const -8) (local.get '$bytes)))) ; str symbol and array all get ptrs just masking FFFFFFF8
                             )
                         )
                     )
              ))))

              ; 0..0<env_ptr32 but still aligned>01001
              ((k_env_alloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$env_alloc '(param $keys i64) '(param $vals i64) '(param $upper i64) '(result i64) '(local $tmp i32)
                    (local.set '$tmp (call '$malloc (i32.const (* 8 3))))
                    (i64.store 0  (local.get '$tmp) (local.get '$keys))
                    (i64.store 8  (local.get '$tmp) (local.get '$vals))
                    (i64.store 16 (local.get '$tmp) (local.get '$upper))
                    ;(i64.store (i32.add (i32.const -16) (local.get '$tmp)) (i64.or (i64.shl (i64.extend_i32_u (local.get '$tmp)) (i64.const 5)) (i64.const #b01001))) ; MDEBUG
                    (i64.or (i64.shl (i64.extend_i32_u (local.get '$tmp)) (i64.const 5)) (i64.const #b01001))
              ))))

              ;  <array_size32><array_ptr29>101 / 0..0 101
              ((k_array1_alloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$array1_alloc '(param $item i64) '(result i64) '(local $tmp i32)
                    (local.set '$tmp (call '$malloc (i32.const 8)))
                    (i64.store 0  (local.get '$tmp) (local.get '$item))
                    ;(i64.store (i32.add (i32.const -16) (local.get '$tmp)) (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000100000005))) ; MDEBUG
                    (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000100000005))
              ))))
              ((k_array2_alloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$array2_alloc '(param $a i64) '(param $b i64) '(result i64) '(local $tmp i32)
                    (local.set '$tmp (call '$malloc (i32.const 16)))
                    (i64.store 0  (local.get '$tmp) (local.get '$a))
                    (i64.store 8  (local.get '$tmp) (local.get '$b))
                    ;(i64.store (i32.add (i32.const -16) (local.get '$tmp)) (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000200000005))) ; MDEBUG
                    (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000200000005))
              ))))
              ((k_array3_alloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$array3_alloc '(param $a i64) '(param $b i64) '(param $c i64) '(result i64) '(local $tmp i32)
                    (local.set '$tmp (call '$malloc (i32.const 24)))
                    (i64.store  0  (local.get '$tmp) (local.get '$a))
                    (i64.store  8  (local.get '$tmp) (local.get '$b))
                    (i64.store 16  (local.get '$tmp) (local.get '$c))
                    ;(i64.store (i32.add (i32.const -16) (local.get '$tmp)) (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000300000005))) ; MDEBUG
                    (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000300000005))
              ))))
              ((k_array5_alloc func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$array5_alloc '(param $a i64) '(param $b i64) '(param $c i64) '(param $d i64) '(param $e i64) '(result i64) '(local $tmp i32)
                    (local.set '$tmp (call '$malloc (i32.const 40)))
                    (i64.store 0   (local.get '$tmp) (local.get '$a))
                    (i64.store 8   (local.get '$tmp) (local.get '$b))
                    (i64.store 16  (local.get '$tmp) (local.get '$c))
                    (i64.store 24  (local.get '$tmp) (local.get '$d))
                    (i64.store 32  (local.get '$tmp) (local.get '$e))
                    ;(i64.store (i32.add (i32.const -16) (local.get '$tmp)) (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000500000005))) ; MDEBUG
                    (i64.or (i64.extend_i32_u (local.get '$tmp)) (i64.const #x0000000500000005))
              ))))

              ; Not called with actual objects, not subject to refcounting
              ((k_int_digits func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$int_digits '(param $int i64) '(result i32) '(local $tmp i32)
                    (_if '$is_neg
                        (i64.lt_s (local.get '$int) (i64.const 0))
                        (then
                            (local.set '$int (i64.sub (i64.const 0) (local.get '$int)))
                            (local.set '$tmp (i32.const 2))
                        )
                        (else
                            (local.set '$tmp (i32.const 1))
                        )
                    )
                    (block '$b
                        (_loop '$l
                            (br_if '$b (i64.le_u (local.get '$int) (i64.const 9)))
                            (local.set '$tmp (i32.add (i32.const 1) (local.get '$tmp)))
                            (local.set '$int (i64.div_u (local.get '$int) (i64.const 10)))
                            (br '$l)
                        )
                    )
                    (local.get '$tmp)
              ))))
              ; Utility method, not subject to refcounting
              ((k_str_len func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$str_len '(param $to_str_len i64) '(result i32) '(local $running_len_tmp i32) '(local $i_tmp i32) '(local $x_tmp i32) '(local $y_tmp i32) '(local $ptr_tmp i32) '(local $item i64)
                    (_if '$is_true '(result i32)
                         (i64.eq (i64.const true_val) (local.get '$to_str_len))
                         (then (i32.const true_length))
                         (else
                            (_if '$is_false '(result i32)
                                 (i64.eq (i64.const false_val) (local.get '$to_str_len))
                                 (then (i32.const false_length))
                                 (else
                                    (_if '$is_str_or_symbol '(result i32)
                                         (i64.eq (i64.const #b11) (i64.and (i64.const #b11) (local.get '$to_str_len)))
                                         (then (_if '$is_str '(result i32)
                                                 (i64.eq (i64.const #b000) (i64.and (i64.const #b100)  (local.get '$to_str_len)))
                                                 (then (i32.add (i32.const 2) (i32.wrap_i64 (i64.shr_u (local.get '$to_str_len) (i64.const 32)))))
                                                 (else (i32.add (i32.const 1) (i32.wrap_i64 (i64.shr_u (local.get '$to_str_len) (i64.const 32)))))
                                               ))
                                         (else
                                            (_if '$is_array '(result i32)
                                                 (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$to_str_len)))
                                                 (then
                                                    (local.set '$running_len_tmp (i32.const 1))
                                                    (local.set '$i_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str_len) (i64.const 32))))
                                                    (local.set '$x_tmp (i32.wrap_i64 (i64.and (local.get '$to_str_len) (i64.const -8))))
                                                    (block '$b
                                                        (_loop '$l
                                                            (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (i32.const 1)))
                                                            (br_if '$b (i32.eq (local.get '$i_tmp) (i32.const 0)))
                                                            (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (call '$str_len (i64.load (local.get '$x_tmp)))))
                                                            (local.set '$x_tmp           (i32.add (local.get '$x_tmp)           (i32.const 8)))
                                                            (local.set '$i_tmp           (i32.sub (local.get '$i_tmp)           (i32.const 1)))
                                                            (br '$l)
                                                        )
                                                    )
                                                    (local.get '$running_len_tmp)
                                                 )
                                                 (else
                                                    (_if '$is_env '(result i32)
                                                        (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$to_str_len)))
                                                        (then
                                                            (local.set '$running_len_tmp (i32.const 0))

                                                            ; ptr to env
                                                            (local.set '$ptr_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str_len) (i64.const 5))))
                                                            ; ptr to start of array of symbols
                                                            (local.set '$x_tmp (i32.wrap_i64 (i64.and (i64.load (local.get '$ptr_tmp)) (i64.const -8))))
                                                            ; ptr to start of array of values
                                                            (local.set '$y_tmp (i32.wrap_i64 (i64.and (i64.load 8 (local.get '$ptr_tmp)) (i64.const -8))))
                                                            ; lenght of both arrays, pulled from array encoding of x
                                                            (local.set '$i_tmp (i32.wrap_i64 (i64.shr_u (i64.load (local.get '$ptr_tmp)) (i64.const 32))))

                                                            (block '$b
                                                                (_loop '$l
                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (i32.const 2)))
                                                                    ; break if 0 length left
                                                                    (br_if '$b (i32.eq (local.get '$i_tmp) (i32.const 0)))

                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp)
                                                                                                          (call '$str_len (i64.load (local.get '$x_tmp)))))
                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp)
                                                                                                          (call '$str_len (i64.load (local.get '$y_tmp)))))
                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (i32.const 2)))

                                                                    (local.set '$x_tmp           (i32.add (local.get '$x_tmp)           (i32.const 8)))
                                                                    (local.set '$y_tmp           (i32.add (local.get '$y_tmp)           (i32.const 8)))
                                                                    (local.set '$i_tmp           (i32.sub (local.get '$i_tmp)           (i32.const 1)))
                                                                    (br '$l)
                                                                )
                                                            )
                                                            ;; deal with upper
                                                            (local.set '$item (i64.load 16 (local.get '$ptr_tmp)))
                                                            (_if '$is_upper_env
                                                                (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$item)))
                                                                (then
                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (i32.const 1)))
                                                                    (local.set '$running_len_tmp (i32.add (local.get '$running_len_tmp) (call '$str_len (local.get '$item))))
                                                                )
                                                            )

                                                            (local.get '$running_len_tmp)
                                                        )
                                                        (else
                                                            (_if '$is_comb '(result i32)
                                                                (i64.eq (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$to_str_len)))
                                                                (then
                                                                    (i32.const 5)
                                                                )
                                                                (else
                                                                    ;; must be int
                                                                    (call '$int_digits (i64.shr_s (local.get '$to_str_len) (i64.const 1)))
                                                                )
                                                            )
                                                        )
                                                    )
                                                 )
                                             )
                                        )
                                    )
                                )
                            )
                        )
                    )
              ))))
              ; Utility method, not subject to refcounting
              ((k_str_helper func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$str_helper '(param $to_str i64) '(param $buf i32) '(result i32) '(local $len_tmp i32) '(local $buf_tmp i32) '(local $ptr_tmp i32) '(local $x_tmp i32) '(local $y_tmp i32) '(local $i_tmp i32) '(local $item i64)
                    (_if '$is_true '(result i32)
                         (i64.eq (i64.const true_val) (local.get '$to_str))
                         (then (memory.copy (local.get '$buf)
                                            (i32.const true_loc)
                                            (i32.const true_length))
                               (i32.const true_length))
                         (else
                            (_if '$is_false '(result i32)
                                 (i64.eq (i64.const false_val) (local.get '$to_str))
                                 (then (memory.copy (local.get '$buf)
                                                    (i32.const false_loc)
                                                    (i32.const false_length))
                                       (i32.const false_length))
                                 (else
                                    (_if '$is_str_or_symbol '(result i32)
                                         (i64.eq (i64.const #b11) (i64.and (i64.const #b11) (local.get '$to_str)))
                                         (then (_if '$is_str '(result i32)
                                                 (i64.eq (i64.const #b000) (i64.and (i64.const #b100) (local.get '$to_str)))
                                                 (then
                                                    (i32.store8 (local.get '$buf) (i32.const #x22))
                                                    (memory.copy (i32.add (i32.const 1) (local.get '$buf))
                                                                 (i32.wrap_i64 (i64.and (i64.const -8) (local.get '$to_str)))
                                                                 (local.tee '$len_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str) (i64.const 32)))))
                                                    (i32.store8 1 (i32.add (local.get '$buf) (local.get '$len_tmp)) (i32.const #x22))
                                                    (i32.add (i32.const 2) (local.get '$len_tmp))
                                                 )
                                                 (else
                                                    (i32.store8 (local.get '$buf) (i32.const #x27))
                                                    (memory.copy (i32.add (i32.const 1) (local.get '$buf))
                                                                 (i32.wrap_i64 (i64.and (i64.const -8) (local.get '$to_str)))
                                                                 (local.tee '$len_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str) (i64.const 32)))))
                                                    (i32.add (i32.const 1) (local.get '$len_tmp))
                                                )
                                               ))
                                         (else
                                            (_if '$is_array '(result i32)
                                                 (i64.eq (i64.const #b101) (i64.and (i64.const #b101) (local.get '$to_str)))
                                                 (then
                                                    (local.set '$len_tmp (i32.const 0))
                                                    (local.set '$i_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str) (i64.const 32))))
                                                    (local.set '$ptr_tmp (i32.wrap_i64 (i64.and (local.get '$to_str) (i64.const -8))))
                                                    (block '$b
                                                        (_loop '$l
                                                            (i32.store8 (i32.add (local.get '$buf) (local.get '$len_tmp))   (i32.const #x20))
                                                            (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                            (br_if '$b (i32.eq (local.get '$i_tmp) (i32.const 0)))
                                                            (local.set '$len_tmp (i32.add (local.get '$len_tmp) (call '$str_helper (i64.load (local.get '$ptr_tmp)) (i32.add (local.get '$buf) (local.get '$len_tmp)))))
                                                            (local.set '$ptr_tmp (i32.add (local.get '$ptr_tmp) (i32.const 8)))
                                                            (local.set '$i_tmp   (i32.sub (local.get '$i_tmp)   (i32.const 1)))
                                                            (br '$l)
                                                        )
                                                    )
                                                    (i32.store8 (local.get '$buf)                                   (i32.const #x28))
                                                    (i32.store8 (i32.add (local.get '$buf) (local.get '$len_tmp))   (i32.const #x29))
                                                    (i32.add (local.get '$len_tmp) (i32.const 1))
                                                 )
                                                 (else
                                                    (_if '$is_env '(result i32)
                                                        (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$to_str)))
                                                        (then
                                                            (local.set '$len_tmp (i32.const 0))

                                                            ; ptr to env
                                                            (local.set '$ptr_tmp (i32.wrap_i64 (i64.shr_u (local.get '$to_str) (i64.const 5))))
                                                            ; ptr to start of array of symbols
                                                            (local.set '$x_tmp (i32.wrap_i64 (i64.and (i64.load (local.get '$ptr_tmp)) (i64.const -8))))
                                                            ; ptr to start of array of values
                                                            (local.set '$y_tmp (i32.wrap_i64 (i64.and (i64.load 8 (local.get '$ptr_tmp)) (i64.const -8))))
                                                            ; lenght of both arrays, pulled from array encoding of x
                                                            (local.set '$i_tmp (i32.wrap_i64 (i64.shr_u (i64.load (local.get '$ptr_tmp)) (i64.const 32))))

                                                            (block '$b
                                                                (_loop '$l
                                                                    (i32.store8 (i32.add (local.get '$buf) (local.get '$len_tmp)) (i32.const #x20))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                                    ; break if 0 length left
                                                                    (br_if '$b (i32.eq (local.get '$i_tmp) (i32.const 0)))

                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (call '$str_helper (i64.load (local.get '$x_tmp)) (i32.add (local.get '$buf) (local.get '$len_tmp)))))
                                                                    (i32.store8 (i32.add (local.get '$len_tmp) (local.get '$buf)) (i32.const #x3A))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                                    (i32.store8 (i32.add (local.get '$len_tmp) (local.get '$buf)) (i32.const #x20))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (call '$str_helper (i64.load (local.get '$y_tmp)) (i32.add (local.get '$buf) (local.get '$len_tmp)))))
                                                                    (i32.store8 (i32.add (local.get '$len_tmp) (local.get '$buf)) (i32.const #x2C))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))

                                                                    (local.set '$x_tmp           (i32.add (local.get '$x_tmp)           (i32.const 8)))
                                                                    (local.set '$y_tmp           (i32.add (local.get '$y_tmp)           (i32.const 8)))
                                                                    (local.set '$i_tmp           (i32.sub (local.get '$i_tmp)           (i32.const 1)))
                                                                    (br '$l)
                                                                )
                                                            )
                                                            ;; deal with upper
                                                            (local.set '$item (i64.load 16 (local.get '$ptr_tmp)))
                                                            (_if '$is_upper_env
                                                                (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$item)))
                                                                (then
                                                                    (i32.store8 -2 (i32.add (local.get '$buf) (local.get '$len_tmp)) (i32.const #x20))
                                                                    (i32.store8 -1 (i32.add (local.get '$buf) (local.get '$len_tmp)) (i32.const #x7C))
                                                                    (i32.store8 (i32.add (local.get '$len_tmp) (local.get '$buf)) (i32.const #x20))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                                    (local.set '$len_tmp (i32.add (local.get '$len_tmp) (call '$str_helper (local.get '$item) (i32.add (local.get '$buf) (local.get '$len_tmp)))))
                                                                )
                                                            )
                                                            (i32.store8 (local.get '$buf)                                   (i32.const #x7B))
                                                            (i32.store8 (i32.add (local.get '$buf) (local.get '$len_tmp))   (i32.const #x7D))
                                                            (local.set '$len_tmp (i32.add (local.get '$len_tmp) (i32.const 1)))
                                                            (local.get '$len_tmp)
                                                        )
                                                        (else
                                                            (_if '$is_comb '(result i32)
                                                                (i64.eq (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$to_str)))
                                                                (then
                                                                    (i32.store (local.get '$buf) (i32.const #x626D6F63))
                                                                    (i32.store8 4 (local.get '$buf)
                                                                                (i32.add (i32.const #x30)
                                                                                         (i32.and (i32.const #b1)
                                                                                                  (i32.wrap_i64 (i64.shr_u (local.get '$to_str) (i64.const 4))))))
                                                                    (i32.const 5)
                                                                )
                                                                (else
                                                                    ;; must be int
                                                                    (local.set '$to_str (i64.shr_s (local.get '$to_str) (i64.const 1)))
                                                                    (local.set '$len_tmp (call '$int_digits (local.get '$to_str)))
                                                                    (local.set '$buf_tmp (i32.add (local.get '$buf) (local.get '$len_tmp)))

                                                                    (_if '$is_neg
                                                                        (i64.lt_s (local.get '$to_str) (i64.const 0))
                                                                        (then
                                                                            (local.set '$to_str (i64.sub (i64.const 0) (local.get '$to_str)))
                                                                            (i64.store8 (local.get '$buf) (i64.const #x2D))
                                                                        )
                                                                    )

                                                                    (block '$b
                                                                        (_loop '$l
                                                                            (local.set '$buf_tmp (i32.sub (local.get '$buf_tmp) (i32.const 1)))
                                                                            (i64.store8 (local.get '$buf_tmp) (i64.add (i64.const #x30) (i64.rem_u (local.get '$to_str) (i64.const 10))))
                                                                            (local.set '$to_str (i64.div_u (local.get '$to_str) (i64.const 10)))
                                                                            (br_if '$b (i64.eq (local.get '$to_str) (i64.const 0)))
                                                                            (br '$l)
                                                                        )
                                                                    )

                                                                    (local.get '$len_tmp)
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
              ))))
              ; Utility method, not subject to refcounting
              ((k_print func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$print '(param $to_print i64) '(local $iov i32) '(local $data_size i32)
                    (local.set '$iov (call '$malloc (i32.add (i32.const 8)
                                                             (local.tee '$data_size (call '$str_len (local.get '$to_print))))))
                    (_drop (call '$str_helper (local.get '$to_print) (i32.add (i32.const 8) (local.get '$iov))))
                    (_if '$is_str (i64.eq (i64.and (local.get '$to_print) (i64.const #b111)) (i64.const #b011))
                         (then
                             (i32.store   (local.get '$iov) (i32.add (i32.const 9) (local.get '$iov)))       ;; adder of data
                             (i32.store 4 (local.get '$iov) (i32.sub (local.get '$data_size) (i32.const 2))) ;; len of data
                         )
                         (else
                             (i32.store   (local.get '$iov) (i32.add (i32.const 8) (local.get '$iov))) ;; adder of data
                             (i32.store 4 (local.get '$iov) (local.get '$data_size))                   ;; len of data
                         )
                    )
                    (_drop (call '$fd_write
                              (i32.const 1)     ;; file descriptor
                              (local.get '$iov) ;; *iovs
                              (i32.const 1)     ;; iovs_len
                              (local.get '$iov) ;; nwritten
                    ))
                    (call '$free (local.get '$iov))
              ))))
              ((k_dup func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$dup '(param $bytes i64) '(result i64) '(local $ptr i32) '(local $old_val i32)
                    (local.set '$ptr (call '$get_ptr (local.get '$bytes)))
                    (_if '$not_null
                        (i32.ne (i32.const 0) (local.get '$ptr))
                        (then
                            (local.set '$ptr (i32.sub (local.get '$ptr) (i32.const 8)))

                            ;(call '$print (i64.const duping_msg_val))
                            ;(call '$print (i64.shl (i64.extend_i32_s (i32.load 4 (local.get '$ptr))) (i64.const 1)))
                            ;(call '$print (i64.shl (i64.extend_i32_s (local.get '$ptr)) (i64.const 1)))
                            ;(call '$print (local.get '$bytes))
                            ;(call '$print (i64.const newline_msg_val))

                            (_if '$not_max_neg
                                ;(i32.ne (i32.const (- #x80000000)) (local.tee '$old_val (i32.load 4 (local.get '$ptr))))
                                (i32.gt_s  (local.tee '$old_val (i32.load 4 (local.get '$ptr))) (i32.const 0))
                                (then
                                    (i32.store 4 (local.get '$ptr) (i32.add (local.get '$old_val) (i32.const 1)))
                                )
                            )
                        )
                    )
                    (local.get '$bytes)
              ))))
              ; currenty func 16 in profile
              ((k_drop func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$drop '(param $it i64) '(local $ptr i32) '(local $tmp_ptr i32) '(local $old_val i32) '(local $new_val i32) '(local $i i32)
                    (local.set '$ptr (call '$get_ptr (local.get '$it)))
                    (_if '$not_null
                        (i32.ne (i32.const 0) (local.get '$ptr))
                        (then

                            ;(call '$print (i64.const dropping_msg_val))
                            ;(call '$print (i64.shl (i64.extend_i32_s (i32.load (i32.add (i32.const -4) (local.get '$ptr)))) (i64.const 1)))
                            ;(call '$print (local.get '$it))
                            ;(call '$print (i64.const newline_msg_val))

                            (_if '$not_max_neg
                                ;(i32.ne (i32.const (- #x80000000)) (local.tee '$old_val (i32.load (i32.add (i32.const -4) (local.get '$ptr)))))
                                (i32.gt_s  (local.tee '$old_val (i32.load (i32.add (i32.const -4) (local.get '$ptr)))) (i32.const 0))
                                (then
                                    (_if '$zero
                                        (i32.eqz (local.tee '$new_val (i32.sub (local.get '$old_val) (i32.const 1))))
                                        (then
                                            (_if '$needs_inner_drop
                                                (i64.eq (i64.const #b01) (i64.and (i64.const #b11) (local.get '$it)))
                                                (then
                                                    (_if '$is_array
                                                        (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$it)))
                                                        (then
                                                            (local.set '$i (i32.wrap_i64 (i64.shr_u (local.get '$it) (i64.const 32))))
                                                            (local.set '$tmp_ptr (local.get '$ptr))
                                                            (_loop '$l
                                                                (call '$drop (i64.load (local.get '$tmp_ptr)))
                                                                (local.set '$tmp_ptr (i32.add (local.get '$tmp_ptr) (i32.const 8)))
                                                                (local.set '$i (i32.sub (local.get '$i) (i32.const 1)))
                                                                (br_if '$l (i32.ne (i32.const 0) (local.get '$i)))
                                                            )
                                                        )
                                                        (else
                                                            (call '$drop (i64.load  0 (local.get '$ptr)))
                                                            (call '$drop (i64.load  8 (local.get '$ptr)))
                                                            (call '$drop (i64.load 16 (local.get '$ptr)))
                                                        )
                                                    )
                                                )
                                            )
                                            (call '$free (local.get '$ptr))
                                        )
                                        (else (i32.store (i32.add (i32.const -4) (local.get '$ptr)) (local.get '$new_val)))
                                    )
                                )
                            )
                        )
                    )
              ))))

              ; Utility method, but does refcount
              ((k_slice_impl func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$slice_impl '(param $array i64) '(param $s i32) '(param $e i32) '(result i64) '(local $size i32) '(local $new_size i32) '(local $i i32) '(local $ptr i32) '(local $new_ptr i32)
                    (local.set '$size (i32.wrap_i64 (i64.shr_u (local.get '$array) (i64.const 32))))
                    (local.set '$ptr  (i32.wrap_i64 (i64.and   (local.get '$array) (i64.const -8))))
                    (_if '$s_lt_0
                        (i32.lt_s (local.get '$s) (i32.const 0))
                        (then
                            (local.set '$s (i32.add (i32.const 1) (i32.add (local.get '$s) (local.get '$size))))
                        )
                    )
                    (_if '$e_lt_0
                        (i32.lt_s (local.get '$e) (i32.const 0))
                        (then
                            (local.set '$e (i32.add (i32.const 1) (i32.add (local.get '$e) (local.get '$size))))
                        )
                    )

                    (_if '$s_lt_0     (i32.lt_s (local.get '$s) (i32.const 0))      (then (unreachable)))
                    (_if '$e_lt_s     (i32.lt_s (local.get '$e) (local.get '$s))    (then (unreachable)))
                    (_if '$e_gt_size  (i32.gt_s (local.get '$e) (local.get '$size)) (then (unreachable)))

                    (local.set '$new_size (i32.sub (local.get '$e) (local.get '$s)))
                    (_if '$new_size_0 '(result i64)
                        (i32.eqz (local.get '$new_size))
                        (then
                            (call '$drop (local.get '$array))
                            (_if '$is_array '(result i64)
                             (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$array)))
                             (then (i64.const nil_val))
                             (else (i64.const emptystr_val)))
                        )
                        (else
                            (_if '$is_array '(result i64)
                             (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$array)))
                             (then
                                  (local.set '$new_ptr  (call '$malloc (i32.shl (local.get '$new_size) (i32.const 3)))) ; malloc(size*8)

                                  (local.set '$i (i32.const 0))
                                  (block '$exit_loop
                                      (_loop '$l
                                          (br_if '$exit_loop (i32.eq (local.get '$i) (local.get '$new_size)))
                                          (i64.store (i32.add (i32.shl (local.get '$i) (i32.const 3)) (local.get '$new_ptr))
                                                     (call '$dup (i64.load (i32.add (i32.shl (i32.add (local.get '$s) (local.get '$i)) (i32.const 3))
                                                                                    (local.get '$ptr))))) ; n[i] = dup(o[i+s])
                                          (local.set '$i (i32.add (i32.const 1) (local.get '$i)))
                                          (br '$l)
                                      )
                                  )
                                  (call '$drop (local.get '$array))

                                  ;(i64.store (i32.add (i32.const -16) (local.get '$new_ptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr))  (i64.const #b101))
                                  ;                                                                   (i64.shl (i64.extend_i32_u (local.get '$new_size)) (i64.const 32)))) ; MDEBUG
                                  (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr))  (i64.const #b101))
                                          (i64.shl (i64.extend_i32_u (local.get '$new_size)) (i64.const 32)))
                             )
                             (else
                                  (local.set '$new_ptr  (call '$malloc (local.get '$new_size))) ; malloc(size)
                                  (memory.copy (local.get '$new_ptr)
                                               (i32.add (local.get '$ptr) (local.get '$s))
                                               (local.get '$new_size))

                                  (call '$drop (local.get '$array))

                                  (i64.store (i32.add (i32.const -16) (local.get '$new_ptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr))  (i64.const #b011))
                                                                                                     (i64.shl (i64.extend_i32_u (local.get '$new_size)) (i64.const 32))))
                                  (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr))  (i64.const #b011))
                                          (i64.shl (i64.extend_i32_u (local.get '$new_size)) (i64.const 32)))
                             )
                            )
                        )
                    )
              ))))

              ; chose k_slice_impl because it will never be called, so that
              ; no function will have a 0 func index and count as falsy
              (dyn_start (+ 0 k_slice_impl))

              (func_id_dynamic_ofset  (+ (- 0 dyn_start) (- num_pre_functions 1)))

              ; This and is 1111100011
              ; The end ensuring 01 makes only
              ; array comb env and bool apply
              ; catching only 0array and false
              ; and a comb with func idx 0
              ; and null env. If we prevent
              ; this from happening, it's
              ; exactly what we want
              (truthy_test (lambda (x) (i64.ne (i64.const #b01) (i64.and (i64.const -29) x))))
              (falsey_test (lambda (x) (i64.eq (i64.const #b01) (i64.and (i64.const -29) x))))

              (set_len_ptr (concat (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))))
                                   (local.set '$ptr (i32.wrap_i64 (i64.and (local.get '$p) (i64.const -8))))
              ))
              (ensure_not_op_n_params_set_ptr_len (lambda (op n) (concat set_len_ptr
                                                                (_if '$is_2_params
                                                                    (op (local.get '$len) (i32.const n))
                                                                    (then
                                                                        (call '$print (i64.const bad_params_number_msg_val))
                                                                        (unreachable)
                                                                    )
                                                                )
                                                        )))
              (drop_p_d (concat 
                    (call '$drop (local.get '$p))
                    (call '$drop (local.get '$d))))

              ((k_log_loc k_log_length datasi) (alloc_data "k_log" datasi))
              (k_log_msg_val (bor (<< k_log_length 32) k_log_loc #b011))
              ((k_log           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$log           '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$print (i64.const log_msg_val))
                (call '$print (local.get '$p))
                (call '$print (i64.const newline_msg_val))
                drop_p_d
                (i64.const nil_val)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_error_loc k_error_length datasi) (alloc_data "k_error" datasi))
              (k_error_msg_val (bor (<< k_error_length 32) k_error_loc #b011))
              ((k_error         func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$error         '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$print (i64.const error_msg_val))
                (call '$print (local.get '$p))
                (call '$print (i64.const newline_msg_val))
                drop_p_d
                (unreachable)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_str_loc k_str_length datasi) (alloc_data "k_str" datasi))
              (k_str_msg_val (bor (<< k_str_length 32) k_str_loc #b011))
              ((k_str           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$str           '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $buf i32) '(local $size i32)
                (local.set '$buf (call '$malloc (local.tee '$size (call '$str_len (local.get '$p)))))
                (_drop (call '$str_helper (local.get '$p) (local.get '$buf)))
                drop_p_d

                (i64.store (i32.add (i32.const -16) (local.get '$buf)) (i64.or (i64.or (i64.shl (i64.extend_i32_u (local.get '$size)) (i64.const 32))
                                                                                (i64.extend_i32_u (local.get '$buf)))
                                                                        (i64.const #b011)))

                (i64.or (i64.or (i64.shl (i64.extend_i32_u (local.get '$size)) (i64.const 32))
                                (i64.extend_i32_u (local.get '$buf)))
                        (i64.const #b011))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              (pred_func    (lambda (name type_check) (func name '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (typecheck 0 (array '(result i64))
                    i64.eq type_check
                    (array (then (i64.const true_val)))
                    (array (else (i64.const false_val)))
                )
                drop_p_d
              )))

              ((k_nil_loc k_nil_length datasi) (alloc_data "k_nil" datasi))
              (k_nil_msg_val (bor (<< k_nil_length 32) k_nil_loc #b011))
              ((k_nil?          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$nil?      (array -1 #x0000000000000005)))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_array_loc k_array_length datasi) (alloc_data "k_array" datasi))
              (k_array_msg_val (bor (<< k_array_length 32) k_array_loc #b011))
              ((k_array?        func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$array?    type_array))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_bool_loc k_bool_length datasi) (alloc_data "k_bool" datasi))
              (k_bool_msg_val (bor (<< k_bool_length 32) k_bool_loc #b011))
              ((k_bool?         func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$bool?     type_bool))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_env_loc k_env_length datasi) (alloc_data "k_env" datasi))
              (k_env_msg_val (bor (<< k_env_length 32) k_env_loc #b011))
              ((k_env?          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$env?      type_env))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_combiner_loc k_combiner_length datasi) (alloc_data "k_combiner" datasi))
              (k_combiner_msg_val (bor (<< k_combiner_length 32) k_combiner_loc #b011))
              ((k_combiner?     func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$combiner  type_combiner))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_string_loc k_string_length datasi) (alloc_data "k_string" datasi))
              (k_string_msg_val (bor (<< k_string_length 32) k_string_loc #b011))
              ((k_string?       func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$string?   type_string))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_int_loc k_int_length datasi) (alloc_data "k_int" datasi))
              (k_int_msg_val (bor (<< k_int_length 32) k_int_loc #b011))
              ((k_int?          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$int?      type_int))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_symbol_loc k_symbol_length datasi) (alloc_data "k_symbol" datasi))
              (k_symbol_msg_val (bor (<< k_symbol_length 32) k_symbol_loc #b011))
              ((k_symbol?       func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (pred_func '$symbol?   type_symbol))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_str_sym_comp func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$str_sym_comp '(param $a i64) '(param $b i64) '(param $lt_val i64) '(param $eq_val i64) '(param $gt_val i64) '(result i64) '(local $result i64) '(local $a_len i32) '(local $b_len i32) '(local $a_ptr i32) '(local $b_ptr i32)
                (local.set '$result (local.get '$eq_val))
                (local.set '$a_len (i32.wrap_i64 (i64.shr_u (local.get '$a) (i64.const 32))))
                (local.set '$b_len (i32.wrap_i64 (i64.shr_u (local.get '$b) (i64.const 32))))
                (local.set '$a_ptr (i32.wrap_i64 (i64.and (local.get '$a) (i64.const #xFFFFFFF8))))
                (local.set '$b_ptr (i32.wrap_i64 (i64.and (local.get '$b) (i64.const #xFFFFFFF8))))
                (block '$b
                    (_if '$a_len_lt_b_len
                        (i32.lt_s (local.get '$a_len) (local.get '$b_len))
                        (then (local.set '$result (local.get '$lt_val))
                              (br '$b)))
                    (_if '$a_len_gt_b_len
                        (i32.gt_s (local.get '$a_len) (local.get '$b_len))
                        (then (local.set '$result (local.get '$gt_val))
                              (br '$b)))

                    (_loop '$l
                        (br_if '$b (i32.eqz (local.get '$a_len)))

                        (local.set '$a (i64.load8_u (local.get '$a_ptr)))
                        (local.set '$b (i64.load8_u (local.get '$b_ptr)))

                        (_if '$a_lt_b
                            (i64.lt_s (local.get '$a) (local.get '$b))
                            (then (local.set '$result (local.get '$lt_val))
                                  (br '$b)))
                        (_if '$a_gt_b
                            (i64.gt_s (local.get '$a) (local.get '$b))
                            (then (local.set '$result (local.get '$gt_val))
                                  (br '$b)))

                        (local.set '$a_len (i32.sub (local.get '$a_len) (i32.const 1)))
                        (local.set '$a_ptr (i32.add (local.get '$a_ptr) (i32.const 1)))
                        (local.set '$b_ptr (i32.add (local.get '$b_ptr) (i32.const 1)))
                        (br '$l)
                    )
                )
                (local.get '$result)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_comp_helper_helper func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$comp_helper_helper '(param $a i64) '(param $b i64) '(param $lt_val i64) '(param $eq_val i64) '(param $gt_val i64) '(result i64) '(local $result i64) '(local $a_tmp i32) '(local $b_tmp i32) '(local $a_ptr i32) '(local $b_ptr i32) '(local $result_tmp i64)
                (block '$b
                    ;; INT
                    (_if '$a_int
                        (i64.eqz (i64.and (i64.const 1) (local.get '$a)))
                        (then
                            (_if '$b_int
                                (i64.eqz (i64.and (i64.const 1) (local.get '$b)))
                                (then
                                    (_if '$a_lt_b
                                        (i64.lt_s (local.get '$a) (local.get '$b))
                                        (then (local.set '$result (local.get '$lt_val))
                                              (br '$b)))
                                    (_if '$a_gt_b
                                        (i64.gt_s (local.get '$a) (local.get '$b))
                                        (then (local.set '$result (local.get '$gt_val))
                                              (br '$b)))
                                    (local.set '$result (local.get '$eq_val))
                                    (br '$b)
                                )
                            )
                            ; Else, b is not an int, so a < b
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_int
                        (i64.eqz (i64.and (i64.const 1) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; STRING
                    (_if '$a_string
                        (i64.eq (i64.const #b011) (i64.and (i64.const #b111) (local.get '$a)))
                        (then
                            (_if '$b_string
                                (i64.eq (i64.const #b011) (i64.and (i64.const #b111) (local.get '$b)))
                                (then
                                    (local.set '$result (call '$str_sym_comp (local.get '$a) (local.get '$b) (local.get '$lt_val) (local.get '$eq_val) (local.get '$gt_val)))
                                    (br '$b))
                            )
                            ; else b is not an int or string, so bigger
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_string
                        (i64.eq (i64.const #b011) (i64.and (i64.const #b111) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; SYMBOL
                    (_if '$a_symbol
                        (i64.eq (i64.const #b111) (i64.and (i64.const #b111) (local.get '$a)))
                        (then
                            (_if '$b_symbol
                                (i64.eq (i64.const #b111) (i64.and (i64.const #b111) (local.get '$b)))
                                (then
                                    (local.set '$result (call '$str_sym_comp (local.get '$a) (local.get '$b) (local.get '$lt_val) (local.get '$eq_val) (local.get '$gt_val)))
                                    (br '$b))
                            )
                            ; else b is not an int or string or symbol, so bigger
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_symbol
                        (i64.eq (i64.const #b111) (i64.and (i64.const #b111) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; ARRAY
                    (_if '$a_array
                        (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$a)))
                        (then
                            (_if '$b_array
                                (i64.eq (i64.const #b101) (i64.and (i64.const #b111) (local.get '$b)))
                                (then
                                    (local.set '$a_tmp (i32.wrap_i64 (i64.shr_u (local.get '$a) (i64.const 32))))
                                    (local.set '$b_tmp (i32.wrap_i64 (i64.shr_u (local.get '$b) (i64.const 32))))

                                    (_if '$a_len_lt_b_len
                                        (i32.lt_s (local.get '$a_tmp) (local.get '$b_tmp))
                                        (then (local.set '$result (local.get '$lt_val))
                                              (br '$b)))
                                    (_if '$a_len_gt_b_len
                                        (i32.gt_s (local.get '$a_tmp) (local.get '$b_tmp))
                                        (then (local.set '$result (local.get '$gt_val))
                                              (br '$b)))

                                    (local.set '$a_ptr (i32.wrap_i64 (i64.and (local.get '$a) (i64.const #xFFFFFFF8))))
                                    (local.set '$b_ptr (i32.wrap_i64 (i64.and (local.get '$b) (i64.const #xFFFFFFF8))))

                                    (_loop '$l
                                        (br_if '$b (i32.eqz (local.get '$a_tmp)))

                                        (local.set '$result_tmp (call '$comp_helper_helper (i64.load (local.get '$a_ptr))
                                                                                           (i64.load (local.get '$b_ptr))
                                                                                           (i64.const -1) (i64.const 0) (i64.const 1)))

                                        (_if '$a_lt_b
                                            (i64.eq (local.get '$result_tmp) (i64.const -1))
                                            (then (local.set '$result (local.get '$lt_val))
                                                  (br '$b)))
                                        (_if '$a_gt_b
                                            (i64.eq (local.get '$result_tmp) (i64.const 1))
                                            (then (local.set '$result (local.get '$gt_val))
                                                  (br '$b)))

                                        (local.set '$a_tmp (i32.sub (local.get '$a_tmp) (i32.const 1)))
                                        (local.set '$a_ptr (i32.add (local.get '$a_ptr) (i32.const 8)))
                                        (local.set '$b_ptr (i32.add (local.get '$b_ptr) (i32.const 8)))
                                        (br '$l)
                                    )
                                    (br '$b))
                            )
                            ; else b is not an int or string or symbol or array, so bigger
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_array
                        (i64.eq (i64.const #b111) (i64.and (i64.const #b111) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; COMBINER
                    (_if '$a_comb
                        (i64.eq (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$a)))
                        (then
                            (_if '$b_comb
                                (i64.eq (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$b)))
                                (then
                                    ; compare func indicies first
                                    (local.set '$a_tmp (i32.wrap_i64 (i64.shr_u (local.get '$a) (i64.const 35))))
                                    (local.set '$b_tmp (i32.wrap_i64 (i64.shr_u (local.get '$b) (i64.const 35))))
                                    (_if '$a_tmp_lt_b_tmp
                                        (i32.lt_s (local.get '$a_tmp) (local.get '$b_tmp))
                                        (then
                                            (local.set '$result (local.get '$lt_val))
                                            (br '$b))
                                    )
                                    (_if '$a_tmp_eq_b_tmp
                                        (i32.gt_s (local.get '$a_tmp) (local.get '$b_tmp))
                                        (then
                                            (local.set '$result (local.get '$gt_val))
                                            (br '$b))
                                    )
                                    ; Idx was the same, so recursively comp envs
                                    (local.set '$result (call '$comp_helper_helper (i64.or (i64.shl (i64.extend_i32_u (local.get '$a_tmp)) (i64.const 5)) (i64.const #b01001))
                                                                                   (i64.or (i64.shl (i64.extend_i32_u (local.get '$b_tmp)) (i64.const 5)) (i64.const #b01001))
                                                                                   (local.get '$lt_val) (local.get '$eq_val) (local.get '$gt_val)))
                                    (br '$b))
                            )
                            ; else b is not an int or string or symbol or array or combiner, so bigger
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_comb
                        (i64.eq (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; ENV
                    (_if '$a_env
                        (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$a)))
                        (then
                            (_if '$b_comb
                                (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$b)))
                                (then
                                    (local.set '$a_ptr (i32.wrap_i64 (i64.shr_u (local.get '$a) (i64.const 5))))
                                    (local.set '$b_ptr (i32.wrap_i64 (i64.shr_u (local.get '$b) (i64.const 5))))

                                    ; First, compare their symbol arrays
                                    (local.set '$result_tmp (call '$comp_helper_helper (i64.load 0 (local.get '$a_ptr))
                                                                                       (i64.load 0 (local.get '$b_ptr))
                                                                                       (i64.const -1) (i64.const 0) (i64.const 1)))
                                    (_if '$a_lt_b
                                        (i64.eq (local.get '$result_tmp) (i64.const -1))
                                        (then (local.set '$result (local.get '$lt_val))
                                              (br '$b)))
                                    (_if '$a_gt_b
                                        (i64.eq (local.get '$result_tmp) (i64.const 1))
                                        (then (local.set '$result (local.get '$gt_val))
                                              (br '$b)))

                                    ; Second, compare their value arrays
                                    (local.set '$result_tmp (call '$comp_helper_helper (i64.load 8 (local.get '$a_ptr))
                                                                                       (i64.load 8 (local.get '$b_ptr))
                                                                                       (i64.const -1) (i64.const 0) (i64.const 1)))
                                    (_if '$a_lt_b
                                        (i64.eq (local.get '$result_tmp) (i64.const -1))
                                        (then (local.set '$result (local.get '$lt_val))
                                              (br '$b)))
                                    (_if '$a_gt_b
                                        (i64.eq (local.get '$result_tmp) (i64.const 1))
                                        (then (local.set '$result (local.get '$gt_val))
                                              (br '$b)))

                                    ; Finally, just accept the result of recursion
                                    (local.set '$result (call '$comp_helper_helper (i64.load 16 (local.get '$a_ptr))
                                                                                   (i64.load 16 (local.get '$b_ptr))
                                                                                   (local.get '$lt_val) (local.get '$eq_val) (local.get '$gt_val)))

                                    (br '$b))
                            )
                            ; else b is bool, so bigger
                            (local.set '$result (local.get '$lt_val))
                            (br '$b)
                        )
                    )
                    (_if '$b_env
                        (i64.eq (i64.const #b01001) (i64.and (i64.const #b11111) (local.get '$b)))
                        (then
                            (local.set '$result (local.get '$gt_val))
                            (br '$b))
                    )
                    ;; BOOL hehe
                    (_if '$a_lt_b
                        (i64.lt_s (local.get '$a) (local.get '$b))
                        (then
                            (local.set '$result (local.get '$lt_val))
                            (br '$b))
                    )
                    (_if '$a_eq_b
                        (i64.eq (local.get '$a) (local.get '$b))
                        (then
                            (local.set '$result (local.get '$eq_val))
                            (br '$b))
                    )
                    (local.set '$result (local.get '$gt_val))
                    (br '$b)
                )
                (local.get '$result)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_comp_helper   func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$comp_helper '(param $p i64) '(param $d i64) '(param $s i64) '(param $lt_val i64) '(param $eq_val i64) '(param $gt_val i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $result i64) '(local $a i64) '(local $b i64)
                set_len_ptr
                (local.set '$result (i64.const true_val))
                (block '$done_block
                    (_loop '$loop
                        (br_if '$done_block (i32.le_u (local.get '$len) (i32.const 1)))
                        (local.set '$a (i64.load (local.get '$ptr)))
                        (local.set '$ptr (i32.add (local.get '$ptr) (i32.const 8)))
                        (local.set '$b (i64.load (local.get '$ptr)))
                        (_if '$was_false
                            (i64.eq (i64.const false_val) (call '$comp_helper_helper (local.get '$a) (local.get '$b) (local.get '$lt_val) (local.get '$eq_val) (local.get '$gt_val)))
                            (then
                                (local.set '$result (i64.const false_val))
                                (br '$done_block)
                            )
                        )
                        (local.set '$len (i32.sub (local.get '$len) (i32.const 1)))
                        (br '$loop)
                    )
                )
                (local.get '$result)
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_eq_loc k_eq_length datasi) (alloc_data "k_eq" datasi))
              (k_eq_msg_val (bor (<< k_eq_length 32) k_eq_loc #b011))
              ((k_eq            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$eq  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const false_val)  (i64.const true_val)  (i64.const false_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_neq_loc k_neq_length datasi) (alloc_data "k_neq" datasi))
              (k_neq_msg_val (bor (<< k_neq_length 32) k_neq_loc #b011))
              ((k_neq           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$neq '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const true_val)   (i64.const false_val) (i64.const true_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_geq_loc k_geq_length datasi) (alloc_data "k_geq" datasi))
              (k_geq_msg_val (bor (<< k_geq_length 32) k_geq_loc #b011))
              ((k_geq           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$geq '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const false_val)  (i64.const true_val)  (i64.const true_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_gt_loc k_gt_length datasi) (alloc_data "k_gt" datasi))
              (k_gt_msg_val (bor (<< k_gt_length 32) k_gt_loc #b011))
              ((k_gt            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$gt  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const false_val)  (i64.const false_val) (i64.const true_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_leq_loc k_leq_length datasi) (alloc_data "k_leq" datasi))
              (k_leq_msg_val (bor (<< k_leq_length 32) k_leq_loc #b011))
              ((k_leq           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$leq '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const true_val)   (i64.const true_val)  (i64.const false_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_lt_loc k_lt_length datasi) (alloc_data "k_lt" datasi))
              (k_lt_msg_val (bor (<< k_lt_length 32) k_lt_loc #b011))
              ((k_lt            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$lt  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (call '$comp_helper (local.get '$p) (local.get '$d) (local.get '$s) (i64.const true_val)   (i64.const false_val) (i64.const false_val))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              (math_function (lambda (name sensitive op)
                (func name           '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $i i32) '(local $cur i64) '(local $next i64)
                    (ensure_not_op_n_params_set_ptr_len i32.eq 0)
                    (local.set '$i   (i32.const 1))
                    (local.set '$cur (i64.load (local.get '$ptr)))
                    (_if '$not_num (i64.ne (i64.const 0) (i64.and (i64.const 1) (local.get '$cur)))
                        (then (unreachable))
                    )
                    (block '$b
                        (_loop '$l
                            (br_if '$b (i32.eq (local.get '$len) (local.get '$i)))
                            (local.set '$ptr (i32.add (i32.const 8) (local.get '$ptr)))
                            (local.set '$next (i64.load (local.get '$ptr)))
                            (_if '$not_num (i64.ne (i64.const 0) (i64.and (i64.const 1) (local.get '$next)))
                                (then (unreachable))
                            )
                            (local.set '$cur (if sensitive (i64.shl (op (i64.shr_s (local.get '$cur) (i64.const 1)) (i64.shr_s (local.get '$next) (i64.const 1))) (i64.const 1))
                                                           (op (local.get '$cur) (local.get '$next))))
                            (local.set '$i (i32.add (local.get '$i) (i32.const 1)))
                            (br '$l)
                        )
                    )
                    drop_p_d
                    (local.get '$cur)
                )
              ))

              ((k_mod_loc k_mod_length datasi) (alloc_data "k_mod" datasi))
              (k_mod_msg_val (bor (<< k_mod_length 32) k_mod_loc #b011))
              ((k_mod           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$mod  true  i64.rem_s))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_div_loc k_div_length datasi) (alloc_data "k_div" datasi))
              (k_div_msg_val (bor (<< k_div_length 32) k_div_loc #b011))
              ((k_div           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$div  true  i64.div_s))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_mul_loc k_mul_length datasi) (alloc_data "k_mul" datasi))
              (k_mul_msg_val (bor (<< k_mul_length 32) k_mul_loc #b011))
              ((k_mul           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$mul  true  i64.mul))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_sub_loc k_sub_length datasi) (alloc_data "k_sub" datasi))
              (k_sub_msg_val (bor (<< k_sub_length 32) k_sub_loc #b011))
              ((k_sub           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$sub  true  i64.sub))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_add_loc k_add_length datasi) (alloc_data "k_add" datasi))
              (k_add_msg_val (bor (<< k_add_length 32) k_add_loc #b011))
              ((k_add           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$add  false i64.add))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_band_loc k_band_length datasi) (alloc_data "k_band" datasi))
              (k_band_msg_val (bor (<< k_band_length 32) k_band_loc #b011))
              ((k_band          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$band false i64.and))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_bor_loc k_bor_length datasi) (alloc_data "k_bor" datasi))
              (k_bor_msg_val (bor (<< k_bor_length 32) k_bor_loc #b011))
              ((k_bor           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$bor  false i64.or))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_bxor_loc k_bxor_length datasi) (alloc_data "k_bxor" datasi))
              (k_bxor_msg_val (bor (<< k_bxor_length 32) k_bxor_loc #b011))
              ((k_bxor          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (math_function '$bxor false i64.xor))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_bnot_loc k_bnot_length datasi) (alloc_data "k_bnot" datasi))
              (k_bnot_msg_val (bor (<< k_bnot_length 32) k_bnot_loc #b011))
              ((k_bnot          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$bnot   '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_int k_bnot_msg_val)
                (i64.xor (i64.const -2) (i64.load (local.get '$ptr)))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_ls_loc k_ls_length datasi) (alloc_data "k_ls" datasi))
              (k_ls_msg_val (bor (<< k_ls_length 32) k_ls_loc #b011))
              ((k_ls            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$ls  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 2)
                (type_assert 0 type_int k_ls_msg_val)
                (type_assert 1 type_int k_ls_msg_val)
                (i64.shl (i64.load 0 (local.get '$ptr)) (i64.shr_s (i64.load 8 (local.get '$ptr)) (i64.const 1)))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_rs_loc k_rs_length datasi) (alloc_data "k_rs" datasi))
              (k_rs_msg_val (bor (<< k_rs_length 32) k_rs_loc #b011))
              ((k_rs            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$rs  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 2)
                (type_assert 0 type_int k_rs_msg_val)
                (type_assert 1 type_int k_rs_msg_val)
                (i64.and (i64.const -2) (i64.shr_s (i64.load 0 (local.get '$ptr)) (i64.shr_s (i64.load 8 (local.get '$ptr)) (i64.const 1))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))


              ((k_builtin_fib_helper            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$builtin_fib_helper  '(param $n i64) '(result i64)
                  (_if '$eq0 '(result i64)
                       (i64.eq (i64.const 0) (local.get '$n))
                      (then (i64.const 2))
                      (else
                        (_if '$eq1 '(result i64)
                            (i64.eq (i64.const 2) (local.get '$n))
                            (then (i64.const 2))
                            (else
                              (i64.add (call '$builtin_fib_helper (i64.sub (local.get '$n) (i64.const 2))) (call '$builtin_fib_helper (i64.sub (local.get '$n) (i64.const 4))))
                            )
                        )
                      )
                  )
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_builtin_fib_loc k_builtin_fib_length datasi) (alloc_data "k_builtin_fib" datasi))
              (k_builtin_fib_msg_val (bor (<< k_builtin_fib_length 32) k_builtin_fib_loc #b011))
              ((k_builtin_fib            func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$builtin_fib  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_int k_builtin_fib_msg_val)
                (call '$builtin_fib_helper (i64.load 0 (local.get '$ptr)))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))


              ((k_concat_loc k_concat_length datasi) (alloc_data "k_concat" datasi))
              (k_concat_msg_val (bor (<< k_concat_length 32) k_concat_loc #b011))
              ((k_concat        func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$concat  '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $size i32) '(local $i i32) '(local $it i64) '(local $new_ptr i32) '(local $inner_ptr i32) '(local $inner_size i32) '(local $new_ptr_traverse i32) '(local $is_str i32)
                set_len_ptr
                (local.set '$size (i32.const 0))
                (local.set '$i (i32.const 0))
                (local.set '$is_str (i32.const 0))
                (block '$b
                    (_loop '$l
                        (br_if '$b (i32.eq (local.get '$len) (local.get '$i)))
                        (local.set '$it (i64.load (i32.add (i32.shl (local.get '$i) (i32.const 3)) (local.get '$ptr))))
                        (_if '$not_array (i64.ne (i64.const #b101) (i64.and (i64.const #b111) (local.get '$it)))
                            (then
                                (_if '$is_string (i64.eq (i64.const #b011) (i64.and (i64.const #b111) (local.get '$it)))
                                    (then
                                         (_if '$is_first (i32.eq (i32.const 0) (local.get '$i))
                                              (then
                                                (local.set '$is_str (i32.const 1))
                                              )
                                              (else
                                                    (_if '$mixed (i32.eqz (local.get '$is_str))
                                                         (then (unreachable)))
                                              )
                                         )
                                    )
                                    (else (unreachable))
                                )
                            )
                            (else
                              (_if '$mixed (local.get '$is_str)
                                   (then (unreachable)))
                            )
                        )
                        (local.set '$size (i32.add (local.get '$size) (i32.wrap_i64 (i64.shr_u (local.get '$it) (i64.const 32)))))
                        (local.set '$i    (i32.add (local.get '$i)    (i32.const 1)))
                        (br '$l)
                    )
                )
                (_if '$size_0 '(result i64)
                    (i32.eqz (local.get '$size))
                    (then (_if 'ret_emptystr '(result i64) (local.get '$is_str)
                               (then (i64.const emptystr_val))
                               (else (i64.const nil_val))))
                    (else
                        (_if 'doing_str '(result i64) (local.get '$is_str)
                          (then
                              (local.set '$new_ptr  (call '$malloc (local.get '$size))) ; malloc(size)
                              (local.set '$new_ptr_traverse (local.get '$new_ptr))

                              (local.set '$i (i32.const 0))
                              (block '$exit_outer_loop
                                  (_loop '$outer_loop
                                      (br_if '$exit_outer_loop (i32.eq (local.get '$len) (local.get '$i)))
                                      (local.set '$it (i64.load (i32.add (i32.shl (local.get '$i) (i32.const 3)) (local.get '$ptr))))

                                      (local.set '$inner_ptr  (i32.wrap_i64 (i64.and   (local.get '$it) (i64.const -8))))
                                      (local.set '$inner_size (i32.wrap_i64 (i64.shr_u (local.get '$it) (i64.const 32))))

                                      (memory.copy (local.get '$new_ptr_traverse)
                                                   (local.get '$inner_ptr)
                                                   (local.get '$inner_size))

                                      (local.set '$new_ptr_traverse    (i32.add (local.get '$new_ptr_traverse) (local.get '$inner_size)))
                                      (local.set '$i (i32.add (local.get '$i) (i32.const 1)))
                                      (br '$outer_loop)
                                  )
                              )

                              ;(i64.store (i32.add (i32.const -16) (local.get '$new_ptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr)) (i64.const #b011))
                              ;                                                               (i64.shl (i64.extend_i32_u (local.get '$size))    (i64.const 32)))) ; MDEBUG

                              (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr)) (i64.const #b011))
                                      (i64.shl (i64.extend_i32_u (local.get '$size))    (i64.const 32)))
                          )
                          (else
                              (local.set '$new_ptr  (call '$malloc (i32.shl (local.get '$size) (i32.const 3)))) ; malloc(size*8)
                              (local.set '$new_ptr_traverse (local.get '$new_ptr))

                              (local.set '$i (i32.const 0))
                              (block '$exit_outer_loop
                                  (_loop '$outer_loop
                                      (br_if '$exit_outer_loop (i32.eq (local.get '$len) (local.get '$i)))
                                      (local.set '$it (i64.load (i32.add (i32.shl (local.get '$i) (i32.const 3)) (local.get '$ptr))))
                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                      ; There's some serious optimization we could do here
                                      ; Moving the items from the sub arrays to this one without
                                      ; going through all the dup/drop
                                      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                      (local.set '$inner_ptr  (i32.wrap_i64 (i64.and   (local.get '$it) (i64.const -8))))
                                      (local.set '$inner_size (i32.wrap_i64 (i64.shr_u (local.get '$it) (i64.const 32))))

                                      (block '$exit_inner_loop
                                          (_loop '$inner_loop
                                              (br_if '$exit_inner_loop (i32.eqz (local.get '$inner_size)))
                                              (i64.store (local.get '$new_ptr_traverse)
                                                         (call '$dup (i64.load (local.get '$inner_ptr))))
                                              (local.set '$inner_ptr           (i32.add (local.get '$inner_ptr)          (i32.const 8)))
                                              (local.set '$new_ptr_traverse    (i32.add (local.get '$new_ptr_traverse)   (i32.const 8)))
                                              (local.set '$inner_size          (i32.sub (local.get '$inner_size)         (i32.const 1)))
                                              (br '$inner_loop)
                                          )
                                      )
                                      (local.set '$i (i32.add (local.get '$i) (i32.const 1)))
                                      (br '$outer_loop)
                                  )
                              )

                              ;(i64.store (i32.add (i32.const -16) (local.get '$new_ptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr)) (i64.const #b101))
                              ;                                                               (i64.shl (i64.extend_i32_u (local.get '$size))    (i64.const 32)))) ; MDEBUG

                              (i64.or (i64.or  (i64.extend_i32_u (local.get '$new_ptr)) (i64.const #b101))
                                      (i64.shl (i64.extend_i32_u (local.get '$size))    (i64.const 32)))
                          )
                         )
                    )
                )
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_slice_loc k_slice_length datasi) (alloc_data "k_slice" datasi))
              (k_slice_msg_val (bor (<< k_slice_length 32) k_slice_loc #b011))
              ((k_slice         func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$slice   '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 3)
                (type_assert 0 (array type_array type_string) k_slice_msg_val)
                (type_assert 1 type_int k_slice_msg_val)
                (type_assert 2 type_int k_slice_msg_val)
                (call '$slice_impl (call '$dup (i64.load  0 (local.get '$ptr)))
                                   (i32.wrap_i64 (i64.shr_s (i64.load  8 (local.get '$ptr)) (i64.const 1)))
                                   (i32.wrap_i64 (i64.shr_s (i64.load 16 (local.get '$ptr)) (i64.const 1))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_idx_loc k_idx_length datasi) (alloc_data "k_idx" datasi))
              (k_idx_msg_val (bor (<< k_idx_length 32) k_idx_loc #b011))
              ((k_idx           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$idx     '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $array i64) '(local $idx i32) '(local $size i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 2)
                (type_assert 0 (array type_array type_string) k_idx_msg_val)
                (type_assert 1 type_int k_idx_msg_val)
                (local.set '$array (i64.load 0 (local.get '$ptr)))
                (local.set '$idx  (i32.wrap_i64 (i64.shr_s (i64.load 8 (local.get '$ptr)) (i64.const 1))))
                (local.set '$size (i32.wrap_i64 (i64.shr_u (local.get '$array) (i64.const 32))))

                (_if '$i_lt_0     (i32.lt_s (local.get '$idx) (i32.const 0))      (then (unreachable)))
                (_if '$i_ge_s     (i32.ge_s (local.get '$idx) (local.get '$size)) (then (unreachable)))

                (typecheck 0 (array '(result i64))
                    i64.eq type_array
                    (array (then
                      (call '$dup (i64.load (i32.add (i32.wrap_i64 (i64.and (local.get '$array) (i64.const -8)))
                                                     (i32.shl (local.get '$idx) (i32.const 3)))))
                    ))
                    (array (else (i64.shl (i64.load8_u (i32.add (i32.wrap_i64 (i64.and (local.get '$array) (i64.const -8)))
                                                       (local.get '$idx)))
                                          (i64.const 1))))
                )
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_len_loc k_len_length datasi) (alloc_data "k_len" datasi))
              (k_len_msg_val (bor (<< k_len_length 32) k_len_loc #b011))
              ((k_len           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$len     '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 (array type_array type_string) k_len_msg_val)
                (i64.and (i64.shr_u (i64.load 0 (local.get '$ptr)) (i64.const 31)) (i64.const -2))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_array_loc k_array_length datasi) (alloc_data "k_array" datasi))
              (k_array_msg_val (bor (<< k_array_length 32) k_array_loc #b011))
              ((k_array         func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$array   '(param $p i64) '(param $d i64) '(param $s i64) '(result i64)
                (local.get '$p)
                (call '$drop (local.get '$d))
                ; s is 0
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_get_loc k_get_length datasi) (alloc_data "k_get-text" datasi))
              (k_get_msg_val (bor (<< k_get_length 32) k_get_loc #b011))
              ((k_get-text      func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$get-text      '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_symbol k_get_msg_val)
                (call '$dup (i64.and (i64.const -5) (i64.load (local.get '$ptr))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_str_loc k_str_length datasi) (alloc_data "k_str" datasi))
              (k_str_msg_val (bor (<< k_str_length 32) k_str_loc #b011))
              ((k_str-to-symbol func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$str-to-symbol '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_string k_str_msg_val)
                (call '$dup (i64.or (i64.const #b100) (i64.load (local.get '$ptr))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_unwrap_loc k_unwrap_length datasi) (alloc_data "k_unwrap" datasi))
              (k_unwrap_msg_val (bor (<< k_unwrap_length 32) k_unwrap_loc #b011))
              ((k_unwrap        func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$unwrap        '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $comb i64) '(local $wrap_level i64)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_combiner k_unwrap_msg_val)
                (local.set '$comb (i64.load (local.get '$ptr)))
                (local.set '$wrap_level (i64.and (i64.shr_u (local.get '$comb) (i64.const 4)) (i64.const #b1)))
                (_if '$wrap_level_0
                    (i64.eqz (local.get '$wrap_level))
                    (then (unreachable))
                )
                (call '$dup (i64.or (i64.and (local.get '$comb) (i64.const -49))
                                    (i64.shl (i64.sub (local.get '$wrap_level) (i64.const 1)) (i64.const 4))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_wrap_loc k_wrap_length datasi) (alloc_data "k_wrap" datasi))
              (k_wrap_msg_val (bor (<< k_wrap_length 32) k_wrap_loc #b011))
              ((k_wrap          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$wrap          '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $comb i64) '(local $wrap_level i64)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_combiner k_wrap_msg_val)
                (local.set '$comb (i64.load (local.get '$ptr)))
                (local.set '$wrap_level (i64.and (i64.shr_u (local.get '$comb) (i64.const 4)) (i64.const #b1)))
                (_if '$wrap_level_1
                    (i64.eq (i64.const 1) (local.get '$wrap_level))
                    (then (unreachable))
                )
                (call '$dup (i64.or (i64.and (local.get '$comb) (i64.const -49))
                                    (i64.shl (i64.add (local.get '$wrap_level) (i64.const 1)) (i64.const 4))))
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_lapply_loc k_lapply_length datasi) (alloc_data "k_lapply" datasi))
              (k_lapply_msg_val (bor (<< k_lapply_length 32) k_lapply_loc #b011))
              ((k_lapply          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$lapply          '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $comb i64) '(local $params i64) '(local $wrap_level i64) '(local $inner_env i64)
                (ensure_not_op_n_params_set_ptr_len i32.lt_u 2)
                (type_assert 0 type_combiner k_lapply_msg_val)
                (type_assert 1 type_array k_lapply_msg_val)
                (local.set '$comb   (call '$dup (i64.load 0 (local.get '$ptr))))
                (local.set '$params (call '$dup (i64.load 8 (local.get '$ptr))))
                (_if '$needs_dynamic_env
                     (i64.ne (i64.const #b0) (i64.and (local.get '$comb) (i64.const #b100000)))
                     (then
                       (_if '$explicit_inner
                            (i32.eq (i32.const 3) (local.get '$len))
                            (then
                              (type_assert 2 type_env k_lapply_msg_val)
                              (call '$drop (local.get '$d))
                              (local.set '$inner_env (call '$dup (i64.load 16 (local.get '$ptr))))
                            )
                            (else
                              (local.set '$inner_env (local.get '$d))
                            )
                       )
                     )
                     (else
                       (call '$drop (local.get '$d))
                       (local.set '$inner_env (i64.const nil_val))
                     )
                )
                (call '$drop (local.get '$p))
                (local.set '$wrap_level (i64.and (i64.shr_u (local.get '$comb) (i64.const 4)) (i64.const #b1)))
                (_if '$wrap_level_ne_1
                    (i64.ne (i64.const 1) (local.get '$wrap_level))
                    (then (unreachable))
                )

                (call_indirect
                    ;type
                    k_wrap
                    ;table
                    0
                    ;params
                    (local.get '$params)
                    ; dynamic env
                    (local.get '$inner_env)
                    ; static env
                    (i64.or (i64.shl (i64.and (local.get '$comb) (i64.const #x3FFFFFFC0))
                                     (i64.const 2)) (i64.const #b01001))
                    ;func_idx
                    (i32.wrap_i64 (i64.shr_u (local.get '$comb) (i64.const 35)))
                )
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_vapply_loc k_vapply_length datasi) (alloc_data "k_vapply" datasi))
              (k_vapply_msg_val (bor (<< k_vapply_length 32) k_vapply_loc #b011))
              ((k_vapply          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$vapply          '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $comb i64) '(local $params i64) '(local $wrap_level i64) '(local $inner_env i64)
                (ensure_not_op_n_params_set_ptr_len i32.ne 3)
                (type_assert 0 type_combiner k_vapply_msg_val)
                (type_assert 1 type_array k_vapply_msg_val)
                (local.set '$comb   (call '$dup (i64.load 0 (local.get '$ptr))))
                (local.set '$params (call '$dup (i64.load 8 (local.get '$ptr))))
                (_if '$needs_dynamic_env
                     (i64.ne (i64.const #b0) (i64.and (local.get '$comb) (i64.const #b100000)))
                     (then
                       (_if '$explicit_inner
                            (i32.eq (i32.const 3) (local.get '$len))
                            (then
                              (type_assert 2 type_env k_vapply_msg_val)
                              (call '$drop (local.get '$d))
                              (local.set '$inner_env (call '$dup (i64.load 16 (local.get '$ptr))))
                            )
                            (else
                              (local.set '$inner_env (local.get '$d))
                            )
                       )
                     )
                     (else
                       (call '$drop (local.get '$d))
                       (local.set '$inner_env (i64.const nil_val))
                     )
                )
                (call '$drop (local.get '$p))
                (local.set '$wrap_level (i64.and (i64.shr_u (local.get '$comb) (i64.const 4)) (i64.const #b1)))
                (_if '$wrap_level_ne_0
                    (i64.ne (i64.const 0) (local.get '$wrap_level))
                    (then (unreachable))
                )

                (call_indirect
                    ;type
                    k_wrap
                    ;table
                    0
                    ;params
                    (local.get '$params)
                    ; dynamic env
                    (local.get '$inner_env)
                    ; static env
                    (i64.or (i64.shl (i64.and (local.get '$comb) (i64.const #x3FFFFFFC0))
                                     (i64.const 2)) (i64.const #b01001))
                    ;func_idx
                    (i32.wrap_i64 (i64.shr_u (local.get '$comb) (i64.const 35)))
                )
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ;true_val          #b000111001
              ;false_val         #b00001100)
              (empty_parse_value #b00101100)
              (close_peren_value #b01001100)
              (error_parse_value #b01101100)
              ; *GLOBAL ALERT*
              ((k_parse_helper  func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$parse_helper  '(result i64) '(local $result i64) '(local $tmp i32) '(local $sub_result i64) '(local $asiz i32) '(local $acap i32) '(local $aptr i32) '(local $bptr i32) '(local $bcap i32) '(local $neg_multiplier i64) '(local $radix i64)
                (block '$b1
                    (block '$b2
                        (_loop '$l
                            (br_if '$b2 (i32.eqz (global.get '$phl)))
                            (local.set '$tmp (i32.load8_u (global.get '$phs)))
                            ;(call '$print (i64.shl (i64.extend_i32_u (local.get '$tmp)) (i64.const 1)))
                            (_if '$whitespace (i32.or (i32.or (i32.eq (i32.const #x9)  (local.get '$tmp))    ; tab
                                                              (i32.eq (i32.const #xA)  (local.get '$tmp)))   ; newline
                                                      (i32.or (i32.eq (i32.const #xD)  (local.get '$tmp))    ; carrige return
                                                              (i32.eq (i32.const #x20) (local.get '$tmp))))  ; space
                                (then
                                    (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                    (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                    (br '$l)
                                )
                            )
                            (_if '$comment (i32.eq (i32.const #x3B)  (local.get '$tmp))
                                (then
                                    (_loop '$li
                                        (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                        (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                        (br_if '$b2 (i32.eqz (global.get '$phl)))
                                        (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                        (br_if '$li (i32.ne (i32.const #xA) (local.get '$tmp)))
                                    )
                                    (br '$l)
                                )
                            )
                        )
                    )
                    (local.set '$result (i64.const empty_parse_value))
                    (_if '$at_least1
                        (i32.ge_u (global.get '$phl) (i32.const 1))
                        (then
                            (local.set '$tmp (i32.load8_u (global.get '$phs)))
                            ; string
                            (_if '$is_open
                                (i32.eq (local.get '$tmp) (i32.const #x22))
                                (then
                                    (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                    (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                    (local.set '$asiz (i32.const 0))
                                    (local.set '$bptr (global.get '$phs))

                                    ; Count size
                                    (block '$b2
                                        (_loop '$il
                                            (_if '$doesnt_have_next
                                                (i32.eqz (global.get '$phl))
                                                (then
                                                    (local.set '$result (i64.const error_parse_value))
                                                    (br '$b1)
                                                )
                                            )

                                            (br_if '$b2 (i32.eq (i32.load8_u (global.get '$phs)) (i32.const #x22)))

                                            (_if '$an_escape
                                                (i32.eq (i32.load8_u (global.get '$phs)) (i32.const #x5C))
                                                (then
                                                    (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                                    (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                                    (_if '$doesnt_have_next
                                                        (i32.eqz (global.get '$phl))
                                                        (then
                                                            (local.set '$result (i64.const error_parse_value))
                                                            (br '$b1)
                                                        )
                                                    )
                                                )
                                            )
                                            (local.set '$asiz (i32.add (local.get '$asiz) (i32.const 1)))

                                            (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                            (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                            (br '$il)
                                        )
                                    )

                                    (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                    (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))

                                    (local.set '$bcap (local.get '$asiz))
                                    (local.set '$aptr (call '$malloc (local.get '$asiz)))

                                    ; copy the bytes, implementing the escapes
                                    (block '$b2
                                        (_loop '$il
                                            (br_if '$b2 (i32.eqz (local.get '$bcap)))

                                            (_if '$an_escape
                                                (i32.eq (i32.load8_u (local.get '$bptr)) (i32.const #x5C))
                                                (then
                                                    (_if '$escaped_slash
                                                        (i32.eq (i32.load8_u 1 (local.get '$bptr)) (i32.const #x5C))
                                                        (then
                                                            (i32.store8 (local.get '$aptr) (i32.const #x5C))
                                                        )
                                                        (else
                                                            (_if '$escaped_quote
                                                                (i32.eq (i32.load8_u 1 (local.get '$bptr)) (i32.const #x22))
                                                                (then
                                                                    (i32.store8 (local.get '$aptr) (i32.const #x22))
                                                                )
                                                                (else
                                                                    (_if '$escaped_newline
                                                                        (i32.eq (i32.load8_u 1 (local.get '$bptr)) (i32.const #x6E))
                                                                        (then
                                                                            (i32.store8 (local.get '$aptr) (i32.const #x0A))
                                                                        )
                                                                        (else
                                                                            (_if '$escaped_tab
                                                                                (i32.eq (i32.load8_u 1 (local.get '$bptr)) (i32.const #x74))
                                                                                (then
                                                                                    (i32.store8 (local.get '$aptr) (i32.const #x09))
                                                                                )
                                                                                (else
                                                                                    (global.set '$phl  (i32.add (global.get '$phl) (i32.sub (global.get '$phs) (local.get '$bptr))))
                                                                                    (global.set '$phs  (local.get '$bptr))
                                                                                    (local.set '$result (i64.const error_parse_value))
                                                                                    (br '$b1)
                                                                                )
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        )
                                                    )
                                                    (local.set '$bptr (i32.add (local.get '$bptr) (i32.const 2)))
                                                )
                                                (else
                                                    (i32.store8 (local.get '$aptr) (i32.load8_u (local.get '$bptr)))
                                                    (local.set '$bptr (i32.add (local.get '$bptr) (i32.const 1)))
                                                )
                                            )
                                            (local.set '$bcap (i32.sub (local.get '$bcap) (i32.const 1)))
                                            (local.set '$aptr (i32.add (local.get '$aptr) (i32.const 1)))
                                            (br '$il)
                                        )
                                    )
                                    (local.set '$aptr (i32.sub (local.get '$aptr) (local.get '$asiz)))


                                    ;(i64.store (i32.add (i32.const -16) (local.get '$aptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$aptr)) (i64.const #x3))
                                    ;                                                        (i64.shl (i64.extend_i32_u (local.get '$asiz)) (i64.const 32)))) ; MDEBUG


                                    (local.set '$result (i64.or (i64.or  (i64.extend_i32_u (local.get '$aptr)) (i64.const #x3))
                                                                (i64.shl (i64.extend_i32_u (local.get '$asiz)) (i64.const 32))))
                                    (br '$b1)
                                )
                            )

                            ; negative int
                            (local.set '$neg_multiplier (i64.const 1))
                            (_if '$is_dash_and_more
                                (i32.and (i32.eq (local.get '$tmp) (i32.const #x2D)) (i32.ge_u (global.get '$phl) (i32.const 2)))
                                (then
                                    (_if '$next_is_letter
                                        (i32.and (i32.ge_u (i32.load8_u 1 (global.get '$phs)) (i32.const #x30)) (i32.le_u (i32.load8_u 1 (global.get '$phs)) (i32.const #x39)))
                                        (then
                                            (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                            (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                            (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                            (local.set '$neg_multiplier (i64.const -1))
                                        )
                                    )
                                )
                            )
                            ; int
                            (local.set '$radix (i64.const 10))
                            (_if '$is_zero_through_nine
                                (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x30)) (i32.le_u (local.get '$tmp) (i32.const #x39)))
                                (then
                                    (local.set '$result (i64.const 0))
                                    (_loop '$il
                                        (_if '$is_zero_through_nine_inner
                                            (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x30)) (i32.le_u (local.get '$tmp) (i32.const #x39)))
                                            (then
                                                (local.set '$tmp (i32.sub (local.get '$tmp) (i32.const #x30)))
                                            )
                                            (else
                                                (local.set '$tmp (i32.sub (local.get '$tmp) (i32.const #x37)))
                                            )
                                        )
                                        (local.set '$result (i64.add (i64.mul (local.get '$radix) (local.get '$result)) (i64.extend_i32_u (local.get '$tmp))))
                                        (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                        (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                        (_if '$at_least1
                                            (i32.ge_u (global.get '$phl) (i32.const 1))
                                            (then
                                                (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                                (_if '$is_hex_and_more
                                                    (i32.and (i32.eq (local.get '$tmp) (i32.const #x78)) (i32.ge_u (global.get '$phl) (i32.const 2)))
                                                    (then
                                                        (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                                        (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                                        (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                                        (local.set '$radix (i64.const 16))
                                                    )
                                                    (else
                                                        (_if '$is_hex_and_more
                                                            (i32.and (i32.eq (local.get '$tmp) (i32.const #x62)) (i32.ge_u (global.get '$phl) (i32.const 2)))
                                                            (then
                                                                (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                                                (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                                                (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                                                (local.set '$radix (i64.const 2))
                                                            )
                                                        )
                                                    )
                                                )
                                                (br_if '$il (i32.or (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x30)) (i32.le_u (local.get '$tmp) (i32.const #x39)))
                                                                    (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x41)) (i32.le_u (local.get '$tmp) (i32.const #x46)))))
                                            )
                                        )
                                    )
                                    (local.set '$result (i64.shl (i64.mul (local.get '$neg_multiplier) (local.get '$result)) (i64.const 1)))
                                    (br '$b1)
                                )
                            )

                            ; []?
                            ; '
                            (_if '$is_quote
                                (i32.eq (local.get '$tmp) (i32.const #x27))
                                (then
                                    (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                    (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                    (local.set '$sub_result (call '$parse_helper))
                                    (_if '$ended
                                        (i64.eq (i64.const close_peren_value) (local.get '$sub_result))
                                        (then
                                            (local.set '$result (i64.const error_parse_value))
                                            (br '$b1)
                                        )
                                    )
                                    (_if '$error
                                        (i32.or (i64.eq (i64.const error_parse_value) (local.get '$sub_result))
                                                (i64.eq (i64.const empty_parse_value) (local.get '$sub_result)))
                                        (then
                                            (local.set '$result (local.get '$sub_result))
                                            (br '$b1)
                                        )
                                    )
                                    (local.set '$result (call '$array2_alloc (i64.const quote_sym_val) (local.get '$sub_result)))
                                    (br '$b1)
                                )
                            )

                            ; symbol
                            (_if '$is_dash_and_more
                                ; 21    !
                                ; 22    "           X
                                ; 23-26 #-&
                                ; 27    '           X
                                ; 28-29 (-)         X
                                ; 2A-2F *-/
                                ; 30-39 0-9         /
                                ; 3A    :
                                ; 3B    ;
                                ; 3C-40 <-@
                                ; 41-5A A-Z
                                ; 5B-60 [-`
                                ; 61-7A a-z
                                ; 7B-7E {-~
                                (i32.or (i32.or         (i32.eq (local.get '$tmp) (i32.const #x21))
                                                        (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x23)) (i32.le_u (local.get '$tmp) (i32.const #x26))))
                                        (i32.or         (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x2A)) (i32.le_u (local.get '$tmp) (i32.const #x2F)))
                                                (i32.or (i32.eq (local.get '$tmp) (i32.const #x3A))
                                                        (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x3C)) (i32.le_u (local.get '$tmp) (i32.const #x7E))))))
                                (then
                                    (local.set '$asiz (i32.const 0))
                                    (local.set '$bptr (global.get '$phs))
                                    (block '$loop_break
                                        (_loop '$il
                                            (global.set '$phs  (i32.add (global.get '$phs)  (i32.const 1)))
                                            (global.set '$phl  (i32.sub (global.get '$phl)  (i32.const 1)))
                                            (local.set '$asiz (i32.add (local.get '$asiz) (i32.const 1)))
                                            (_if '$doesnt_have_next
                                                (i32.eqz (global.get '$phl))
                                                (then (br '$loop_break))
                                            )
                                            (local.set '$tmp (i32.load8_u (global.get '$phs)))
                                            (br_if '$il (i32.or (i32.or         (i32.eq (local.get '$tmp) (i32.const #x21))
                                                                                (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x23)) (i32.le_u (local.get '$tmp) (i32.const #x26))))
                                                                        (i32.or (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x2A)) (i32.le_u (local.get '$tmp) (i32.const #x3A)))
                                                                                (i32.and (i32.ge_u (local.get '$tmp) (i32.const #x3C)) (i32.le_u (local.get '$tmp) (i32.const #x7E))))))
                                        )
                                    )
                                    (_if '$is_true1
                                        (i32.eq (local.get '$asiz) (i32.const 4))
                                        (then
                                            (_if '$is_true2
                                                (i32.eq (i32.load (local.get '$bptr)) (i32.const #x65757274))
                                                (then
                                                    (local.set '$result (i64.const true_val))
                                                    (br '$b1)
                                                )
                                            )
                                        )
                                    )
                                    (_if '$is_false1
                                        (i32.eq (local.get '$asiz) (i32.const 5))
                                        (then
                                            (_if '$is_false2
                                                (i32.and (i32.eq (i32.load (local.get '$bptr)) (i32.const #x736C6166)) (i32.eq (i32.load8_u 4 (local.get '$bptr)) (i32.const #x65)))
                                                (then
                                                    (local.set '$result (i64.const false_val))
                                                    (br '$b1)
                                                )
                                            )
                                        )
                                    )
                                    (local.set '$aptr (call '$malloc (local.get '$asiz)))
                                    (memory.copy (local.get '$aptr)
                                                 (local.get '$bptr)
                                                 (local.get '$asiz))


                                    ;(i64.store (i32.add (i32.const -16) (local.get '$aptr)) (i64.or (i64.or  (i64.extend_i32_u (local.get '$aptr)) (i64.const #x7))
                                    ;                                                        (i64.shl (i64.extend_i32_u (local.get '$asiz)) (i64.const 32)))) ; MDEBUG

                                    (local.set '$result (i64.or (i64.or  (i64.extend_i32_u (local.get '$aptr)) (i64.const #x7))
                                                                (i64.shl (i64.extend_i32_u (local.get '$asiz)) (i64.const 32))))
                                    (br '$b1)
                                )
                            )

                            ; lists (arrays)!
                            (_if '$is_open
                                (i32.eq (local.get '$tmp) (i32.const #x28))
                                (then
                                    (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                    (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                    (local.set '$asiz (i32.const 0))
                                    (local.set '$acap (i32.const 4))
                                    (local.set '$aptr (call '$malloc (i32.const (* 4 8))))
                                    (_loop '$il
                                        (local.set '$sub_result (call '$parse_helper))
                                        (_if '$ended
                                            (i64.eq (i64.const close_peren_value) (local.get '$sub_result))
                                            (then
                                                (_if '$nil
                                                    (i32.eqz (local.get '$asiz))
                                                    (then
                                                        (call '$free (local.get '$aptr))
                                                        (local.set '$result (i64.const nil_val))
                                                    )
                                                    (else
                                                        (local.set '$result (i64.or (i64.or  (i64.extend_i32_u (local.get '$aptr)) (i64.const #x5))
                                                                                    (i64.shl (i64.extend_i32_u (local.get '$asiz)) (i64.const 32))))
                                                    )
                                                )
                                                (br '$b1)
                                            )
                                        )
                                        (_if '$error
                                            (i32.or (i64.eq (i64.const error_parse_value) (local.get '$sub_result))
                                                    (i64.eq (i64.const empty_parse_value) (local.get '$sub_result)))
                                            (then
                                                (local.set '$result (local.get '$sub_result))
                                                (br '$b1)
                                            )
                                        )
                                        (_if '$need_to_grow
                                            (i32.eq (local.get '$asiz) (local.get '$acap))
                                            (then
                                                (local.set '$bcap (i32.shl (local.get '$acap) (i32.const 1)))
                                                (local.set '$bptr (call '$malloc (i32.shl (local.get '$bcap) (i32.const 3))))
                                                (local.set '$asiz (i32.const 0))
                                                (_loop '$iil
                                                    (i64.store            (i32.add (local.get '$bptr) (i32.shl (local.get '$asiz) (i32.const 3)))
                                                                (i64.load (i32.add (local.get '$aptr) (i32.shl (local.get '$asiz) (i32.const 3)))))
                                                    (local.set '$asiz (i32.add (local.get '$asiz) (i32.const 1)))
                                                    (br_if '$iil (i32.lt_u (local.get '$asiz) (local.get '$acap)))
                                                )
                                                (local.set '$acap (local.get '$bcap))
                                                (call '$free (local.get '$aptr))
                                                (local.set '$aptr (local.get '$bptr))
                                            )
                                        )
                                        (i64.store  (i32.add (local.get '$aptr) (i32.shl (local.get '$asiz) (i32.const 3)))
                                                    (local.get '$sub_result))
                                        (local.set '$asiz (i32.add (local.get '$asiz) (i32.const 1)))
                                        (br '$il)
                                    )
                                )
                            )
                            (_if '$is_close
                                (i32.eq (local.get '$tmp) (i32.const #x29))
                                (then
                                    (local.set '$result (i64.const close_peren_value))
                                    (global.set '$phs (i32.add (global.get '$phs) (i32.const 1)))
                                    (global.set '$phl (i32.sub (global.get '$phl) (i32.const 1)))
                                    (br '$b1)
                                )
                            )
                        )
                    )
                )
                (local.get '$result)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_read_loc k_read_length datasi) (alloc_data "k_read" datasi))
              (k_read_msg_val (bor (<< k_read_length 32) k_read_loc #b011))
              ((k_read-string   func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$read-string   '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $ptr i32) '(local $len i32) '(local $str i64) '(local $result i64) '(local $tmp_result i64) '(local $tmp_offset i32)
                (ensure_not_op_n_params_set_ptr_len i32.ne 1)
                (type_assert 0 type_string k_read_msg_val)
                (local.set '$str (i64.load (local.get '$ptr)))
                ;(call '$print (local.get '$str))
                (global.set '$phl (i32.wrap_i64 (i64.shr_u (local.get '$str) (i64.const 32))))
                (global.set '$phs (i32.wrap_i64 (i64.and   (local.get '$str) (i64.const #xFFFFFFF8))))
                (local.set '$result (call '$parse_helper))
                (_if '$was_empty_parse
                    (i32.or         (i64.eq (i64.const error_parse_value) (local.get '$result))
                            (i32.or (i64.eq (i64.const empty_parse_value) (local.get '$result))
                                    (i64.eq (i64.const close_peren_value) (local.get '$result))))
                    (then
                        (call '$print (i64.const couldnt_parse_1_msg_val))
                        (call '$print (local.get '$str))
                        (call '$print (i64.const couldnt_parse_2_msg_val))
                        (call '$print (i64.shl (i64.add (i64.const 1) (i64.sub (i64.shr_u (local.get '$str) (i64.const 32)) (i64.extend_i32_u (global.get '$phl)))) (i64.const 1)))
                        (call '$print (i64.const newline_msg_val))
                        (unreachable)
                    )
                )
                (_if '$remaining
                    (i32.ne (i32.const 0) (global.get '$phl))
                    (then
                        (local.set '$tmp_offset (global.get '$phl))
                        (local.set '$tmp_result (call '$parse_helper))
                        (_if '$wasnt_empty_parse
                            (i64.ne (i64.const empty_parse_value) (local.get '$tmp_result))
                            (then
                                (call '$print (i64.const parse_remaining_msg_val))
                                (call '$print (i64.shl (i64.sub (i64.shr_u (local.get '$str) (i64.const 32)) (i64.extend_i32_u (local.get '$tmp_offset))) (i64.const 1)))
                                (call '$print (i64.const newline_msg_val))
                                (unreachable)
                            )
                        )
                    )
                )
                (local.get '$result)
                drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))




              (front_half_stack_code (lambda (call_val env_val) (_if '$debug_level (i32.ne (i32.const -1) (global.get '$debug_depth)) (then (global.set '$stack_trace (call '$array3_alloc call_val
                                                                                                       env_val
                                                                                                       (call '$dup (global.get '$stack_trace))))))))
              (back_half_stack_code (concat (_if '$debug_level (i32.ne (i32.const -1) (global.get '$debug_depth)) (then
                                            (i64.load 16 (i32.wrap_i64 (i64.and (i64.const -8) (global.get '$stack_trace))))
                                            (call '$drop (global.get '$stack_trace))
                                            (global.set '$stack_trace)))))
              ;(front_half_stack_code (lambda (call_val env_val) (array)))
              ;(back_half_stack_code (array))

              ((k_call_zero_len_loc     k_call_zero_len_length datasi) (alloc_data "tried to eval a 0-length call" datasi))
               (k_call_zero_len_msg_val (bor (<< k_call_zero_len_length 32) k_call_zero_len_loc #b011))

              ((k_call_not_a_function_loc     k_call_not_a_function_length datasi) (alloc_data "tried to eval a call to not a function " datasi))
               (k_call_not_a_function_msg_val (bor (<< k_call_not_a_function_length 32) k_call_not_a_function_loc #b011))

              ; Helper method, doesn't refcount consume parameters
              ; but does properly refcount internally / dup returns
              ((k_eval_helper func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$eval_helper '(param $it i64) '(param $env i64) '(result i64) '(local $len i32) '(local $ptr i32) '(local $current_env i64) '(local $res i64) '(local $env_ptr i32) '(local $tmp_ptr i32) '(local $i i32) '(local $comb i64) '(local $params i64) '(local $wrap i32) '(local $tmp i64)


            ; The cool thing about Vau calculus / Kernel / Kraken
            ; is that everything is a value that evaluates to itself except symbols
            ; and arrays.
            (_if '$is_value '(result i64)
                 (i64.ne (i64.const #b101) (i64.and (i64.const #b101) (local.get '$it)))
                 (then
                   ; it's a value, we can just return it!
                   (call '$dup (local.get '$it))
                 )
                 (else
                   (_if '$is_symbol '(result i64)
                        (i64.eq (i64.const #b111) (i64.and (i64.const #b111) (local.get '$it)))
                        (then
                          ; look it up in the environment
                          ; 0..0<env_ptr32 but still aligned>01001
                          ;   Env object is <key_array_value><value_array_value><upper_env_value>
                          ;       each being the full 64 bit objects.
                          (local.set '$current_env (local.get '$env))

                          (block '$outer_loop_break
                                (_loop '$outer_loop
                                      (local.set '$env_ptr (i32.wrap_i64 (i64.shr_u (local.get '$current_env) (i64.const 5))))

                                      (local.set '$len (i32.wrap_i64 (i64.shr_u (i64.load 0 (local.get '$env_ptr)) (i64.const 32))))
                                      (local.set '$ptr (i32.wrap_i64 (i64.and   (i64.load 0 (local.get '$env_ptr)) (i64.const -8))))
                                      (local.set '$i   (i32.const 0))

                                      (block '$inner_loop_break
                                            (_loop '$inner_loop
                                                   (br_if '$inner_loop_break (i32.eqz (local.get '$len)))
                                                   (_if '$found_it
                                                        ; We should intern symbols so we can do this
                                                        ;(i64.eq (local.get '$it) (i64.load (local.get '$ptr)))
                                                        (i64.eq (i64.const 1)
                                                                (call '$str_sym_comp (local.get '$it) (i64.load (local.get '$ptr)) (i64.const 0) (i64.const 1) (i64.const 0)))
                                                        (then
                                                          (local.set '$res (call '$dup (i64.load (i32.add (i32.wrap_i64 (i64.and (i64.load 8 (local.get '$env_ptr)) (i64.const -8)))
                                                                                                          (i32.shl (local.get '$i) (i32.const 3))))))
                                                          (br '$outer_loop_break)
                                                        )
                                                   )
                                                   (local.set '$len (i32.sub (local.get '$len) (i32.const 1)))
                                                   (local.set '$ptr (i32.add (local.get '$ptr) (i32.const 8)))
                                                   (local.set '$i   (i32.add (local.get '$i)   (i32.const 1)))
                                                   (br '$inner_loop)
                                            )
                                      )
                                      ; try in upper
                                      (local.set '$current_env (i64.load 16 (local.get '$env_ptr)))
                                      (br_if '$outer_loop (i64.ne (i64.const nil_val) (local.get '$current_env)))
                                )
                                ; Ended at upper case
                                (call '$print (i64.const hit_upper_in_eval_msg_val))
                                (call '$print (local.get '$it))
                                (call '$print (i64.const newline_msg_val))
                                (local.set '$res (call (+ 2 func_idx) (call '$array1_alloc (call '$dup (local.get '$it))) (call '$dup (local.get '$env)) (i64.const nil_val)))
                          )
                          (local.get '$res)
                        )
                        (else
                          ;  <array_size32><array_ptr29>101 / 0..0 101
                          (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$it) (i64.const 32))))
                          (local.set '$ptr (i32.wrap_i64 (i64.and   (local.get '$it) (i64.const -8))))
                          (_if '$zero_length
                               (i32.eqz (local.get '$len))
                               (then (call '$print (i64.const k_call_zero_len_msg_val))
                                     (unreachable)))
                          ; its a call, evaluate combiner first then
                          (local.set '$comb (call '$eval_helper (i64.load 0 (local.get '$ptr)) (local.get '$env)))
                          ; check to make sure it's a combiner <func_idx29>|<env_ptr29><usesde1><wrap1>0001
                          (_if '$isnt_function
                                (i64.ne (i64.const #b0001) (i64.and (i64.const #b1111) (local.get '$comb)))
                                (then (call '$print (i64.const k_call_not_a_function_msg_val))
                                      (call '$print (i64.shl (local.get '$comb) (i64.const 1)))
                                      (call '$print (local.get '$comb))
                                      (unreachable)))
                          (local.set '$wrap (i32.wrap_i64 (i64.and (i64.const #b1) (i64.shr_u (local.get '$comb) (i64.const 4)))))
                          (local.set '$params (call '$slice_impl (call '$dup (local.get '$it)) (i32.const 1) (local.get '$len)))
                          ; we'll reuse len and ptr now for params
                          (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$params) (i64.const 32))))
                          (local.set '$ptr (i32.wrap_i64 (i64.and   (local.get '$params) (i64.const -8))))
                          ; then evaluate parameters wrap times (only 0 or 1 right now)
                          (block '$wrap_loop_break
                                 (_loop '$wrap_loop
                                        (br_if '$wrap_loop_break (i32.eqz (local.get '$wrap)))

                                        (local.set '$i   (i32.const 0))
                                        (block '$inner_eval_loop_break
                                               (_loop '$inner_eval_loop
                                                      (br_if '$inner_eval_loop_break (i32.eq (local.get '$len) (local.get '$i)))

                                                      (local.set '$tmp_ptr (i32.add (local.get '$ptr) (i32.shl (local.get '$i) (i32.const 3))))
                                                      (local.set '$tmp (call '$eval_helper (i64.load (local.get '$tmp_ptr)) (local.get '$env)))
                                                      (call '$drop (i64.load (local.get '$tmp_ptr)))
                                                      (i64.store (local.get '$tmp_ptr) (local.get '$tmp))

                                                      (local.set '$i   (i32.add (local.get '$i)   (i32.const 1)))
                                                      (br '$inner_eval_loop)
                                               )
                                        )
                                        (local.set '$wrap (i32.sub (local.get '$wrap) (i32.const 1)))
                                        (br '$wrap_loop)
                                 )
                          )
                          (front_half_stack_code (call '$dup (local.get '$it)) (call '$dup (local.get '$env)))
                          ; Also, this really should tail-call when we support it
                          (call_indirect
                                ;type
                                k_wrap
                                ;table
                                0
                                ;params
                                (local.get '$params)
                                ; dynamic env
                                (_if '$needs_dynamic_env '(result i64)
                                     (i64.ne (i64.const #b0) (i64.and (local.get '$comb) (i64.const #b100000)))
                                     (then (call '$dup (local.get '$env)))
                                     (else (i64.const nil_val)))
                                ; static env
                                (i64.or (i64.shl (i64.and (local.get '$comb) (i64.const #x3FFFFFFC0))
                                                 (i64.const 2)) (i64.const #b01001))
                                ;func_idx
                                (i32.wrap_i64 (i64.shr_u (local.get '$comb) (i64.const 35)))
                          )
                          back_half_stack_code
                        )
                   )
                 )
            )

              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_eval_loc k_eval_length datasi) (alloc_data "k_eval" datasi))
              (k_eval_msg_val (bor (<< k_eval_length 32) k_eval_loc #b011))
              ((k_eval          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$eval          '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $len i32) '(local $ptr i32)

                  (ensure_not_op_n_params_set_ptr_len i32.lt_u 1)
                  (_if '$using_d_env '(result i64)
                        (i32.eq (i32.const 1) (local.get '$len))
                        (then
                            (call '$eval_helper (i64.load 0 (local.get '$ptr)) (local.get '$d))
                        )
                        (else
                            (type_assert 1 type_env k_eval_msg_val)
                            (call '$eval_helper (i64.load 0 (local.get '$ptr)) (i64.load 8 (local.get '$ptr)))
                        )
                  )
                  drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_debug_parameters_loc k_debug_parameters_length datasi) (alloc_data "parameters to debug were " datasi))
              (k_debug_parameters_msg_val (bor (<< k_debug_parameters_length 32) k_debug_parameters_loc #b011))

              ((k_debug_prompt_loc k_debug_prompt_length datasi) (alloc_data "debug_prompt > " datasi))
              (k_debug_prompt_msg_val (bor (<< k_debug_prompt_length 32) k_debug_prompt_loc #b011))

              ((k_debug_exit_loc k_debug_exit_length datasi) (alloc_data "exit" datasi))
               (k_debug_exit_msg_val (bor (<< k_debug_exit_length 32) k_debug_exit_loc #b011))

              ((k_debug_abort_loc k_debug_abort_length datasi) (alloc_data "abort\n" datasi))
               (k_debug_abort_msg_val (bor (<< k_debug_abort_length 32) k_debug_abort_loc #b011))

              ((k_debug_redebug_loc k_debug_redebug_length datasi) (alloc_data "redebug\n" datasi))
               (k_debug_redebug_msg_val (bor (<< k_debug_redebug_length 32) k_debug_redebug_loc #b011))

              ((k_debug_print_st_loc k_debug_print_st_length datasi) (alloc_data "print_st\n" datasi))
               (k_debug_print_st_msg_val (bor (<< k_debug_print_st_length 32) k_debug_print_st_loc #b011))

              ((k_debug_help_loc k_debug_help_length datasi) (alloc_data "help\n" datasi))
               (k_debug_help_msg_val (bor (<< k_debug_help_length 32) k_debug_help_loc #b011))

              ((k_debug_help_info_loc k_debug_help_info_length datasi) (alloc_data "commands: help, print_st, print_envs, print_all, redebug, or (exit <exit value>)\n" datasi))
               (k_debug_help_info_msg_val (bor (<< k_debug_help_info_length 32) k_debug_help_info_loc #b011))

              ((k_debug_print_envs_loc k_debug_print_envs_length datasi) (alloc_data "print_envs\n" datasi))
               (k_debug_print_envs_msg_val (bor (<< k_debug_print_envs_length 32) k_debug_print_envs_loc #b011))

              ((k_debug_print_all_loc k_debug_print_all_length datasi) (alloc_data "print_all\n" datasi))
               (k_debug_print_all_msg_val (bor (<< k_debug_print_all_length 32) k_debug_print_all_loc #b011))

              ((k_debug_loc k_debug_length datasi) (alloc_data "k_debug" datasi))
              (k_debug_msg_val (bor (<< k_debug_length 32) k_debug_loc #b011))
              ((k_debug           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$debug           '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $len i32) '(local $buf i32) '(local $str i64) '(local $tmp_read i64) '(local $tmp_evaled i64) '(local $to_ret i64) '(local $tmp_ptr i32)
                (global.set '$debug_depth  (i32.add (global.get '$debug_depth) (i32.const 1)))
                (call '$print (i64.const k_debug_parameters_msg_val))
                (call '$print (local.get '$p))
                (call '$print (i64.const newline_msg_val))

                (block '$varadic_loop_exit
                    (_loop '$varadic_loop
                          (call '$print (i64.const k_debug_prompt_msg_val))

                          (local.set '$len (i32.const 100))
                          (i32.store 4 (i32.const iov_tmp) (local.get '$len))
                          (i32.store 0 (i32.const iov_tmp) (local.tee '$buf (call '$malloc (local.get '$len))))
                          (_drop (call '$fd_read
                                (i32.const 0)               ;; file descriptor
                                (i32.const iov_tmp)         ;; *iovs
                                (i32.const 1)               ;; iovs_len
                                (i32.const (+ 8 iov_tmp))   ;; nwritten
                          ))

                          (local.set '$str (i64.or (i64.shl (i64.extend_i32_u (i32.load 8 (i32.const iov_tmp))) (i64.const 32))
                                                   (i64.extend_i32_u (i32.or (local.get '$buf) (i32.const #b011)))))

                          (local.set '$tmp_evaled (i64.const 0))
                          (_if '$print_help (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_help_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                 (call '$print (i64.const k_debug_help_info_msg_val))
                                 (call '$drop  (local.get '$str))
                                 (br '$varadic_loop)
                               )
                          )
                          (_if '$print_st (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_print_st_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                 (local.set '$tmp_read (global.get '$stack_trace))
                                 (block '$print_loop_exit
                                        (_loop '$print_loop
                                               (br_if '$print_loop_exit (i64.eq (i64.const nil_val) (local.get '$tmp_read)))
                                               (call '$print (local.get '$tmp_evaled))
                                               (local.set '$tmp_evaled (i64.add (local.get '$tmp_evaled) (i64.const 2)))
                                               (call '$print (i64.const space_msg_val))
                                               (call '$print (i64.load 0 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (call '$print (i64.const newline_msg_val))
                                               (local.set '$tmp_read (i64.load 16 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (br '$print_loop)
                                        )
                                 )
                                 (call '$drop  (local.get '$str))
                                 (br '$varadic_loop)
                               )
                          )
                          (_if '$print_envs (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_print_envs_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                 (local.set '$tmp_read (global.get '$stack_trace))
                                 (block '$print_loop_exit
                                        (_loop '$print_loop
                                               (br_if '$print_loop_exit (i64.eq (i64.const nil_val) (local.get '$tmp_read)))
                                               (call '$print (local.get '$tmp_evaled))
                                               (call '$print (i64.const space_msg_val))
                                               (call '$print (i64.load 8 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (local.set '$tmp_evaled (i64.add (local.get '$tmp_evaled) (i64.const 2)))
                                               (call '$print (i64.const newline_msg_val))
                                               (local.set '$tmp_read (i64.load 16 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (br '$print_loop)
                                        )
                                 )
                                 (call '$drop  (local.get '$str))
                                 (br '$varadic_loop)
                               )
                          )
                          (_if '$print_all (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_print_all_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                 (local.set '$tmp_read (global.get '$stack_trace))
                                 (block '$print_loop_exit
                                        (_loop '$print_loop
                                               (br_if '$print_loop_exit (i64.eq (i64.const nil_val) (local.get '$tmp_read)))
                                               (call '$print (local.get '$tmp_evaled))
                                               (local.set '$tmp_evaled (i64.add (local.get '$tmp_evaled) (i64.const 2)))
                                               (call '$print (i64.const space_msg_val))
                                               (call '$print (i64.load 0 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (call '$print (i64.const space_msg_val))
                                               (call '$print (i64.load 8 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (call '$print (i64.const newline_msg_val))
                                               (local.set '$tmp_read (i64.load 16 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))))
                                               (br '$print_loop)
                                        )
                                 )
                                 (call '$drop  (local.get '$str))
                                 (br '$varadic_loop)
                               )
                          )
                          (_if '$abort (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_abort_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                    (call '$drop  (local.get '$str))
                                    (unreachable)
                               )
                          )

                          (_if '$redebug (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_redebug_msg_val) (local.get '$str) (i64.const 0) (i64.const 1) (i64.const 0)))
                               (then
                                 (call '$drop  (local.get '$str))
                                 (global.get '$debug_func_to_call)
                                 (global.get '$debug_params_to_call)
                                 (global.get '$debug_env_to_call)

                                 (local.set '$tmp_evaled (call_indirect
                                     ;type
                                     k_log
                                     ;table
                                     0
                                     ;params
                                     (call '$dup (global.get '$debug_params_to_call))
                                     ;top_env
                                     (call '$dup (global.get '$debug_env_to_call))
                                     ; static env
                                     (i64.or (i64.shl (i64.and (call '$dup (global.get '$debug_func_to_call)) (i64.const #x3FFFFFFC0)) (i64.const 2)) (i64.const #b01001))
                                     ;func_idx
                                     (i32.wrap_i64 (i64.shr_u (global.get '$debug_func_to_call) (i64.const 35)))
                                 ))

                                 (call '$print (local.get '$tmp_evaled))
                                 (call '$drop (local.get '$tmp_evaled))
                                 (call '$print (i64.const newline_msg_val))

                                 (global.set '$debug_env_to_call)
                                 (global.set '$debug_params_to_call)
                                 (global.set '$debug_func_to_call)
                                 (br '$varadic_loop)
                               )
                          )



                          (local.set '$tmp_read (call '$read-string (call '$array1_alloc (local.get '$str)) (i64.const nil_val) (i64.const nil_val)))
                          (_if '$arr (i64.eq (i64.const #b101) (i64.and (local.get '$tmp_read) (i64.const #b111)))
                               (then
                                (_if '$arr (i64.ge_u (i64.const 2) (i64.shr_u (local.get '$tmp_read) (i64.const 32)))
                                     (then
                                         (_if '$exit (i64.eq (i64.const 1) (call '$str_sym_comp (i64.const k_debug_exit_msg_val)
                                                                                                (i64.load 0 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8))))
                                                                                                (i64.const 0) (i64.const 1) (i64.const 0)))
                                              (then
                                                (local.set '$to_ret (call '$eval_helper (i64.load 8 (i32.wrap_i64 (i64.and (local.get '$tmp_read) (i64.const -8)))) (local.get '$d)))
                                                (call '$drop (local.get '$tmp_read))
                                                (br '$varadic_loop_exit)
                                              )
                                         )
                                     )
                                )
                               )
                          )
                          (local.set '$tmp_evaled (call '$eval_helper (local.get '$tmp_read) (local.get '$d)))
                          (call '$print (local.get '$tmp_evaled))
                          (call '$drop (local.get '$tmp_read))
                          (call '$drop (local.get '$tmp_evaled))
                          (call '$print (i64.const newline_msg_val))
                          (br '$varadic_loop)
                    )
                )
                (global.set '$debug_depth  (i32.sub (global.get '$debug_depth) (i32.const 1)))
                drop_p_d
                (local.get '$to_ret)
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_vau_helper_loc k_vau_helper_length datasi) (alloc_data "k_vau_helper" datasi))
              (k_vau_helper_msg_val (bor (<< k_vau_helper_length 32) k_vau_helper_loc #b011))
              ((k_vau_helper          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$vau_helper '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $len i32) '(local $ptr i32) '(local $i_se i64) '(local $i_des i64) '(local $i_params i64) '(local $i_is_varadic i64) '(local $min_num_params i32) '(local $i_body i64) '(local $new_env i64)

                  ; get env ptr
                  (local.set '$ptr (i32.wrap_i64 (i64.shr_u (local.get '$s) (i64.const 5))))
                  ; get value array ptr
                  (local.set '$ptr (i32.wrap_i64 (i64.and (i64.load 8 (local.get '$ptr)) (i64.const -8))))


                  (local.set '$i_se             (call '$dup (i64.load  0 (local.get '$ptr))))
                  (local.set '$i_des                        (i64.load  8 (local.get '$ptr)))
                  (local.set '$i_params         (call '$dup (i64.load 16 (local.get '$ptr))))
                  (local.set '$i_is_varadic                 (i64.load 24 (local.get '$ptr)))
                  (local.set '$i_body           (call '$dup (i64.load 32 (local.get '$ptr))))


                  ; reusing len for i_params
                  (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$i_params) (i64.const 32))))
                  (local.set '$ptr (i32.wrap_i64 (i64.and   (local.get '$i_params) (i64.const -8))))


                  ; each branch consumes i_params, p, d, and i_se
                  (_if '$varadic
                        (i64.eq (local.get '$i_is_varadic) (i64.const true_val))
                        (then
                            (_if '$using_d_env
                                  (i64.ne (local.get '$i_des) (i64.const nil_val))
                                  (then
                                      (local.set '$min_num_params (i32.sub (local.get '$len) (i32.const 2)))
                                      (_if '$wrong_no_params
                                           ; with both de and varadic, needed params is at least two less than the length of our params
                                           (i32.lt_u (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))) (local.get '$min_num_params))
                                           (then (call '$print (i64.const bad_params_number_msg_val))
                                                 (unreachable)))

                                           (local.set '$new_env (call '$env_alloc
                                                                       (local.get '$i_params)
                                                                       (call '$concat (call '$array3_alloc (call '$slice_impl (call '$dup (local.get '$p))
                                                                                                                              (i32.const 0)
                                                                                                                              (local.get '$min_num_params))
                                                                                                           (call '$array1_alloc (call '$slice_impl (local.get '$p)
                                                                                                                                                   (local.get '$min_num_params)
                                                                                                                                                   (i32.const -1)))
                                                                                                           (call '$array1_alloc (local.get '$d)))
                                                                                      (i64.const nil_val)
                                                                                      (i64.const nil_val))
                                                                       (local.get '$i_se)))
                                  )
                                  (else
                                      (local.set '$min_num_params (i32.sub (local.get '$len) (i32.const 1)))
                                      (_if '$wrong_no_params
                                           (i32.lt_u (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))) (local.get '$min_num_params))
                                           (then (call '$print (i64.const bad_params_number_msg_val))
                                                 (unreachable)))

                                           (local.set '$new_env (call '$env_alloc
                                                                       (local.get '$i_params)
                                                                       (call '$concat (call '$array2_alloc (call '$slice_impl (call '$dup (local.get '$p))
                                                                                                                              (i32.const 0)
                                                                                                                              (local.get '$min_num_params))
                                                                                                           (call '$array1_alloc (call '$slice_impl (local.get '$p)
                                                                                                                                                   (local.get '$min_num_params)
                                                                                                                                                   (i32.const -1))))
                                                                                      (i64.const nil_val)
                                                                                      (i64.const nil_val))
                                                                       (local.get '$i_se)))
                                           (call '$drop (local.get '$d))
                                  )
                            )
                        )
                        (else
                            (_if '$using_d_env
                                  (i64.ne (local.get '$i_des) (i64.const nil_val))
                                  (then
                                      (local.set '$min_num_params (i32.sub (local.get '$len) (i32.const 1)))
                                      (_if '$wrong_no_params
                                           (i32.ne (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))) (local.get '$min_num_params))
                                           (then (call '$print (i64.const bad_params_number_msg_val))
                                                 (unreachable)))

                                           (local.set '$new_env (call '$env_alloc
                                                                       (local.get '$i_params)
                                                                       (call '$concat (call '$array2_alloc (local.get '$p)
                                                                                                           (call '$array1_alloc (local.get '$d)))
                                                                                      (i64.const nil_val)
                                                                                      (i64.const nil_val))
                                                                       (local.get '$i_se)))
                                  )
                                  (else
                                      (local.set '$min_num_params (local.get '$len))
                                      (_if '$wrong_no_params
                                           (i32.ne (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))) (local.get '$min_num_params))
                                           (then (call '$print (i64.const bad_params_number_msg_val))
                                                 (unreachable)))

                                           (local.set '$new_env (call '$env_alloc
                                                                       (local.get '$i_params)
                                                                       (local.get '$p)
                                                                       (local.get '$i_se)))
                                           (call '$drop (local.get '$d))
                                  )
                            )
                        )
                  )

                  (call '$eval_helper (local.get '$i_body) (local.get '$new_env))

                  (call '$drop (local.get '$i_body))
                  (call '$drop (local.get '$new_env))
                  (call '$drop (local.get '$s))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              ((k_env_symbol_loc k_env_symbol_length datasi) (alloc_data "env_symbol" datasi))
              (k_env_symbol_val (bor (<< k_env_symbol_length 32) k_env_symbol_loc #b111))

              ((k_des_symbol_loc k_des_symbol_length datasi) (alloc_data "des_symbol" datasi))
              (k_des_symbol_val (bor (<< k_des_symbol_length 32) k_des_symbol_loc #b111))

              ((k_param_symbol_loc k_param_symbol_length datasi) (alloc_data "param_symbol" datasi))
              (k_param_symbol_val (bor (<< k_param_symbol_length 32) k_param_symbol_loc #b111))

              ((k_varadic_symbol_loc k_varadic_symbol_length datasi) (alloc_data "varadic_symbol" datasi))
              (k_varadic_symbol_val (bor (<< k_varadic_symbol_length 32) k_varadic_symbol_loc #b111))

              ((k_body_symbol_loc k_body_symbol_length datasi) (alloc_data "body_symbol" datasi))
               (k_body_symbol_val (bor (<< k_body_symbol_length 32) k_body_symbol_loc #b111))

              ((k_and_symbol_loc k_and_symbol_length datasi) (alloc_data "&" datasi))
               (k_and_symbol_val (bor (<< k_and_symbol_length 32) k_and_symbol_loc #b111))

              ((k_env_dparam_body_array_loc k_env_dparam_body_array_len datasi) (alloc_data (concat (i64_le_hexify k_env_symbol_val)
                                                                                                    (i64_le_hexify k_des_symbol_val)
                                                                                                    (i64_le_hexify k_param_symbol_val)
                                                                                                    (i64_le_hexify k_varadic_symbol_val)
                                                                                                    (i64_le_hexify k_body_symbol_val)
                                                                                            ) datasi))
              (k_env_dparam_body_array_val (bor (<< 5 32) k_env_dparam_body_array_loc #b101))


              ((k_vau_loc k_vau_length datasi) (alloc_data "k_vau" datasi))
              (k_vau_msg_val (bor (<< k_vau_length 32) k_vau_loc #b011))
              ((k_vau           func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$vau           '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $len i32) '(local $ptr i32) '(local $i i32) '(local $des i64) '(local $params i64) '(local $is_varadic i64) '(local $body i64) '(local $tmp i64)

                (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$p) (i64.const 32))))
                (local.set '$ptr (i32.wrap_i64 (i64.and   (local.get '$p) (i64.const -8))))

                (_if '$using_d_env
                      (i32.eq (i32.const 3) (local.get '$len))
                      (then
                          (local.set '$des      (call '$dup (i64.load 0  (local.get '$ptr))))
                          (local.set '$params   (call '$dup (i64.load 8  (local.get '$ptr))))
                          (local.set '$body     (call '$dup (i64.load 16 (local.get '$ptr))))
                          )
                      (else
                          (local.set '$des      (i64.const nil_val))
                          (local.set '$params   (call '$dup (i64.load 0  (local.get '$ptr))))
                          (local.set '$body     (call '$dup (i64.load 8  (local.get '$ptr))))
                      )
                )

                (local.set '$is_varadic   (i64.const false_val))
                (local.set '$len (i32.wrap_i64 (i64.shr_u (local.get '$params) (i64.const 32))))
                (local.set '$ptr (i32.wrap_i64 (i64.and   (local.get '$params) (i64.const -8))))
                (local.set '$i (i32.const 0))
                (block '$varadic_break
                    (_loop '$varadic_loop
                        (br_if '$varadic_break (i32.eq (local.get '$i) (local.get '$len)))
                        (_if 'this_varadic
                             (i64.eq (i64.const 1)
                                     (call '$str_sym_comp (i64.const k_and_symbol_val) (i64.load (local.get '$ptr)) (i64.const 0) (i64.const 1) (i64.const 0)))
                             (then
                                 (local.set '$is_varadic (i64.const true_val))

                                 (local.set '$tmp (call '$array1_alloc (call '$dup (i64.load 8 (local.get '$ptr)))))
                                 (local.set '$params (call '$concat (call '$array2_alloc (call '$slice_impl (local.get '$params) (i32.const 0) (local.get '$i))
                                                                                         (local.get '$tmp))
                                                                    (i64.const nil_val)
                                                                    (i64.const nil_val)))

                                 (br '$varadic_break)
                             )
                        )
                        (local.set '$ptr (i32.add (local.get '$ptr) (i32.const 8)))
                        (local.set '$i   (i32.add (local.get '$i)   (i32.const 1)))
                        (br '$varadic_loop)
                    )
                )
                (_if '$using_d_env
                      (i64.ne (local.get '$des) (i64.const nil_val))
                      (then
                           (local.set '$params (call '$concat (call '$array2_alloc (local.get '$params) (call '$array1_alloc (call '$dup (local.get '$des))))
                                                     (i64.const nil_val)
                                                     (i64.const nil_val)))
                      )
                )

                ;  <func_idx29>|<env_ptr29><usesde1><wrap1>0001
                (i64.or (i64.or (i64.or (i64.const (<< (- k_vau_helper dyn_start) 35))
                                        (i64.and (i64.shr_u (call '$env_alloc (i64.const k_env_dparam_body_array_val)
                                                                              (call '$array5_alloc (local.get '$d)
                                                                                                   (local.get '$des)
                                                                                                   (local.get '$params)
                                                                                                   (local.get '$is_varadic)
                                                                                                   (local.get '$body))
                                                                              (i64.const nil_val))
                                                            (i64.const 2)) ;env looks like 0..0<env_ptr32 but still aligned>01001
                                                 (i64.const -64)))

                                (_if '$using_d_env '(result i64)
                                     (i64.ne (local.get '$des) (i64.const nil_val))
                                     (then (i64.const #b100000))
                                     (else (i64.const #b000000))))
                         (i64.const #b0001))
                (call '$drop (local.get '$p))
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))
              ((k_cond_loc k_cond_length datasi) (alloc_data "k_cond" datasi))
              (k_cond_msg_val (bor (<< k_cond_length 32) k_cond_loc #b011))
              ((k_cond          func_idx funcs) (array func_idx (+ 1 func_idx) (concat funcs (func '$cond          '(param $p i64) '(param $d i64) '(param $s i64) '(result i64) '(local $len i32) '(local $ptr i32) '(local $tmp i64)
                  set_len_ptr
                  ;yall
                  (block '$cond_loop_break_ok
                      (block '$cond_loop_break_err
                          (_loop '$cond_loop
                              (br_if '$cond_loop_break_err (i32.le_s (local.get '$len) (i32.const 1)))

                              (local.set '$tmp (call '$eval_helper (i64.load 0 (local.get '$ptr)) (local.get '$d)))
                              (_if 'cond_truthy
                                   (truthy_test (local.get '$tmp))
                                   (then
                                       (call '$drop (local.get '$tmp))
                                       (local.set '$tmp (call '$eval_helper (i64.load 8 (local.get '$ptr)) (local.get '$d)))
                                       (br '$cond_loop_break_ok)
                                   )
                                   (else (call '$drop (local.get '$tmp)))
                              )
                              (local.set '$len (i32.sub (local.get '$len) (i32.const 2)))
                              (local.set '$ptr (i32.add (local.get '$ptr) (i32.const 16)))
                              (br '$cond_loop)
                          )
                      )
                      (call '$print (i64.const no_true_cond_msg_val))
                      (unreachable)
                  )
                  (local.get '$tmp)
                  drop_p_d
              ))))
              ((func_idx funcs) (array (+ 1 func_idx) (concat funcs (func '$dummy '(result i64) (i64.const 0)))))

              (get_passthrough (dlambda (hash (datasi funcs memo env pectx)) (dlet ((r (get-value-or-false memo hash)))
                                                                     (if r (array r nil nil (array datasi funcs memo env pectx)) #f))))

                                    ;  <func_idx29>|<env_ptr29><usesde1><wrap1>0001
              (mod_fval_to_wrap (lambda (it) (cond ((= nil it)                                                      it)
                                                   ;((and (= (band it #b1111) #b0001) (= #b0 (band (>> it 35) #b1))) (- it (<< 1 35)))
                                                   ((and (= (band it #b1111) #b0001) (= #b0 (band (>> it 35) #b1))) (dlet ( (r (- it (<< 1 35)))
                                                                                                                            (_ (true_print "changing " it " to " r ", that is " (>> it 35) " to " (>> r 35)))
                                                                                                                           ) r) )
                                                   (true                                                            it))))

              ; This is the second run at this, and is a little interesting
              ; It can return a value OR code OR an error string. An error string should be propegated,
              ; unless it was expected as a possiblity, which can happen when compling a call that may or
              ; may not be a Vau. When it recurses, if the thing you're currently compiling could be a value
              ; but your recursive calls return code, you will likely have to swap back to code.

              ; ctx is (datasi funcs memo env pectx)
              ; return is (value? code? error? (datasi funcs memo env pectx))
              (compile-inner (rec-lambda compile-inner (ctx c need_value inside_veval s_env_access_code) (cond
                    ((val? c)   (dlet ((v (.val c)))
                                     (cond ((int? v)    (array  (<< v 1) nil nil ctx))
                                           ((= true v)  (array  true_val nil nil ctx))
                                           ((= false v) (array false_val nil nil ctx))
                                           ((str? v)    (or (get_passthrough (.hash c) ctx)
                                                            (dlet ( ((datasi funcs memo env pectx) ctx)
                                                                    ((c_loc c_len datasi) (alloc_data v datasi))
                                                                    (a (bor (<< c_len 32) c_loc #b011))
                                                                    (memo (put memo (.hash c) a))
                                                                  ) (array a nil nil (array datasi funcs memo env pectx)))))
                                           (true        (error (str "Can't compile impossible value " v))))))
                    ((marked_symbol? c) (cond ((.marked_symbol_is_val c) (or (get_passthrough (.hash c) ctx)
                                                                             (dlet ( ((datasi funcs memo env pectx) ctx)
                                                                                     ((c_loc c_len datasi) (alloc_data (get-text (.marked_symbol_value c))  datasi))
                                                                                     (result (bor (<< c_len 32) c_loc #b111))
                                                                                     (memo (put memo (.hash c) result))
                                                                                   ) (array result nil nil (array datasi funcs memo env pectx)))))



                                              (true (dlet ( ((datasi funcs memo env pectx) ctx)
                                                            ; not a recoverable error, so just do here
                                                            (_ (if (= nil env) (error "nil env when trying to compile a non-value symbol")))
                                                            (lookup_helper (rec-lambda lookup-recurse (dict key i code level) (cond
                                                                ((and (= i (- (len dict) 1)) (= nil (idx dict i))) (array nil (str "for code-symbol lookup, couldn't find " key)))
                                                                ((= i (- (len dict) 1))                            (lookup-recurse (.env_marked (idx dict i)) key 0 (i64.load 16 (i32.wrap_i64 (i64.shr_u code ;(call '$print (i64.const going_up_msg_val))
                                                                                                                                                                                                               (i64.const 5)))) (+ level 1)))
                                                                ((= key (idx (idx dict i) 0))                      (if (and (not inside_veval) (= 0 level)) (array (local.get key) nil)
                                                                                                                       (array (i64.load (* 8 i)                                               ; offset in array to value
                                                                                                                        (i32.wrap_i64 (i64.and (i64.const -8)                       ; get ptr from array value
                                                                                                                                               (i64.load 8 (i32.wrap_i64 (i64.shr_u code
                                                                                                                                                             (i64.const 5)) ;(call '$print (i64.const got_it_msg_val))
                                                                                                                                                                         ))))) nil)))
                                                                (true                                              (lookup-recurse dict key (+ i 1) code level)))))


                                                            ((val err) (lookup_helper (.env_marked env) (.marked_symbol_value c) 0 s_env_access_code 0))
                                                            (err (mif err (str "got " err ", started searching in " (str_strip env)) (if need_value (str "needed value, but non val symbol " (.marked_symbol_value c)) nil)))
                                                            (result (mif val (call '$dup val)))
                                                          ) (array nil result err (array datasi funcs memo env pectx))))))
                   ((marked_array? c) (if (.marked_array_is_val c) (or (get_passthrough (.hash c) ctx)
                                                                       (dlet ((actual_len (len (.marked_array_values c))))
                                                                         (if (= 0 actual_len) (array nil_val nil nil ctx)
                                                                             (dlet ( ((comp_values err ctx) (foldr (dlambda (x (a err ctx)) (dlet (((v c e ctx) (compile-inner ctx x need_value inside_veval s_env_access_code)))
                                                                                                                                         (array (cons (mod_fval_to_wrap v) a) (or (mif err err false) (mif e e false) (mif c (str "got code " c) false)) ctx))) (array (array) nil ctx) (.marked_array_values c)))
                                                                                   ) (mif err (array nil nil (str err ", from an array value compile " (str_strip c)) ctx) (dlet (
                                                                                    ((datasi funcs memo env pectx) ctx)
                                                                                    ((c_loc c_len datasi) (alloc_data (apply concat (map i64_le_hexify comp_values)) datasi))
                                                                                    (result (bor (<< actual_len 32) c_loc #b101))
                                                                                    (memo (put memo (.hash c) result))
                                                                                   ) (array result nil nil (array datasi funcs memo env pectx))))))))

                                                        ; This is the other half of where we notice & tie up recursion based on partial eval's noted rec-stops
                                                        ; Other half is below in comb compilation
                                                        (or (and (!= nil (.marked_array_this_rec_stop c)) (get_passthrough (idx (.marked_array_this_rec_stop c) 0) ctx))
                                                        (if need_value                            (array nil nil (str "errr, needed value and was call " (str_strip c)) ctx)
                                                        (if (= 0 (len (.marked_array_values c)))  (array nil nil (str "errr, empty call array" (str_strip c)) ctx)
                                                                       (dlet (

                                                        ; This can weirdly cause infinate recursion on the compile side, if partial_eval
                                                        ; returns something that, when compiled, will cause partial eval to return that thing again.
                                                        ; Partial eval won't recurse infinately, since it has memo, but it can return something of that
                                                        ; shape in that case which will cause compile to keep stepping.

                                                        ((datasi funcs memo env pectx) ctx)
                                                        (hit_recursion (= 'RECURSE_FAIL (get-value-or-false memo (.hash c))))
                                                        ;(_ (true_print "hit recursion? " hit_recursion))

                                                        (compile_params (lambda (unval_and_eval ctx params)
                                                                (foldr (dlambda (x (a err ctx)) (dlet (

                                                                    ((datasi funcs memo env pectx) ctx)
                                                                    ((x err ctx) (mif err                 (array nil err ctx)
                                                                                 (if (not unval_and_eval) (array x err ctx)
                                                                                                          (dlet (
                                                                                                            ((ok x) (try_unval x (lambda (_) nil)))
                                                                                                            (err (if (not ok) "couldn't unval in compile" err))

                                                                                                    ; TODO: This might fail because we don't have the real env stack, which we *should*!
                                                                                                    ; In the mean time, if it does, just fall back to the non-more-evaled ones.
                                                                                                            ((pectx e pex) (if (or (!= nil err) hit_recursion)
                                                                                                                               (array pectx err nil)
                                                                                                                               (partial_eval_helper x false env (array nil nil) pectx 1 false)))

                                                                                                            (ctx (array datasi funcs memo env pectx))

                                                                                                          ) (array (mif e x pex) err ctx)))))
                                                                    ((datasi funcs memo env pectx) ctx)
                                                                    (memo (put memo (.hash c) 'RECURSE_FAIL))
                                                                    (ctx (array datasi funcs memo env pectx))
                                                                    ((val code err ctx) (mif err (array nil nil err ctx)
                                                                                                 (compile-inner ctx x false inside_veval s_env_access_code)))
                                                                    ((datasi funcs memo env pectx) ctx)
                                                                    (memo (put memo (.hash c) 'RECURSE_OK))
                                                                    (ctx (array datasi funcs memo env pectx))
                                                                    ) (array (cons (mif val (i64.const (mod_fval_to_wrap val)) code) a) err ctx)))

                                                                (array (array) nil ctx) params)))

                                                        (func_param_values (.marked_array_values c))
                                                        (num_params (- (len func_param_values) 1))
                                                        (params (slice func_param_values 1 -1))
                                                        (func_value (idx func_param_values 0))

                                                        (wrap_level (if (or (comb? func_value) (prim_comb? func_value)) (.any_comb_wrap_level func_value) nil))
                                                        ; I don't think it makes any sense for a function literal to have wrap > 0
                                                        (_ (if (and (!= nil wrap_level) (> wrap_level 0)) (error "call to function literal has wrap >0")))

                                                        ;; Test for the function being a constant to inline
                                                        ;; Namely, vcond (also veval!)
                                                        (single_num_type_check (lambda (code) (concat (local.set '$type_check_tmp code)
                                                                                                      (_if '$not_num
                                                                                                           (i64.ne (i64.const 0) (i64.and (i64.const 1) (local.get '$type_check_tmp)))
                                                                                                           (then (unreachable))
                                                                                                      )
                                                                                                      (local.get '$type_check_tmp))))
                                                        (gen_numeric_impl (lambda (operation)
                                                                              (dlet (((param_codes err ctx) (compile_params false ctx params)))
                                                                                    (mif err (array nil nil (str err " from function params in call to comb " (str_strip c)) ctx)
                                                                                             (array nil (foldl (lambda (running_code val_code) (operation running_code
                                                                                                                                                          (single_num_type_check val_code)))
                                                                                                               (single_num_type_check (idx param_codes 0))
                                                                                                               (slice param_codes 1 -1)) nil ctx)))
                                                                              ))
                                                        ) (cond
                                                            ((and (prim_comb? func_value) (= (.prim_comb_sym func_value) 'veval)) (dlet (

                            (_ (if (!= 2 (len params)) (error "call to veval has != 2 params!")))
                            ((datasi funcs memo env pectx) ctx)
                            ((val code err (datasi funcs memo ienv pectx)) (compile-inner (array datasi funcs memo (idx params 1) pectx) (idx params 0) false true (local.get '$s_env)))
                            (ctx (array datasi funcs memo env pectx))
                            ; If it's actual code, we have to set and reset s_env
                            ((code env_err ctx) (mif code (dlet (
                                                              ((env_val env_code env_err ctx) (compile-inner ctx (idx params 1) false inside_veval s_env_access_code))
                                                              (full_code (concat (local.get '$s_env)
                                                                                 (local.set '$s_env (mif env_val (i64.const env_val) env_code))
                                                                                 code
                                                                                 (local.set '$tmp)
                                                                                 (local.set '$s_env)
                                                                                 (local.get '$tmp)))
                                                              ) (array full_code env_err ctx))
                                                           (array code nil ctx)))
                                                     ) (array (mod_fval_to_wrap val) code (mif err err env_err) ctx)))

                                                            ((and (prim_comb? func_value) (= (.prim_comb_sym func_value) 'vcond))
                                                                                                               (dlet (
                                                                                                                ((datasi funcs memo env pectx) ctx)
                                                                                                                ((param_codes err ctx) (compile_params false ctx params))
                                                                                                               )
                                                                                        (mif err (array nil nil (str err " from function params in call to comb " (str_strip c)) ctx)
                                                                                                                     (array nil ((rec-lambda recurse (codes i) (cond
                                                                                                                        ((< i (- (len codes) 1)) (_if '_cond_flat '(result i64)
                                                                                                                                                    (truthy_test (idx codes i))
                                                                                                                                                    (then (idx codes (+ i 1)))
                                                                                                                                                    (else (recurse codes (+ i 2)))
                                                                                                                                                ))
                                                                                                                        ((= i (- (len codes) 1)) (error "compiling bad length comb"))
                                                                                                                        (true                    (unreachable))
                                                                                                                    )) param_codes 0) err ctx))))


                                                            ((and (prim_comb? func_value) (= (.prim_comb_sym func_value) '+)) (gen_numeric_impl i64.add))
                                                            ((and (prim_comb? func_value) (= (.prim_comb_sym func_value) '-)) (gen_numeric_impl i64.sub))


                                                            (true (dlet (
                                                                ((param_codes first_params_err ctx) (compile_params false ctx params))
                                                                ((func_val func_code func_err ctx) (compile-inner ctx func_value false inside_veval s_env_access_code))

                                                                ((unval_param_codes err ctx) (compile_params true ctx params))
                                                                ((bad_unval_params_msg_val _ _ ctx) (compile-inner ctx (marked_val (str "error was with unval-evaling parameters of " (str_strip c))) true inside_veval s_env_access_code))
                                                                (wrap_param_code (lambda (code) (concat
                                                                                                    (local.get '$tmp) ; saving ito restore it
                                                                                                    code
                                                                                                    (local.set '$param_ptr (call '$malloc (i32.const (* 8 num_params))))
                                                                                                    (flat_map (lambda (i) (i64.store (* i 8) (local.set '$tmp) (local.get '$param_ptr) (local.get '$tmp)))
                                                                                                              (range (- num_params 1) -1))
                                                                                                    (local.set '$tmp) ; restoring tmp
                                                                                                  )))
                                                                (wrap_0_inner_code (apply concat param_codes))
                                                                (wrap_0_param_code (wrap_param_code wrap_0_inner_code))
                                                                (wrap_1_inner_code
                                                                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                   ; Since we're not sure if it's going to be a vau or not,
                                                                   ; this code might not be compilable, so we gracefully handle
                                                                   ; compiler errors and instead emit code that throws the error if this
                                                                   ; spot is ever reached at runtime.
                                                                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                                                   (mif err (concat (call '$print (i64.const bad_not_vau_msg_val))
                                                                                    (call '$print (i64.const bad_unval_params_msg_val))
                                                                                    (call '$print (i64.shl (local.get '$tmp) (i64.const 1)))
                                                                                    (unreachable))
                                                                            (apply concat unval_param_codes)))
                                                                (wrap_1_param_code (wrap_param_code wrap_1_inner_code))
                                                                (wrap_x_param_code (concat
                                                                                    ; TODO: Handle other wrap levels
                                                                                    (call '$print (i64.const weird_wrap_msg_val))
                                                                                    (unreachable)))

                                                                ((source_code ctx) (mif (.marked_array_source c) (dlet (((code _ _ ctx) (compile-inner ctx (.marked_array_source c) true inside_veval s_env_access_code))
                                                                                                                        ) (array code ctx))
                                                                                                                 (array k_cond_msg_val ctx)))
                                                                (result_code (mif func_val
                                                                              (concat
                                                                                (front_half_stack_code (i64.const source_code) (call '$dup s_env_access_code))
                                                                                (call (- (>> func_val 35) func_id_dynamic_ofset (- 0 num_pre_functions) 1)
                                                                                    ;params
                                                                                    (mif (= #b0 (band (>> func_val 35) #b1))
                                                                                         (concat
                                                                                               (dlet ((wrap_level (>> (band func_val #x10) 4)))
                                                                                                     (cond ((= 0 wrap_level) wrap_0_inner_code)
                                                                                                           ((= 1 wrap_level) wrap_1_inner_code)
                                                                                                           (true             wrap_x_param_code)))
                                                                                                ;dynamic env (is caller's static env)
                                                                                                ; hay, we can do this statically! the static version of the dynamic check
                                                                                                (mif (!= 0 (band func_val #b100000))
                                                                                                     (call '$dup s_env_access_code)
                                                                                                     (array))
                                                                                         )
                                                                                         (concat
                                                                                                (dlet ((wrap_level (>> (band func_val #x10) 4)))
                                                                                                      (cond ((= 0 wrap_level) wrap_0_param_code)
                                                                                                            ((= 1 wrap_level) wrap_1_param_code)
                                                                                                            (true             wrap_x_param_code)))
                                                                                                (i64.or (i64.extend_i32_u (local.get '$param_ptr))
                                                                                                        (i64.const (bor (<< num_params 32) #x5)))
                                                                                                ;dynamic env (is caller's static env)
                                                                                                ; hay, we can do this statically! the static version of the dynamic check
                                                                                                (mif (!= 0 (band func_val #b100000))
                                                                                                     (call '$dup s_env_access_code)
                                                                                                     (i64.const nil_val))
                                                                                          )
                                                                                    )
                                                                                    ; static env
                                                                                    (i64.const (bor (<< (band func_val #x3FFFFFFC0) 2) #b01001))
                                                                                )
                                                                                back_half_stack_code
                                                                              )
                                                                              (concat
                                                                                func_code
                                                                                (local.set '$tmp)
                                                                                (_if '$is_wrap_0
                                                                                    (i64.eq (i64.const #x00) (i64.and (local.get '$tmp) (i64.const #x30)))
                                                                                    (then wrap_0_param_code)
                                                                                    (else
                                                                                        (_if '$is_wrap_1
                                                                                            (i64.eq (i64.const #x10) (i64.and (local.get '$tmp) (i64.const #x10)))
                                                                                            (then wrap_1_param_code)
                                                                                            (else wrap_x_param_code)
                                                                                        )
                                                                                    )
                                                                                )
                                                                                (front_half_stack_code (i64.const source_code) (call '$dup s_env_access_code))
                                                                                (call_indirect
                                                                                    ;type
                                                                                    k_vau
                                                                                    ;table
                                                                                    0
                                                                                    ;params
                                                                                    (i64.or (i64.extend_i32_u (local.get '$param_ptr))
                                                                                            (i64.const (bor (<< num_params 32) #x5)))
                                                                                    ;dynamic env (is caller's static env)
                                                                                    (_if '$needs_dynamic_env '(result i64)
                                                                                         (i64.ne (i64.const #b0) (i64.and (local.get '$tmp) (i64.const #b100000)))
                                                                                         (then (call '$dup s_env_access_code))
                                                                                         (else (i64.const nil_val)))
                                                                                    ; static env
                                                                                    (i64.or (i64.shl (i64.and (local.get '$tmp) (i64.const #x3FFFFFFC0))
                                                                                                     (i64.const 2)) (i64.const #b01001))
                                                                                    ;func_idx
                                                                                    (i32.wrap_i64 (i64.shr_u (local.get '$tmp) (i64.const 35)))
                                                                                )
                                                                                back_half_stack_code
                                                                               )))
                                                                ) (array nil result_code (mif func_err func_err first_params_err) ctx)))
                                                        )))))))

                    ((marked_env? c) (or (get_passthrough (.hash c) ctx) (dlet ((e (.env_marked c))

                                            (generate_env_access (dlambda ((datasi funcs memo env pectx) env_id reason) ((rec-lambda recurse (code this_env)
                                                (cond
                                                    ((= env_id (.marked_env_idx this_env)) (array nil (call '$dup code) nil (array datasi funcs memo env pectx)))
                                                    ((= nil (.marked_env_upper this_env))  (array nil nil (str "bad env, upper is nil and we haven't found " env_id ", (this is *possiblely* because we're not recreating val/notval chains?) maxing out at " (str_strip this_env) ",  having started at " (str_strip env) ", we're generating because " reason) (array datasi funcs memo env pectx)))
                                                    (true                                  (recurse (i64.load 16 (i32.wrap_i64 (i64.shr_u code (i64.const 5))))
                                                                                                    (.marked_env_upper this_env)))
                                                )
                                            ) s_env_access_code env)))

                                            ) (if (not (marked_env_real? c)) (begin (print_strip "env wasn't real: " (marked_env_real? c) ", so generating access (env was) " c) (if need_value (array nil nil (str "marked env not real, though we need_value: " (str_strip c)) ctx) (generate_env_access ctx (.marked_env_idx c) "it wasn't real: " (str_strip c))))
                                            (dlet (


                                            ((kvs vvs ctx) (foldr (dlambda ((k v) (ka va ctx)) (dlet (((kv _    _   ctx) (compile-inner ctx (marked_symbol nil k) true inside_veval s_env_access_code))
                                                                                                      ((vv code err ctx) (compile-inner ctx v need_value inside_veval s_env_access_code))
                                                                                                      ;(_ (print_strip "result of (kv is " kv ") v compile-inner vv " vv " code " code " err " err ", based on " v))
                                                                                                      ;(_ (if (= nil vv) (print_strip "VAL NIL CODE IN ENV B/C " k " = " v) nil))
                                                                                                      ;(_ (if (!= nil err) (print_strip "ERRR IN ENV B/C " err " " k " = " v) nil))
                                                                                                      )
                                                                                                      (if (= false ka) (array false va ctx)
                                                                                                      (if (or (= nil vv) (!= nil err)) (array false (str "vv was " vv " err is " err " and we needed_value? " need_value " based on v " (str_strip v)) ctx)
                                                                                                                                       (array (cons kv ka) (cons (mod_fval_to_wrap vv) va) ctx)))))
                                                                  (array (array) (array) ctx)
                                                                  (slice e 0 -2)))
                                            ((uv ucode err ctx) (mif (idx e -1) (compile-inner ctx (idx e -1) need_value inside_veval s_env_access_code)
                                                                                (array nil_val nil nil ctx)))
                                            ) (mif (or (= false kvs) (= nil uv) (!= nil err)) (begin (print_strip "kvs " kvs " vvs " vvs " uv " uv " or err " err " based off of " c) (if need_value (array nil nil (str "had to generate env access (course " need_value ") for " (str_strip c) "vvs is " vvs " err was " err) ctx) (generate_env_access ctx (.marked_env_idx c) (str " vvs " vvs " uv " uv " or err " err " based off of " (str_strip c)))))
                                            (dlet (
                                                ((datasi funcs memo env pectx) ctx)
                                                ((kvs_array datasi) (if (= 0 (len kvs)) (array nil_val datasi)
                                                                                        (dlet (((kvs_loc kvs_len datasi) (alloc_data (apply concat (map i64_le_hexify kvs)) datasi)))
                                                                                              (array (bor (<< (len kvs) 32) kvs_loc #b101) datasi))))
                                                ((vvs_array datasi) (if (= 0 (len vvs)) (array nil_val datasi)
                                                                                        (dlet (((vvs_loc vvs_len datasi) (alloc_data (apply concat (map i64_le_hexify vvs)) datasi)))
                                                                                              (array (bor (<< (len vvs) 32) vvs_loc #b101) datasi))))
                                                (all_hex (map i64_le_hexify (array kvs_array vvs_array uv)))
                                                ((c_loc c_len datasi) (alloc_data (apply concat all_hex) datasi))
                                                (result (bor (<< c_loc 5) #b01001))
                                                (memo (put memo (.hash c) result))
                                            ) (array result nil nil (array datasi funcs memo env pectx)))))))))

                    ((prim_comb? c) (cond ((= 'vau           (.prim_comb_sym c))  (array (bor (<< (- k_vau dyn_start)           35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'cond          (.prim_comb_sym c))  (array (bor (<< (- k_cond dyn_start)          35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'eval          (.prim_comb_sym c))  (array (bor (<< (- k_eval dyn_start)          35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'read-string   (.prim_comb_sym c))  (array (bor (<< (- k_read-string dyn_start)   35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'log           (.prim_comb_sym c))  (array (bor (<< (- k_log dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'debug         (.prim_comb_sym c))  (array (bor (<< (- k_debug dyn_start)         35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'error         (.prim_comb_sym c))  (array (bor (<< (- k_error dyn_start)         35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'str           (.prim_comb_sym c))  (array (bor (<< (- k_str dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '>=            (.prim_comb_sym c))  (array (bor (<< (- k_geq dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '>             (.prim_comb_sym c))  (array (bor (<< (- k_gt dyn_start)            35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '<=            (.prim_comb_sym c))  (array (bor (<< (- k_leq dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '<             (.prim_comb_sym c))  (array (bor (<< (- k_lt dyn_start)            35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '!=            (.prim_comb_sym c))  (array (bor (<< (- k_neq dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '=             (.prim_comb_sym c))  (array (bor (<< (- k_eq dyn_start)            35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '%             (.prim_comb_sym c))  (array (bor (<< (- k_mod dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '/             (.prim_comb_sym c))  (array (bor (<< (- k_div dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '*             (.prim_comb_sym c))  (array (bor (<< (- k_mul dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '+             (.prim_comb_sym c))  (array (bor (<< (- k_add dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '-             (.prim_comb_sym c))  (array (bor (<< (- k_sub dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'band          (.prim_comb_sym c))  (array (bor (<< (- k_band dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'bor           (.prim_comb_sym c))  (array (bor (<< (- k_bor dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'bxor          (.prim_comb_sym c))  (array (bor (<< (- k_bxor dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'bnot          (.prim_comb_sym c))  (array (bor (<< (- k_bnot dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '<<            (.prim_comb_sym c))  (array (bor (<< (- k_ls dyn_start)            35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= '>>            (.prim_comb_sym c))  (array (bor (<< (- k_rs dyn_start)            35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'builtin_fib   (.prim_comb_sym c))  (array (bor (<< (- k_builtin_fib dyn_start)   35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'array         (.prim_comb_sym c))  (array (bor (<< (- k_array dyn_start)         35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'concat        (.prim_comb_sym c))  (array (bor (<< (- k_concat dyn_start)        35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'slice         (.prim_comb_sym c))  (array (bor (<< (- k_slice dyn_start)         35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'idx           (.prim_comb_sym c))  (array (bor (<< (- k_idx dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'len           (.prim_comb_sym c))  (array (bor (<< (- k_len dyn_start)           35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'array?        (.prim_comb_sym c))  (array (bor (<< (- k_array? dyn_start)        35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'get-text      (.prim_comb_sym c))  (array (bor (<< (- k_get-text dyn_start)      35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'str-to-symbol (.prim_comb_sym c))  (array (bor (<< (- k_str-to-symbol dyn_start) 35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'bool?         (.prim_comb_sym c))  (array (bor (<< (- k_bool? dyn_start)         35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'nil?          (.prim_comb_sym c))  (array (bor (<< (- k_nil? dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'env?          (.prim_comb_sym c))  (array (bor (<< (- k_env? dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'combiner?     (.prim_comb_sym c))  (array (bor (<< (- k_combiner? dyn_start)     35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'string?       (.prim_comb_sym c))  (array (bor (<< (- k_string? dyn_start)       35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'int?          (.prim_comb_sym c))  (array (bor (<< (- k_int? dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'symbol?       (.prim_comb_sym c))  (array (bor (<< (- k_symbol? dyn_start)       35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'unwrap        (.prim_comb_sym c))  (array (bor (<< (- k_unwrap dyn_start)        35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'vapply        (.prim_comb_sym c))  (array (bor (<< (- k_vapply dyn_start)        35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'lapply        (.prim_comb_sym c))  (array (bor (<< (- k_lapply dyn_start)        35) (<< 1 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                          ((= 'wrap          (.prim_comb_sym c))  (array (bor (<< (- k_wrap dyn_start)          35) (<< 0 5) (<< (.prim_comb_wrap_level c) 4) #b0001) nil nil ctx))
                                                                                         (error (str "Can't compile prim comb " (.prim_comb_sym c) " right now"))))




                    ((comb? c) (dlet (
                                    ((wrap_level env_id de? se variadic params body rec_hashes) (.comb c))
                                    (_ (mif (> wrap_level 1) (error "wrap level TOO DARN HIGH")))

                                    ; Let's look and see if we can eta-reduce!
                                    ; This is done here during code gen (when you would expect it earlier, like as part of partial eval)
                                    ; because we currently only "tie the knot" for Y combinator based recursion here
                                    ; at compile time (indeed, part of that happens in the block down below where we put our func value into memo before compiling),
                                    ; and so we can only tell here weather or not it will be safe to remove the level of lazyness (because we get a func value back instead of code)
                                    ; and perform the eta reduction.

                                    (attempt_reduction (and
                                                         variadic
                                                         (= 1 (len params))
                                                         (= 4 (len (.marked_array_values body)))
                                                         (prim_comb? (idx (.marked_array_values body) 0))
                                                         (= 'lapply (.prim_comb_sym (idx (.marked_array_values body) 0)))
                                                         (marked_symbol? (idx (.marked_array_values body) 2))
                                                         (not (.marked_symbol_is_val (idx (.marked_array_values body) 2)))
                                                         (= (idx params 0) (.marked_symbol_value (idx (.marked_array_values body) 2)))
                                                         (marked_symbol? (idx (.marked_array_values body) 3))
                                                         (not (.marked_symbol_is_val (idx (.marked_array_values body) 3)))
                                                         (= de? (.marked_symbol_value (idx (.marked_array_values body) 3)))
                                                         ))

                                    (full_params (concat params (mif de? (array de?) (array))))
                                    (normal_params_length (if variadic (- (len params) 1) (len params)))
                                    (compile_body_part (lambda (ctx body_part) (dlet (
                                                (inner_env (make_tmp_inner_env params de? se env_id))
                                                ((params_vec _ _ ctx) (compile-inner ctx (marked_array true false nil (map (lambda (k) (marked_symbol nil k)) full_params) nil) true false s_env_access_code))
                                                (new_get_s_env_code (_if '$have_s_env '(result i64)
                                                                         (i64.ne (i64.const nil_val) (local.get '$s_env))
                                                                         (then (local.get '$s_env))
                                                                         (else (local.tee '$s_env (call '$env_alloc (i64.const params_vec)

                                                                                                                    (local.set '$tmp_ptr (call '$malloc (i32.const (* 8 (len full_params)))))
                                                                                                                    (flat_map (lambda (i) (i64.store (* i 8) (local.get '$tmp_ptr)
                                                                                                                                                     (call '$dup (local.get (idx full_params i))))) 
                                                                                                                              (range 0 (len full_params)))
                                                                                                                    (i64.or (i64.extend_i32_u (local.get '$tmp_ptr))
                                                                                                                            (i64.const (bor (<< (len full_params) 32) #x5)))

                                                                                                                    (local.get '$outer_s_env)))
                                                                               (local.set '$outer_s_env (i64.const nil_val))
                                                                               )))
                                                ((datasi funcs memo env pectx) ctx)
                                                ((inner_value inner_code err ctx) (compile-inner (array datasi funcs memo inner_env pectx) body_part false false new_get_s_env_code))
                                                ; Don't overwrite env with what was our inner env! Env is returned as part of context to our caller!
                                                ((datasi funcs memo _was_inner_env pectx) ctx)
                                          ) (array inner_value inner_code err (array datasi funcs memo env pectx)))))

                                    ((early_quit err ctx) (mif attempt_reduction
                                                      (dlet (
                                                          ((inner_value inner_code err ctx) (compile_body_part ctx (idx (.marked_array_values body) 1)))
                                                          ; set it's wrap level to our wrap level
                                                          (inner_value (mif inner_value (bor (band inner_value (bnot (<< 1 4))) (<< wrap_level 4))))
                                                      ) (array inner_value err ctx))
                                                      (array nil nil ctx)))

                                    ) (mif (and (!= nil early_quit) (= nil err)) (array ;(mod_fval_to_wrap early_quit)
                                                                                        early_quit
                                                                                        nil nil ctx)
                                          (dlet (

                                    ((env_val env_code env_err ctx) (if (and need_value (not (marked_env_real? se)))
                                                                        (array nil nil "Env wasn't real when compiling comb, but need value" ctx)
                                                                        (compile-inner ctx se need_value inside_veval s_env_access_code)))
                                    (_ (if (not (or (= nil env_val) (int? env_val))) (error "BADBADBADenv_val")))
                                    (calculate_combined_value (lambda (env_val func_val) (bor (band #x7FFFFFFC0 (>> env_val 2)) func_val)))
                                    (maybe_func (get_passthrough (.hash c) ctx))
                                    ((func_value _ func_err ctx) (mif maybe_func maybe_func
                                        (dlet (

                                        ((datasi funcs memo env pectx) ctx)
                                        (old_funcs funcs)
                                        (funcs (concat funcs (array nil)))
                                        (our_wrap_func_idx (+ (len funcs) func_id_dynamic_ofset))
                                        (_ (true_print "Our wrapper id is " our_wrap_func_idx))
                                        (funcs (concat funcs (array nil)))
                                        (our_func_idx (+ (len funcs) func_id_dynamic_ofset))
                                        (_ (true_print "Our inner id is " our_func_idx))
                                        (calculate_func_val (lambda (wrap) (bor (<< our_func_idx 35) (<< (mif de? 1 0) 5) (<< wrap 4) #b0001)))
                                        (func_value (calculate_func_val wrap_level))
                                        ; if variadic, we just use the wrapper func and don't expect callers to know that we're varidic
                                        (func_value (mif variadic (mod_fval_to_wrap func_value) func_value))
                                        (memo (mif env_val (foldl (dlambda (memo (hash wrap)) (put memo hash (calculate_combined_value env_val (calculate_func_val wrap)))) memo rec_hashes)
                                                           memo))

                                        (ctx (array datasi funcs memo env pectx))

                                        (parameter_symbols (map (lambda (k) (array 'param k 'i64)) full_params))


                                        ((inner_value inner_code err ctx) (compile_body_part ctx body))
                                        (inner_code (mif inner_value (i64.const (mod_fval_to_wrap inner_value)) inner_code))
                                        (wrapper_func (func '$wrapper_func '(param $params i64) '(param $d_env i64) '(param $outer_s_env i64) '(result i64) '(local $param_ptr i32)
                                                ;(call '$print (i64.const 2674))
                                                (_if '$params_len_good
                                                    (if variadic    (i64.lt_u (i64.shr_u (local.get '$params) (i64.const 32)) (i64.const (- (len params) 1)))
                                                                    (i64.ne   (i64.shr_u (local.get '$params) (i64.const 32)) (i64.const (len params))))
                                                    (then
                                                        (call '$drop (local.get '$params))
                                                        (call '$drop (local.get '$outer_s_env))
                                                        (call '$drop (local.get '$d_env))
                                                        (call '$print (i64.const bad_params_number_msg_val))
                                                        (unreachable)
                                                    )
                                                )
                                                (call (+ (len old_funcs) 1 num_pre_functions)
                                                      (local.set '$param_ptr (i32.wrap_i64 (i64.and (i64.const -8) (local.get '$params))))
                                                      (flat_map (lambda (i) (call '$dup (i64.load (* i 8) (local.get '$param_ptr)))) (range 0 normal_params_length))
                                                      (if variadic
                                                           (call '$slice_impl (local.get '$params) (i32.const (- (len params) 1)) (i32.const -1))
                                                           (call '$drop (local.get '$params)))
                                                      (mif de?
                                                            (local.get '$d_env)
                                                            (call '$drop (local.get '$d_env)))
                                                      (local.get '$outer_s_env))
                                        ))
                                        (our_func (apply func (concat (array '$userfunc) parameter_symbols (array '(param $outer_s_env i64) '(result i64) '(local $param_ptr i32) '(local $s_env i64) '(local $tmp_ptr i32) '(local $tmp i64) '(local $type_check_tmp i64)

                                            (local.set '$s_env (i64.const nil_val))

                                            inner_code

                                            (call '$drop (local.get '$s_env))
                                            (call '$drop (local.get '$outer_s_env))
                                            (flat_map (lambda (k) (call '$drop (local.get k))) full_params)

                                        ))))
                                        ; replace our placeholder with the real one
                                        ((datasi funcs memo env pectx) ctx)
                                        (funcs (concat old_funcs wrapper_func our_func (drop funcs (+ 2 (len old_funcs)))))
                                        (memo (put memo (.hash c) func_value))

                                        ) (array func_value nil err (array datasi funcs memo env pectx)))
                                    ))
                                    (_ (print_strip "returning " func_value " for " c))
                                    (_ (if (not (int? func_value)) (error "BADBADBADfunc")))

                                    ;  <func_idx29>|<env_ptr29><usesde1><wrap1>0001
                                    ;                       e29><2><4> = 6
                                    ; 0..0<env_ptr29><3 bits>01001
                                    ;                       e29><3><5> = 8
                                    ; 0..0<env_ptr32 but still aligned>01001
                                    ; x+2+4 = y + 3 + 5
                                    ; x + 6 = y + 8
                                    ; x - 2 = y
                                ) (mif env_val  (array (calculate_combined_value env_val func_value) nil (mif func_err (str func_err ", from compiling comb body") (mif env_err (str env_err ", from compiling comb env") nil)) ctx)
                                                (array nil (i64.or (i64.const (mod_fval_to_wrap func_value)) (i64.and (i64.const #x7FFFFFFC0) (i64.shr_u env_code (i64.const 2)))) (mif func_err (str func_err ", from compiling comb body (env as code)") (mif env_err (str env_err ", from compiling comb env (as code)") nil)) ctx))
                                ))))

                   (true        (error (str "Can't compile-inner impossible " c)))
              )))

              ;(_ (println "compiling partial evaled " (str_strip marked_code)))
              ;(_ (true_print "compiling partial evaled " (true_str_strip marked_code)))
              ;(_ (true_print "compiling partial evaled "))
              (memo empty_dict)
              (ctx (array datasi funcs memo root_marked_env pectx))

              ((exit_val _ _ ctx)               (compile-inner ctx (marked_symbol nil 'exit) true false (array)))
              ((read_val _ _ ctx)               (compile-inner ctx (marked_symbol nil 'read) true false (array)))
              ((write_val _ _ ctx)              (compile-inner ctx (marked_symbol nil 'write) true false (array)))
              ((open_val _ _ ctx)               (compile-inner ctx (marked_symbol nil 'open) true false (array)))
              ((monad_error_msg_val _ _ ctx)    (compile-inner ctx (marked_val "Not a legal monad ( ['read fd len <cont(data error_no)>] / ['write fd data <cont(num_written error_no)>] / ['open fd path <cont(new_fd error_no)>] /['exit exit_code])") true false (array)))
              ((bad_read_val _ _ ctx)           (compile-inner ctx (marked_val "<error with read>") true false (array)))
              ((exit_msg_val _ _ ctx)           (compile-inner ctx (marked_val "Exiting with code: ") true false (array)))
              ((root_marked_env_val _ _ ctx)    (compile-inner ctx root_marked_env true false (array)))


              ((compiled_value_ptr compiled_value_code compiled_value_error ctx) (compile-inner ctx marked_code true false (array)))
              ((datasi funcs memo root_marked_env pectx) ctx)
              (compiled_value_code (mif compiled_value_ptr (i64.const (mod_fval_to_wrap compiled_value_ptr)) compiled_value_code))

              ; Swap for when need to profile what would be an error
              ;(compiled_value_ptr (mif compiled_value_error 0 compiled_value_ptr))
              (_ (mif compiled_value_error (error compiled_value_error)))

              ; Ok, so the outer loop handles the IO monads
                ; ('exit        code)
                ; ('read        fd   len       <cont (data error?)>)
                ; ('write       fd   "data"    <cont (num_written error?)>)
                ; ('open        fd   path      <cont (opened_fd error?)>)
                ; Could add some to open like lookup flags, o flags, base rights
                ;                             ineriting rights, fdflags

              (start (func '$start '(local $it i64) '(local $tmp i64) '(local $ptr i32) '(local $monad_name i64) '(local $len i32) '(local $buf i32) '(local $code i32) '(local $str i64) '(local $result i64) '(local $debug_malloc_print i32)
                    (local.set '$it (if needs_runtime_eval (call '$eval_helper compiled_value_code (i64.const root_marked_env_val))
                                                           compiled_value_code))
                    (block '$exit_block
                        (block '$error_block
                            (_loop '$l
                                ; Not array -> out
                                (br_if '$error_block (i64.ne (i64.const #b101) (i64.and (i64.const #b101) (local.get '$it))))
                                ; less than len 2 -> out
                                (br_if '$error_block (i64.lt_u (i64.shr_u (local.get '$it) (i64.const 32)) (i64.const 2)))
                                (local.set '$ptr (i32.wrap_i64 (i64.and (local.get '$it) (i64.const -8))))
                                ; second entry isn't an int -> out
                                (br_if '$error_block (i64.ne (i64.and (i64.load 8 (local.get '$ptr)) (i64.const #b1)) (i64.const #b0)))
                                (local.set '$monad_name (i64.load (local.get '$ptr)))

                                ; ('exit        code)
                                (_if '$is_exit
                                    (i64.eq (i64.const exit_val) (local.get '$monad_name))
                                    (then
                                        ; len != 2
                                        (br_if '$error_block (i64.ne (i64.shr_u (local.get '$it) (i64.const 32)) (i64.const 2)))
                                        (call '$print (i64.const exit_msg_val))
                                        (call '$print (i64.load 8 (local.get '$ptr)))
                                        (br '$exit_block)
                                    )
                                )

                                ; if len != 4
                                (br_if '$error_block (i64.ne (i64.shr_u (local.get '$it) (i64.const 32)) (i64.const 4)))

                                ; ('read        fd   len       <cont (data error_code)>)
                                (_if '$is_read
                                    (i64.eq (i64.const read_val) (local.get '$monad_name))
                                    (then
                                        ; third entry isn't an int -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 16 (local.get '$ptr)) (i64.const #b1)) (i64.const #b0)))
                                        ; fourth entry isn't a comb -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 24 (local.get '$ptr)) (i64.const #b1111)) (i64.const #b0001)))
                                        ; iov <32bit len><32bit addr> + <32bit num written>
                                        (i32.store 4 (i32.const iov_tmp) (local.tee '$len (i32.wrap_i64 (i64.shr_u (i64.load 16 (local.get '$ptr)) (i64.const 1)))))
                                        (i32.store 0 (i32.const iov_tmp) (local.tee '$buf (call '$malloc (local.get '$len))))
                                        (local.set '$code (call '$fd_read
                                              (i32.wrap_i64 (i64.shr_u (i64.load 8 (local.get '$ptr)) (i64.const 1)))    ;; file descriptor
                                              (i32.const iov_tmp)                                                      ;; *iovs
                                              (i32.const 1)                                                            ;; iovs_len
                                              (i32.const (+ 8 iov_tmp))                                                ;; nwritten
                                        ))
                                        ;  <string_size32><string_ptr29>011
                                        ;(i64.store (i32.add (i32.const -16) (local.get '$buf))
                                        ;      (i64.or (i64.shl (i64.extend_i32_u (i32.load 8 (i32.const iov_tmp))) (i64.const 32))
                                        ;              (i64.extend_i32_u (i32.or (local.get '$buf) (i32.const #b011))))) ; MDEBUG

                                        (local.set '$str (i64.or (i64.shl (i64.extend_i32_u (i32.load 8 (i32.const iov_tmp))) (i64.const 32))
                                                                 (i64.extend_i32_u (i32.or (local.get '$buf) (i32.const #b011)))))
                                        (_if '$is_error
                                            (i32.eqz (local.get '$code))
                                            (then
                                                (local.set '$result (call '$array2_alloc (local.get '$str)
                                                                                         (i64.const 0)))
                                            )
                                            (else
                                                (call '$drop (local.get '$str))
                                                (local.set '$result (call '$array2_alloc (i64.const bad_read_val)
                                                                                         (i64.shl (i64.extend_i32_u (local.get '$code)) (i64.const 1))))
                                            )
                                        )

                                        (local.set '$tmp (call '$dup (i64.load 24 (local.get '$ptr))))
                                        (call '$drop (global.get '$debug_func_to_call))
                                        (call '$drop (global.get '$debug_params_to_call))
                                        (call '$drop (global.get '$debug_env_to_call))
                                        (global.set '$debug_func_to_call   (call '$dup (local.get '$tmp)))
                                        (global.set '$debug_params_to_call (call '$dup (local.get '$result)))
                                        (global.set '$debug_env_to_call (i64.const root_marked_env_val))
                                        (call '$drop (local.get '$it))
                                        (local.set '$it (call_indirect
                                            ;type
                                            k_vau
                                            ;table
                                            0
                                            ;params
                                            (local.get '$result)
                                            ;top_env
                                            (i64.const root_marked_env_val)
                                            ; static env
                                            (i64.or (i64.shl (i64.and (local.get '$tmp) (i64.const #x3FFFFFFC0)) (i64.const 2)) (i64.const #b01001))
                                            ;func_idx
                                            (i32.wrap_i64 (i64.shr_u (local.get '$tmp) (i64.const 35)))
                                        ))
                                        (br '$l)
                                    )
                                )

                                ; ('write       fd   "data"    <cont (num_written error_code)>)
                                (_if '$is_write
                                    (i64.eq (i64.const write_val) (local.get '$monad_name))
                                    (then
                                        ; third entry isn't a string -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 16 (local.get '$ptr)) (i64.const #b111)) (i64.const #b011)))
                                        ; fourth entry isn't a comb -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 24 (local.get '$ptr)) (i64.const #b1111)) (i64.const #b0001)))
                                        ;  <string_size32><string_ptr29>011
                                        (local.set '$str (i64.load 16 (local.get '$ptr)))

                                        ; iov <32bit addr><32bit len> + <32bit num written>
                                        (i32.store 0 (i32.const iov_tmp) (i32.wrap_i64 (i64.and   (local.get '$str) (i64.const #xFFFFFFF8))))
                                        (i32.store 4 (i32.const iov_tmp) (i32.wrap_i64 (i64.shr_u (local.get '$str) (i64.const 32))))
                                        (local.set '$code (call '$fd_write
                                              (i32.wrap_i64 (i64.shr_u (i64.load 8 (local.get '$ptr)) (i64.const 1)))  ;; file descriptor
                                              (i32.const iov_tmp)                                                      ;; *iovs
                                              (i32.const 1)                                                            ;; iovs_len
                                              (i32.const (+ 8 iov_tmp))                                                ;; nwritten
                                        ))
                                        (local.set '$result (call '$array2_alloc (i64.shl (i64.extend_i32_u (i32.load (i32.const (+ 8 iov_tmp)))) (i64.const 1))
                                                                                 (i64.shl (i64.extend_i32_u (local.get '$code))                   (i64.const 1))))

                                        (local.set '$tmp (call '$dup (i64.load 24 (local.get '$ptr))))
                                        (call '$drop (global.get '$debug_func_to_call))
                                        (call '$drop (global.get '$debug_params_to_call))
                                        (call '$drop (global.get '$debug_env_to_call))
                                        (global.set '$debug_func_to_call   (call '$dup (local.get '$tmp)))
                                        (global.set '$debug_params_to_call (call '$dup (local.get '$result)))
                                        (global.set '$debug_env_to_call (i64.const root_marked_env_val))
                                        (call '$drop (local.get '$it))
                                        (local.set '$it (call_indirect
                                            ;type
                                            k_vau
                                            ;table
                                            0
                                            ;params
                                            (local.get '$result)
                                            ;top_env
                                            (i64.const root_marked_env_val)
                                            ; static env
                                            (i64.or (i64.shl (i64.and (local.get '$tmp) (i64.const #x3FFFFFFC0)) (i64.const 2)) (i64.const #b01001))
                                            ;func_idx
                                            (i32.wrap_i64 (i64.shr_u (local.get '$tmp) (i64.const 35)))
                                        ))
                                        (br '$l)
                                    )
                                )
                                ; ('open        fd   path      <cont (opened_fd error?)>)
                                (_if '$is_open
                                    (i64.eq (i64.const open_val) (local.get '$monad_name))
                                    (then
                                        ; third entry isn't a string -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 16 (local.get '$ptr)) (i64.const #b111)) (i64.const #b011)))
                                        ; fourth entry isn't a comb -> out
                                        (br_if '$error_block (i64.ne (i64.and (i64.load 24 (local.get '$ptr)) (i64.const #b1111)) (i64.const #b0001)))
                                        ;  <string_size32><string_ptr29>011
                                        (local.set '$str (i64.load 16 (local.get '$ptr)))

                                        (local.set'$code (call '$path_open
                                            (i32.wrap_i64 (i64.shr_u (i64.load 8 (local.get '$ptr)) (i64.const 1))) ;; file descriptor
                                            (i32.const 0)                                                           ;; lookup flags
                                            (i32.wrap_i64 (i64.and   (local.get '$str) (i64.const #xFFFFFFF8)))     ;; path string  *
                                            (i32.wrap_i64 (i64.shr_u (local.get '$str) (i64.const 32)))             ;; path string  len
                                            (i32.const 1)                                                           ;; o flags
                                            (i64.const 66)                                                          ;; base rights
                                            (i64.const 66)                                                          ;; inheriting rights
                                            (i32.const 0)                                                           ;; fdflags
                                            (i32.const iov_tmp)                                                     ;; opened fd out ptr
                                        ))

                                        (local.set '$result (call '$array2_alloc (i64.shl (i64.extend_i32_u (i32.load (i32.const iov_tmp))) (i64.const 1))
                                                                                 (i64.shl (i64.extend_i32_u (local.get '$code))             (i64.const 1))))

                                        (local.set '$tmp (call '$dup (i64.load 24 (local.get '$ptr))))
                                        (call '$drop (global.get '$debug_func_to_call))
                                        (call '$drop (global.get '$debug_params_to_call))
                                        (call '$drop (global.get '$debug_env_to_call))
                                        (global.set '$debug_func_to_call   (call '$dup (local.get '$tmp)))
                                        (global.set '$debug_params_to_call (call '$dup (local.get '$result)))
                                        (global.set '$debug_env_to_call (i64.const root_marked_env_val))
                                        (call '$drop (local.get '$it))
                                        (local.set '$it (call_indirect
                                            ;type
                                            k_vau
                                            ;table
                                            0
                                            ;params
                                            (local.get '$result)
                                            ;top_env
                                            (i64.const root_marked_env_val)
                                            ; static env
                                            (i64.or (i64.shl (i64.and (local.get '$tmp) (i64.const #x3FFFFFFC0)) (i64.const 2)) (i64.const #b01001))
                                            ;func_idx
                                            (i32.wrap_i64 (i64.shr_u (local.get '$tmp) (i64.const 35)))
                                        ))
                                        (br '$l)
                                    )
                                )
                            )
                        )
                        ; print error
                        (call '$print (i64.const monad_error_msg_val))
                        (call '$print (local.get '$it))
                    )
                    (call '$drop (local.get '$it))
                    (call '$drop (global.get '$debug_func_to_call))
                    (call '$drop (global.get '$debug_params_to_call))
                    (call '$drop (global.get '$debug_env_to_call))

                    (i64.shl (i64.extend_i32_s (global.get '$num_frees))   (i64.const 1))
                    (i64.shl (i64.extend_i32_s (global.get '$num_mallocs)) (i64.const 1))
                    (i64.shl (i64.extend_i32_s (global.get '$num_sbrks))   (i64.const 1))

                    ;(local.set '$debug_malloc_print (global.get '$debug_malloc_head))

                    (call '$print (i64.const newline_msg_val))
                    (call '$print )
                    (call '$print (i64.const newline_msg_val))
                    (call '$print )
                    (call '$print (i64.const newline_msg_val))
                    (call '$print )


                    ; MDEBUG
                   ;(call '$print (i64.const newline_msg_val))
                   ;(call '$print (i64.const newline_msg_val))
                   ;(block '$print_loop_br
                   ;    (_loop '$print_loop
                   ;        (br_if '$print_loop_br (i32.eq (local.get '$debug_malloc_print) (i32.const 0)))

                   ;        (call '$print (i64.const space_msg_val))
                   ;        (call '$print (i64.shl (i64.extend_i32_s (i32.load 20 (local.get '$debug_malloc_print))) (i64.const 1)))
                   ;        (call '$print (i64.const space_msg_val))

                   ;        (call '$print (i64.load 8 (local.get '$debug_malloc_print)))
                   ;        (local.set '$debug_malloc_print (i32.load (local.get '$debug_malloc_print)))
                   ;        (call '$print (i64.const newline_msg_val))
                   ;        (br '$print_loop)
                   ;    )
                   ;)
                    ; MDEBUG



              ))
              ((watermark datas) datasi)
          ) (concat
              (global '$data_end '(mut i32) (i32.const watermark))
              datas funcs start
              (table  '$tab (len funcs) 'funcref)
              (apply elem (cons (i32.const 0) (range dyn_start (+ num_pre_functions (len funcs)))))
              (memory '$mem (+ 2 (>> watermark 16)))
          ))
          (export "memory" '(memory $mem))
          (export "_start" '(func   $start))
    )))))


    (run_partial_eval_test (lambda (s) (dlet (
                            (_ (print "\n\ngoing to partial eval " s))
                            ((pectx err result) (partial_eval (read-string s)))
                            (_ (true_print "result of test \"" s "\" => " (true_str_strip result) " and err " err))
                            ;(_ (mif result (true_print "with a hash of " (.hash result))))
                            ) nil)))


    (test-most (lambda () (begin
        (print (val? '(val)))
        (print "take 3" (take '(1 2 3 4 5 6 7 8 9 10) 3))
        ; shadowed by wasm
        ;(print "drop 3" (drop '(1 2 3 4 5 6 7 8 9 10) 3))
        (print (slice '(1 2 3) 1 2))
        (print (slice '(1 2 3) 1 -1))
        (print (slice '(1 2 3) -1 -1))
        (print (slice '(1 2 3) -2 -1))

        (print "ASWDF")
        (print (str-to-symbol (str '(a b))))
        (print (symbol? (str-to-symbol (str '(a b)))))
        (print ( (dlambda ((a b)) a) '(1337 1338)))
        (print ( (dlambda ((a b)) b) '(1337 1338)))

        (print (str 1 2 3 (array 1 23 4) "a" "B"))

        (print (dlet ( (x 2) ((a b) '(1 2)) (((i i2) i3) '((5 6) 7)) ) (+  x a b i i2 i3)))

        (print (array 1 2 3))
        (print (command-line-arguments))

        ;(print (call-with-input-string "'(1 2)" (lambda (p) (read p))))
        (print (read (open-input-string "'(3 4)")))

        (print "if tests")
        (print (if true 1 2))
        (print (if false 1 2))
        (print (if true 1))
        (print (if false 1))
        (print "if tests end")

        (print "mif tests")
        (print (mif true 1 2))
        (print (mif false 1 2))
        (print (mif true 1))
        (print (mif false 1))
        (print "2 nils")
        (print (mif nil 1 2))
        (print (mif nil 1))
        (print "2 1s")
        (print (mif 1 1 2))
        (print (mif 1 1))
        (print "mif tests end")

        (print (get-value (put (put empty_dict 3 4) 1 2) 3))
        (print (get-value (put (put empty_dict 3 4) 1 2) 1))

        (print (get-value-or-false (put (put empty_dict 3 4) 1 2) 3))
        (print (get-value-or-false (put (put empty_dict 3 4) 1 2) 1))
        (print (get-value-or-false (put (put empty_dict 3 4) 1 2) 5))

        (print "zip " (zip '(1 2 3) '(4 5 6) '(7 8 9)))

        (print (run_partial_eval_test "(+ 1 2)"))
        ;(print) (print)
        (print (run_partial_eval_test "(cond false 1 true 2)"))
        (print (run_partial_eval_test "(log 1)"))
        (print (run_partial_eval_test "((vau (x) (+ x 1)) 2)"))


        (print (run_partial_eval_test "(+ 1 2)"))
        (print (run_partial_eval_test "(vau (y) (+ 1 2))"))
        (print (run_partial_eval_test "((vau (y) (+ 1 2)) 4)"))
        (print (run_partial_eval_test "((vau (y) y) 4)"))
        (print (run_partial_eval_test "((vau (y) (+ 13 2 y)) 4)"))
        (print (run_partial_eval_test "((wrap (vau (y) (+ 13 2 y))) (+ 3 4))"))
        (print (run_partial_eval_test "(vau de (y) (+ (eval y de) (+ 1 2)))"))
        (print (run_partial_eval_test "((vau de (y) ((vau dde (z) (+ 1 (eval z dde))) y)) 17)"))

        (print (run_partial_eval_test "(cond false 1 false 2 (+ 1 2) 3 true 1337)"))
        (print (run_partial_eval_test "(vau de (x) (cond false 1 false 2 x 3 true 42))"))
        (print (run_partial_eval_test "(vau de (x) (cond false 1 false 2 3 x true 42))"))

        (print (run_partial_eval_test "(combiner? true)"))
        (print (run_partial_eval_test "(combiner? (vau de (x) x))"))
        (print (run_partial_eval_test "(vau de (x) (combiner? x))"))

        (print (run_partial_eval_test "((vau (x) x) a)"))

        (print (run_partial_eval_test "(env? true)"))
        ; this doesn't partially eval, but it could with a more percise if the marked values were more percise
        (print (run_partial_eval_test "(vau de (x) (env? de))"))
        (print (run_partial_eval_test "(vau de (x) (env? x))"))
        (print (run_partial_eval_test "((vau de (x) (env? de)) 1)"))

        (print (run_partial_eval_test "((wrap (vau (let1) (let1 a 12 (+ a 1)))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_partial_eval_test "((wrap (vau (let1) (let1 a 12 (vau (x) (+ a 1))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_partial_eval_test "((wrap (vau (let1) (let1 a 12 (wrap (vau (x) (+ x a 1)))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))
        (print (run_partial_eval_test "((wrap (vau (let1) (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a))))))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        ;(print "\n\nnil test\n")
        ;(print (run_partial_eval_test "nil"))
        ;(print (run_partial_eval_test "(nil? 1)"))
        ;(print (run_partial_eval_test "(nil? nil)"))

        (print "\n\nlet 4.3\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))
        (print "\n\nlet 4.7\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ x a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))

        (print "\n\nlet 5\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                    (let1 a 12 (wrap (vau (x) (let1 y (+ x a 1) (+ y x a))))
                                ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))"))

        (print "\n\nlambda 1\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (lambda (x) x)
                                   ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))
        (print "\n\nlambda 2\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (let1 a 12
                                         (lambda (x) (+ a x)))
                                   ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))
        (print "\n\nlambda 3\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (let1 a 12
                                         (lambda (x) (let1 b (+ a x)
                                                             (+ a x b))))
                                   ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))

        (print (run_partial_eval_test "(array 1 2 3 4 5)"))
        (print (run_partial_eval_test "((wrap (vau (a & rest) rest)) 1 2 3 4 5)"))

        (print "\n\nrecursion test\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
                                                                                     true     1                               )) 5)
                                   ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))

        (print "\n\nlambda recursion test\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (lambda (n) ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
                                                                                     true     1                               )) n))
                                   ))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))

        ; The issue with this one is that (x2 x2) trips the infinate recursion protector, but then
        ; that array gets marked as attempted & needing no more evaluation, and is frozen forever.
        ; Then, when the recursion is actually being used, it won't keep going and you only get
        ; the first level.
        (print "\n\nlambda recursion Y combiner test\n\n")
        (print (run_partial_eval_test "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (let1 lapply (lambda (f1 p)     (eval (concat (array (unwrap f1)) p)))
                                   (let1 Y (lambda (f3)
                                       ((lambda (x1) (x1 x1))
                                        (lambda (x2) (f3 (lambda (& y) (lapply (x2 x2) y))))))
                                   ((Y (lambda (recurse) (lambda (n) (cond (!= 0 n) (* n (recurse (- n 1)))
                                                                           true     1))))
                                    5)
                                   ))))) (vau de (s v b) (eval (array (array wrap (array vau (array s) b)) v) de)))"))


        (print (run_partial_eval_test "(len \"asdf\")"))
        (print (run_partial_eval_test "(idx \"asdf\" 1)"))
        (print (run_partial_eval_test "(slice \"asdf\" 1 3)"))
        (print (run_partial_eval_test "(concat \"asdf\" \";lkj\")"))


        (print "ok, hex of 0 is " (hex_digit #\0))
        (print "ok, hex of 1 is " (hex_digit #\1))
        (print "ok, hex of a is " (hex_digit #\a))
        (print "ok, hex of A is " (hex_digit #\A))
        (print "ok, hexify of 1337 is " (i64_le_hexify 1337))
        (print "ok, hexify of 10 is " (i64_le_hexify 10))
        (print "ok, hexify of 15 is " (i64_le_hexify 15))
        (print "ok, hexfy of 15 << 60 is " (i64_le_hexify (<< 15 60)))
        (dlet (
            ;(output1 (wasm_to_binary (module)))
            ;(output2 (wasm_to_binary (module
            ;      (import "wasi_unstable" "path_open"
            ;              '(func $path_open  (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
            ;                                 (result i32)))
            ;      (import "wasi_unstable" "fd_prestat_dir_name"
            ;              '(func $fd_prestat_dir_name  (param i32 i32 i32)
            ;                               (result i32)))
            ;      (import "wasi_unstable" "fd_read"
            ;              '(func $fd_read  (param i32 i32 i32 i32)
            ;                               (result i32)))
            ;      (import "wasi_unstable" "fd_write"
            ;              '(func $fd_write (param i32 i32 i32 i32)
            ;                               (result i32)))
            ;      (memory '$mem 1)
            ;      (global '$gi 'i32       (i32.const 8))
            ;      (global '$gb '(mut i64) (i64.const 9))
            ;      (table  '$tab 2 'funcref)
            ;      (data (i32.const 16) "HellH") ;; adder to put, then data


            ;      (func '$start
            ;            (i32.store (i32.const 8) (i32.const 16))  ;; adder of data
            ;            (i32.store (i32.const 12) (i32.const 5)) ;; len of data
            ;            ;; open file
            ;            (call 0 ;$path_open
            ;                  (i32.const 3) ;; file descriptor
            ;                  (i32.const 0) ;; lookup flags
            ;                  (i32.const 16) ;; path string  *
            ;                  (i32.load (i32.const 12)) ;; path string  len
            ;                  (i32.const 1) ;; o flags
            ;                  (i64.const 66) ;; base rights
            ;                  (i64.const 66) ;; inheriting rights
            ;                  (i32.const 0) ;; fdflags
            ;                  (i32.const 4) ;; opened fd out ptr
            ;             )
            ;            (drop)
            ;            (block '$a
            ;                (block '$b
            ;                    (br '$a)
            ;                    (br_if  '$b
            ;                            (i32.const 3))
            ;                    (_loop '$l
            ;                        (br '$a)
            ;                        (br '$l)
            ;                    )
            ;                    (_if '$myif
            ;                        (i32.const 1)
            ;                        (then
            ;                            (i32.const 1)
            ;                            (drop)
            ;                            (br '$b)
            ;                        )
            ;                        (else
            ;                            (br '$myif)
            ;                        )
            ;                    )
            ;                    (_if '$another
            ;                        (i32.const 1)
            ;                        (br '$b))
            ;                    (i32.const 1)
            ;                    (_if '$third
            ;                        (br '$b))
            ;                    (_if '$fourth
            ;                        (br '$fourth))
            ;                )
            ;            )
            ;            (call '$fd_read
            ;                  (i32.const 0) ;; file descriptor
            ;                  (i32.const 8) ;; *iovs
            ;                  (i32.const 1) ;; iovs_len
            ;                  (i32.const 12) ;; nwritten, overwrite buf len with it
            ;            )
            ;            (drop)

            ;            ;; print name
            ;            (call '$fd_write
            ;                  (i32.load (i32.const 4)) ;; file descriptor
            ;                  (i32.const 8) ;; *iovs
            ;                  (i32.const 1) ;; iovs_len
            ;                  (i32.const 4) ;; nwritten
            ;            )
            ;            (drop)
            ;      )

            ;      (elem (i32.const 0) '$start '$start)
            ;      (export "memory" '(memory $mem))
            ;      (export "_start" '(func   $start))
            ;)))
            (output3 (compile (partial_eval (read-string "(array 1 (array ((vau (x) x) a) (array \"asdf\"))  2)")) false))
            (output3 (compile (partial_eval (read-string "(array 1 (array 1 2 3 4) 2 (array 1 2 3 4))")) false))
            (output3 (compile (partial_eval (read-string "empty_env")) false))
            (output3 (compile (partial_eval (read-string "(eval (array (array vau ((vau (x) x) (a b)) (array (array vau ((vau (x) x) x) (array) ((vau (x) x) x)))) 1 2) empty_env)")) false))
            (output3 (compile (partial_eval (read-string "(eval (array (array vau ((vau (x) x) (a b)) (array (array vau ((vau (x) x) x) (array) ((vau (x) x) x)))) empty_env 2) empty_env)")) false))
            (output3 (compile (partial_eval (read-string "(eval (array (array vau ((vau (x) x) x) (array) ((vau (x) x) x))))")) false))
            (output3 (compile (partial_eval (read-string "(vau (x) x)")) false))
            (output3 (compile (partial_eval (read-string "(vau (x) 1)")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) exit) 1)")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (array ((vau (x) x) exit) 1)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (array ((vau (x) x) exit) written)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) written))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) code))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (array 1337 written 1338 code 1339)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (cond (= 0 code) written true code)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (str (= 0 code) written true (array) code)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (log (= 0 code) written true (array) code)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (error (= 0 code) written true code)))")) false))
            ;(output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (or (= 0 code) written true code)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (+ written code 1337)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (- written code 1337)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (* written 1337)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (/ 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (% 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (band 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (bor 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (bnot written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (bxor 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (<< 1337 written)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (>> 1337 written)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (<= (array written) (array 1337))))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"true\" true 3))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"     true\" true 3))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"     true   \" true 3))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"     false\" true 3))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"(false (true () true) true)\" true 3))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (read-string (cond written \"(false (true () true) true) true\" true 3))))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) open) 3 \"test_out\" (vau (fd code) (array ((vau (x) x) write) fd \"waa\" (vau (written code) (array written code)))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) open) 3 \"test_out\" (vau (fd code) (array ((vau (x) x) read) fd 10 (vau (data code) (array data code)))))")) false))

            ;(_ (print (slurp "test_parse_in")))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) open) 3 \"test_parse_in\" (vau (fd code) (array ((vau (x) x) read) fd 1000 (vau (data code) (read-string data)))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"test_parse_in\" (vau (written code) (array (array written))))")) false))


            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (slice args 1 -1)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (len args)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (idx args 0)))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (slice (concat args (array 1 2 3 4) args) 1 -2)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (str-to-symbol (str args))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (get-text (str-to-symbol (str args)))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (wrap (cond args idx true 0))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (wrap (wrap (cond args idx true 0)))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (wrap (wrap (wrap (cond args idx true 0))))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (unwrap (cond args idx true 0))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) (unwrap (cond args vau true 0))))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (written code) (array (nil? written) (array? written) (bool? written) (env? written) (combiner? written) (string? written) (int? written) (symbol? written))))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau de (written code) (array (nil? (cond written (array) true 4)) (array? (cond written (array 1 2) true 4)) (bool? (= 3 written)) (env? de) (combiner? (cond written (vau () 1) true 43)) (string? (cond written \"a\" 3 3)) (int? (cond written \"a\" 3 3)) (symbol? (cond written ((vau (x) x) x) 3 3)) written)))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (& args) args))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (a & args) a))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"waa\" (vau (a & args) args))")) false))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) read) 0 10 (vau (data code) data))")) false))
            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) read) 0 10 (vau (data code) (array ((vau (x) x) write) 1 data (vau (written code) (array written code)))))")) false))

            (output3 (compile (partial_eval (read-string "(wrap (vau (x) x))")) false))
            (output3 (compile (partial_eval (read-string "len")) false))
            (output3 (compile (partial_eval (read-string "vau")) false))
            (output3 (compile (partial_eval (read-string "(array len 3 len)")) false))
            (output3 (compile (partial_eval (read-string "(+ 1 1337 (+ 1 2))")) false))
            (output3 (compile (partial_eval (read-string "\"hello world\"")) false))
            (output3 (compile (partial_eval (read-string "((vau (x) x) asdf)")) false))
            (output3 (compile (partial_eval (read-string "((wrap (vau (let1)
                                   (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
                                   (array ((vau (x) x) write) 1 \"hahah\" (vau (written code) ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
                                                                                                                                                true     1)) written)))
                                   ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))")) false))
            (_ (write_file "./csc_out.wasm" output3))
            (output3 (compile (partial_eval (read-string "(nil? 1)")) false))
            ;(output3 (compile (partial_eval (read-string "(nil? nil)")) false))
        ) (void))
    )))

    (single-test (lambda () (dlet (
        ;(output3 (compile (partial_eval (read-string "1337")) false))
        ;(output3 (compile (partial_eval (read-string "\"This is a longish sring to make sure alloc data is working properly\"")) false))
        ;(output3 (compile (partial_eval (read-string "((vau (x) x) write)")) false))
        ;(output3 (compile (partial_eval (read-string "(wrap (vau (x) x))")) false))
        ;(output3 (compile (partial_eval (read-string "(wrap (vau (x) (log 1337)))")) false))
        ;(output3 (compile (partial_eval (read-string "(wrap (vau (x) (+ x 1337)))")) false))
        ;(output3 (compile (partial_eval (read-string "(array ((vau (x) x) write) 1 \"w\" (vau (written code) (+ written code 1337)))")) false))
        ;(output3 (compile (partial_eval (read-string "((wrap (vau (let1)
        ;                       (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
        ;                       (array ((vau (x) x) write) 1 \"hahah\" (vau (written code) ((lambda (x n) (x x n)) (lambda (recurse n) (cond (!= 0 n) (* n (recurse recurse (- n 1)))
        ;                                                                                                                                    true     1)) written)))
        ;                       ))) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))")) false))


        ;(output3 (compile (partial_eval (read-string
        ;                           "((wrap (vau root_env (quote)
        ;                            ((wrap (vau (let1)

        ;                            (let1 lambda (vau se (p b) (wrap (eval (array vau p b) se)))
        ;                            (let1 current-env (vau de () de)
        ;                            (let1 lapply (lambda (f p)     (eval (concat (array (unwrap f)) p) (current-env)))
        ;                            (array (quote write) 1 \"test_self_out2\" (vau (written code) 1))
        ;                            )))

        ;                            )) (vau de (s v b) (eval (array (array vau (array s) b) (eval v de)) de)))
        ;                            )) (vau (x5) x5))")) false))
        ;(_ (write_file "./csc_out.wasm" output3))

        ;(_ (write_file "./csc_out.wasm" (compile (partial_eval (read-string
        ;    "(array ((vau (x5) x5) write)  1 \"written\" (vau (written code) (len (cond (= 0 written) \"asdf\" true \"sdf\"))))")) false)))

        ;(_ (write_file "./csc_out.wasm" (compile (partial_eval (read-string
        ;    "(array ((vau (x5) x5) write)  1 \"written\" (vau (written code) (idx (cond (= 0 written) \"asdf\" true \"sdf\") 1)))")) false)))

        ;(_ (write_file "./csc_out.wasm" (compile (partial_eval (read-string
        ;    "(array ((vau (x5) x5) write)  1 \"written\" (vau (written code) (slice (cond (= 0 written) \"asdf\" true \"abcdefghi\") 1 3)))")) false)))

        ;(_ (write_file "./csc_out.wasm" (compile (partial_eval (read-string
        ;    "(array ((vau (x5) x5) write)  1 \"written\" (vau (written code) (concat \"hehe\" (cond (= 0 written) \"asdf\" true \"abcdefghi\"))))")) false)))

        (_ (write_file "./csc_out.wasm" (compile (partial_eval (read-string
            "(array ((vau (x) x) write)  1 \"enter form: \" (vau (written code)
                  (array ((vau (x) x) read) 0 60 (vau (data code)
                        (array ((vau (x) x) exit) (eval (read-string data)))
                  ))

             ))")) false)))

            (output3 (compile (partial_eval (read-string "(array ((vau (x) x) read) 0 10 (vau (data code) data))")) false))

    ) void)))

    (run-compiler (lambda (dont_compile f)
      (dlet (
            (_ (true_print "reading in!"))
            (read_in    (read-string (slurp f)))
            ;(_ (true_print "read in, now evaluating"))
            (evaled     (if dont_compile (array (array 0 empty_dict) nil (mark read_in))
                                         (partial_eval read_in)))
            ;(_ (true_print "done partialy evaling, now compiling"))
            (bytes      (compile evaled dont_compile))
            ;(_ (true_print "compiled, writng out"))
            (_          (write_file "./csc_out.wasm" bytes))
            ;(_ (true_print "written out"))
        ) (void))
    ))

)
   (begin
       ;(test-most)
       ;(single-test)
       ;(run-compiler "small_test.kp")
       ;(run-compiler "to_compile.kp")
       (true_print "args are " args)
       (dlet ( (com (if (> (len args) 0) (idx args 0) "")) )
             (cond ((= "test"   com) (test-most))
                   ((= "single" com) (single-test))
                   (true             (run-compiler (and (>= (len args) 2) (= "no_compile" (idx args 1))) com))))

       ;(true_print "GLOBAL_MAX was " GLOBAL_MAX)
       ;(profile-dump-html)
   )
)

;;;;;;;;;;;;;;
; Known TODOs
;;;;;;;;;;;;;;
;
; * NON NAIVE REFCOUNTING
; EVENTUALLY: Support some hard core partial_eval that an fully make (foldl or stuff) short circut effeciencly with double-inlining, finally
;   addressing the strict-languages-don't-compose thing


; Suspected needed for performance

; Opt not passing dynamic env around
; gets:
;     not creating dynamic env
;     not creating param arrays
; needs:
;     analysis of static calls
;     inlining of single use funcs
;           otherwise lets are single use closures that do use their dynamic env just by virtue of being closures that take the dynamic env as the static env
;                 wait, this is still vaguely ok - will stop at function boundries
;     Debugging restart-rerun
;
; THUS TODO:
;     opt versions of functions with backup code
;           CAN BE A DEBUGGING CHECK IN WRAPPER FUNC!
;     inlining of single use closures
;           also primitives?
;                 dup and drop!
;                 idx, +, -, *, etc
