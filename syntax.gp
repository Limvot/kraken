
; LISP style
(do
    (println (+ 2 3))
    (println (- 2 3))
    (println (* 2 3))
    (println (/ 2 3))
    (println "howdy")
)

; let's add some stuff to our grammer...
(add_grammer_rule :semi_forms [:form ";"] (fn* (xs) (list (first xs))))
(add_grammer_rule :semi_forms [:form ";" :optional_WS :semi_forms ] (fn* (xs) (cons (first xs) (nth xs 3))))
(add_grammer_rule :form ["{" :optional_WS :semi_forms :optional_WS "}"] (fn* (xs) `(do ,(nth xs 2))))
(add_grammer_rule :form [:atom :form ] (fn* (xs) `(~(first xs) ,(nth xs 1))))
(add_grammer_rule :form [:form :optional_WS "$" :atom :optional_WS :form] (fn* (xs) `(~(nth xs 3) ~(nth xs 0) ~(nth xs 5))))

; C style
{
    println( 2 $+ 3 );
    println( 2 $- 3 );
    println( 2 $* 3 );
    println( 2 $/ 3 );
    println("howdy");
}

; combo
(println (list 1 2 4 $+ 5))
{ println( (+ 2 3) ); }
