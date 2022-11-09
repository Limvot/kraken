#!/usr/bin/env bash
#{
# Thanks to http://rosettacode.org/wiki/Multiline_shebang#PicoLisp
exec pil $0 $1
# }#

(de safe (Q D S) (let (C (car S))
                      (case C 
                        (NIL T)
                        (T  (and (<> Q C) (<> Q (+ C D)) (<> Q (- C D)) (safe Q (+ D 1) (cdr S)) ))
                      )
                 )
)

(de appendS (Q S X) (cond ((<= Q 0) X)
                         ((safe Q 1 S) (appendS (- Q 1) S (cons (cons Q S) X)))
                         (T            (appendS (- Q 1) S X) )
                   )

)

(de extendS (Q A X) (let (S (car X))
                        (case S 
                          (NIL A)
                          (T (extendS Q (appendS Q S A) (cdr X)))
                        )
                    )
)

(de findS (N Q) (cond ((= 0 Q) (cons (cons NIL NIL) NIL))
                     (T     (extendS N NIL (findS N (- Q 1))))))

(de nqueens (N) (length (findS N N)))

(bye (println (nqueens (car (str (opt))))))


