#!/usr/bin/env bash
#{
# Thanks to http://rosettacode.org/wiki/Multiline_shebang#PicoLisp
exec pil $0 $1
# }#



(de fib (n) (cond ((= 0 n)   1)
                  ((= 1 n)   1)
                  (1         (+ (fib (- n 1)) (fib (- n 2))))))

(bye (println (fib (car (str (opt))))))
