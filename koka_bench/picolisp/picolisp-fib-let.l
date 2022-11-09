#!/usr/bin/env bash
#{
# Thanks to http://rosettacode.org/wiki/Multiline_shebang#PicoLisp
exec pil $0 $1
# }#



(de fib (N) (cond ((= 0 N)   1)
                  ((= 1 N)   1)
                  (T         (let (A (fib (- N 1))
				                   B (fib (- N 2))
								   ) (+ A B)))))

(bye (println (fib (car (str (opt))))))
