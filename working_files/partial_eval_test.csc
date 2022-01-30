

; Going to set some aliases just for this, the scheme version
; commenting out the first let with it's final ) should make this
; legal kraken
(import (chicken process-context))
(import (chicken port))
(load "partial_eval.csc")
(import (partial_eval))
(let* (
       (array list)
       (concat append)
       (len length)
       (idx list-ref)

       ;(array vector)
       ;(concat vector-append) ; only in extension vector library!
       ;(len vector-length)
       ;(idx vector-ref)

       (= equal?)
      )

(print (array 1 2 3))
(print (command-line-arguments))

(print (call-with-input-string "'(1 2)" (lambda (p) (read p))))

(print partial_eval)


)



