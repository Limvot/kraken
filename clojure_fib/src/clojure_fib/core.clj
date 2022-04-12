(ns clojure-fib.core
  (:gen-class))

(defn fib
  [n]
  (cond (= n 0) 1
        (= n 1) 1
        :else   (+ (fib (- n 1)) (fib (- n 2)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Enter to fib: ")
  (println "was " (fib (Integer/parseInt (read-line)))))
