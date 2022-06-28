module nqueens
import std/num/int32
import std/os/env


pub fun fib( n : int ) : div int
  match n
    0     -> 1
    1     -> 1
    x     -> fib(x - 1) + fib(x - 2)

pub fun main()
  fib(get-args().head("").parse-int.default(30)).println
