
use std::io;

fn fib(n: i64) -> i64 {
    match n {
        0 => 1,
        1 => 1,
        o => fib(o-1) + fib(o-2),
    }
}

fn main() {
    println!("enter number to fib:");
    let mut buffer = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut buffer).unwrap();
    println!("{}", fib(buffer.trim().parse::<i64>().unwrap()));
}
