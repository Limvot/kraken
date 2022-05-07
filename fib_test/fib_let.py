import sys
def fib(n):
    if n == 0:
        return 1
    elif n == 1:
        return 1
    else:
        r1 = fib(n-1)
        r2 = fib(n-2)
        return r1 + r2
print(fib(int(sys.argv[1])))
