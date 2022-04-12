
int fib(n) {
	if (n == 0) {
		return 1;
	} else if (n == 1) {
		return 1;
	} else {
		int r1 = fib(n-1);
		int r2 = fib(n-2);
		return r1 + r2;
	}
}
int main(int argc, char **argv) {
	printf("%d\n", fib(atoi(argv[1])));
	return 0;
}
