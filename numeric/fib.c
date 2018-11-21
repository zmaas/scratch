#include <stdio.h>

long fib(long i, long cache[]) {
	long temp;
	if (cache[i]) {
		return cache[i];
	}
	if (i == 0 || i == 1) {
		return 1;
	}
	temp = i + fib(i-1, cache);
	cache[i] = temp;
	return temp;
}

int main()
{
	static long cache[100000];
	long test = fib(2, cache);
	printf("Got: %ld\n", test);
	return 0;
}
