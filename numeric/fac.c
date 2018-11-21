#include <stdio.h>

long factorial(long i, long lookup_table[]) {
	long temp;
	if (lookup_table[i]) {
		return lookup_table[i];
	}
	if (i == 0 || i == 1) {
		return 1;
	} else {
		temp = i * factorial(i-1, lookup_table);
		lookup_table[i] = temp;
		return temp;
	}
}

int main()
{
	static long lookup_table[100];
	long ans = factorial(25, lookup_table);
	printf("Got: %ld\n", ans);
	return 0;
}
