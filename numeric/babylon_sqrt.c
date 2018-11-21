#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#define	iter(x, y) ((0.5) * (x + (x / y)))

double sqrt(double num) {
	double guess = num / 2;
	double tmp = 1;
	while (tmp != guess) {
		guess	=	tmp;
		tmp = iter(guess, num);
	}
	return guess;
}

double timeit() {
	struct timespec start, end;

	clock_gettime(CLOCK_MONOTONIC, &start);
	sqrt(34821);
	clock_gettime(CLOCK_MONOTONIC, &end);

	return (end.tv_nsec - start.tv_nsec);
}

int main()
{
	const int iters = 1000000;
	double tot = 0;
	for (int i = 0; i < iters; ++i) {
		tot += timeit();
	}
	tot /= iters;
	printf("Got: %f\n", sqrt(34821));
	printf("Time: %f\n", tot);
	return 0;
}
