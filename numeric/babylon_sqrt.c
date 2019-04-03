#include <stdio.h>

#define	iter(x, y) ((0.5) * (x + (y / x)))

double sqrt(double num) {
	double guess = num / 2;
	double tmp = 1;
	while (tmp != guess) {
		guess	=	tmp;
		tmp = iter(guess, num);
	}
	return guess;
}

int main()
{
	const int iters = 100;
	for (int i = 2; i <= iters; ++i) {
		double k = sqrt(i);
		printf("%f\n", k);
	}
	return 0;
}
