package main

import (
	"fmt"
	"math"
)

// Sqrt calculates the square root of a float64.
func Sqrt(x float64) float64 {
	var z = x	/ 2;
	for math.Abs(x - (z * z)) > 1e-11 {
		z -= (z * z - x) / (2 * z);
	};
	return z;
}

func main() {
	const	iters = 10000;

	for i := 2.0; i < iters; i++ {
		go fmt.Println(Sqrt(i));
	}

}
