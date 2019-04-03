package main

import (
	"fmt"
	"math"
)

// Sqrt calculates the square root of a float64.
func Sqrt(x float64) float64 {
	var z = x / 2
	for math.Abs(x-(z*z)) > 1e-11 {
		z -= (z*z - x) / (2 * z)
	}
	return z
}

func main() {
	const iters float64 = 100
	var arr [int(iters)]float64

	for i := 2.0; i < iters; i++ {
			arr[int(i)] = Sqrt(i)
	}

	for i := 2.0; i < iters; i++ {
		fmt.Println(arr[int(i)])
	}

}
