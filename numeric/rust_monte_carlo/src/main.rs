extern crate rand;
extern crate rayon;
use rayon::prelude::*;

fn main() {
    println!("Calculating PI using Monte Carlo Random Sampling");
    // Number of iterations to run
    let iters: u32 = 1_000_000_000;
    let pi:f32 = calc_pi(iters);
    println!("Pi: {}", pi);
}

fn calc_pi(iters: u32) -> f32 {
    // Accumulator for number of points in the circle
    let count = (0..iters)
        .into_par_iter()
        .filter_map(|_| {
        let x = rand::random::<f32>() * 2f32 - 1f32;
        let y = rand::random::<f32>() * 2f32 - 1f32;
        if square(x) + square(y) <= 1.0 {Some(1)} else {None}
    }).count();
    return 4.0 * (count as f32 / iters as f32);
}

fn square(x: f32) -> f32 {
    return x * x;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_square_works() {
        assert_eq!(square(5.0), 25.0);
        assert_eq!(square(-3.0), 9.0);
    }

    #[test]
    fn test_main_calculates_pi() {
        assert!(calc_pi(1_000_000) - 3.1415926535 < 0.01);
    }
}
