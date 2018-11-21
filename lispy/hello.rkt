#lang racket

(define (reduce f xs)
  (and (not (empty? xs)) (foldl f (first xs) (rest xs))))

(reduce
 + (filter
		(lambda (x) (or
						(= 0 (modulo x 3))
						(= 0 (modulo x 5))))
		(range 1001)))

(random 1 10)
