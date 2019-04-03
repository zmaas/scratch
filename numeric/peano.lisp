;; A playground using set theoretic mathematics

;; Zero is the null set
(defvar *0* '())

;; A simple implementation of the successor operation to construct our
;; arithmetic.
(defun S (x)
	"The successor operation is defined as S(x) = x U {x}"
	(declare (type list x))
	(list x (list x)))

;; Further natural numbers
(defun make-peano (x)
	"Generate symbols for peano numbers up to x. This generates a set of
symbols *1*, *2*, *3*, ..., *x* for use in peano arithmetic."
	(assert (and (numberp x) (> x 0)))
	(let ((nval (intern (concatenate 'string "*" (write-to-string x) "*")))
				(nmin (intern (concatenate 'string "*" (write-to-string (- x 1)) "*"))))
		(eval `(defparameter ,nval (S ,nmin)))))

(defun gen-peano-up-to (x)
	"Generate every peano number from 1 to x, and generate it as a
global symbol"
	(declare (type number x))
	(mapcar 'make-peano
					 (loop for i from 1 to x collect i)))

;; Some other arithmetic
(defun dec (x)
	"A decrement function with the condition that we may not subtract
below zero"
	(declare (type list x))
	(if (eq x nil)
			nil
			(car x)))

;; (defun plus (x y)
;; 	)

(gen-peano-up-to 10)
