;;; Trivial	Memoized Factorial Function

(declaim (optimize (speed 3) (safety 0))
				 (ftype (function (fixnum) integer) fac)
				 (ftype (function (fixnum) fixnum) fib))

(defparameter *fac-cache* (make-hash-table))
(defparameter *fib-cache* (make-hash-table))

(defun fac (n)
	"Memoized factorial helper using hash table *fac-cache*. Takes N, an
integer, and *FAC-CACHE*, a global hash table. Implements a recursive
definition of the factorial which is memoized to optimize performance
on subsequent runs.

Usage: (fac 10) => 3628800"
	(declare (type fixnum n))
	(cond
		((gethash n *fac-cache*))
		((< n 2) 1)
		(t
		 (let ((v (* n (fac (- n 1)))))
			 (setf (gethash n *fac-cache*) v)
			 v))))

(defun fib (n)
	"Memoized fibonacci sequence helper using hash table *fib-cache*. Takes N, an
integer, and *FIB-CACHE*, a global hash table. Implements a recursive
definition of the fibonacci sequence which is memoized to optimize performance
on subsequent runs.

Usage: (fib 10) => 89"
	(declare (type fixnum n))
	(cond
		((gethash n *fib-cache*))
		((< n 2) 1)
		(t
		 (let ((v (+ (fib (- n 1)) (fib (- n 2)))))
			 (setf (gethash n *fib-cache*) v)
			 v))))

(defun main ()
	(time (fac 10000)))
