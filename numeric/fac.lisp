;;; Trivial	Memoized Factorial Function

(ql:quickload :sb-posix)

(defun fac-h (n cache)
	"Memoized factorial helper using hash table cache. Takes N, an
integer, and CACHE, a hash table. Implements a recursive definition.

Usage: (fac-h 10 cache) => 3628800"
	(declare (optimize (speed 3))
					 (type fixnum n))
	(cond
		((gethash n cache))
		((< n 2) 1)
		(t
		 (let ((v (* n (fac (- n 1)))))
			 (setf (gethash n cache) v)
			 v))))

(defun fac (n)
	"Memoized factorial function, using a local hash table. Most of the
overhead in this function comes from creating the local hash table."
	(let ((cache (make-hash-table)))
		(fac-h n cache)))

(defun main ()
	(print (fac 10000))
	(sb-posix:exit 0))
