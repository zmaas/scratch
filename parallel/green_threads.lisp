;; Playing around with green-threads

(ql:quickload 'green-threads)

(defun in_circle (x y)
	(<= (+ (expt x 2) (expt y 2)) 1))

(defun pi-iter ()
	(in-circle (random 1.0) (random 1.0)))

(defun pll_print (x)
	(green-threads:with-green-thread
		(print x)))

(loop for i from 0 to 10 do (pll_print i))
