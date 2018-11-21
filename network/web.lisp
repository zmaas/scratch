(ql:quickload :hunchentoot)

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

(hunchentoot:define-easy-handler (say-hi :uri "/hi") (name)
	(setf (hunchentoot:content-type*) "text/plain")
	(format nil "Hey~@[ ~A~]!" name))
