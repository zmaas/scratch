(defun range (max &key (min 0) (step 1))
	(loop for i from min below max by step
				collect i))

(defun eu1 ()
	(declare (optimize (speed 3)))
	(print
	 (reduce '+
					 (remove-if-not
						#'(lambda (x) (or
											(= 0 (rem x 3))
											(= 0 (rem x 5))))
						(range 10001)))))

(defun coin_check (TRIES)
	(print "Checking Randomness")
	(print (/ (reduce '+ (loop for i below
																	 (+ TRIES 1) collect
																	 (random 2))) TRIES)))

(defun get-input (prompt)
	(print prompt)
	(read))

(defun pkg-init ()
	"Use quicklisp to load needed packages"
	(ql:quickload :drakma)
	(ql:quickload :flexi-streams)
	(ql:quickload :yason))

(defun json-query (api query)
	"Take a (string) api address and an associated (string) query,
returning a lisp hash-table of the returned JSON"
	(declare (type string api query))
	(yason:parse
	 (flexi-streams:octets-to-string
		(drakma:http-request (concatenate 'string api query)))))

(defun json-multi-query (api queries)
	(map 'list #'(lambda (x) (json-query api x)) queries))

(defun get-multi-prop (prop hashlist)
	(map 'list #'(lambda (x) (gethash prop x)) hashlist))

(pkg-init)

;; Snazzy implementation of clojure's thread-first and thread-last
;; macros, based on	https://github.com/nightfly19/cl-arrows

(defun inserter (in-fun)
	"Insert function forms for threading macro"
	;; Create a closure, taking the accumlator value and remaining forms
	;; items
	(lambda (acc next)
		;; List means we have a function left to call, so we do
		(if (listp next)
				;; If we have a list, call the function on the input. This
				;; will be insert-first or insert-last for our threading
				;; macros, so that we return the properly-formed form.
				(funcall in-fun acc next)
				;; Otherwise,	make acc the last of next
				(list next acc))))

(defun insert-first (arg form)
	"Insert arg as the first term of form (between car and cdr)"
	(list* (car form) arg (cdr form)))

(defun insert-last (arg form)
	"Append arg to the list form"
	(append form (list arg)))

(defmacro -> (ini &rest forms)
	"Implementation of clojure's thread-first arrow macro."
	(reduce (inserter #'insert-first)
					forms
					:initial-value ini))

(defmacro ->> (ini &rest forms)
	"Implementation of clojure's thread-last arrow macro."
	(reduce (inserter #'insert-last)
					forms
					:initial-value ini))

;; List Flattening Function
(defun flatten (s)
	"Recursively flatten all sublists into a single list"
	(cond
		;; Nill -> Nil
		((null s) nil)
		;; Atom -> List
		((atom s) (list s))
		;; List -> Recurse
		(t (mapcan #'flatten s))))
