;;; db.lisp ---	Database for Practical  Common Lisp
;;
;; Filename: db.lisp
;; Description:
;; Author:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Implements the database described in Practical Common Lisp Ch3.
;; Code is modified to work for tracking a library of digital books in
;; an efficient manner.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;
;; Initialization ;;
;;;;;;;;;;;;;;;;;;;;

(defvar *db* nil)

(defvar *book-fields* ;;	Generalize data to a macro TODO
	'(:title
		:author
		:path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Housekeeping Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-book (title author path)
	"Adds a BOOK object to the database. This needs to be updated to use
a class or struct instead, along with file-parsing capabilities for
the document format."
	(list :title title :author author :path path))

(defun add-record (book)
	"Adds a book to our overall database list."
	(push book *db*))

(defun dump-db ()
	"Pretty print the existing entries in *db*"
	(dolist (book *db*)
		(format t "~{~a:~10t~a~%~}~%" book)))

(defun prompt-read (prompt)
	(format *query-io* "~a: " prompt)
	(force-output *query-io*)
	(read-line *query-io*))

(defun prompt-for-book ()
	(make-book
	 (prompt-read  "Title")
	 (prompt-read "Author")
	 (prompt-read "Path")))

(defun add-books ()
	(loop while (y-or-n-p "Add a Book?")
				do (add-record (prompt-for-book))))

;;;;;;;;;;;;;;;;;;;
;; IO and Saving ;;
;;;;;;;;;;;;;;;;;;;

(defun save-db (filename)
  (with-open-file (out filename
											 :direction :output
											 :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; (defun search-db (query &optional field)
;; 	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; db.lisp ends here
