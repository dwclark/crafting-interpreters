(defpackage :lox
  (:use :cl :scanner :tokens)
  (:export #:run #:run-file #:print-report #:print-error))

(in-package :lox)

(defparameter *had-error* nil)

(defun print-report (line message &optional (where ""))
  (format t "[line ~A] Error~A: ~A~%" line where message)
  (setf *had-error* t))

(defun print-error (token message)
  (if (eq (token-type token) 't-eof)
      (print-report (token-line token) " at end" message)
      (print-report (token-line token) (format nil " '~A'" (token-lexeme token)) message)))

(defun run (str)
  (let ((*had-error* nil))
    (scanner:scan str #'print-report)))

(defun run-file (file-name)
  (run (with-open-file (stream file-name)
	 (let ((contents (make-string (file-length stream))))
	   (read-sequence contents stream)
	   contents))))

