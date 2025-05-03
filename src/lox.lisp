(defpackage :lox
  (:use :cl :scanner :tokens)
  (:export #:run #:run-file #:print-report #:print-error #:print-runtime-error #:interpret))

(in-package :lox)

(defparameter *had-error* nil)
(defparameter *had-runtime-error* nil)

(defun print-report (line message &optional (where ""))
  (format t "[line ~A] Error~A: ~A~%" line where message)
  (setf *had-error* t))

(defun print-error (token message)
  (if (eq (token-type token) 't-eof)
      (print-report (token-line token) " at end" message)
      (print-report (token-line token) (format nil " '~A'" (token-lexeme token)) message)))

(defun print-runtime-error (re)
  (format t "~A~%[line ~A]" (slot-value re 'interpreter:text) (token-line (slot-value re 'interpreter:token)))
  (setf *had-runtime-error* t))

(defun stringify (val)
  (if (null val) "nil"
      (let ((str (format nil "~A" val)))
	(if (and (typep val 'double-float) (search ".0d0" str))
	    (subseq str 0 (- (length str) 4))
	    str))))

(defun run (str)
  (let ((*had-error* nil))
    (let ((expr (parser:parse (scanner:scan str #'print-report) #'print-error)))
      (if *had-error*
	  (return-from run))
      (interpret expr))))

(defun run-file (file-name)
  (run (with-open-file (stream file-name)
	 (let ((contents (make-string (file-length stream))))
	   (read-sequence contents stream)
	   contents))))

(defun interpret (expr)
  (handler-case
      (let ((*had-runtime-error* nil)
	    (val (interpreter:interpret expr)))
	(format t "~A~%" (stringify val)))
    (interpreter:runtime-error (re)
      (print-runtime-error re))))

