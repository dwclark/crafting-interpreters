(defpackage :parser
  (:use #:cl #:tokens)
  (:export #:parse))

(in-package :parser)

(defparameter *tokens* nil)
(defparameter *current* -1)
(defparameter *error-func* nil)

(define-condition parser-error (error)
  ((text :initarg :text :reader text)))

(defun new-error (token message)
  (funcall *error-func* token message)
  (make-instance 'parser-error :text message))

(defun at-endp ()
  (eq (token-type (peek)) 't-eof))

(defun peek ()
  (aref *tokens* *current*))

(defun previous ()
  (aref *tokens* (1- *current*)))

(defun advance ()
  (if (not (at-endp))
      (incf *current*))
  (previous))

(defun check (token)
  (if (at-endp)
      nil
      (eq (token-type (peek)) token)))

(defun consume (type message)
  (if (check type)
      (advance)
      (error (new-error (peek) message))))

(defun match (&rest list-tokens)
  (loop for token in list-tokens
	do (when (check token)
	     (advance)
	     (return t))
	finally (return nil)))

(defun primary ()
  (cond ((match 't-false) (ast:make-literal :value 't-false))
	((match 't-true) (ast:make-literal :value 't-true))
	((match 't-nil) (ast:make-literal :value nil))
	((match 't-number) (ast:make-literal :value (coerce (token-literal (previous)) 'double-float)))
	((match 't-string) (ast:make-literal :value (token-literal (previous))))
	((match 't-left-paren)
	 (let ((expr (expression)))
	   (consume 't-right-paren "Expect ')' after expression")
	   (ast:make-grouping :expression expr)))
	(t (raise (new-error "Expect expression")))))
	       
(defun unary ()
  (if (match 't-bang 't-minus)
      (let ((operator (previous))
	    (right (unary)))
	(ast:make-unary :operator operator :right right))
      (primary)))

(defun factor ()
  (loop with expr = (unary)
	while (match 't-slash 't-star)
	do (let ((operator (previous))
		 (right (unary)))
	     (setf expr (ast:make-binary :left expr :operator operator :right right)))
	finally (return expr)))

(defun term ()
  (loop with expr = (factor)
	while (match 't-minus 't-plus)
	do (let ((operator (previous))
		 (right (factor)))
	     (setf expr (ast:make-binary :left expr :operator operator :right right)))
	finally (return expr)))

(defun comparison ()
  (loop with expr = (term)
	while (match 't-greater 't-greater-equal 't-less 't-less-equal)
	do (let ((operator (previous))
		 (right (term)))
	     (setf expr (ast:make-binary :left expr :operator operator :right right)))
	finally (return expr)))

(defun equality ()
  (loop with expr = (comparison)
	while (match 't-bang-equal 't-equal-equal)
	do (let ((operator (previous))
		 (right (comparison)))
	     (setf expr (ast:make-binary :left expr :operator operator :right right)))
	finally (return expr)))

(defun expression ()
  (equality))

(defun synchronize ()
  (advance)
  (loop while (not (at-endp))
	do (if (eq (token-type (previous)) 't-semicolon)
	       (return nil))
	   (if (member (token-type (peek)) '(t-class t-for t-fun t-if t-print
					     t-return t-var t-while))
	       (return nil))
	   (advance)))

(defmacro with-context (func tokens error-func)
  `(let ((*tokens* ,tokens)
	 (*current* 0)
	 (*error-func* ,error-func))
     (handler-case
	 (funcall ,func)
       (parser-error (pe) (format t "~A:" (text pe))))))

(defun parse (tokens error-func)
  (with-context #'expression tokens error-func))

