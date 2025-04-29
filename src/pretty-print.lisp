(defpackage :pretty-print
  (:use :cl :tokens :ast)
  (:export #:pretty-print))

(in-package :pretty-print)

(defgeneric pretty-print (e))

(defun parenthesize (name &rest exprs)
  (format nil "(~A~{ ~A~})" name (mapcar #'pretty-print exprs)))

(defmethod pretty-print ((e binary))
  (parenthesize (token-lexeme (binary-operator e))
		(binary-left e)
		(binary-right e)))

(defmethod pretty-print ((e grouping))
  (parenthesize "group" (grouping-expression e)))

(defmethod pretty-print ((e literal))
  (literal-value e))

(defmethod pretty-print ((e unary))
  (parenthesize (token-lexeme (unary-operator e))
		(unary-right e)))

