(defpackage :ast
  (:use :cl :tokens)
  (:export #:binary #:make-binary #:binary-left #:binary-operator #:binary-right
	   #:grouping #:make-grouping #:grouping-expression
	   #:literal #:make-literal #:literal-value
	   #:unary #:make-unary #:unary-operator #:unary-right))

(in-package :ast)

(defstruct expr)

(defstruct (binary (:include expr))
  (left 'expr)
  (operator 'token)
  (right 'expr))

(defstruct (grouping (:include expr))
  (expression 'expr))

(defstruct (literal (:include expr))
  value)

(defstruct (unary (:include expr))
  (operator 'token)
  (right 'expr))

