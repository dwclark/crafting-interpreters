(defpackage :tokens
  (:use #:cl)
  (:export #:make-token #:token-type #:token-lexeme #:token-literal #:token-line))

(in-package :tokens)

(let ((num -1))
  (dolist (sym '(t-left-paren t-right-paren t-left-brace t-right-brace
		 t-comma t-dot t-minus t-plus t-semicolon t-slash t-star
		 
		 t-bang t-bang-equal t-equal t-equal-equal
		 t-greater t-greater-equal t-less t-less-equal
		 
		 t-identifier t-string t-number
		 
		 t-and t-class t-else t-false t-fun t-for t-if t-nil t-or
		 t-print t-return t-super t-this t-true t-var t-while
		 
		 t-eof))
    (setf (symbol-value sym) (incf num))
    (export sym)))

(defstruct token
  (type nil :type symbol :read-only t)
  (lexeme "" :type string :read-only t)
  (literal nil :read-only t)
  (line -1 :type integer :read-only t))
