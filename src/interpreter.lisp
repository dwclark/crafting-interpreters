(defpackage :interpreter
  (:use :cl :ast :tokens)
  (:export #:interpret #:runtime-error #:text #:token))

(in-package :interpreter)

(define-condition runtime-error (error)
  ((text :initarg :text :reader text)
   (token :initarg :token :reader token)))

(defun truthy (v)
  (if (or (null v) (eq 't-false v))
      nil
      t))

(defun check-number-operands (op left right)
  (if (and (typep left 'double-float) (typep right 'double-float))
      t
      (error 'runtime-error :token op :text "Operands must be numbers")))

(defgeneric interpret (e))

(defmethod interpret ((e literal))
  (literal-value e))

(defmethod interpret ((e grouping))
  (interpret (grouping-expression e)))

(defmethod interpret ((e unary))
  (let ((right (interpret (unary-right e))))
    (case (token-type (unary-operator e))
      (t-minus (- right))
      (t-bang (not (truthy right))))))

(defmethod interpret ((e binary))
  (let ((left (interpret (binary-left e)))
	(op (binary-operator e))
	(right (interpret (binary-right e))))
    (case (token-type op)
      (t-minus
       (check-number-operands op left right)
       (- left right))

      (t-slash
       (check-number-operands op left right)
       (/ left right))
      
      (t-star
       (check-number-operands op left right)
       (* left right))
      
      (t-plus
       (cond ((and (typep left 'number) (typep right 'number))
	      (check-number-operands op left right)
	      (+ left right))
	     ((and (typep left 'string) (typep right 'string))
	      (concatenate 'string left right))
	     (t (error 'runtime-error :token op :text "Operands must be both numbers or strings"))))
      
      (t-greater
       (check-number-operands op left right)
       (> left right))

      (t-greater-equal
       (check-number-operands op left right)
       (>= left right))
      
      (t-less
       (check-number-operands op left right)
       (< left right))
      
      (t-less-equal
       (check-number-operands op left right)
       (<= left right))
      
      (t-bang-equal
       (check-number-operands op left right)
       (not (equal left right)))
      (t-equal-equal
       (check-number-operands op left right)
       (equal left right)))))

