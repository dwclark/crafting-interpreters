(defpackage :ci-test
  (:use :cl :fiveam :ast :tokens :pretty-print))

(in-package :ci-test)

(def-suite ci-test)
(in-suite ci-test)

(defun pretty-expr ()
  (pretty-print
   (make-binary :left (make-unary :operator (tokens:make-token :type 'tokens:t-minus :lexeme "-" :line 1)
				  :right (make-literal :value 123))
		:operator (make-token :type 'tokens:t-star :lexeme "*" :line 1)
		:right (make-grouping :expression (make-literal :value 45.67)))))

(test test-pretty-print
  (is (string= "(* (- 123) (group 45.67))" (pretty-expr))))
	       
