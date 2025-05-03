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

(test basic-parsing
  (is (equalp (parser:parse (scanner:scan "(400 + 200)" #'lox:print-report) #'lox:print-error)
	      (read-from-string "#S(GROUPING
		 :EXPRESSION #S(BINARY
				:LEFT #S(LITERAL :VALUE 400.0)
				:OPERATOR #S(TOKEN
					     :TYPE T-PLUS
					     :LEXEME \"+\"
					     :LITERAL NIL
					     :LINE 1)
				:RIGHT #S(LITERAL :VALUE 200.0)))")))

  (is (equalp (parser:parse (scanner:scan "4 * 5 + 7" #'lox:print-report) #'lox:print-error)
	      (read-from-string "#S(BINARY
				   :LEFT #S(BINARY
					    :LEFT #S(LITERAL :VALUE 4.0)
					    :OPERATOR #S(TOKEN :TYPE T-STAR :LEXEME \"*\" :LITERAL NIL :LINE 1)
					    :RIGHT #S(LITERAL :VALUE 5.0))
				   :OPERATOR #S(TOKEN :TYPE T-PLUS :LEXEME \"+\" :LITERAL NIL :LINE 1)
				   :RIGHT #S(LITERAL :VALUE 7.0))")))

  (is (equalp (parser:parse (scanner:scan "-(4 + 5)" #'lox:print-report) #'lox:print-report)
	      (read-from-string "#S(UNARY
				   :OPERATOR #S(TOKEN :TYPE T-MINUS :LEXEME \"-\" :LITERAL NIL :LINE 1)
				   :RIGHT #S(GROUPING
					     :EXPRESSION #S(BINARY
							    :LEFT #S(LITERAL :VALUE 4.0)
							    :OPERATOR #S(TOKEN
									 :TYPE T-PLUS
									 :LEXEME \"+\"
									 :LITERAL NIL
									 :LINE 1)
							    :RIGHT #S(LITERAL :VALUE 5.0))))"))))
