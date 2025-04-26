(defpackage :scanner
  (:use :cl :tokens :parse-float)
  (:import-from #:alexandria #:plist-hash-table)
  (:export #:scan))

(in-package :scanner)

(defparameter *keywords*
  (alexandria:plist-hash-table '("and" t-and
				 "class" t-class
				 "else" t-else
				 "false" t-false
				 "fun" t-fun
				 "for" t-for
				 "if" t-if
				 "nil" t-nil
				 "or" t-or
				 "print" t-print
				 "return" t-return
				 "super" t-super
				 "this" t-this
				 "true" t-true
				 "var" t-var
				 "while" t-while)
			       :test #'equal))

(defparameter *parsing* nil)
(defparameter *on-error* nil)
(defparameter *start* 0)
(defparameter *current* 0)
(defparameter *line* 0)
(defparameter *tokens* nil)

(defun at-endp ()
  (>= *current* (length *parsing*)))

(defun advance ()
  (let ((ret (aref *parsing* *current*)))
    (incf *current*)
    ret))

(defun match-p (expected)
  (cond ((at-endp) nil)
	(t (cond ((not (char= (aref *parsing* *current*) expected)) nil)
		 (t (incf *current*)
		    t)))))

(defun alpha-p (c)
  (let ((code (char-code c)))
    (or (<= (char-code #\a) code (char-code #\z))
	(<= (char-code #\A) code (char-code #\Z))
	(char= c #\_))))

(defun alpha-numeric-p (c)
  (or (digit-char-p c) (alpha-p c)))

(defun peek ()
  (if (at-endp)
      #\Nul
      (aref *parsing* *current*)))

(defun peek-next ()
  (if (>= (1+ *current*) (length *parsing*))
      #\Nul
      (aref *parsing* (1+ *current*))))

(defun add-token (t-type &optional (literal nil))
  (vector-push-extend (make-token :type t-type :lexeme (subseq *parsing* *start* *current*)
				  :literal literal :line *line*)
		      *tokens*))

(defun string-literal ()
  (loop while (and (not (char= #\" (peek)))
		   (not (at-endp)))
	do (if (char= #\Newline (peek)) (incf *line*))
	   (advance))

  (when (at-endp)
      (funcall *on-error* *line* "Unterminated string.")
      (return-from string-literal))

  (advance)
  (add-token 't-string (subseq *parsing* (1+ *start*) (1- *current*))))

(defun number-literal ()
  (loop while (digit-char-p (peek))
	do (advance))

  (when (and (char= (peek) #\.) (digit-char-p (peek-next)))
    (advance)
    (loop while (digit-char-p (peek))
	  do (advance)))

  (add-token 't-number (parse-float *parsing* :start *start* :end *current*)))

(defun identifier ()
  (loop while (alpha-numeric-p (peek))
	do (advance))
  (let* ((val (subseq *parsing* *start* *current*))
	 (tok (gethash val *keywords*)))
    (add-token (if tok tok 't-identifier))))

(defun scan-token ()
  (let ((c (advance)))
    (cond ((char= c #\() (add-token 't-left-paren))
	  ((char= c #\)) (add-token 't-right-paren))
	  ((char= c #\{) (add-token 't-left-brace))
	  ((char= c #\}) (add-token 't-right-brace))
	  ((char= c #\,) (add-token 't-comma))
	  ((char= c #\.) (add-token 't-dot))
	  ((char= c #\-) (add-token 't-minus))
	  ((char= c #\+) (add-token 't-plus))
	  ((char= c #\;) (add-token 't-semicolon))
	  ((char= c #\*) (add-token 't-star))
	  ((char= c #\!) (add-token (if (match-p #\=) 't-bang-equal 't-bang)))
	  ((char= c #\=) (add-token (if (match-p #\=) 't-equal-equal 't-equal)))
	  ((char= c #\<) (add-token (if (match-p #\=) 't-less-equal 't-less)))
	  ((char= c #\>) (add-token (if (match-p #\=) 't-greater-equal 't-greater)))
	  ((char= c #\/) (if (match-p #\/)
			     (loop while (and (not (char= (peek) #\Newline))
					      (not (at-endp)))
				   do (advance))
			     (add-token 't-slash)))
	  ((member c '(#\Space #\Tab #\Return) :test #'char=) nil)
	  ((char= c #\Newline) (incf *line*))
	  ((char= c #\") (string-literal))
	  ((digit-char-p c) (number-literal))
	  ((alpha-p c) (identifier))
	  (t (funcall *on-error* *line* "unexpected character."))
	  )))

(defun scan (str on-error)
  (let ((*parsing* str)
	(*on-error* on-error)
	(*start* 0)
	(*current* 0)
	(*line* 1)
	(*tokens* (make-array 16 :fill-pointer 0 :adjustable t)))
    (loop while (not (at-endp))
	  do (setf *start* *current*)
	     (scan-token)
	  finally (vector-push-extend (make-token :type 't-eof :lexeme ""
						  :literal nil :line *line*) *tokens*))
    *tokens*))
