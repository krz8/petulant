(in-package #:petulant)

;; I'm not sure why AWHILE isn't in the Anaphora package, but... whatev.

(defmacro awhile (test &body body)
  "Evaluate TEST, binding its value to IT.  So long as IT is non-NIL, BODY
is evaluated in that context."
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

(defun strcat (&rest strings)
  "Concatenate all string arguments together, returning a new string."
  (apply #'concatenate 'string strings))

(defun slashify (x)
  "Prefix the string X with a slash character, unless it already has
one, or is an empty string."
  (cond
    ((zerop (length x))
     x)
    ((char= #\/ (char x 0))
     x)
    (t
     (strcat "/" x))))

(defun stringify (x)
  "Returns a string based on X.  NIL becomes an empty string.
Characters are converted to strings of length 1.  Keywords are
returned as upper-case strings.  Numbers are converted to decimal
strings."
  (cond
    ((stringp x) x)
    ((null x) "")
    ((characterp x) (string x))
    ((numberp x) (format nil "~d" x))
    ((symbolp x) (symbol-name x))
    (t (format nil "~s" x))))		; hope for the best

(defun wc/make-setfs (n string vars)
  "Generate the middle SETF forms for a case clause in the WITH-CHARS
macro.  Effectively, where N is a case of the length of the string
being dissected, this turns out as many SETF clauses of the variables
named in VARS that can be resolved against the length of STRING.  When
N is 0, a dangling SETF with no forms is avoided and NIL is returned.

   \(wc/make-setfs 0 'foo '\(a b c\)\)
=> NIL
   \(wc/make-setfs 1 'foo '\(a b c\)\)
=> \(SETF A \(CHAR FOO 0\)\)
   \(wc/make-setfs 3 'foo '\(a b c\)\)
=> \(SETF A \(CHAR FOO 0\) B \(CHAR FOO 1\) C \(CHAR FOO 2\)\)"
  (when (> n 0)
    `(setf ,@(iterate (for i from 0 below n)
		      (for v in vars)
		      (appending `(,v (char ,string ,i)))))))

(defun wc/make-case-clause (n string vars)
  "Generate a clause for the case statement inside the WITH-CHARS macro.
Specifically, the clause to be executed when the string's length is N.
When N is equal to, or greater than, the number of variable named in
the list VARS, a default case clause (T) is emitted.

   \(wc/make-case-clause 0 'foo '\(a b c\)\)
=> \(0 NIL\)
   \(wc/make-case-clause 1 'foo '\(a b c\)\)
=> \(1 \(SETF A \(CHAR FOO 0\)\)\)
   \(wc/make-case-clause 2 'foo '\(a b c\)\)
=> \(2 \(SETF A \(CHAR FOO 0\) B \(CHAR FOO 1\)\)\)
   \(wc/make-case-clause 3 'foo '\(a b c\)\)
=> \(T \(SETF A \(CHAR FOO 0\) B \(CHAR FOO 1\) C \(CHAR FOO 2\)\)\)
   \(wc/make-case-clause 4 'foo '\(a b c\)\)
=> \(T \(SETF A \(CHAR FOO 0\) B \(CHAR FOO 1\) C \(CHAR FOO 2\)\)\)

Note that at 3, which is the largest value that '\(A B C\) permits,
the case selector turns to T."
  `(,(if (>= n (length vars)) t n)
    ,(wc/make-setfs n string vars)))

(defmacro with-chars ((&rest vars) string &body body)
  "Evaluate BODY after binding the variables in VARS to the first characters
of the STRING.  The variables are NIL when STRING is not long enough.

   (WITH-CHARS (A B C D) \"foo\"
     (LIST A B C D))
=> (#\\f #\\o #\\o NIL)"
  (let ((nvars (length vars)))
    (once-only (string)
      `(let ,vars
	 (case (length ,string)
	   ,@(iterate
	       (for i from 0 to nvars)
	       (collect (wc/make-case-clause i string vars))))
	 ,@body))))

(defun split (char-bag string)
  "Return a list of strings that are subsequences of STRING, splitting
it up on boundaries where any character in the list CHAR-BAG match.
This is a lot like SPLIT-SEQUENCE, possibly faster depending on the
Lisp system, but at the cost of some bells and whistles.

   \(SPLIT '\(#\Space #\Tab #\Newline #\Return\) \"  hello,  world  \"\)
=> \(\"hello,\" \"world\"\)"
  (flet ((bagp (ch) (member ch char-bag)))
    (cond
      ((zerop (length string))
       '(""))
      ((not (position-if #'bagp string))
       (list string))
      (t
       (let ((str (string-trim char-bag string))
	     (i 0) (res))
	 (awhile (position-if #'bagp str :start i)
	   (push (subseq str i it) res)
	   (setq i (position-if (lambda (ch) (not (bagp ch))) str :start it)))
	 (when i
	   (push (subseq str i) res))
	 (nreverse res))))))

(defparameter *ws* '(#\Space #\Tab #\Newline #\Return #\Page)
  "A list of common whitespace characters.")

"~{~<~%~1:;~a~>~^ ~}~%~%"

(defun par (text &key (stream *standard-output*))
  "Splits the string TEXT up into individual words, and formats them
into a nicely wrapped paragraph."
  (format stream "~{~<~%~1:;~a~>~^ ~}~%~%" (split *ws* text)))

(defun hanging-par (label text
		    &key (stream *standard-output*) (eol "~%") indentlength)
  "Presents a hanging paragraph onto STREAM.  The exdented text,
starting at the beginning of the first line, is supplied by the LABEL
string.  The TEXT string is broken up on whitespace boundaries and
flowed onto the remainder of the line until the right margin is
encountered.  Remaining words are placed on as many subsequent lines
as necessary, each of those lines indented by spaces.  The number of
spaces used to indent all remaining lines is given by INDENTLENGTH; if
that argument is not provided, the width of LABEL is used instead."
  (let* ((spaces (make-string (or indentlength (length label))
			      :initial-element #\Space))
	 (words (split *ws* text))
	 (format (strcat "~a~{~<" eol spaces "~1:;~a~>~^ ~}" eol)))
    (format stream format label words)))

(defun pad (string minlength &optional (minpad 2))
  "Return a new string that is STRING but with spaces appended in
order to make its length equal to MINLENGTH.  At least MINPAD spaces
appears at the right of STRING, no matter how long the resulting
string is.  This is used to set off a tag in a paragraph with a
hanging tag, as in an option in a usage message.

   \(PAD \"foo\" 8) => \"foo     \"
   \(PAD \"foo\" 5) => \"foo  \"
   \(PAD \"foo\" 2) => \"foo  \"

   \(PAD \"blah\" 3 0\) => \"blah\" 
   \(PAD \"blah\" 3 1\) => \"blah \" 
   \(PAD \"blah\" 3 2\) => \"blah  \" 
   \(PAD \"blah\" 4 0\) => \"blah\" 
   \(PAD \"blah\" 4 1\) => \"blah \" 
   \(PAD \"blah\" 4 2\) => \"blah  \" 
   \(PAD \"blah\" 5 0\) => \"blah \" 
   \(PAD \"blah\" 5 1\) => \"blah \" 
   \(PAD \"blah\" 5 2\) => \"blah  \" 
   \(PAD \"blah\" 6 0\) => \"blah  \" 
   \(PAD \"blah\" 6 1\) => \"blah  \" 
   \(PAD \"blah\" 6 2\) => \"blah  \""
  (let ((str (string-right-trim *ws* string)))
    (strcat str (make-string (max minpad (- minlength (length str)))
			     :initial-element #\Space))))

(defgeneric collecting (function container)
  (:documentation "COLLECTING calls FUNCTION for every conceptual
element of CONTAINER, gathering the results into a list, which is
returned.  By \"conceptual element\", we mean e.g., a pair \(key and
value\) for a hash table, an individual element for a sequence, a
character for a string, and so on.  Method specialization will be
performed only for CONTAINER; FUNCTION is always assumed to be a
function of the proper arguments."))

(defmethod collecting (function (container hash-table))
  "For every key and value pair in the hash table CONTAINER, call
FUNCTION once with those as its arguments.  The results of FUNCTION
are gathered into a list and returned to the caller.  No order can be
expected of the elements of the returned list."
  (declare (type function function))
  (let (results)
    (maphash (lambda (k v) (push (funcall function k v) results)) container)
    results))

(defmethod collecting (function (container sequence))
  "For every element in the sequence CONTAINER, call FUNCTION once
with that as its argument.  The results of FUNCTION are gathered into
a list and returned to the caller."
  (declare (type function function))
  (map 'list function container))

(defmethod collecting (function (container list))
  "For every element in the list CONTAINER, call FUNCTION once with
that as its argument.  The results of FUNCTION are gathered into a
list and returned to the caller."
  (declare (type function function))
  (mapcar function container))
