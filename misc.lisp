(in-package #:petulant)

;; I'm not sure why AWHILE isn't in the Anaphora package, but... whatev.

(defmacro awhile (test &body body)
  "Evaluate TEST, binding its value to IT.  So long as IT is non-NIL, BODY
is evaluated in that context."
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

;;; This used to be a function, but if you think about it, there's no
;;; advantage here to going through an extra set of lambda &rest parsing
;;; and ginning up an (apply #'concatenate ...) here over a compile-time
;;; rewrite into the function that does all the heavy lifting anyway.

(defmacro strcat (&rest strings)
  "Concatenate all string arguments together, returning a new string."
  `(concatenate 'string ,@strings))

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
