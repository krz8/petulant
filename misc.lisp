(in-package #:petulant)

(defun make-option-fixer (&optional flavor)
  "Returns a function that \"fixes up\" a single argument (a character
or a string) for use in a returned list of options seen on a command
line.  If its argument is a character, that character is to be
returned; if its argument is a string longer than one character, that
string is to be returned; if its argument is a string of a single
character, that character is returned.

   (FUNCALL (MAKE-OPTION-FIXER) #\\f)   => #\\f
   (FUNCALL (MAKE-OPTION-FIXER) \"foo\") => \"foo\"
   (FUNCALL (MAKE-OPTION-FIXER) \"f\")   => #\\f
   (FUNCALL (MAKE-OPTION-FIXER) \"\")    => \"\"

When FLAVOR is :STRING, all single characters are returned as strings
instead of the behavior above.

   (FUNCALL (MAKE-OPTION-FIXER :STRING) #\\f)   => \"f\"
   (FUNCALL (MAKE-OPTION-FIXER :STRING) \"foo\") => \"foo\"
   (FUNCALL (MAKE-OPTION-FIXER :STRING) \"f\")   => \"f\"
   (FUNCALL (MAKE-OPTION-FIXER :STRING) \"\")    => \"\"

When FLAVOR is :UP or :DOWN, the returned function will also upcase or
downcase its argument.

   (FUNCALL (MAKE-OPTION-FIXER :UP) #\\f)   => #\\F
   (FUNCALL (MAKE-OPTION-FIXER :UP) #\\F)   => #\\F
   (FUNCALL (MAKE-OPTION-FIXER :UP) \"Foo\") => \"FOO\"
   (FUNCALL (MAKE-OPTION-FIXER :DOWN) \"F\") => #\f
   (FUNCALL (MAKE-OPTION-FIXER :DOWN) \"\")  => \"\"

When FLAVOR is :KEY, the returned function will convert its argument
into an uppercase keyword symbol.

   (FUNCALL (MAKE-OPTION-FIXER :KEY) #\\f)   => :F
   (FUNCALL (MAKE-OPTION-FIXER :KEY) #\\F)   => :F
   (FUNCALL (MAKE-OPTION-FIXER :KEY) \"Foo\") => :FOO
   (FUNCALL (MAKE-OPTION-FIXER :KEY) \"F\")   => :F
   (FUNCALL (MAKE-OPTION-FIXER :KEY) \"\")    => NIL     ; not :||

Generally speaking, existing conventions in Unix CLI environments
would set FLAVOR to :STRING or NIL, while Windows CLI environments
would usually prefer :KEY or :UP due to the prevalence of case
folding.  My advice is that when you are writing utilties to run in
both environments, unless you're reinventing ls(1), avoid case
distinction and use :KEY."
  (labels ((plain (x)
	     (declare (type (or character string) x))
	     (cond
	       ((characterp x)
		x)
	       ((= (length x) 1)
		(char x 0))
	       (t
		x)))
	   (stringify (x)
	     (declare (type (or character string) x))
	     (string x))
	   (upcase (x)
	     (declare (type (or character string) x))
	     (let ((y (plain x)))
	       (cond
		 ((characterp y)
		  (char-upcase y))
		 (t
		  (string-upcase y)))))
	   (downcase (x)
	     (declare (type (or character string) x))
	     (let ((y (plain x)))
	       (cond
		 ((characterp y)
		  (char-downcase y))
		 (t
		  (string-downcase y)))))
	   (keyify (x)
	     (declare (type (or character string) x))
	     (cond
	       ((characterp x)
		(intern (string (char-upcase x)) "KEYWORD"))
	       ((zerop (length x))
		nil)
	       (t
		(intern (string-upcase x) "KEYWORD")))))
    (case flavor
      (:string #'stringify)
      (:up #'upcase)
      (:down #'downcase)
      (:key #'keyify)
      (t #'plain))))

;; This could still be improved so that there is a single test against
;; the string's length; that is, just one call to >, not one for every
;; variable binding.  But that makes this macro waaaaaay longer for a
;; marginal improvement, and this operation just doesn't happen often
;; enough to warrant it. (Yet.)

(defmacro with-chars ((&rest vars) string &body body)
  "Evaluate BODY after binding the variables in VARS to the first characters
of the STRING.  The variables are NIL when STRING is not long enough.

   (LET ((STR \"foo\"))
     (WITH-CHARS (A B C D) STR
       (LIST A B C D)))
=> (#\\f #\\o #\\o NIL)"
  (let ((len (gensym)))
    `(let* ((,len (or (and ,string (length ,string)) 0))
	    ,@ (do* ((v vars (cdr v))
		     (i 0 (1+ i))
		     (r))
		    ((null v) r)
		 (push `(,(car v) (and (> ,len ,i) (char ,string ,i))) r)))
       ,@body)))

(defun fold? (flavor)
  "Returns NIL unless FLAVOR is one of :UP, :DOWN, or :KEY.  In other
words, if we're handed a flavor that requires us to perform case
insensitive matching, recognize it here."
  (member flavor '(:up :down :key)))

(defun chars-and-strings (list &optional flavor)
  "Given a list of chars and strings, return two values.  The first
value is a list of all the characters in LIST, and the second value is
a list of all the strings in LIST.  Order is not preserved.  The
values of the returned lists are folded to uppercase when FLAVOR is
one of :UP, :DOWN, or :KEY."
  (let (chars strings (fold? (fold? flavor)))
    (mapc #'(lambda (x) (if (characterp x)
			    (push (if fold? (char-upcase x) x) chars)
			    (push (if fold? (string-upcase x) x) strings)))
	  list)
    (values chars strings)))

(defun make-optwitharg-tester (list &optional flavor)
  "Given a list of strings and characters describing options that take
arguments, return a function that returns NIL unless its argument is
in that list.  Matching is case-sensitive, unless FLAVOR is one of :UP,
:DOWN, or :KEY.  The returned function's closure may share some
elements with LIST, so take care not to modify those elements."
  (let ((fold? (fold? flavor)))
    (multiple-value-bind (chars strings) (chars-and-strings list flavor)
      (if fold?
	  #'(lambda (x)
	      (if (characterp x)
		  (member (char-upcase x) chars :test #'char=)
		  (member (string-upcase x) strings :test #'string=)))
	  #'(lambda (x)
	      (if (characterp x)
		  (member x chars :test #'char=)
		  (member x strings :test #'string=)))))))
