(in-package #:petulant)

;; I'm not sure why AWHILE isn't in the Anaphora package, but... whatev.

(defmacro awhile (test &body body)
  "Evaluate TEST, binding its value to IT.  So long as IT is non-NIL, BODY
is evaluated in that context."
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

(defun fold? (flavor)
  "In a few different places, we need to know if we should be folding
case or not, according to the option processing flavor selected by the
user.  At present, we know that when the user selects one
of :UP, :DOWN, or :KEY, case folding should be performed."
  (member flavor '(:up :down :key)))

(defun make-str= (&optional flavor)
  "Generate and return a function for comparing strings (options)
according to a user preference.  When the user wants to receive all
options in :UP, :DOWN, or :KEY format, it would be least surprising if
we used a case-insensitive string comparison in option parsing;
otherwise, string comparisons should be sensitive to case."
  (or (and (fold? flavor) #'string-equal)
      #'string=))

(defun make-char= (&optional flavor)
  "Generate and return a function for comparing characters according
to a user preference.  When the user wants to receive all options
in :UP, :DOWN, or :KEY format, it would be least surprising if we used
a case-insensitive string comparison in option parsing; otherwise,
string comparisons should be sensitive to case."
  (or (and (fold? flavor) #'char-equal)
      #'char=))

(defun make-string-fixer (&optional flavor)
  "Returns a function that \"fixes up\" its single argument when
returning options seen on a command line to the caller.  When FLAVOR
is NIL, the returned function is like IDENTITY; in other words, the
caller receives the option as found.  When FLAVOR is :UP or :DOWN, the
option is upcased or downcased, respectively.  Finally, FLAVOR can
be :KEY, with which the returned function converts the upcased form of
its argument to a keyword symbol.  In that last case, empty strings
map to NIL.

Case folding is more appropriate under Windows than Unix, as Windows
is traditionally case insensitive at its command line, while Unix is
most often case sensitive.  :KEY implies case insensitivity as well.
Simple processing, though, can use :KEY without surprising the end
user too much.  Unless you're reinventing ls(1), case insensitive
argument processing in Unix-like environments isn't necessarily a bad
thing.

   (FUNCALL (MAKE-STRING-FIXER) \"FoO\")       => \"FoO\"
   (FUNCALL (MAKE-STRING-FIXER) \"\")          => \"\"
   (FUNCALL (MAKE-STRING-FIXER :UP) \"FoO\")   => \"FOO\"
   (FUNCALL (MAKE-STRING-FIXER :DOWN) \"FoO\") => \"foo\"
   (FUNCALL (MAKE-STRING-FIXER :KEY) \"f\")    => :F
   (FUNCALL (MAKE-STRING-FIXER :KEY) \"foo\")  => :FOO
   (FUNCALL (MAKE-STRING-FIXER :KEY) \"FoO\")  => :FOO
   (FUNCALL (MAKE-STRING-FIXER :KEY) \"FOO\")  => :FOO
   (FUNCALL (MAKE-STRING-FIXER :KEY) \"\")     => NIL       ; not :||"
  (case flavor
    (:up   #'(lambda (x) (string-upcase (string x))))
    (:down #'(lambda (x) (string-downcase (string x))))
    (:key  #'(lambda (x) (let ((y (string x)))
			   (unless (zerop (length y))
			     (intern (string-upcase y) "KEYWORD")))))
    (t     #'(lambda (x) (string x)))))

(defun wc/make-setfs (n string vars)
  "Generate the middle SETF forms for a case clause in the WITH-CHARS macro.
Effectively, where N is a case of the length of the string being
dissected, this turns out as many SETF clauses of the variables named
in VARS that can be resolved against the length of STRING.  When N is
0, NIL is returned.

Example return values:  NIL
                        (SETF A (CHAR STRING 0))
                        (SETF A (CHAR STRING 0) B (CHAR STRING 1))
                        (SETF A (CHAR STRING 0) B (CHAR STRING 1)
                              C (CHAR STRING 2))"
  (when (> n 0)
    (list (append '(setf)
	     (iterate
	       (for i from 0 below n)
	       (for v in vars)
	       (appending `(,v (char ,string ,i))))))))

(defun wc/make-case-clause (n string vars)
  "Generate a clause for the case statement inside the WITH-CHARS macro.
Specifically, the clause to be executed when the string's length is N.
When N is equal to, or greater than, the number of variable named in
the list VARS, a default case clause (T) is emitted.

Example return values:  (0)
                        (1 (SETF A (CHAR STRING 0)))
                        (2 (SETF A (CHAR STRING 1) B (CHAR STRING 2)))"
  (append
   (list (if (>= n (length vars)) t n))
   (wc/make-setfs n string vars)))

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

(defun char-or-string (x)
  "When X is a character, or when X is a string whose length is not 1,
it is returned.  When X is a string whose length is 1, the character
making up that string is returned.
   (CHAR-OR-STRING #\\a)   => #\\a
   (CHAR-OR-STRING \"b\")   => #\\b
   (CHAR-OR-STRING \"cee\") => \"cee\"
   (CHAR-OR-STRING \"\")    => \"\""
  (declare (type (or character string) x))
  (cond
    ((characterp x) x)
    ((= (length x) 1) (char x 0))
    (t x)))

(defun chars-and-strings (list)
  "Given a list of characters and strings, return two values.  The
first is a list of characters from LIST, and the second is a list of
strings from LIST.  Any strings in LIST whose length is 1 is rendered
as a single character in the first return value.
   (CHARS-AND-STRINGS '(#\\a \"b\" \"cee\" \"\")
=> (#\\a #\\b)
   (\"cee\" \"\")"
  (let (chars strings)
    (mapc #'(lambda (x) (let ((y (char-or-string x)))
			  (if (characterp y)
			      (push y chars)
			      (push y strings))))
	  list)
    (values chars strings)))

(defun make-optargp (optargs &optional flavor)
  "Given a list of characters and strings specifying options that take
arguments, generate and return a function that can test any single
string or character and return non-NIL when it appears in OPTARGS.
This function treats characters and strings of length 1 equivalently
via CHAR-OR-STRING.  If flavor is :UP, :DOWN, or :KEY, the testing
will be insensitive to case.

The closure containing the returned function shares structure with
OPTARGS, so don't modify it after the fact.

   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") \"foo\") => T
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") \"Foo\") => NIL
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") #\\a) => T
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") \"a\") => T
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") #\\b) => T
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") \"b\") => T
   (FUNCALL (MAKE-OPTARGP '(\"foo\" #\\a \"b\") \"beta\") => NIL"
  (multiple-value-bind (optchars optstrings)
      (chars-and-strings optargs)
    (let ((c= (if (fold? flavor) #'char-equal #'char=))
	  (s= (if (fold? flavor) #'string-equal #'string=)))
      #'(lambda (x) (let ((y (char-or-string x)))
				(if (characterp y)
				    (member y optchars :test c=)
				    (member y optstrings :test s=)))))))

(defun all-truncated-strings (strings)
  "Given a list of STRINGS, return a list of those strings plus all
the truncated variations of those strings.  The returned list shares
no list structure with STRINGS, but the strings it contains are all
subsequences of the strings in STRINGS.

(A side effect of implementation, any empty string or NIL value in
STRINGS will be silently dropped.)

   (ALL-TRUNCATED-STRINGS '(\"alpha\" \"beta\"))
=> (\"a\" \"al\" \"alp\" \"alph\" \"alpha\" \"b\" \"be\" \"bet\" \"beta\")"
  (mapcan #'(lambda (str) (iterate
			    (for i from 1 to (length str))
			    (collect (subseq str 0 i))))
	  strings))

(defun count-strings (strings &optional safe)
  "Given a list of STRINGS, return a new list whose elements are cons
cells.  The car of each cell is a string from STRINGS, and its cdr is
the number of times it appeared in STRINGS.

COUNT-STRINGS may modify the list structure of STRINGS via SORT.  If
it is safe to do so, set SAFE to TRUE.  Otherwise, COUNT-STRINGS will
create an internal copy of STRINGS in order to preserve the list
structure of the supplied STRINGS argument.

   (COUNT-STRINGS '(\"a\" \"bc\" \"a\" \"cd\" \"a\"))
=> ((\"a\" . 3) (\"bc\" . 1) (\"cd\" . 1)"
  (let (result)
    (mapc #'(lambda (s) (if (or (null result) (string/= (caar result) s))
			    (push (cons s 1) result)
			    (incf (cdar result))))
	  (sort (if safe strings (copy-seq strings)) #'string<))
    result))

(defun unique-substrings (strings)
  "Given a list of STRINGS, generate all the truncated permutations of
every string in that list.  Return only those strings that appear once
across all the permutations of all the words in the list.  There is no
specified order to the results.

(A side effect of implementation, any empty string or NIL value in
STRINGS will be silently dropped.)

   (UNIQUE-SUBSTRINGS '(\"alpha\" \"beta\" \"ant\" \"beat\" \"coo\" \"bop\")
=> (\"coo\" \"co\" \"c\" \"bop\" \"bo\" \"beta\" \"bet\" \"beat\" \"bea\"
    \"ant\" \"an\" \"alpha\" \"alph\" \"alp\" \"al\") "
  (mapcan #'(lambda (x) (if (= (cdr x) 1)
			    (list (car x))
			    nil))
	  (count-strings (all-truncated-strings strings) t)))

(defun isolate-switches (string)
  "Given a string that begins with at least one Windows CLI switch,
return a list of strings that exist between slashes, skipping leading,
multiple, and trailing slashes.  Arguments to switches introduced with
nna colon are preserved with the switch.

If this looks like SPLIT-SEQUENCE, well, you're not wrong.
ISOLATE-SWITCHES exists because one day, this is going to require some
kind of weird escape processing and probably a state machine of some
kind.  In the meantime, our mini-split-sequence hack is good enough.

   (ISOLATE-SWITCHES \"/a\")                 => (\"a\")
   (ISOLATE-SWITCHES \"/ab\")                => (\"ab\")
   (ISOLATE-SWITCHES \"/a/bc\")              => (\"a\" \"bc\")
   (ISOLATE-SWITCHES \"/a/bc:de\")           => (\"a\" \"bc:de\")
   (ISOLATE-SWITCHES \"/a/bc:de/f\")         => (\"a\" \"bc:de\" \"f\")
   (ISOLATE-SWITCHES \"/a/bc:de/f/\")        => (\"a\" \"bc:de\" \"f\")
   (ISOLATE-SWITCHES \"///a///bc:de///f//\") => (\"a\" \"bc:de\" \"f\")
   (ISOLATE-SWITCHES \"\")                   => NIL"
  (let ((str (string-trim "/" string))
	(i 0) (res))
    (unless (zerop (length str))
      (awhile (position #\/ str :start i)
	(push (subseq str i it) res)
	(setq i (position #\/ str :start it :test #'char/=)))
      (when i
	(push (subseq str i) res))
      (nreverse res))))
