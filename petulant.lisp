(in-package #:petulant)

(defun parse-unix-cli (arglist optargs &optional (optfixer #'identity))
  "Parse ARGLIST, a list of strings, as if they were an argument
vector from the command line, according to most commonly accepted
POSIX and GNU semantics.  Two values are returned: the first is a list
describing the options and their arguments in a regular fashion, the
second is a list of strings that consititute the unprocessed remainder
of the command line (that is, arguments to the command).

OPTARGS is a list of characters (single letter options) and
strings (so called \"long\" options) that take arguments.  Any option
encountered in ARGLIST that does not appear in OPTARGS is simply taken
as a flag.  Note that a long option in the form \"--foo=bar\" is
processed exactly as if \"foo\" was present in OPTARGS.

OPTFIXER is a function used to render an option into a form the caller
prefers in the first return value.  If no function is supplied,
#'IDENTITY is assumed.  For example, OPTFIXER can be used to downcase
all options seen on the command, or to render them as keywords, and so
on.  See MAKE-OPTION-FIXER for details."
  (block parser
    (let ((longp-fn (make-optwitharg-tester optargs foldcase)))
      (do ((opts) (av arglist))
	  ((null av) (values opts nil))
	(labels ((is- (&rest cc) (apply #'char= #\- cc))
		 (advance () (setf av (cdr av)))
		 (done () (return-from parser (values opts av)))
		 (longp (opt) (funcall longp-fn opt))
		 (add1 (x) (push (funcall optfixer x) opts))
		 (add2 (x y) (push (list (funcall optfixer x) y) opts))
		 (--abc (x) (aif (position #\= x)
				 (add2 (subseq x 2 it) (subseq x (1+ it)))
				 (add1 (subseq x 2))))
		 (-abc (x) (format t "-abc for ~s~%" x)))
	  (with-chars (c0 c1 c2) (car av)
	    (cond
	      ((not (and c0 c1 (is- c0))) ; "" "x" etc
	       (done))
	      ((and (is- c0 c1) (not c2)) ; "--"
	       (advance)
	       (done))
	      ;; ((is- c1)
	      ;;  ())
	      ((is- c1)
	       (--abc (car av)))
	      (t
	       (-abc (car av)))))
	  (advance))))))

#|

(defun parse-windows-cli (arglist optargs optfixer)
  (labels ((is/ (&rest cc) (apply #'char= #\/ cc)))
    (mapcar #'is- arglist)))

(defun parse-cli (arglist &optional optargs (style #+windows :UP
						   #-windows NIL))
  (funcall #+windows #'cli-parse-windows
	   #-windows #'cli-parse-unix
	   arglist (str-to-list optargs) (make-option-fixer style)))

(defun optchar (c)
  "Return T if character C is a hyphen or a slash.  Hyphens are the
POSIX way of introducing an option on a command line, while Windows
uses a slash."
  (and c (characterp c)
       (or (char= c #\-) (char= c #\/))))

(defun cli-parse (arglist &optional optargs style)
  "Parse the supplied argument list into regular forms that describe
the command line arguments present in the list of strings ARGLIST.
A single dash introduces one or more short options, interpreted as
simple flags; a double dash introduces a long option, which may take
an optional value if an equals sign is present.  A slash introduces
an option as well, which may take on a value introduced by a colon.

Two values are returned (so use MV-BIND or something appropriate).
The first returned value is a list.  Each element of this list is a
character, a string, a keyword, or a list.  In the latter, the first
element of the sublist is a character, string, or keyword, followed by
a supplied value.  The order of this list is not guaranteed (and, in
fact, is reversed from the order the options appear in ARGLIST).

The second returned value is also a list, containing all unprocessed
strings from ARGLIST.  This list may be NIL if ARGLIST was fully
consumed.

Take care with the return values, as either or both return values
often share structure with ARGLIST.

OPTARGS names the short options (individual characters) that should be
understood to take a value.  Thus, the idiomatic (… \"-o\" \"file\" …)
would be recognized in ARGLIST if OPTARGS contained #\\o.  OPTARGS can
be either a list of characters, or a single string of all such
characters.

When STYLE is NIL, characters and strings are returned in the same
case they were found in ARGLIST.  When STYLE is :DOWN, everything is
folded to lowercase characters and strings.  When STYLE is :KEY,
options are mapped to keyword values (i.e., \"foo\" becomes :FOO, #\\a
becomes :A) in the returned list.

Reflecting the support for both Unix and Windows CLI standards, the
way options combine on the command line is better demonstrated than
described:

   (CLI-PARSE (\"-a\" \"/b\" \"/c\" \"-d\"))
=> (#\\d #\\c #\\b #\\a)
   NIL

   (CLI-PARSE (\"-ab\" \"/c\" \"-d\"))
=> (#\\d #\\c #\\b #\\a)
   NIL

   (CLI-PARSE (\"/a/b\" \"-cd\")
=> (#\\d #\\c #\\b #\\a)
   NIL

   (CLI-PARSE (\"foo\" \"bar\"))
=> NIL
   (\"foo\" \"bar\")

   (CLI-PARSE (\"-ab\" \"foo\"))
=> (#\\b #\\a)
   (\"foo\")

   (CLI-PARSE (\"--ab\" \"foo\"))
=> (\"ab\")
   (\"foo\")

   (CLI-PARSE (\"--output=file\" \"-v\" \"foo\"))
=> (#\\v (\"output\" \"file\"))
   (\"foo\")

   (CLI-PARSE (\"/output:file\" \"/v\" \"foo\"))
=> (#\\v (\"output\" \"file\"))
   (\"foo\")

   (CLI-PARSE (\"-o\" \"file\" \"-v\" \"foo\") \"o\")
=> (#\\v (#\\o \"file\"))
   (\"foo\")

   (CLI-PARSE (\"-ofile\" \"-v\" \"foo\") \"o\")
=> (#\\v (#\\o \"file\"))
   (\"foo\")

   (CLI-PARSE (\"-ofile\" \"-v\" \"foo\"))
=> (#\\v #\\e #\\l #\\i #\\f #\\o)
   (\"foo\")

   (CLI-PARSE (\"/o:file\" \"/v\" \"foo\"))
=> (#\\v (#\\o \"file\"))
   (\"foo\")

   (CLI-PARSE (\"-o\" \"file\" \"--\" \"-v\" \"foo\") \"o\")
=> ((#\\o \"file\"))
   (\"-v\" \"foo\")

   (CLI-PARSE (\"-o\" \"file\" \"\" \"-v\" \"foo\") \"o\")
=> ((#\\o \"file\"))
   (\"\" \"-v\" \"foo\")
"
  (block nil
    (let ((optargs (str-to-list optargs)))
      (do ((opts) (av arglist))
	  ((null av) (values opts nil))
	(labels ((advance ()
		   (setf av (cdr av)))
		 (done ()
		   (return-from nil (values opts av)))
		 (addstr (str)
		   (push (if (= (length str) 1)
			     (char str 0)
			     str)
			 opts))
		 (/abc (s)
		   (push s opts))
		 (-abc (s)
		   (mapc #'(lambda (ch) (push ch opts))
			 (cdr (str-to-list s))))
		 (--abc=xyz (s i)
		   (push s opts))
		 (--abc (s)
		   (let ((i (position #\= s)))
		     (if i
			 (--abc=xyz s i)
			 (addstr (subseq s 2))))))
	  (with-chars (c0 c1 c2) (car av)
	    (cond
	      ((not (and c0 c1 (optchar c0)))          ; "" "x"
	       (done))
	      ((and (char= #\- c0 c1) (not c2))        ; "--"
	       (advance)
	       (done))
	      ((char= c0 #\-)
	       (if (char= c1 #\-)
		   (--abc (car av))
		   (-abc (car av))))
	      ((char= c0 #\/)
	       (/abc (car av)))
	      (t
	       (error "should never get here")))
	    (advance)))))))


#|

;;; This ought to be its own library.  Yes, there is a cl-getopt library
;;; which looks like some Debian fork of Kevin Rosenberg's original
;;; getopt library.  Coming from a Unix background and now hiding behind
;;; enemy lines in Windows, I wanted a little more than getopt(3).

;;; It's expected that we're processing a list of strings, as we'd
;;; receive from Roswell, that are equivalent to "char **argv" in Unix
;;; systems.  The caller provides, in addition to the argument list
;;; from the environment, a closure to call.  We invoke this closure
;;; for each decoded argument from the command line, until there are
;;; no more.  We also provide sentinels and other special values for
;;; the caller.
;;;
;;; The lambda list for the closer provided by the caller is
;;;     (shortopt &optional longopt argument)
;;;
;;; Where SHORTOPT is a single character, LONGOPT is typically a keyword
;;; corresponding to the SHORTOPT, and ARGUMENT is some string that
;;; was supplied for the argument.
;;;
;;; We can handle
;;;     foo -a -b -c
;;;     foo -abc
;;;     foo -a -bc
;;;     foo /a -b /c
;;;     foo /a/b -c
;;;     foo -a -b --clown=bozo
;;;
;;; When desired, we can call the supplied closure with three NIL values
;;; to signify the end of processing.

;;; The rules for argument processing are simple.  Supply a list as in
;;; the following.

;;; ((:flag #\a)               ;; "a" is NOT matched in a case-independent way
;;;  (:flag #\b "beta")
;;;  (:reqarg #\c "clown")     ;; "clown" is matched in a case-independent way
;;;  (:optarg #\v "verbose"))

;;; CLI-ARGS returns a list of the remaining arguments that weren't part
;;; of an option.

;; -ffoo vs -abc
;; distinguished by :arg f and :flag a

(defun canonicalize-one-spec (kind &rest options)
  "Return a list that begins with KIND, contains a string that is an
aggregation of all characters in OPTIONS, and then contains all the
strings left in options.
   (CANONI... :FLAG #\v)                => (:FLAG \"v\")
   (CANONI... :ARG #\o \"output\")      => (:ARG \"o\" \"output\")
   (CANONI... :FLAG #\a \"foobar\" #\b) => (:FLAG \"ab\" \"foobar\")
   (CANONI... :ARG \"baz\")             => (:ARG \"\" \"baz\")
   (CANONI... :FLAG \"foo\" \"bar\") => (:FLAG \"\" \"foo\" \"bar\")
This function is used in the analysis of argument specifications for
command line processing.  With the returned values, it's easier to
check if an option matches via FIND and other tools.  A small attempt
at handling specifiers that aren't characters or strings, but that's
mostly to prevent runtime errors, it isn't useful functionality."
  (let ((chars (make-array 0 :element-type 'character :adjustable t
			     :fill-pointer 0))
	(strings nil))
    (labels ((deal-with (x)
	       (cond
		((characterp x) (vector-push-extend x chars))
		((stringp x) (push x strings))
		(t (deal-with (format nil "~s" x))))))
      (mapc #'deal-with options))
    (nconc (list kind chars) strings)))

(defun canonicalize-specs (argument-specs)
  (mapcar #'(lambda (x)
	      (apply #'canonicalize-one-spec x))
	  argument-specs))

;; A previous version of CLI-ARGS took a mapping approach to
;; processing a command line.  This turned out to not work as well as
;; I'd hoped, as mapping is more uniform across arguments than
;; iteration typically is.  There's so much variation in option
;; processing (Consider "-abc" vs "-a" "-b" "-c", "-ofoo" vs "-o"
;; "foo", and so on), that you really do need an iterative approach to
;; handle it all sanely. It's not the first time the iterative
;; approach was cleaner, but ugh, it really mkes you question if you
;; truly understand the problem.  Anyway...

(defun cli-args (closure argspecs arglist &optional nil-when-done)
  (block nil
    (let ((specs (canonicalize-specs argspecs))
	  (args arglist))
      (labels ((advance () (setf args (cdr args))))
	(until (null args)
	  (with-chars (c0 c1 c2)
	      (car args)
	    (unless (optchar c0)
	      (return-from nil args))
	    (when (and (optchar c1) (not c2))
	      (return-from nil (cdr args)))
	    (advance)))))))

#+nil
(defun cli-args (closure argspecs arglist &optional nil-when-done)
  (block nil
    (let ((specs (canonicalize-specs argspecs)))
      (print specs)
      (return-from nil nil)
      (mapl #'(lambda (alist)
			(with-chars (c0 c1 c2)
			    (car alist)
			  (unless (optchar c0)            ; no more options
			    (return-from nil alist))
			  (when (and (optchar c1) (not c2))    ; "--" seen
			    (return-from nil (cdr alist)))
			  (unless c2
			    (format t "flag ~c~%" c1))))
		  arglist))))

;; no, can't quite fit this into a mapping form, it's not the right
;; answer to the problem.  going to have to write an iterative
;; approach that slides the list along manually.  Otherwise, we can't
;; handle "-ofoo" and "-o foo" equally correctly.


|#
