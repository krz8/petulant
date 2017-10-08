(in-package #:petulant)

(defun parse-unix-cli (arglist fn
		       &optional
			 (optargp-fn (constantly nil)))
  "This is the low level parser for Unix-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

PARSE-UNIX-CLI works through ARGLIST, a flat list of strings from a
command line, parsing it according to most POSIX and GNU behaviors.
As options are identified, OPTARGP-FN is called to determine if that
option takes an argument.

FN is called for each option (with or without an argument) and every
non-option argument.  Each call has three arguments.  The first is
always :ARG or :OPT.  When :ARG, the second argument is a non-option
argument string from the command line, and the third argument is NIL.
When :OPT, the second argument is an option (a string) found on the
command line, eliding any leading dashes, and the third argument is
any argument to that option or NIL.

OPTARGP-FN, if supplied, is a mechanism for the caller to indicate
when an option, long or short, should take the next word in ARGLIST as
an argument.  The default binding of OPTARGP-FN always returns NIL,
indicating that any ambiguous option is assumed not to take an
argument.  The only non-ambiguous option with an argument are long
options that use the \"=\" character (e.g., \"--foo=bar\").

Generally speaking, the calls to FN proceed from the head to the tail
of ARGLIST, and from left to right within each string of ARGLIST.
This is useful to know in testing, but callers probably should not
rely on any specific ordering."
  (do ((av arglist))
      ((null av) t)
    (labels ((optargp (x) (funcall optargp-fn x))
	     (is- (&rest chars) (apply #'char= #\- chars))
	     (advance () (setf av (cdr av)))
	     (opt! (o &optional a) (funcall fn :opt o a))
	     (arg! (a) (funcall fn :arg a nil))
	     (long (opt) (let ((o (subseq opt 2)))	       ; "--foo…"
			   (acond
			     ((position #\= o)		       ; "--foo=xyz"
			      (opt!
			       (subseq o 0 it)
			       (unless (= it (1- (length o)))  ; "--foo="
				 (subseq o (1+ it)))))
			     ((optargp o)                      ; "--foo" "xyz"
			      (opt! o (cadr av))
			      (advance))
			     (t				       ; "--foo"
			      (opt! o)))))
	     (short (opt) (iterate			       ; "-f…"
			   (for i index-of-string opt)
			   (for c = (string (char opt i)))
			   (if-first-time (next-iteration)) ; skip leading -
			   (cond
			     ((not (optargp c))		       ; "-fgh"
			      (opt! c))
			     ((< i (1- (length opt)))	       ; "-ffile"
			      (opt! c (subseq opt (1+ i)))
			      (finish))
			     (t				       ; "-f" "file"
			      (opt! c (cadr av))
			      (advance)
			      (finish))))))
      (with-chars (c0 c1 c2) (car av)
	(cond
	  ((not (and c0 c1 (is- c0)))			       ; "" "x" 
	   (arg! (car av)))
	  ((and (is- c0 c1) (null c2))			       ; "--"
	   (loop (unless (advance)
		   (return))
	      (arg! (car av))))
	  ((is- c1)					       ; "--…"
	   (long (car av)))
	  (t						       ; "-…"
	   (short (car av)))))
      (advance))))

(defun parse-windows-cli (arglist fn
			  &optional
			    (swargp-fn (constantly nil)))
  "This is the low level parser for Windows-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

PARSE-WINDOWS-CLI works through ARGLIST, a flat list of strings
delivered from some OS-specific wrapper in the Lisp environment
parsing it according to most Windows behaviors.  As switches are
identified, SWARGP-FN is called to determine if that switch takes an
argument.

FN is called for each switch (with or without an argument) and every
non-switch argument.  Each call has three arguments.  The first is
always :ARG or :OPT.  When :ARG, the second argument is a non-switch
argument string from the command line, and the third argument is NIL.
When :OPT, the second argument is a switch (a string) found on the
command line, eliding its leading slash, and the third argument is
any argument to that option or NIL.

SWARGP-FN, if supplied, is a mechanism for the caller to indicate when
a switch should take an argument.  The default binding of SWARGP-FN
always returns NIL, indicating that any ambiguous switch is assumed
not to take an argument.  A non-ambiguous switch with an argument is
one that uses the colon character (e.g., \"/foo:bar\").

Generally speaking, the calls to FN proceed from the head to the tail
of ARGLIST, and from left to right within each string of ARGLIST.
This is useful to know in testing, but callers probably should not
rely on any specific ordering."
  (do ((av (canonicalize-windows-args arglist)))
      ((null av) t)
    (labels ((swargp (x) (funcall swargp-fn x))
	     (advance () (setf av (cdr av)))
	     (opt! (o &optional a) (funcall fn :opt o a))
	     (arg! (a) (funcall fn :arg a nil)))
      (let* ((str (car av))
	     (len (or (and str (length str)) 0)))
	(acond
	  ((zerop len)					       ; nil ""
	   (arg! str))
	  ((string= str "//")				       ; "//"
	   (loop (unless (advance)
		   (return))
	      (arg! (car av))))
	  ((char/= (char str 0) #\/)			       ; "foo"
	   (arg! str))
	  ((position #\: str)				       ; "/foo:…"
	   (opt! (subseq str 1 it)
		 (unless (= it (1- len))		       ; "/foo:xyz"
		   (subseq str (1+ it)))))
	  ((swargp (subseq str 1))			       ; "/foo" "xyz"
	   (opt! (subseq str 1) (cadr av))
	   (advance))
	  (t						       ; "/foo"
	   (opt! (subseq str 1)))))
      (advance))))

(defun argv ()
  "Returns a list of strings representing the command-line from the
environment.  This is necessarily OS specific.  It's assumed that
the list returned by ARGV does not include the executable name, image
name, argv[0], or other non-argument information."
  #+ccl ccl:*command-line-argument-list*
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+acl (cdr (sys:command-line-arguments))
  #- (or ccl sbcl clisp acl)
  (error "Petulant needs to be ported to this Lisp environment."))

(defun canonicalize-styles (styles)
  "Given STYLES, which might be a keyword or a list of keywords,
return a complete list of keywords.  Mostly, this function deals
with implications: for example, a style of :KEY implies both
:UP and :FOLD.

Once processed by CANONICALIZE-STYLES, the keyword :CANON is pushed to
the front of the resulting list.  By leaving :CANON at the front,
future calls of CANONICALIZE-STYLES can quickly detect when they've
already run on a list, and avoid duplicating work when called more
than once.

   (CANONICALIZE-STYLES '(:UNIX :KEY))
=> (:CANON :FOLD :UP :UNIX :KEY)
   (CANONICALIZE-STYLES '(:FOO :BAR)
=> (:CANON :FOO :BAR)
   (CANONICALIZE-STYLES :DOWN)
=> (:CANON :FOLD :DOWN)
   (CANONICALIZE-STYLES :FOLD)
=> (:CANON :FOLD)"
  (let ((res (ensure-list styles)))
    (unless (member :canon res)
      (when (member :key res)
	(push :up res))
      (when (or (member :up res) (member :down res))
	(push :fold res))
      (push :canon res))
    res))

(defmacro with-styles-canon ((var val) &body body)
  "Evaluate BODY in a context where VAR is bound to the canonicalized
styles based on VAL.  This macro can be used more than once."
  `(let ((,var (canonicalize-styles ,val)))
     ,@body))

(defun windowsp (styles)
  "Allow the user to determine which style of option processing we
use (Windows or Unix), but also allow the current environment to
determine a default.  STYLES is a list of keywords affecting the
behavior of Petulant, and it is assumed these keywords have already
been rendered into a canonical set of styles.

When STYLES contains :UNIX, return false.  Else, when STYLES
contains :WINDOWS, return true.  Otherwise, no matter what other
values STYLES might hold, it is taken to not specify Windows/Unix
behavior.  In that case, return true if :WINDOWS is on the
CL:*FEATURES* list, else false."
  (unless (member :unix styles)
    (or (member :windows styles)
	(featurep :windows))))

(defun simple-parse-cli (fn &key arglist optarg-p-fn styles)
  "This is the low level parser for command-lines.  If you're simply
using Petulant in your application (i.e., you aren't developing
Petulant), you might want to consider calling a higher level function;
SIMPLE-PARSE-CLI is mainly for implementation of other Petulant
functionality.

SIMPLE-PARSE-CLI works through an argument list, a flat list of
strings representing the command-line.  It parses this list according
to the dominant style for a specific operating system (e.g.,
slash-based switches under Windows, and hyphen-based options under
Unix).

FN is called for each option (with or without an argument) and every
non-option argument identified during parsing.  Each call to FN has
three arguments.  The first is always :ARG or :OPT.  When :ARG, the
second argument is a non-switch argument string from the command line,
and the third argument is NIL.  When :OPT, the second argument is a
switch (a string) found on the command line, eliding its leading
slash, and the third argument is any argument to that option or NIL.

Generally speaking, the calls to FN proceed from the head to the tail
of the list of argument strings, and from left to right within each
string of that list.  This is useful to know in testing, but callers
probably should not rely on any specific ordering.

ARGLIST, if supplied, is a list of strings to parse.  By default,
SIMPLE-PARSE-CLI will parse a list of strings provided by the Lisp
environment representing the command-line with which the application
was started.

OPTARG-P-FN, if supplied, is a function.  Petulant recognizes certain
long options (\"--foo=bar\") and switches (\"/foo:bar\") that
unambiguously present an option taking an argument.  However, Petulant
cannot know for certain when a short option (\"-f\" \"bar\") takes an
option, nor can it discern when a long option (\"--foo\" \"bar\") or a
switch (\"/foo\" \"bar\") lacking extra punctuation takes an argument.
The caller can supply a function taking the name of the option as a
string (\"f\" or \"foo\") and returning true or false to indicate if
it takes an argument.

STYLES can be used to select a particular style of command-line
processing.  By default, SIMPLE-PARSE-CLI will choose the style based
on the current operating system environment (using *FEATURES*).
However, the caller can force a particular style by supplying :UNIX
or :WINDOWS, or by supplying a list containing :UNIX or :WINDOWS, to
this argument."
  (with-styles-canon (styles styles)
    (funcall (if (windowsp styles) #'parse-windows-cli #'parse-unix-cli)
	     (or arglist (argv))
	     fn
	     (or optarg-p-fn (constantly nil)))))

(defun cb (&rest args)
  (format t "cb~{ ~s~}~%" args))

(defun parse-cli (fn &key arglist optargs aliases styles)
  "PARSE-CLI examines the command-line with which an application was
invoked.  According to a given style (Windows or Unix), options (aka
switches) and arguments are recognized.

FN is a function supplied by the caller, which is called for each
option or argument identified by PARSE-CLI.  Each call to FN has three
arguments.  The first is the keyword :OPT or :ARG, indicating whether
an option \(aka switch\) or an non-option argument was found.
When :ARG, the second argument is a string, an argument from the
command-line that was not associated with an option, and the third
argument is NIL.  When :OPT, the second argument is a string naming an
option, and the third argument is an argument associated with that
option, or NIL.

ARGLIST causes PARSE-CLI to parse a supplied list of strings, instead
of the default command-line that was supplied to the application.
These strings are parsed exactly as if they appeared on the
command-line, each string corresponding to one \"word\".

   (parse-cli … :arglist '(\"-xv\" \"-f\" \"foo.tar\") … )

OPTARGS can be used to supply a list of strings naming options
\(switches\) that take arguments.  While Petulant can recognize some
options and their arguments, there are also ambiguous situations that
arise during parsing.  Petulant assumes that any ambiguous option does
not take an argument unless it appears in the list of string supplied
to OPTARGS.

ALIASES is used to define aliases between options; in other words,
mapping one option to another.  Most often, this is used to implement
partial matching \(e.g., allowing \"--in\" to be recognized as
\"--input\"\), it can also be used to map short options with long
options \(\"-v\" becomes \"--verbose\"\), and even to map entirely
different words to one another \(\"/transparency\" might be mapped to
\"/alpha\"\).  ALIASES is an association list.  For each element, the
CAR is a string naming the option being mapped to.  The rest of the
element is a list of alternate strings that should be recognized as
aliases to the CAR.  For example,

  (parse-cli … :aliases '((\"alpha\" \"transparency\")
                          (\"input\" \"inpu\" \"inp\" \"in\" \"i\")) … )

STYLES is a keyword, or a list of keywords, that influence Petulant's
behavior.  Recognized keywords are listed here; unrecognized keywords
are silently ignored.

   :FOLD    All string comparisons are case-insensitive.
   :UP      All option names presented to FN will be converted to
            upper case.  Implies :FOLD.
   :DOWN    All option names presented to FN will be converted to
            lower case.  Implies :FOLD.
   :KEY     All option names presented to FN will be converted to
            symbols in the keyword package.  Implies :UP.
   :UNIX    Disregard the current running system, and process the
            command-line arguments as if in a Unix environment.
   :WINDOWS Disregard the current running system, and process the
            command-line arguments as if in a Windows environment."
  (simple-parse-cli fn :arglist arglist :styles styles))

