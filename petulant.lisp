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
    ;; We're calling (MEMBER :CANON RES) here; a slightly less robust
    ;; but faster approach would be (EQ :CANON (CAR RES)) since :CANON
    ;; should be leftmost in RES once we've run once, but that relies
    ;; on no one messing with STYLES once it's been processed.
    (unless (member :canon res)
      (when (member :key res)
	(push :up res))
      (unless (member :nofold res)
	(when (or (member :up res)
		  (member :down res)
		  (member :windows res)
		  (and (featurep :windows) (not (member :unix res))))
	  (push :fold res)))
      (push :canon res))		; always last!
    res))

(defmacro with-styles-canon ((var val) &body body)
  "Evaluate BODY in a context where VAR is bound to the canonicalized
styles based on VAL.  This macro can be used more than once;
CANONICALIZE-STYLES is written to make that situation harmless."
  `(let ((,var (canonicalize-styles ,val)))
     ,@body))

(defun windowsp (styles)
  "Allow the user to determine which style of option processing we
use (Unix or Windows), but also allow the current environment to
determine a default.  STYLES is a list of keywords affecting the
behavior of Petulant.

When STYLES contains :UNIX, return false.  Else, when STYLES
contains :WINDOWS, return true.  Otherwise, no matter what other
values STYLES might hold, it is taken to not specify Unix/Windows
behavior.  In that case, return true if :WINDOWS is on the
CL:*FEATURES* list, else false."
  (with-styles-canon (styles styles)
    (unless (member :unix styles)
      (or (member :windows styles)
	  (featurep :windows)))))

(defun foldp (styles)
  "Returns true when STYLES indicates that case-insensitive matching
should be employed.  Specifically, this is described by :NOFOLD not
being present in STYLES and :FOLD being present.  \(:NOFOLD overrides
any :FOLD that might be present.\)"
  (with-styles-canon (styles styles)
    (and (not (member :nofold styles))
	 (member :fold styles))))

(defun simple-parse-cli (fn &key arglist optarg-p-fn styles)
  "This is the low level parser for command-lines.  If you're simply
using Petulant in your application \(i.e., you aren't developing
Petulant\), you might want to consider calling a higher level function;
SIMPLE-PARSE-CLI is mainly for implementation of other Petulant
functionality.

SIMPLE-PARSE-CLI works through an argument list, a flat list of
strings representing the command-line.  It parses this list according
to the dominant style for a specific operating system \(e.g.,
hyphen-based options under Unix, or slash-based switches under
Windows\).

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

(defun str=-fn (styles)
  "Return a function to be used in comparing option strings.  When
STYLES contains :FOLD \(or :FOLD is implied by another value\), and it
does not contain the overriding keyword :NOFOLD, a case-insensitive
function is returned; otherwise, a case sensitive string comparison
function is returned."
  (with-styles-canon (styles styles)
    (if (and (not (member :nofold styles)) (member :fold styles))
	#'string-equal
	#'string=)))

;;; write down a set of optargs, optflags, aliases, and :partial
;;;
;;; work through what an optarg-p-fn would look like
;;; work through what an aliasing would look like
;;;
;;; determine order (alias, then optarg-p?  vice versa?)
;;; or how things would nest, whatever
;;; beginning to end

(defun optarg-p-fn (optargs aliases styles)
  "Given a list of options that take arguments, OPTARGS, an alist of
option aliases, and a list of STYLES for processing, return a function
of one argument that returns true when its argument, an option
recognized by the simple CLI parser, should be considered an
argument-bearing option.  If STYLES contains :FOLD or another value
that implies :FOLD, use case-insensitive string matching; otherwise,
exact \(case-sensitive\) matching is employed.

Note: even if :KEY is present in STYLES, only strings are compared
by the returned function."
  (with-styles-canon (styles styles)
    (let ((str=-fn (str=-fn styles)))
      #'(lambda (opt)
	  (member opt optargs
		  :test #'(lambda (x y) (funcall str=-fn x y)))))))

(defun option-hackers (optargs styles)
  "Compose a new function that calls other functions to transform
\(hack\) a single argument that is an option name.  These other
functions are based on the contents of STYLES and OPTARGS, and might
change the case of the option, they might replace it with a symbol
from the keyword package, they might substitute aliases or recognize
partial matches.  In the end, the returned function will call FN after
all the other work is done."
  (with-styles-canon (styles styles)
    (let ((funcs nil))
      (when (member :down styles)
	(push #'string-downcase funcs))
      (when (member :up styles)
	(push #'string-upcase funcs))
      (when (member :key styles)
	(push #'(lambda (x) (intern x "KEYWORD")) funcs))
      (apply #'compose funcs))))

(defun parse-cli (fn &key arglist optargs optflags aliases styles)
  "PARSE-CLI examines the command-line with which an application was
invoked.  According to given styles, options (aka switches) and
arguments are recognized.

FN is a function supplied by the caller, which is called for each
option or argument identified by PARSE-CLI.  Each call to FN has three
arguments.  The first is the keyword :OPT or :ARG, indicating whether
an option \(aka switch\) or an non-option argument was found.
When :ARG, the second argument is a string, an argument from the
command-line that was not associated with an option, and the third
argument is NIL.  When :OPT, the second argument is usually a string
naming an option \(although see STYLES below\), and the third argument
is a string value associated with that option, or NIL.

OPTARGS, if supplied, is a list of all options \(short or long\) that
require an argument.  While Petulant can recognize options that
explicitly take an argument \(as in \"--file=foo.psd\" or
\"/file:foo.psd\"\), it needs the hint in OPTARGS to recognize other
patterns \(such as \"-f\" \"foo.psd\", or \"/file\" \"foo.psd\"\).
Simply place the option (no leading hyphens or slashes) as a string in
this list.  The call below would recognize both \"-f\" and \"--file\"
as requiring an argument.

   \(parse-cli … :optargs '\(\"f\" \"file\"\) … \)

OPTFLAGS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  This argument has no effect on
PARSE-CLI unless :PARTIAL appears in STYLES.  See :PARTIAL below.

ARGLIST causes PARSE-CLI to parse a specified list of strings, instead
of the default command-line that was supplied to the application.
These strings are parsed exactly as if they appeared on the
command-line, each string corresponding to one \"word\".

   \(parse-cli … :arglist '\(\"-xv\" \"-f\" \"foo.tar\"\) … \)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is an association list, in which every element has a primary option
string as its CAR, and a list of alternative strings as their CDR.
For example, in the call below, both \"/sleep\" and \"/wait\" would be
recognized in an argument list, but FN would be called with \"/delay\"
in either event.

  \(parse-cli … :aliases '\(\(\"alpha\" \"transparency\"\)
                          \(\"delay\" \"sleep\" \"wait\"\)\) … \)

STYLES is a keyword, or a list of keywords, that influence Petulant's
behavior.  Recognized keywords are as follows; unrecognized keywords
are silently ignored.

   :NOFOLD  String matching between OPTARGS, OPTFLAGS, ALIASES, and the
            command-line being parsed is sensitive to case.  This
            exists solely to override any semantics implied
            by :WINDOWS, :UNIX, :UP, :DOWN, :KEY, and the local Lisp
            environment.  Overrides :FOLD.
   :FOLD    String matching between OPTARGS, OPTFLAGS, ALIASES, and
            the command-line being parsed is insensitive to case.
   :UP      All option names presented to FN will be converted to
            upper case.  Implies :FOLD.
   :DOWN    All option names presented to FN will be converted to
            lower case.  Implies :FOLD.
   :KEY     All option names presented to FN will be converted to
            symbols in the keyword package.  Implies :UP.
   :PARTIAL Support partial matches of options.  When present,
            Petulant will support unambiguous partial matches of
            options \(as they appear in OPTARGS, OPTFLAGS, and
            ALIASES\).  For example, if OPTARGS contained \"beat\",
            then :PARTIAL would trigger aliases of \"b\", \"be\", and
            \"bea\" for \"beat\".  But, if OPTFLAGS also contained
            \"bop\" then \"b\" would no longer be automatically
            created as an alias, and \"bo\" would be added as an alias
            for \"bop\".
   :UNIX    Disregard the current running system, and process the
            command-line arguments as if in a Unix environment.
   :WINDOWS Disregard the current running system, and process the
            command-line arguments as if in a Windows environment.
            Also implies :FOLD."
  (with-styles-canon (styles styles)
    (let ((optcounts (count-options optargs optflags aliases styles)))
      (simple-parse-cli
       (option-hackers fn optargs styles)
       :arglist arglist
       :styles styles
       :optarg-p-fn (optarg-p-fn optargs styles)))))

;;; okay. think.
;;; cb takes THREE arguments
;;; you're trying to compose it with functions that take ONE argument
;;; and in those cases, it's the SECOND argument of the callback
;;; that needs the hackery.
;;;
;;; Break FN out of the compose hacks, and call it manually.
;;; can compose take nothing but a list? or does it have to start
;;; with an argument separate from, say, (&rest funcs).
;;; Why is it defined that way?
