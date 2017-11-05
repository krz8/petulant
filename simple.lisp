(in-package #:petulant)

(defun parse-unix-cli (arglist fn
		       &optional
			 (argoptp-fn (constantly nil))
			 (chgname-fn #'identity))
  "This is the low level parser for Unix-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

PARSE-UNIX-CLI works through ARGLIST, a flat list of strings from a
command line, parsing it according to most POSIX and GNU behaviors.
As options are identified, ARGOPTP-FN is called to determine if that
option takes an argument.

FN is called for each option \(with or without an argument\) and every
non-option argument.  Each call has three arguments.  The first is
always :ARG or :OPT.  When :ARG, the second argument is a non-option
argument string from the command line, and the third argument is NIL.
When :OPT, the second argument is an option \(a string\) found on the
command line, eliding any leading dashes, and the third argument is
any argument to that option or NIL.

ARGOPTP-FN, if supplied, is a mechanism for the caller to indicate
when an option, long or short, should take the next word in ARGLIST as
an argument.  The default binding of ARGOPTP-FN always returns NIL,
indicating that any ambiguous option is assumed not to take an
argument.  The only non-ambiguous option with an argument are long
options that use the \"=\" character \(e.g., \"--foo=bar\"\).

CHGNAME-FN, if supplied, can be used to change a detected option from
one value to another, taking a string and returning a string to use in
its place.  It could be used to implement aliases or partial matching,
for example.  Every detected option, long or short, is passed through
this function before processing continues; it is called before
ARGOPTP-FN, for example.

Generally speaking, the calls to FN proceed from the head to the tail
of ARGLIST, and from left to right within each string of ARGLIST.
This is useful to know in testing, but callers probably should not
rely on any specific ordering."
  (do ((av arglist))
      ((null av) t)
    (labels ((argoptp (x) (funcall argoptp-fn x))
	     (chgname (x) (funcall chgname-fn x))
	     (is- (&rest chars) (apply #'char= #\- chars))
	     (advance () (setf av (cdr av)))
	     (opt! (o &optional a) (funcall fn :opt o a))
	     (arg! (a) (funcall fn :arg a nil))
	     (long (opt)
	       (let* ((o (subseq opt 2))                    ; "--foo…"
		      (i (position #\= o))		    ; "--foo=…"
		      (f (chgname (if i
				      (subseq o 0 i)
				      o))))
		 (cond
		   (i                                       ; "--foo=…"
		    (opt! f (unless (= i (1- (length o)))   ; "--foo="
			      (subseq o (1+ i)))))          ; "--foo=xyz"
		   ((argoptp f)                             ; "--foo" "xyz"
		    (opt! f (cadr av))
		    (advance))
		   (t                                       ; "--foo"
		    (opt! f)))))
	     (short (opt)
	       (iterate
		 (for i index-of-string opt)
		 (for c = (string (char opt i)))
		 (if-first-time (next-iteration))           ; skip leading -
		 (let ((f (chgname c)))
		   (cond
		     ((not (argoptp f))			    ; "-fgh"
		      (opt! f))
		     ((< i (1- (length opt)))		    ; "-ffile"
		      (opt! f (subseq opt (1+ i)))
		      (finish))
		     (t					    ; "-f" "file"
		      (opt! f (cadr av))
		      (advance)
		      (finish)))))))
      (with-chars (c0 c1 c2)
	  (car av)
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
			    (swargp-fn (constantly nil))
			    (chgname-fn #'identity))
  "This is the low level parser for Windows-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

PARSE-WINDOWS-CLI works through ARGLIST, a flat list of strings
delivered from some OS-specific wrapper in the Lisp environment
parsing it according to most Windows behaviors.  As switches are
identified, SWARGP-FN is called to determine if that switch takes an
argument.

FN is called for each switch \(with or without an argument\) and every
non-switch argument.  Each call has three arguments.  The first is
always :ARG or :OPT.  When :ARG, the second argument is a non-switch
argument string from the command line, and the third argument is NIL.
When :OPT, the second argument is a switch \(a string\) found on the
command line, eliding its leading slash, and the third argument is any
argument to that option or NIL.

SWARGP-FN, if supplied, is a mechanism for the caller to indicate when
a switch should take an argument.  The default binding of SWARGP-FN
always returns NIL, indicating that any ambiguous switch is assumed
not to take an argument.  A non-ambiguous switch with an argument is
one that uses the colon character \(e.g., \"/foo:bar\"\).

CHGNAME-FN, if supplied, can be used to change a detected switch from
one value to another, taking a string and returning a string to use in
its place.  It could be used to implement aliases or partial matching,
for example.  Every detected switch is passed through this function
before processing continues; it is called before SWARGP-FN, for
example.

Generally speaking, the calls to FN proceed from the head to the tail
of ARGLIST, and from left to right within each string of ARGLIST.
This is useful to know in testing, but callers probably should not
rely on any specific ordering."
  (do ((av (canonicalize-windows-args arglist)))
      ((null av) t)
    (labels ((swargp (x) (funcall swargp-fn x))
	     (chgname (x) (funcall chgname-fn x))
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
	   (opt! (chgname (subseq str 1 it))
		 (unless (= it (1- len))		       ; "/foo:xyz"
		   (subseq str (1+ it)))))
	  (t
	   (let ((f (chgname (subseq str 1))))
	     (cond
	       ((swargp f)                                     ; "/foo" "xyz"
		(opt! f (cadr av))
		(advance))
	       (t                                              ; "/foo"
		(opt! f)))))))
      (advance))))

(defun argv ()
  "Returns a list of strings representing the command-line from the
environment.  This is necessarily OS specific.  It's assumed that the
list returned by ARGV does not include the executable name, image
name, argv[0], or other non-argument information."
  #+ccl ccl:*command-line-argument-list*
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+acl (cdr (sys:command-line-arguments))
  #- (or ccl sbcl clisp acl)
  (error "Petulant needs to be ported to this Lisp environment."))

(defun canonicalize-styles (styles)
  "Given STYLES, which might be a keyword or a list of keywords,
return a complete list of keywords and any other keywords they imply.

If :UNIX or :WINDOWS appears in styles, it is left as-is.  Otherwise,
CL:*FEATURES* is consulted, and if :WINDOWS appears there, it is added
to STYLES.  In this way, we support a default based on the local
operating system, but make it easy for clients of Petulant to override
this forcing one or the other behavior.

If :KEY appears in STYLES, :UP is added to STYLES as well.

If :STR= does not appear in STYLES, and one of :UP, :DOWN, or :WINDOWS
appears, then :STREQ is added to STYLES.

Once processed by CANONICALIZE-STYLES, the keyword :CANON is pushed to
the front of the resulting list.  By leaving :CANON at the front,
future calls of CANONICALIZE-STYLES can quickly detect when they've
already run on a list, and avoid duplicating work when called more
than once.

   \(CANONICALIZE-STYLES '\(:UNIX :KEY\)\)
=> \(:CANON :STREQ :UP :UNIX :KEY\)
   \(CANONICALIZE-STYLES '\(:FOO :BAR\)\)
=> \(:CANON :FOO :BAR\)
   \(CANONICALIZE-STYLES :DOWN\)
=> \(:CANON :STREQ :DOWN\)
   \(CANONICALIZE-STYLES :STREQ\)
=> \(:CANON :STREQ\)"
  (let ((res (ensure-list styles)))
    ;; We're calling (MEMBER :CANON RES) here; a slightly less robust
    ;; but faster approach would be (EQ :CANON (CAR RES)) since :CANON
    ;; should be leftmost in RES once we've run once, but that relies
    ;; on no one messing with STYLES once it's been processed.
    (unless (member :canon res)
      (unless (or (member :windows res) (member :unix res))
	(push (if (featurep :windows) :windows :unix) res))
      (when (member :key res)
	(push :up res))
      (unless (member :str= res)
	(when (or (member :up res)
		  (member :down res)
		  (member :windows res))
	  (push :streq res)))
      (push :canon res))		; always last!
    res))

(defmacro with-styles-canon ((var val) &body body)
  "Evaluate BODY in a context where VAR is bound to the canonicalized
styles based on VAL.  This macro can be used more than once;
CANONICALIZE-STYLES is written to make that situation harmless."
  `(let ((,var (canonicalize-styles ,val)))
     ,@body))

(defun foldp (styles)
  "Returns true when STYLES indicates that case-insensitive matching
should be employed.  Specifically, this is described by :STR= not
being present in STYLES and :STREQ being present.  \(:STR= overrides
any :STREQ that might be present.\)"
  (with-styles-canon (styles styles)
    (and (not (member :str= styles))
	 (member :streq styles))))

(defun simple-parse-cli (fn &key arglist argopt-p-fn chgname-fn styles)
  "This is the low level parser for command-lines.  If you're simply
using Petulant in your application \(i.e., you aren't developing
Petulant\), you might want to consider calling a higher level
function; SIMPLE-PARSE-CLI is mainly for implementation of other
Petulant functionality.

SIMPLE-PARSE-CLI works through an argument list, a flat list of
strings representing the command-line.  It parses this list according
to the dominant style for a specific operating system \(e.g.,
hyphen-based options under Unix, or slash-based switches under
Windows\).

FN is called for each option \(with or without an argument\) and every
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

ARGOPT-P-FN, if supplied, is a function.  Petulant recognizes certain
long options (\"--foo=bar\") and switches (\"/foo:bar\") that
unambiguously present an option taking an argument.  However, Petulant
cannot know for certain when a short option (\"-f\" \"bar\") takes an
option, nor can it discern when a long option (\"--foo\" \"bar\") or a
switch (\"/foo\" \"bar\") lacking extra punctuation takes an argument.
The caller can supply a function taking the name of the option as a
string (\"f\" or \"foo\") and returning true or false to indicate if
it takes an argument.

CHGNAME-FN, if supplied, can be used to change a detected switch from
one value to another, taking a string and returning a string to use in
its place.  It could be used to implement aliases or partial matching,
for example.  Every detected switch is passed through this function
before processing continues; it is called before ARGOPT-P-FN, for
example.

STYLES can be used to select a particular style of command-line
processing.  By default, SIMPLE-PARSE-CLI will choose the style based
on the current operating system environment \(using *FEATURES*\).
However, the caller can force a particular style by supplying :UNIX
or :WINDOWS, or by supplying a list containing :UNIX or :WINDOWS, to
this argument."
  (with-styles-canon (styles styles)
    (funcall (if (member :windows styles) #'parse-windows-cli #'parse-unix-cli)
	     (or arglist (argv))
	     fn
	     (or argopt-p-fn (constantly nil))
	     (or chgname-fn #'identity))))

