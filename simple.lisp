(in-package #:petulant)

(defun isolate-switches (string)
  "Given a string that begins with at least one Windows CLI switch
character, return a list of strings that exist between slashes,
skipping leading, multiple, and trailing slashes.  Arguments to
switches introduced with a colon are preserved with the switch.

If this looks like SPLIT-SEQUENCE, well, you're not wrong.
ISOLATE-SWITCHES exists because one day, this is going to require some
kind of weird escape processing and probably a state machine of some
kind.  In the meantime, this mini-split-sequence hack is good enough.

   \(ISOLATE-SWITCHES \"/a\"\)                 => \(\"a\"\)
   \(ISOLATE-SWITCHES \"/ab\"\)                => \(\"ab\"\)
   \(ISOLATE-SWITCHES \"/a/bc\"\)              => \(\"a\" \"bc\"\)
   \(ISOLATE-SWITCHES \"/a/bc:de\"\)           => \(\"a\" \"bc:de\"\)
   \(ISOLATE-SWITCHES \"/a/bc:de/f\"\)         => \(\"a\" \"bc:de\" \"f\"\)
   \(ISOLATE-SWITCHES \"/a/bc:de/f/\"\)        => \(\"a\" \"bc:de\" \"f\"\)
   \(ISOLATE-SWITCHES \"///a///bc:de///f//\"\) => \(\"a\" \"bc:de\" \"f\"\)
   \(ISOLATE-SWITCHES \"\"\)                   => \(\"\"\)
   \(ISOLATE-SWITCHES \"/\"\)                  => \(\"/\"\)
   \(ISOLATE-SWITCHES \"//\"\)                 => \(\"//\"\)"
  (cond
    ((zerop (length string))                       '(""))
    ((not (position #\/ string :test #'char/=))    (list string))
    (t
     (let ((str (string-trim "/" string))
	   (i 0) (res))
       (awhile (position #\/ str :start i)
	 (push (subseq str i it) res)
	 (setq i (position #\/ str :start it :test #'char/=)))
       (when i
	 (push (subseq str i) res))
       (nreverse res)))))

(defun canonicalize-windows-args (strings)
  "Given a list of strings that represents command line options and
arguments passed in a Windows environment, break up combined switches
and return a new list of strings that is easier to parse.  The original
set of STRINGS is broken down via ISOLATE-SWITCHES.

Strings like \"\", \"/\", \"//\", and so on are special, we preserve
them as they are.

   (CANONICALIZE-WINDOWS-ARGS '(\"abc\" nil \"\" \"def\")
=> (\"abc\" \"\" \"def\")
   (CANONICALIZE-WINDOWS-ARGS '(\"abc\" \"//\" \"def\")
=> (\"abc\" \"//\" \"def\")
   (CANONICALIZE-WINDOWS-ARGS '(\"/abc\" \"def\")
=> (\"/abc\" \"def\")
   (CANONICALIZE-WINDOWS-ARGS '(\"/a/bc\" \"def\")
=> (\"/a\" \"/bc\" \"def\")
   (CANONICALIZE-WINDOWS-ARGS '(\"/a/bc\" \"def\" \"/ef:gh\")
=> (\"/a\" \"/bc\" \"def\" \"/ef:gh\")
   (CANONICALIZE-WINDOWS-ARGS '(\"/a/bc\" \"def\" \"/ef\" \"gh\")
=> (\"/a\" \"/bc\" \"def\" \"/ef\" \"gh\")
   (CANONICALIZE-WINDOWS-ARGS '(\"/a/bc\" \"def\" \"/e/f:g/h\")
=> (\"/a\" \"/bc\" \"def\" \"/e\" \"/f:g\" \"/h\""
  (let ((result))
    (labels ((collect (x) (push x result)))
      (mapc #'(lambda (sw)
		(cond
		  ((null sw))
		  ((and (> (length sw) 0) (char= #\/ (char sw 0)))
		   (mapc #'(lambda (s) (collect (slashify s)))
			 (isolate-switches sw)))
		  (t
		   (collect sw))))
	    strings))
    (nreverse result)))

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

(defun styles-to-hash (styles)
  "Given STYLES, which is a keyword or a list of keywords, populate a
hash with a complete list of keywords and any other keywords implied
by those supplied and the running system.  This function converts the
STYLES list to a hash to speed lookups, and \"fleshes out\" the
keyword list so that it deetermines all aspects of Petulant's
processing based on what was originally supplied by the caller.

First, all keywords in STYLES are added to the returned hash.

Next, we ensure that one of :UNIX or :WINDOWS always appears in the
hash.  If not already present, CL:*FEATURES* is consulted to determine
which of those keywords will appear.

If :KEY appears in the hash, and neither :UP nor :DOWN appear in
STYLES, then :UP is added to the hash.

If :UP or :DOWN appear in the hash, and neither :STREQ nor :STR=
appear, then :STREQ is added to the hash.

If neither :STREQ nor :STR= appear in the hash, but :WINDOWS does,
then :STREQ is added to the hash, otherwise :STR= is added."
  (let ((styles (ensure-list styles))
	(hash (make-hash-table)))
    (labels ((set! (&rest flags) (mapc (lambda (f) (setf (gethash f hash) f))
				       flags))
	     (set? (&rest flags) (some (lambda (f) (gethash f hash))
				       flags)))
      (apply #'set! styles)
      (unless (set? :windows :unix)
	(set! (cond ((featurep :windows) :windows)
		    (t                   :unix))))
      (unless (set? :up :down)
	(when (set? :key)
	  (set! :up)))
      (unless (set? :str= :streq)
	(set! (or (and (set? :up :down :windows) :streq)
		  :str=))))
    hash))

(defun with-stylehash ((var) &body body)
  "WITH-STYLEHASH takes the name of a variable that might be a list of
style keywords or a hash as returned by STYLES-TO-HASH, and ensures
that the variable is bound to such a hash for the duration of BODY.
This macro is used to allow multiple functions in the Petulant
hierarchy to be entry points to an external caller \(who would use a
list of style keywords\) while allowing higher functions in Petulant
to call those same functions with an already-converted and
already-fleshed-out hash of those keywords.

   \(defun foo \(styles\)
     \(with-stylehash \(styles\)
       ...
       \(bar styles\)
       ...\)\)"
  `(let ((,var (if (hash-table-p ,var) ,var (styles-to-hash ,var))))
     ,@body))

(defun foldp (stylehash)
  "Returns true when STYLEHASH indicates that case-insensitive matching
should be employed."
  (gethash :streq stylehash))

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
