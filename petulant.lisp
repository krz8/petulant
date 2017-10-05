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

(defun windowsp (style)
  "If STYLE is :WINDOWS, or if STYLE is a list containing :WINDOWS as
a non-nested element, return T.  Otherwise, examine CL:*FEATURES*.  If
*FEATURES* contains a keyword that is associated with Windows, return
T.  Otherwise, NIL is returned."
  (or (eq :windows style)
      (member :windows style)
      (member :windows *features*)))

(defun simple-parse-cli (fn &key arglist optarg-p-fn style)
  "This is the low level parser for command-lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mainly for implementation of other
Petulant functionality.

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

STYLE can be used to select a particular style of command-line
processing.  By default, SIMPLE-PARSE-CLI will choose the style based
on the current operating system environment (using *FEATURES*).
However, the caller can force a particular style by supplying :UNIX
or :WINDOWS, or by supplying a list containing :UNIX or :WINDOWS, to
this argument."
  (funcall (if (windowsp style) #'parse-windows-cli #'parse-unix-cli)
	   (or arglist (argv))
	   fn
	   (or sw-arg-p (constantly nil))))

(defun cb (kind a b)
  (format t "cb ~s ~s ~s~%" kind a b))
