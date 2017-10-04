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

(defun cb (kind a b)
  (format t "cb ~s ~s ~s~%" kind a b))
