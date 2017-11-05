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

(defun cb (&rest args)
  (format t "cb~{ ~s~}~%" args))

(defun str=-fn (styles)
  "Return a function to be used in comparing option strings for
equality, based on FOLDP."
  (with-styles-canon (styles styles)
    (if (member :streq styles)
	#'string-equal
	#'string=)))

(defun eq=-fn (styles)
  "Return a function to be used in comparing option strings for
equality, based on FOLDP.  Where STR=-FN returns a string comparison
function, this returns an equality function \(e.g., for use in hash
tables\)."
  (with-styles-canon (styles styles)
    (if (member :streq styles)
	#'equalp
	#'equal)))

(defun str<-fn (styles)
  "Return a function to be used in comparing option strings for
sorting, based on FOLDP."
  (with-styles-canon (styles styles)
    (if (member :streq styles)
	#'string-lessp
	#'string<)))

(defun str/=-fn (styles)
  "Return a function to be used in comparing option strings for
inequality, based on FOLDP."
  (with-styles-canon (styles styles)
    (if (member :streq styles)
	#'string-not-equal
	#'string/=)))

(defun partials-fn (argopts flagopts aliases styles)
  "When STYLES contains :PARTIAL, return a function that implements
partial matching for all the options seen in ARGOPTS, FLAGOPTS, and
ALIASES; otherwise, #'IDENTITY is returned and no partial matching is
supported.

When partial matching is desired, ARGOPTS FLAGOPTS ALIASES and STYLES
are taken in the same format as PARSE-CLI.  A function is returned
that takes a single string as an option or argument appearing on a
command-line, recognizing unambiguous partial matches of options as
they appear in the three supplied lists, and returning a possibly new
string that PARSE-CLI should process as if it were the original word
from the command-line."
  (with-styles-canon (styles styles)
    (cond
      ((member :partial styles)
       (let ((dict (make-dict :loose (foldp styles)))
	     (str= (str=-fn styles)))
	    (labels ((maybe-add (o)
		       (unless (dict-word-p dict o)
			 (dict-add dict o))))
	      (mapc #'maybe-add (append argopts flagopts))
	      (mapc (lambda (alist)
		      (mapc #'maybe-add (cdr alist)))
		    aliases))
	    (let ((minwords (minwords dict)))
	      (lambda (x) 
		(block nil
		  (let ((len (length x)))
		    (mapc (lambda (partial)
			    (destructuring-bind (min max option) partial
			      (when (and (<= min len max)
					 (funcall str= x (subseq option 0 len)))
				(return option))))
			  minwords)
		    x))))))
      (t
       #'identity))))

(defun build-alias-hash (aliases styles)
  "Returns a hash table initialized with the option alias list ALIASES.
In that alist, the CDR of each element are all the strings that are
mapped to the CAR of the element.  STYLES is used to determine how to
compare strings within the hash table."
  (with-styles-canon (styles styles)
    (let ((hash (make-hash-table :test (eq=-fn styles))))
      (mapc (lambda (entry)
	      (let ((value (car entry)))
		(mapc (lambda (key)
			(setf (gethash key hash) value))
		      (cdr entry))))
	    aliases)
      hash)))

(defun aliases-fn (aliases styles)
  "Given ALIASES, a list that maps one or more strings to an intended
option, this creates the function that performs that mapping according
to STYLES.  The returns function takes an option string as parsed by
SIMPLE-PARSE-CLI, and returns a string to use in its stead.

ALIASES is an association list of strings, where the CAR of each entry
is an intended option, and the CDR is a list of strings that are
mapped to the intended option.

STYLES is a keyword or list of keywords influencing the matching
between command-line options and the list of aliases seen here.  In
order of priority, :STR= prevents case folding, :STREQ directs
case-insensitive matching, :UP and :DOWN imply :STREQ.  Note that
ALIASES-FN treats :KEY as :UP \(mapping options to keyword values
happens elsewhere\)."
  (with-styles-canon (styles styles)
    (let ((hash (build-alias-hash aliases styles)))
      (lambda (x) (aif (gethash x hash)
		       it
		       x)))))

(defun build-argopt-hash (argopts styles)
  "Returns a hash table initialized with the options listed in
ARGOPTS.  Each key simply maps to T; this hash is treated as a set
function.  STYLES is used to initialize the hash table's equality
test."
  (with-styles-canon (styles styles)
    (let ((hash (make-hash-table :test (eq=-fn styles))))
      (mapc (lambda (argopt) (setf (gethash argopt hash) t))
	    argopts)
      hash)))

(defun argopt-p-fn (argopts styles)
  "Given ARGOPTS, a list of strings denoting options that take
arguments, this returns a function that can be used by Petulant to
test if an ambiguous option consumes arguments or not.

STYLES is a keyword or list of keywords influencing the comparison
between options provided by the calling application and options
supplied by the user.  In order of priority, :STR= prevents case
folding, :STREQ directs case-insensitive matching, and :UP :DOWN
and :KEY all imply :STREQ."
  (with-styles-canon (styles styles)
    (let ((hash (build-argopt-hash argopts styles)))
      (lambda (x) (gethash x hash)))))

(defun hack-option-fn (styles)
  "Compose a new function that calls other functions to transform
\(hack\) a single argument that is an option name.  These other
functions are based on the contents of STYLES and ARGOPTS, and might
change the case of the option, they might replace it with a symbol
from the keyword package, they might substitute aliases or recognize
partial matches.  The results of passing an option string through
the composed function is an object (string or keyword) ready for
the supplied callback function."
  (with-styles-canon (styles styles)
    (let ((funcs nil))
      (when (member :down styles)
	(push #'string-downcase funcs))
      (when (member :up styles)
	(push #'string-upcase funcs))
      (when (member :key styles)
	(push (lambda (x) (intern x "KEYWORD")) funcs))
      (when (null funcs)
	(push #'identity funcs))
      (apply #'compose funcs))))

(defun parse-cli (fn &key argopts flagopts aliases arglist styles)
  "PARSE-CLI examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.

FN is a function supplied by the caller, which is called for each
option or argument identified by PARSE-CLI.  Each call to FN has three
arguments.  The first is the keyword :OPT or :ARG, indicating whether
an option \(aka switch\) or an non-option argument was found.
When :ARG, the second argument is a string, an argument from the
command-line that was not associated with an option, and the third
argument is NIL.  When :OPT, the second argument is usually a string
naming an option \(although see STYLES below\), and the third argument
is a string value associated with that option, or NIL.

ARGOPTS, if supplied, is a list of all options \(short or long\) that
require an argument.  While Petulant can automatically recognize some
options that explicitly take an argument \(as in \"--file=foo.psd\" or
\"/file:foo.psd\"\), it needs the hint in ARGOPTS to recognize other
patterns \(such as \"-f\" \"foo.psd\", or \"/file\" \"foo.psd\"\).
Simply place the option (no leading hyphens or slashes) as a string in
this list.  The call below would recognize both \"-f\" and \"--file\"
as requiring an argument.  \(Note that \"f\" in the list is better
handled by an alias below, or by the use of :PARTIAL in STYLES; its
presence here is merely for example.\) ARGOPTS does not limit the
options that PARSE-CLI handles, even those with arguments; it is
merely a hint that

   \(parse-cli … :argopts '\(\"f\" \"file\"\) … \)

FLAGOPTS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  This argument has no effect on
PARSE-CLI unless :PARTIAL appears in STYLES.  See :PARTIAL below.

   \(parse-cli … :flagopts '\(\"verbose\" \"debug\" \"trace\"\) … \)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is a list of lists.  Every element of ALIASES is a list naming the
primary option first, followed by all aliases for it.  For example, in
the call below, both \"/sleep\" and \"/wait\" would be recognized by
PARSE-CLI, but processed as if \"/delay\" were seen.

  \(parse-cli … :aliases '\(\(\"alpha\" \"transparency\"\)
			   \(\"delay\" \"sleep\" \"wait\"\)
			   \(\"file\" \"f\"\)\) … \)

ARGLIST causes PARSE-CLI to parse a specified list of strings, instead
of the default command-line that was supplied to the application.
These strings are parsed exactly as if they appeared on the
command-line, each string corresponding to one \"word\".

   \(parse-cli … :arglist '\(\"-xv\" \"-f\" \"foo.tar\"\) … \)

STYLES is a keyword, or a list of keywords, that influence Petulant's
behavior.  Recognized keywords are as follows; unrecognized keywords
are silently ignored.

   :STR=    String matching between ARGOPTS, FLAGOPTS, ALIASES, and
	    the command-line being parsed is sensitive to case.  This
	    exists solely to override any folding semantics implied
	    by :WINDOWS, :UNIX, :UP, :DOWN, :KEY, and the local Lisp
	    environment.  Overrides :STREQ.  Its name is meant to be
	    evocative of STRING=.

   :STREQ   String matching between ARGOPTS, FLAGOPTS, ALIASES, and
	    the command-line being parsed is insensitive to case.  Its
	    name is meant to be evocative of STRING-EQUAL.

   :UP      All option names presented to FN will be converted to
	    upper case.  Implies :STREQ.

   :DOWN    All option names presented to FN will be converted to
	    lower case.  Implies :STREQ.

   :KEY     All option names presented to FN will be converted to
	    symbols in the keyword package.  Implies :UP.

   :PARTIAL Support partial matches of options.  When present,
	    Petulant will support unambiguous partial matches of
	    options \(as they appear in ARGOPTS, FLAGOPTS, and
	    ALIASES\).  For example, if ARGOPTS contained \"beat\",
	    then :PARTIAL would trigger aliases of \"b\", \"be\", and
	    \"bea\" for \"beat\".  But, if FLAGOPTS also contained
	    \"bop\" then \"b\" would no longer be automatically
	    created as an alias, and \"bo\" would be added as an alias
	    for \"bop\".

   :UNIX    Disregard the current running system, and process the
	    command-line arguments as if in a Unix environment.

   :WINDOWS Disregard the current running system, and process the
	    command-line arguments as if in a Windows environment.
	    Also implies :STREQ."
  (with-styles-canon (styles styles)
    (let ((hack-fn (hack-option-fn styles)))
      (flet ((cb (x y z)
	       (case x
		 (:opt (funcall fn x (funcall hack-fn y) z))
		 (:arg (funcall fn x y z)))))
	(simple-parse-cli #'cb
			  :argopt-p-fn (argopt-p-fn argopts styles)
			  :chgname-fn (compose (aliases-fn aliases styles)
					       (partials-fn argopts flagopts
							    aliases styles))
			  :arglist arglist
			  :styles styles)))))

(defun get-cli (&key argopts flagopts aliases arglist styles)
  "GET-CLI takes the same keyword arguments, and offers the same
functionality, as the function it wraps, PARSE-CLI.  See that
function for complete documentation.  Unlike PARSE-CLI, however,
it returns a single list of things found on the command-line, in
the order that they were found.  Each element of this list is,
itself, a list of two or three values.  If the first element is
:ARG, then the second argument is a string naming an argument not
associated with an option on the command-line.  Otherwise, the first
element is :OPT, the second argument is the option \(as a string or
keyword symbol, according to the STYLES argument\), and if the option
has an argument, that string appears as the third element.

All arguments and options to GET-CLI share the same name and carry the
same functionality as they appear in PARSE-CLI."
  (let (results)
    (parse-cli (lambda (&rest args) (push args results))
	       :argopts argopts :flagopts flagopts :aliases aliases
	       :arglist arglist :styles styles)
    (nreverse results)))

(defun label-option (option type styles &optional (padding "  "))
  "Given a string naming an OPTION, some kind of type specification
going with it (:FLAG appearing for plain options, and something else
for options that take arguments), and a styles list, return a string
to be used in a usage message for this option.  The string is always
padded with two spaces at its beginning.

   \(label-option \"alpha\" :string :unix\)
=> \"  --alpha=VAL\"
   \(label-option \"beta\" :string :windows\)
=> \"  /beta:VAL\"
   \(label-option \"gamma\" :flag :unix\)
=> \"  --gamma\"
   \(label-option \"delta\" :flag :windows\)
=> \"  /delta\"
   \(label-option \"e\" :flag :unix\)
=> \"  -e\"
   \(label-option \"z\" :string :unix\)
=> \"  -z VAL\""
  (with-styles-canon (styles styles)
    (let ((winp (member :windows styles))
	  (shortp (< (length option) 2))
	  (flagp (eq :flag type)))
      (strcat padding
	      (cond (winp   "/")
		    (shortp "-")
		    (t      "--"))
	      option
	      (cond (flagp  "")
		    (winp   ":VAL")
		    (shortp " VAL")
		    (t      "=VAL"))))))

(defun widest-option-label (opthash styles)
  "Given the type hash of all options to our application, a hash
containing only those options that take arguments, and the influencing
styles of our application, format each of them as they would appear in
a usage message via LABEL-OPTION, and return the length of the longest
string \(e.g., \" --config=VAL\"\)."
  (with-styles-canon (styles styles)
    (reduce #'max
	    (maphash/c (lambda (k v) (length (label-option k v styles)))
		       opthash))))

(defun fmt (format-args)
  "Format the list FORMAT-ARGS as if they were arguments to the FORMAT
utility, returning the resulting string.  The first element of
FORMAT-ARGS must be a string, and any subsequent elements are as
called for in the first string."
  (apply #'format nil format-args))

(defun canonicalize-type (type-spec)
  "Try to put TYPE-SPEC into a canonical form, undoing the
abbreviations and shortcuts that are commonly accepted.  If TYPE-SPEC
is a keyword, then it is reformed to a list with * in the position of
its arguments.  If it is a list, but it takes more arguments than are
provided, * is appended for each of the missing arguments."
  (when type-spec
    (destructuring-bind (&optional x y z)
	(ensure-list type-spec)
      (case x
	((:integer :float :ratio :rational :real) ; two arguments
	 (list x (or y '*) (or z '*)))
	(:string			; one argument
	 (list x (or y '*)))
	(otherwise
	 (list x))))))

(defun option-type (type-spec)
  "This function returns text that at least somewhat describes the
supplied type for use in a help or usage message.  In addition to
obvious types like :FLOAT, :INTEGER, :RATIO, :RATIONAL, and :REAL, the
following pseudo-types are supported:

:KEY is not really a type, but instead, represents the intent to take
the supplied string, trim whitespace from either end, convert it to
upper case, and intern the result as a symbol in the keyword package.

:READ is not really a type, but instead, represents the desire to call
the Lisp READ-FROM-STRING on the supplied argument and take the result
as-is.  This could be used for reading lists from the command-line or
other weirdness.  This could lead to very unexpected behavior \(you
know how users are\), so use this pseudo-type with great care.

:STRING is slightly different than the built-in Lisp STRING type.
When its single optional argument appears, it is taken as a maximum
length for the string."
  (destructuring-bind (&optional d0 d1 d2)
      (canonicalize-type type-spec)
    (apply #'strcat
	   (case d0
	     ((:float :integer :ratio :rational :real)
	      (list "This option takes"
		    (case d0
		      (:integer " an integer")
		      (:float " a floating point value")
		      (:rational " a rational number (like 6 or 11/3)")
		      (:ratio " a ratio (like -1/3 or 11/3)")
		      (:real " a number"))
		    (cond
		      ((and (eq d1 '*) (eq d2 '*))
		       "")
		      ((eq d2 '*)
		       (format nil " no less than ~a" d1))
		      ((eq d1 '*)
		       (format nil " no more than ~a" d2))
		      (t
		       (format nil " between ~a and ~a" d1 d2)))
		    ". "))
	     (:string
	      (if (eq '* d1)
		  '("")
		  (list (format nil "This option takes a string no more than ~
                                     ~d characters in length." d1))))
	     (:key
	      '("This option takes a single short alphanumeric word, starting"
		"with a letter, containing no whitespace or other symbols. "))
	     (otherwise
	      '(""))))))

(defparameter *ws* '(#\Space #\Tab #\Newline #\Return #\Page)
  "A list of common whitespace characters.")

(defun pad (string minlength &optional (minpad 2))
  "Return a new string that is STRING but with spaces appended in
order to make its length equal to MINLENGTH.  At least MINPAD spaces
appears at the right of STRING, no matter how long the resulting
string is.  This is used to set off a tag in a paragraph with a
hanging tag, as in an option in a usage message.

   \(PAD \"foo\" 8) => \"foo     \"
   \(PAD \"foo\" 5) => \"foo  \"
   \(PAD \"foo\" 2) => \"foo  \"

   \(PAD \"blah\" 3 0\) => \"blah\" 
   \(PAD \"blah\" 3 1\) => \"blah \" 
   \(PAD \"blah\" 3 2\) => \"blah  \" 
   \(PAD \"blah\" 4 0\) => \"blah\" 
   \(PAD \"blah\" 4 1\) => \"blah \" 
   \(PAD \"blah\" 4 2\) => \"blah  \" 
   \(PAD \"blah\" 5 0\) => \"blah \" 
   \(PAD \"blah\" 5 1\) => \"blah \" 
   \(PAD \"blah\" 5 2\) => \"blah  \" 
   \(PAD \"blah\" 6 0\) => \"blah  \" 
   \(PAD \"blah\" 6 1\) => \"blah  \" 
   \(PAD \"blah\" 6 2\) => \"blah  \""
  (let ((str (string-right-trim *ws* string)))
    (strcat str (make-string (max minpad (- minlength (length str)))
			     :initial-element #\Space))))

(defun hanging-par (label text &optional stream indentlength)
  "Presents a hanging paragraph onto STREAM or *STANDARD-OUTPUT* if
STREAM is not provided.  The exdented text, starting at the beginning
of the first line, is supplied by the LABEL string.  The TEXT string
is broken up on whitespace boundaries and flowed onto the remainder of
the line until the right margin is encountered.  Remaining words are
placed on as many subsequent lines as necessary, each of those lines
indented by spaces.  The number of spaces used to indent all remaining
lines is given by INDENTLENGTH; if that argument is not provided, the
width of LABEL is used instead."
  (let* ((spaces (make-string (or indentlength (length label))
			      :initial-element #\Space))
	 (words (split *ws* text))
	 (format (strcat "~a~{~<~%" spaces "~1:;~a~>~^ ~}~%")))
    (format (or stream *standard-output*) format label words)))

(defun option-text (option dochash)
  "Return a string that may be empty describing OPTION.  Looks for a
closure in the dochash, and if one exists, executes it (we assume it
came from SPEC-CLI, and therefore returns a string).  Otherwise,
returns an empty string."
  (or (aand (gethash option dochash) (funcall it))
      ""))

(defun option-aliases (option aliases styles)
  (with-styles-canon (styles styles)
    (apply #'format nil
	   "~#[~;An alias for this option is ~s.~
             ~;Aliases for this option are ~s and ~s.~
             ~;Aliases for this option are ~@{~#[~; and~] ~s~^,~}.~]"
	   (cdr (assoc option aliases :test (str=-fn styles))))))

(defun usage-option (option opthash dochash aliases styles tagwidth stream)
  "Format a full description of the string named OPTION onto the
supplied STREAM.  OPTHASH is the hash mapping options to their types,
and DOCHASH is the hash mapping options to closures that provide
descriptions.  TAGWIDTH provides a specific width for the lefthand
column in which the options appear."
  (with-styles-canon (styles styles)
    (let* ((type (gethash option opthash))
	   (label (pad (label-option option type styles) tagwidth)))
      (hanging-par label
		   (strcat (option-text option dochash)
			   " " (option-type type)
			   " " (option-aliases option aliases styles))
		   stream tagwidth))))

(defun usage-header (appname summary namewidth stream)
  "Given an string APPNAME and its possibly long SUMMARY (which can be
NIL), format the pair as a hanging paragraph onto STREAM.  SUMMARY is
a closure that generates the text to be formatted, or NIL."
  (let ((namewidth (min namewidth (+ 3 (length appname)))))
    (if (null summary)
	(format stream "~a:~%" appname)
	(hanging-par (pad (strcat appname ":") namewidth)
		     (funcall summary)
		     stream namewidth))
    (terpri stream)))

(defun usage-footer (tail styles stream)
  "If TAIL is not NIL, call it to obtain text, and render it onto the
named output stream or *STANDARD-OUTPUT*, wrapping the text at the
same right margin as USAGE-HEADER and USAGE-OPTION."
  (with-styles-canon (styles styles)
    (flet ((par (text)
	     (let ((words (split *ws* text)))
	       (when (not (zerop (length (car words))))
		 (format stream "~{~<~%~1:;~a~>~^ ~}~%~%" words)))))
      (par (strcat (if (and (member :unix styles)
			    (foldp styles))
              "Options are case-insensitive (-x and -X are equivalent). "
		       "")
		   (if (and (member :windows styles)
			    (not (foldp styles)))
              "Options are case sensitive (/x and /X are different). "
		       "")
		   (if (member :partial styles)
              "Options may be abbreviated to their shortest unique specifier. "
		       "")))
      (when tail
	(par (funcall tail))))))

(defun usage (appname summary tail opthash dochash aliases styles
	      &key (stream *standard-output*) (maxappwidth 18) (maxoptwidth 16))
  "Display a usage message on the supplied stream, describing all the
options the application supports on its command-line.  MAXAPPWIDTH and
MAXOPTWIDTH can be used to supply maximum indentation of the SUMMARY
and each option's description \(though longer texts will be formatted
reasonably\); use those keywords to change the default sizes."
  (with-styles-canon (styles styles)
    (let ((optwidth (min (+ 2 (widest-option-label opthash styles))
			 maxoptwidth))
	  (printed nil))
      (usage-header appname summary maxappwidth stream)
      (mapc (lambda (option)
	      (usage-option option opthash dochash aliases styles
			    optwidth stream)
	      (setf printed t))
	    (sort (hash-table-keys opthash) #'string<))
      (when printed
	(terpri stream))
      (usage-footer tail styles stream))))

(defun sort-out-options (optspecs)
  "Given specifications for options taking arguments and options that
are only flags, return two values: a list of all the options taken by
the application, and a hash mapping all options to their types
\(including the pseudo-type :FLAG indicating the option takes no
argument\)."
  (let ((opthash (make-hash-table))
	(dochash (make-hash-table)))
    (mapc (lambda (spec) (destructuring-bind (option type docfn)
			     spec
			   (setf (gethash option opthash) type
				 (gethash option dochash) docfn)))
	  optspecs)
    (values opthash dochash)))

#+nil
(defun spec-cli* (name summary tail optspecs aliases styles args)
    "Receives a parsed specification of command-line options and
arguments from the SPEC-CLI macro."
    (multiple-value-bind (opthash dochash)
	(sort-out-options optspecs)
      (usage name summary tail opthash dochash aliases styles)))

#-nil
(defun spec-cli* (name summary tail options)
  "for debugging"
  (format t "spec-cli* starting at ~d~%" (get-universal-time))
  (format t "spec-cli* pausing three seconds~%")
  (sleep 3)
  (format t "name ~s ~s~%" (type-of name) name)
  (when summary
    (format t "summary ~s~%" (funcall summary)))
  (when tail
    (format t "tail ~s~%" (funcall tail)))
  (princ "options")
  (pprint options *standard-output*))

(defmacro spec-cli (&rest forms)
  "Using a series of forms specifying a complete command-line
interface presented to the user, blah blah blah...

\(:NAME \"appname\"\) provides a name for the application. Eventually,
we might tease the name under which we were invoked out of the running
Lisp environment, but for now, every well-behaved application should
supply a name for itself.

   \(:name \"sharpen\"\)

\(:SUMMARY …\) is used to provide a short summary of the application.
It takes a FORMAT control string and optional subsequent forms that
are evaluated when a usage message needs to be generated.  The
resulting string is reformatted and justified to fit in the usage
message, using all whitespace (newline, carriage returns, space
characters) as locations to provide text wrapping. \(So don't bother
trying any fancy formatting.\)

   \(:summary \"Stimulate the beaded hamster.\"\)
   \(:summary \"Does the thing with the thing and the other things ~
	     and reports things found \(version ~a, ~a\).\"
	     \(get-version-string\) \(get-release-date\)\)

\(:TAIL …\) provides further text to appear at the bottom of a usage
page, perhaps supplying copyright, author information, or anything
else that isn't important enough to appear in the summary text at the
start of the usage page.  Its specification semantics are the same as
with :SUMMARY above.

\(:FLAGOPT \"option\" [\"text description\" …]\) names a single option
that is taken as a flag (that is, it is not expected to take an
argument value).  If a text description is provided, it and subsequent
arguments are only evaluated when necessary for display, sent through
FORMAT, and placed in usage messages, re-justified as necessary.

    \(:flagopt \"verbose\"\)
    \(:flagopt \"verbose\" \"Increase the detail in trace messages.\"\)

\(:ARGOPT \"option\" [type-spec] [\"text description\" …]\) is similar
to :FLAGOPT, except that it has an optional argument naming a type for
the argument to this option.  If the type-spec is not supplied, it is
assumed to be :STRING, meaning that a string of any length is
acceptable.

    \(:argopt \"config\"\)
    \(:argopt \"config\" \"Supplies an alternate configuration file.\")

- :STRING and \(:STRING *\) both describe a string of any length.
  Alternatively, a type \(:STRING N\) indicates a string whose length
  will be truncated if its length exceeds N characters.  No processing
  is performed on a string (other than the aforementioned truncation),
  so almost any string that can be presented from your shell or
  command interpreter is supported.

    \(:argopt \"title\"\)
    \(:argopt \"title\" \(:string 50\)\)   
    \(:argopt \"title\" \(:string 50\)
              \"Names the main title of the report.\"\)

- Numeric types may be specified via :FLOAT, :INTEGER, :RATIO,
  :RATIONAL, and :REAL.  :FLOAT indicates a floating point decimal
  number.  :INTEGER indicates a whole decimal number, of course.
  :RATIO accepts precise fractional values such as -1/3 or 11/5.
  :RATIONAL is a supertype of both integers and ratios, and all the
  preceding numeric types are subtypes of :REAL.

    \(:argopt \"delay\" :real\)
    \(:argopt \"delay\" :real \"Specifies the number of seconds ~
                            to wait between iterations.\"\)

- The preceding numeric types may be expressed in a list with up to
  two optional values.  If a second argument exists, it provides a
  minimum value for the number; if a third argument exists, it
  provides a maximum value; either value may be specified as an
  asterisk meaning \"any value\".  For examples, \(:INTEGER 0 100\) is
  used in an option whose argument must be an integer in the interval
  [0,100].  \(:FLOAT 0\) and \(:FLOAT 0 *\) both describe a floating
  point value that cannot be negative.  \(:RATIONAL * 1\) indicates a
  number whose value cannot exceed 1.

    \(:argopt \"volume\" \(:real 0 10\)\)
    \(:argopt \"volume\" \(:real 0 11\) \"This one goes to 11.\"\)

- A pseudo-type :KEY can be specified to describe an option's argument
  that will be converted to uppercase and interned as a symbol in the
  keyword package.  The argument should start with a letter, and the
  remainder of the argument should be restrained to alphanumeric
  characters and punctuation in ~!@$%^&*-+=_[]{}<>,./?.  Other
  characters can be accepted, but they funky syntax that usually
  defeats the purpose of using a keyword in the first place.  Note
  that the colon that usually introduces a keyword symbol in Lisp is
  not present in the command-line argument using :KEY.

  \(spec-cli ... \(:argopt \"color\" :key\) ...\)
  $ app --color=red
  SPEC-CLI returns :RED as the argument of \"color\".

- The pseudo-type :ONE-OF can be used to introduce a list of strings
  that the user may supply as an argument to the option.  The list
  can be specified as strings or symbols; any match from the user
  counts.  The comparison is not sensitive to case, and a symbol in
  the keyword package will be returned, regardless of the specification.

   \(:argopt \"channel\" \(:one-of :red :green :blue\)\)

   \(:argopt \"xfer\" \(:one-of \"rectlin\" \"sigmoid\" \"thresh\" \"lin\"\)\)

- A pseudo-type :READ can be specified that will call the Lisp reader
  to parse the supplied argument string.  This can be exploited to
  accept many data types and expressions \(such as arrays and lists\).
  HOWEVER this can also be risky in that the running Lisp environment
  is opened up to whatever the reader can be tricked into evaluating.
  While something as obvious as \"\(ERASE-HARD-DISK\)\" won't do the
  trick, it isn't inconceivable that a given Lisp implementation's
  reader might have undesirable side effects that can be invoked or
  otherwise tricked by a clever user.  Use the :READ psuedo-type with
  extreme caution in an executable released into the wild.

  - \(spec-cli ... \(:argopt \"foo\" :read\) ...\)

  - C:\\Users\\krz> app /foo:(erase-disk)

  - SPEC-CLI returns a list of one item, the symbol ERASE-DISK, as the
    argument of the \"foo\" option.

\(:ALIAS \"option\" \"alias\" [\"alias\" …]\) establishes one or more
aliases for a given option. Multiple instances of :ALIAS for the same
option accumulate. \(aka :ALIASES\)

   \(:alias \"dry-run\" \"n\"\)
   \(:alias \"alpha\" \"fade\" \"transparency\"\)

\(:STYLE style [style …]\) supplies one or more style options, as
documented in PARSE-CLI, to influence the parsing of the
command-line. Multiple instances of :STYLE accumulate. \(aka :STYLES\)

   \(:style :key :unix\)

\(:ARG \"command-line-arg\" [\"command-line-arg\"]\) supplies one or
more strings to be used instead of the application's actual
command-line.  Multiple instances of :ARG accumulate. \(aka :ARGS\)"
  ;; The main point of this macro is to make it easy for the caller to
  ;; specify forms, which we'll lightly parse, calling the real
  ;; SPEC-CLI* function with regular forms and lambdas encapsulating
  ;; the arguments SPEC-CLI was called with.  E.g., for options,
  ;; element 0 is always the kind of option, element 1 is always the
  ;; option string, element 2 is always a type (even for flag option),
  ;; and element 3 is always a closure to be invoked later when we
  ;; need the string to document the option.
  ;;
  ;; Also, SPEC-CLI provides hand-holding.  So much hand-holding.  I
  ;; figure if someone is using SPEC-CLI and not GET-CLI or PARSE-CLI,
  ;; they want all the functionality (including the kitchen sink).
  ;; So, we'll give it to them, catching as many problem situations as
  ;; we can.  If the caller can get through SPEC-CLI without warnings
  ;; or errors, there's no reason for them to expect anything but
  ;; success.
  (let ((keypkg (find-package :keyword))
	name summary tail
	options ;; aliases styles args
	)
    (macrolet ((wrn (x &rest y) `(warn ,(strcat "SPEC-CLI: " x) ,@y))
	       (err (x &rest y) `(error ,(strcat "SPEC-CLI: " x) ,@y)))
      (labels
	  ((stringify (x) (if (stringp x) x (format nil "~a" x)))
	   (name (form)
	     (when name
	       (wrn "(:NAME ...) should only appear once."))
	     (cond
	       ((cddr form)
		(err "(:NAME ...) must have exactly two elements."))
	       ((not (stringp (cadr form)))
		(err "In (:NAME ...) the second element must be a string."))
	       (t
		(setf name (cadr form)))))
	   (summary (form)
	     (when summary
	       (wrn "(:SUMMARY ...) should only appear once."))
	     (setf summary `(lambda () (format nil ,@(cdr form)))))
	   (tail (form)
	     (when tail
	       (wrn "(:TAIL ...) should only appear once."))
	     (setf tail `(lambda () (format nil ,@(cdr form)))))
	   (flagopt (form)
	     (cond
	       ((not (stringp (cadr form)))
		(err "In (:FLAGOPT ...), the second element, which names ~
                      an option, must be a string."))
	       ((zerop (length (cadr form)))
		(err "In (:FLAGOPT ...), the second element, which names ~
                      an option, must not be an empty string."))
	       (t
		(push (destructuring-bind (key opt &optional x &rest y)
			  form
			(declare (ignore key y))
			(cond
			  ((null x)
			   `(,opt (:flag) nil))
			  ((stringp x)
			   `(,opt (:flag)
				  (lambda () (format nil ,@(cddr form)))))
			  (t
			   (err "In (:FLAGOPT ~s ...), if a third element ~
                                 is provided, it must be a FORMAT control ~
                                 string." opt))))
		      options))))
	   (num-num-form (opt form)
	     (cond
	       ((null (cdr form))
		`(,(car form) * *))
	       ((not (or (numberp (cadr form))
			 (eq (cadr form) '*)))
		(err "In (:ARGOPT ~s ...), arguments to ~s can only be ~
                      a number or *." opt (car form)))
	       ((null (cddr form))
		`(,(car form) ,(cadr form) *))
	       ((not (or (numberp (caddr form))
			 (eq (caddr form) '*)))
		(err "In (:ARGOPT ~s ...), arguments to ~s can only be ~
                      a number or *." opt (car form)))
	       ((not (null (cdddr form)))
		(err "In (:ARGOPT ~s ...), ~s can only take up to two ~
                      arguments." opt (car form)))
	       (t
		`(,(car form) ,(cadr form) ,(caddr form)))))
	   (num-form (opt form)
	     (cond
	       ((null (cdr form))
		`(,(car form) *))
	       ((not (or (numberp (cadr form))
			 (eq (cadr form) '*)))
		(err "In (:ARGOPT ~s ...), the argument to ~s can only be ~
                      a number or *." opt (car form)))
	       ((not (null (cddr form)))
		(err "In (:ARGOPT ~s ...), ~s can only take zero or one ~
                      argument." opt (car form)))
	       (t
		`(,(car form) ,(cadr form)))))
	   (only-form (opt form)
	     (if (cdr form)
		 (err "In (:ARGOPT ~s ...), ~s takes no arguments."
		      opt (car form))
		 `(,opt)))
	   (argopt (form)
	     (cond
	       ((not (stringp (cadr form)))
		(err "In (:ARGOPT ...), the second element, which names ~
                      an option, must be a string."))
	       ((zerop (length (cadr form)))
		(err "In (:ARGOPT ...), the second element, which names ~
                      an option, must not be an empty string."))
	       (t
		(push (let ((opt (cadr form))
			    (type (caddr form))
			    (docs (cdddr form)))
			(cond
			  ((null type)
			   (setf type '(:string *)))
			  ((stringp type)
			   (setf docs (cddr form)
				 type '(:string *)))
			  ((symbolp type)
			   (cond
			     ((eq (symbol-package type) keypkg)
			      (case type
				   ((:real :rational :ratio :integer :float)
				    (setf type `(,type * *)))
				   (:string
				    (setf type `(,type *)))
				   ((:key :read :flag)
				    (setf type `(,type)))
				   (:one-of
				    (err "In (:ARGOPT ~s ...), :ONE-OF must ~
                                          appear in a sublist." opt))
				   (otherwise
				    (err "In (:ARGOPT ~s ...), ~s is an
                                      unrecognized type." opt type))))
			     (t
			      (err "In (:ARGOPT ~s ...), ~s must be a symbol ~
                                    in the keyword package." opt type))))
			  ((listp type)
			   (cond
			     ((or (not (symbolp (car type)))
				  (not (eq (symbol-package (car type)) keypkg)))
			      (err "In (:ARGOPT ~s ...), ~s must be a symbol ~
                                    in the keyword package." opt (car type)))
			     (t
			      (case (car type)
				((:real :rational :ratio :integer :float)
				 (setf type (num-num-form opt type)))
				(:string
				 (setf type (num-form opt type)))
				((:key :read :flag)
				 (setf type (only-form opt type)))
				(:one-of
				 (err "In (:ARGOPT ~s ...), :ONE-OF is not ~
                                       yet supported." opt))
				(otherwise
				 (err "In (:ARGOPT ~s ...), ~s is not a ~
                                       recognized type." opt (car type)))))))
			  (t
			   (err "In (:ARGOPT ~s ...), ~s is not a recognized ~
                                 type." opt type)))
			(cond
			  ((stringp (car docs))
			   (setf docs `(lambda () (format nil ,@docs))))
			  ((not (null docs))
			   (err "In (:ARGOPT ~s ...), the fourth element ~
                                   must be a FORMAT control string." opt))
			  (t
			   (setf docs nil)))
			`(,opt ,type ,docs))
		      options)))))
	(mapc (lambda (f)
		(if (listp f)
		    (case (car f)
		      (:name (name f))
		      (:summary (summary f))
		      (:tail (tail f))
		      (:flagopt (flagopt f))
		      (:argopt (argopt f))
		      (otherwise (err "~s is not a recognized form." f)))
		    (err "~s must be a list." f)))
	      forms))
	(unless name
	  (wrn "(:NAME ...) missing, using (:NAME \"nemo\") for now.")
	  (setf name "nemo")))
    `(spec-cli* ,name ,summary ,tail ',options)))

		;; ,(if options `(list ,@options) nil)
		;; ,(if aliases `',aliases nil)
		;; ,(if styles `',styles nil)
		;; ,(if args `',(nreverse args))))))


	;;    (mapc (lambda (form)
	;; 		 ;; We can't use CASE here because the first clause is
	;; 		 ;; meant to catch the situation where we don't have a
	;; 		 ;; (car form) as our key to CASE.  So, we use a COND and
	;; 		 ;; fake it with a macro.  If we use a CASE as an else clause
	;; 		 ;; of an IF, or as a T of a COND, everything reallllly winds
	;; 		 ;; up slammed against the right margin.  I should factor this
	;; 		 ;; out, anyway, but let's get it all working first.
	;; 		 (symbol-macrolet
	;; 		     ((key (car form)))
	;; 		   (cond
	;; 		     ((not (listp form))
	;; 		      (error "SPEC-CLI: ~s is not a list." form))

	;; 		     ((eq :name key)
	;; 		      (when name
	;; 			(warn "SPEC-CLI: (:NAME ...) should only appear once."))
	;; 		      (cond
	;; 			((cddr form)
	;; 			 (error "SPEC-CLI: (:NAME ...) must have two elements."))
	;; 			((not (stringp (cadr form)))
	;; 			 (error "SPEC-CLI: In (:NAME ...) the second element ~
        ;;                       must be a string."))
	;; 			(t
	;; 			 (setf name (cadr form)))))

	;; 		     ((eq :summary key)
	;; 		      (when summary
	;; 			(warn "SPEC-CLI: (:SUMMARY ...) should only appear once."))
	;; 		      (if (stringp (cadr form))
	;; 			  (setf summary
	;; 				`(lambda () (format nil ,@(cdr form))))
	;; 			  (error "SPEC-CLI: In (:SUMMARY ...), the first ~
        ;;                        argument must be a FORMAT string.")))

	;; 		     ((eq :tail key)
	;; 		      (when tail
	;; 			(warn "SPEC-CLI: (:TAIL ...) should only appear once."))
	;; 		      (if (stringp (cadr form))
	;; 			  (setf tail
	;; 				`(lambda () (format nil ,@(cdr form))))
	;; 			  (error "SPEC-CLI: In (:TAIL ...), the first ~
        ;;                        argument must be a FORMAT string.")))

	;; 		     ((eq :flagopt key)
	;; 		      (cond
	;; 			((zerop (length (cadr form)))
	;; 			 (error "SPEC-CLI: In (:FLAGOPT ...), the second ~
        ;;                       element names an option and cannot be an ~
        ;;                       empty string."))
	;; 			((or (null (cadr form))
	;; 			     (not (stringp (cadr form))))
	;; 			 (error "SPEC-CLI: In (:FLAGOPT ...), the second ~
        ;;                       element must be a string naming the option."))
	;; 			(t
	;; 			 (push (destructuring-bind (key opt &optional x &rest y)
	;; 				   form
	;; 				 (declare (ignore y))
	;; 				 (cond
	;; 				   ((null x)
	;; 				    (list opt :flag nil))
	;; 				   ((stringp x)
	;; 				    `(list ,opt :flag
	;; 					   (lambda () (format nil ,@(cddr form)))))
	;; 				   (t
	;; 				    (error "SPEC-CLI: In (~s ~s ...), ~
        ;;                                  the third element must be a FORMAT ~
        ;;                                  string." key opt))))
	;; 			       options))))

	;; 		     ((eq :one-of key)
	;; 		      (error "SPEC-CLI: (:ONE-OF ...) not yet implemented."))
		  
	;; 		     ((eq :argopt key)
	;; 		      (cond
	;; 			((or (null (cadr form))
	;; 			     (not (stringp (cadr form))))
	;; 			 (error "SPEC-CLI: In (:ARGOPT ...), the second ~
        ;;                       element must be a string naming the option."))
	;; 			((zerop (length (cadr form)))
	;; 			 (error "SPEC-CLI: In (:ARGOPT ...), the second ~
        ;;                       element names an option and cannot be an ~
        ;;                       empty string."))
	;; 			(t
	;; 			 (push (destructuring-bind (key opt &optional x y &rest z)
	;; 				   form
	;; 				 (declare (ignore z))
	;; 				 (cond
	;; 				   ((null x)
	;; 				    (cond
	;; 				      ((null y)
	;; 				       `(list ,opt :string nil))
	;; 				      ((stringp y)
	;; 				       `(list ,opt :string
	;; 					      (lambda ()
	;; 						(format nil ,@(cdddr form)))))
	;; 				      (t
	;; 				       (error "SPEC-CLI: In (~s ~s NIL ...), ~
        ;;                                     the fourth element ~s must be ~
        ;;                                     a FORMAT string." key opt y))))

	;; 				   ((symbolp x)
	;; 				    (cond
	;; 				      ((not (eq keypkg (symbol-package x)))
	;; 				       (error "SPEC-CLI: In (~s ~s ...), the ~
        ;;                                     type argument ~s must be a ~
        ;;                                     symbol in the keyword package."
	;; 					      key opt x))
	;; 				      ((null y)
	;; 				       `(list ,opt ,x nil))
	;; 				      (t
	;; 				       `(list ,opt ,x
	;; 					      (lambda ()
	;; 						(format nil ,@(cdddr form)))))))

	;; 				   ((listp x)
	;; 				    (cond
	;; 				      ((or (not (symbolp (car x)))
	;; 					   (not (eq keypkg
	;; 						    (symbol-package (car x)))))
	;; 				       (error "SPEC-CLI: In (~s ~s ...), the ~
        ;;                                     type argument ~s must be a ~
        ;;                                     symbol in the keyword package."
	;; 					      key opt (car x)))
	;; 				      ((null y)
	;; 				       `(list ,opt ,x nil))
	;; 				      (t
	;; 				       `(list ,opt ,x
	;; 					      (lambda ()
	;; 						(format nil ,@(cdddr form)))))))

	;; 				   ((stringp x)
	;; 				    `(list ,opt ,nil
	;; 					   (lambda ()
	;; 					     (format nil ,@(cddr form)))))))
	;; 			       options))))

	;; 		     ((member key '(:arg :args))
	;; 		      (mapc (lambda (x) (push (stringify x) args))
	;; 			    (cdr form)))

	;; 		     ((member key '(:style :styles))
	;; 		      (mapc (lambda (x)
	;; 			      (if (and (symbolp x)
	;; 				       (eq keypkg (symbol-package x)))
	;; 				  (push x styles)
	;; 				  (error "SPEC-CLI: In (~a ...), all ~
        ;;                                arguments must be keyword symbols."
	;; 					 key)))
	;; 			    (cdr form)))

	;; 		     ((member key '(:alias :aliases))
	;; 		      (mapc (lambda (x)
	;; 			      (unless (stringp x)
	;; 				(error "SPEC-CLI: In (~a ...), all arguments ~
        ;;                              must be strings." key)))
	;; 			    (cdr form))
	;; 		      (push (cdr form) aliases))

	;; 		     (t
	;; 		      (error "SPEC-CLI: (~s ...) is not a recognized form."
	;; 			     key)))))
	;; 	   forms))
	;; (unless name
	;;   (warn "SPEC-CLI: (:NAME ...) missing, using (:NAME \"nemo\") for now."))
	;; `(spec-cli* ,(or name "nemo") ,summary ,tail
	;; 	    ,(if options `(list ,@options) nil)
	;; 	    ,(if aliases `',aliases nil)
	;; 	    ,(if styles `',styles nil)
	;; 	    ,(if args `',(nreverse args))))))

;;; okay, fucked up the options quoting now
;;; trying to make a form like
;;; (spec-cli* ...
;;;            `(("verbose" :flag ,(lambda () (format nil "blah blah")))
;;;
;;; But now I think that's wrong.
;;; We should let spec-cli* create the lambda
;;; It should receive
;;;
;;; (spec-cli* ...
;;;            '(("verbose" :flag "blah ~ blah" 42)
;;;              ("other"))
