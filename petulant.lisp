(in-package #:petulant)

(defun parse-unix-cli (arglist fn
		       &optional
			 (optargp-fn (constantly nil))
			 (chgname-fn #'identity))
  "This is the low level parser for Unix-style command lines.  If
  you're an end-user of Petulant, you might want to consider calling a
  higher level function; this one is mostly for implementation of
  other Petulant functionality.

  PARSE-UNIX-CLI works through ARGLIST, a flat list of strings from a
  command line, parsing it according to most POSIX and GNU behaviors.
  As options are identified, OPTARGP-FN is called to determine if that
  option takes an argument.

  FN is called for each option \(with or without an argument\) and every
  non-option argument.  Each call has three arguments.  The first is
  always :ARG or :OPT.  When :ARG, the second argument is a non-option
  argument string from the command line, and the third argument is
  NIL.  When :OPT, the second argument is an option \(a string\) found
  on the command line, eliding any leading dashes, and the third
  argument is any argument to that option or NIL.

  OPTARGP-FN, if supplied, is a mechanism for the caller to indicate
  when an option, long or short, should take the next word in ARGLIST
  as an argument.  The default binding of OPTARGP-FN always returns
  NIL, indicating that any ambiguous option is assumed not to take an
  argument.  The only non-ambiguous option with an argument are long
  options that use the \"=\" character \(e.g., \"--foo=bar\"\).

  CHGNAME-FN, if supplied, can be used to change a detected option
  from one value to another, taking a string and returning a string to
  use in its place.  It could be used to implement aliases or partial
  matching, for example.  Every detected option, long or short, is
  passed through this function before processing continues; it is
  called before OPTARGP-FN, for example.

  Generally speaking, the calls to FN proceed from the head to the
  tail of ARGLIST, and from left to right within each string of
  ARGLIST.  This is useful to know in testing, but callers probably
  should not rely on any specific ordering."
  (do ((av arglist))
      ((null av) t)
    (labels ((optargp (x) (funcall optargp-fn x))
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
		   ((optargp f)                             ; "--foo" "xyz"
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
		     ((not (optargp f))			    ; "-fgh"
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
  higher level function; this one is mostly for implementation of
  other Petulant functionality.

  PARSE-WINDOWS-CLI works through ARGLIST, a flat list of strings
  delivered from some OS-specific wrapper in the Lisp environment
  parsing it according to most Windows behaviors.  As switches are
  identified, SWARGP-FN is called to determine if that switch takes an
  argument.

  FN is called for each switch \(with or without an argument\) and every
  non-switch argument.  Each call has three arguments.  The first is
  always :ARG or :OPT.  When :ARG, the second argument is a non-switch
  argument string from the command line, and the third argument is
  NIL.  When :OPT, the second argument is a switch \(a string\) found on
  the command line, eliding its leading slash, and the third argument
  is any argument to that option or NIL.

  SWARGP-FN, if supplied, is a mechanism for the caller to indicate
  when a switch should take an argument.  The default binding of
  SWARGP-FN always returns NIL, indicating that any ambiguous switch
  is assumed not to take an argument.  A non-ambiguous switch with an
  argument is one that uses the colon character \(e.g., \"/foo:bar\"\).

  CHGNAME-FN, if supplied, can be used to change a detected switch
  from one value to another, taking a string and returning a string to
  use in its place.  It could be used to implement aliases or partial
  matching, for example.  Every detected switch is passed through this
  function before processing continues; it is called before SWARGP-FN,
  for example.

  Generally speaking, the calls to FN proceed from the head to the
  tail of ARGLIST, and from left to right within each string of
  ARGLIST.  This is useful to know in testing, but callers probably
  should not rely on any specific ordering."
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
  environment.  This is necessarily OS specific.  It's assumed that
  the list returned by ARGV does not include the executable name,
  image name, argv[0], or other non-argument information."
  #+ccl ccl:*command-line-argument-list*
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+acl (cdr (sys:command-line-arguments))
  #- (or ccl sbcl clisp acl)
  (error "Petulant needs to be ported to this Lisp environment."))

(defun canonicalize-styles (styles)
  "Given STYLES, which might be a keyword or a list of keywords,
  return a complete list of keywords and any other keywords they imply.

  If :UNIX or :WINDOWS appears in styles, it is left as-is.
  Otherwise, CL:*FEATURES* is consulted, and if :WINDOWS appears
  there, it is added to STYLES.  In this way, we support a default
  based on the local operating system, but make it easy for clients of
  Petulant to override this forcing one or the other behavior.

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

(defun simple-parse-cli (fn &key arglist optarg-p-fn chgname-fn styles)
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

  OPTARG-P-FN, if supplied, is a function.  Petulant recognizes certain
  long options (\"--foo=bar\") and switches (\"/foo:bar\") that
  unambiguously present an option taking an argument.  However, Petulant
  cannot know for certain when a short option (\"-f\" \"bar\") takes an
  option, nor can it discern when a long option (\"--foo\" \"bar\") or a
  switch (\"/foo\" \"bar\") lacking extra punctuation takes an argument.
  The caller can supply a function taking the name of the option as a
  string (\"f\" or \"foo\") and returning true or false to indicate if
  it takes an argument.

  CHGNAME-FN, if supplied, can be used to change a detected switch
  from one value to another, taking a string and returning a string to
  use in its place.  It could be used to implement aliases or partial
  matching, for example.  Every detected switch is passed through this
  function before processing continues; it is called before OPTARG-P-FN,
  for example.

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
	     (or optarg-p-fn (constantly nil))
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

(defun partials-fn (optargs optflags aliases styles)
  "When STYLES contains :PARTIAL, return a function that implements
  partial matching for all the options seen in OPTARGS, OPTFLAGS, and
  ALIASES; otherwise, #'IDENTITY is returned and no partial matching
  is supported.

  When partial matching is desired, OPTARGS OPTFLAGS ALIASES and
  STYLES are taken in the same format as PARSE-CLI.  A function is
  returned that takes a single string as an option or argument
  appearing on a command-line, recognizing unambiguous partial matches
  of options as they appear in the three supplied lists, and returning
  a possibly new string that PARSE-CLI should process as if it were
  the original word from the command-line."
  (with-styles-canon (styles styles)
    (cond
      ((member :partial styles)
       (let ((dict (make-dict :loose (foldp styles)))
	     (str= (str=-fn styles)))
	    (labels ((maybe-add (o)
		       (unless (dict-word-p dict o)
			 (dict-add dict o))))
	      (mapc #'maybe-add (append optargs optflags))
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

(defun build-optarg-hash (optargs styles)
  "Returns a hash table initialized with the options listed in
  OPTARGS.  Each key simply maps to T; this hash is treated as a set
  function.  STYLES is used to initialize the hash table's equality
  test."
  (with-styles-canon (styles styles)
    (let ((hash (make-hash-table :test (eq=-fn styles))))
      (mapc (lambda (optarg) (setf (gethash optarg hash) t))
	    optargs)
      hash)))

(defun optarg-p-fn (optargs styles)
  "Given OPTARGS, a list of strings denoting options that take
  arguments, this returns a function that can be used by Petulant to
  test if an ambiguous option consumes arguments or not.

  STYLES is a keyword or list of keywords influencing the comparison
  between options provided by the calling application and options
  supplied by the user.  In order of priority, :STR= prevents case
  folding, :STREQ directs case-insensitive matching, and :UP :DOWN
  and :KEY all imply :STREQ."
  (with-styles-canon (styles styles)
    (let ((hash (build-optarg-hash optargs styles)))
      (lambda (x) (gethash x hash)))))

(defun hack-option-fn (styles)
  "Compose a new function that calls other functions to transform
\(hack\) a single argument that is an option name.  These other
functions are based on the contents of STYLES and OPTARGS, and might
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

(defun parse-cli (fn &key optargs optflags aliases arglist styles)
  "PARSE-CLI examines the command-line with which an application was
  invoked.  According to given styles and the local environment,
  options (aka switches) and arguments are recognized.

  FN is a function supplied by the caller, which is called for each
  option or argument identified by PARSE-CLI.  Each call to FN has
  three arguments.  The first is the keyword :OPT or :ARG, indicating
  whether an option \(aka switch\) or an non-option argument was
  found.  When :ARG, the second argument is a string, an argument from
  the command-line that was not associated with an option, and the
  third argument is NIL.  When :OPT, the second argument is usually a
  string naming an option \(although see STYLES below\), and the third
  argument is a string value associated with that option, or NIL.

  OPTARGS, if supplied, is a list of all options \(short or long\)
  that require an argument.  While Petulant can automatically
  recognize some options that explicitly take an argument \(as in
  \"--file=foo.psd\" or \"/file:foo.psd\"\), it needs the hint in
  OPTARGS to recognize other patterns \(such as \"-f\" \"foo.psd\", or
  \"/file\" \"foo.psd\"\).  Simply place the option (no leading
  hyphens or slashes) as a string in this list.  The call below would
  recognize both \"-f\" and \"--file\" as requiring an argument.
  \(Note that \"f\" in the list is better handled by an alias below,
  or by the use of :PARTIAL in STYLES; its presence here is merely
  for example.\) OPTARGS does not limit the options that PARSE-CLI
  handles, even those with arguments; it is merely a hint that

     \(parse-cli … :optargs '\(\"f\" \"file\"\) … \)

  OPTFLAGS, if supplied, is a list of all the options \(short or
  long\) that do not take an argument.  This argument has no effect on
  PARSE-CLI unless :PARTIAL appears in STYLES.  See :PARTIAL below.

     \(parse-cli … :optflags '\(\"verbose\" \"debug\" \"trace\"\) … \)

  ALIASES can be used to supply one or more alternative options that,
  when encountered, are considered aliases for another option.
  ALIASES is a list of lists.  Every element of ALIASES is a list
  naming the primary option first, followed by all aliases for it.
  For example, in the call below, both \"/sleep\" and \"/wait\" would
  be recognized by PARSE-CLI, but processed as if \"/delay\" were
  seen.

    \(parse-cli … :aliases '\(\(\"alpha\" \"transparency\"\)
			     \(\"delay\" \"sleep\" \"wait\"\)
			     \(\"file\" \"f\"\)\) … \)

  ARGLIST causes PARSE-CLI to parse a specified list of strings,
  instead of the default command-line that was supplied to the
  application.  These strings are parsed exactly as if they appeared
  on the command-line, each string corresponding to one \"word\".

     \(parse-cli … :arglist '\(\"-xv\" \"-f\" \"foo.tar\"\) … \)

  STYLES is a keyword, or a list of keywords, that influence
  Petulant's behavior.  Recognized keywords are as follows;
  unrecognized keywords are silently ignored.

     :STR=    String matching between OPTARGS, OPTFLAGS, ALIASES, and
	      the command-line being parsed is sensitive to case.
	      This exists solely to override any folding semantics
	      implied by :WINDOWS, :UNIX, :UP, :DOWN, :KEY, and the
	      local Lisp environment.  Overrides :STREQ.  Its name is
	      meant to be evocative of STRING=.

     :STREQ   String matching between OPTARGS, OPTFLAGS, ALIASES, and
	      the command-line being parsed is insensitive to case.
              Its name is meant to be evocative of STRING-EQUAL.

     :UP      All option names presented to FN will be converted to
	      upper case.  Implies :STREQ.

     :DOWN    All option names presented to FN will be converted to
	      lower case.  Implies :STREQ.

     :KEY     All option names presented to FN will be converted to
	      symbols in the keyword package.  Implies :UP.

     :PARTIAL Support partial matches of options.  When present,
              Petulant will support unambiguous partial matches of
	      options \(as they appear in OPTARGS, OPTFLAGS, and
	      ALIASES\).  For example, if OPTARGS contained \"beat\",
	      then :PARTIAL would trigger aliases of \"b\", \"be\",
	      and \"bea\" for \"beat\".  But, if OPTFLAGS also
	      contained \"bop\" then \"b\" would no longer be
	      automatically created as an alias, and \"bo\" would be
	      added as an alias for \"bop\".

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
			  :optarg-p-fn (optarg-p-fn optargs styles)
			  :chgname-fn (compose (aliases-fn aliases styles)
					       (partials-fn optargs optflags
							    aliases styles))
			  :arglist arglist
			  :styles styles)))))

(defun get-cli (&key optargs optflags aliases arglist styles)
  "GET-CLI takes the same keyword arguments, and offers the same
  functionality, as the function it wraps, PARSE-CLI.  See that
  function for complete documentation."
  (let (results)
    (parse-cli (lambda (x y z) (push (list x y z) results))
	       :optargs optargs :optflags optflags :aliases aliases
	       :arglist arglist :styles styles)
    (nreverse results)))
