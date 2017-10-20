(in-package #:petulant)

(defun parse-unix-cli (arglist fn
		       &optional
			 (argoptp-fn (constantly nil))
			 (chgname-fn #'identity))
  "This is the low level parser for Unix-style command lines.  If
  you're an end-user of Petulant, you might want to consider calling a
  higher level function; this one is mostly for implementation of
  other Petulant functionality.

  PARSE-UNIX-CLI works through ARGLIST, a flat list of strings from a
  command line, parsing it according to most POSIX and GNU behaviors.
  As options are identified, ARGOPTP-FN is called to determine if that
  option takes an argument.

  FN is called for each option \(with or without an argument\) and every
  non-option argument.  Each call has three arguments.  The first is
  always :ARG or :OPT.  When :ARG, the second argument is a non-option
  argument string from the command line, and the third argument is
  NIL.  When :OPT, the second argument is an option \(a string\) found
  on the command line, eliding any leading dashes, and the third
  argument is any argument to that option or NIL.

  ARGOPTP-FN, if supplied, is a mechanism for the caller to indicate
  when an option, long or short, should take the next word in ARGLIST
  as an argument.  The default binding of ARGOPTP-FN always returns
  NIL, indicating that any ambiguous option is assumed not to take an
  argument.  The only non-ambiguous option with an argument are long
  options that use the \"=\" character \(e.g., \"--foo=bar\"\).

  CHGNAME-FN, if supplied, can be used to change a detected option
  from one value to another, taking a string and returning a string to
  use in its place.  It could be used to implement aliases or partial
  matching, for example.  Every detected option, long or short, is
  passed through this function before processing continues; it is
  called before ARGOPTP-FN, for example.

  Generally speaking, the calls to FN proceed from the head to the
  tail of ARGLIST, and from left to right within each string of
  ARGLIST.  This is useful to know in testing, but callers probably
  should not rely on any specific ordering."
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

(defun simple-parse-cli (fn &key arglist argopt-p-fn chgname-fn styles)
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

  ARGOPT-P-FN, if supplied, is a function.  Petulant recognizes certain
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
  function before processing continues; it is called before ARGOPT-P-FN,
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
  ALIASES; otherwise, #'IDENTITY is returned and no partial matching
  is supported.

  When partial matching is desired, ARGOPTS FLAGOPTS ALIASES and
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
  option or argument identified by PARSE-CLI.  Each call to FN has
  three arguments.  The first is the keyword :OPT or :ARG, indicating
  whether an option \(aka switch\) or an non-option argument was
  found.  When :ARG, the second argument is a string, an argument from
  the command-line that was not associated with an option, and the
  third argument is NIL.  When :OPT, the second argument is usually a
  string naming an option \(although see STYLES below\), and the third
  argument is a string value associated with that option, or NIL.

  ARGOPTS, if supplied, is a list of all options \(short or long\)
  that require an argument.  While Petulant can automatically
  recognize some options that explicitly take an argument \(as in
  \"--file=foo.psd\" or \"/file:foo.psd\"\), it needs the hint in
  ARGOPTS to recognize other patterns \(such as \"-f\" \"foo.psd\", or
  \"/file\" \"foo.psd\"\).  Simply place the option (no leading
  hyphens or slashes) as a string in this list.  The call below would
  recognize both \"-f\" and \"--file\" as requiring an argument.
  \(Note that \"f\" in the list is better handled by an alias below,
  or by the use of :PARTIAL in STYLES; its presence here is merely
  for example.\) ARGOPTS does not limit the options that PARSE-CLI
  handles, even those with arguments; it is merely a hint that

     \(parse-cli … :argopts '\(\"f\" \"file\"\) … \)

  FLAGOPTS, if supplied, is a list of all the options \(short or
  long\) that do not take an argument.  This argument has no effect on
  PARSE-CLI unless :PARTIAL appears in STYLES.  See :PARTIAL below.

     \(parse-cli … :flagopts '\(\"verbose\" \"debug\" \"trace\"\) … \)

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

     :STR=    String matching between ARGOPTS, FLAGOPTS, ALIASES, and
	      the command-line being parsed is sensitive to case.
	      This exists solely to override any folding semantics
	      implied by :WINDOWS, :UNIX, :UP, :DOWN, :KEY, and the
	      local Lisp environment.  Overrides :STREQ.  Its name is
	      meant to be evocative of STRING=.

     :STREQ   String matching between ARGOPTS, FLAGOPTS, ALIASES, and
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
	      options \(as they appear in ARGOPTS, FLAGOPTS, and
	      ALIASES\).  For example, if ARGOPTS contained \"beat\",
	      then :PARTIAL would trigger aliases of \"b\", \"be\",
	      and \"bea\" for \"beat\".  But, if FLAGOPTS also
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
  :ARG, then the second argument is a string naming an argument
  not associated with an option on the command-line.  Otherwise,
  the first element is :OPT, the second argument is the option \(as
  a string or keyword symbol, according to the STYLES argument\),
  and if the option has an argument, that string appears as the
  third element.

  All arguments and options to GET-CLI share the same name and carry
  the same functionality as they appear in PARSE-CLI."
  (let (results)
    (parse-cli (lambda (&rest args) (push args results))
	       :argopts argopts :flagopts flagopts :aliases aliases
	       :arglist arglist :styles styles)
    (nreverse results)))

(defun hanging-par (stream label
		    &optional format-string &rest other-format-args)
  "Format a hanging paragraph onto the supplied STREAM.  LABEL is a
  simple string that is the hang, it appears at the left margin and
  will be suffixed with a colon and a space.  At that resulting
  column, text is generated using FORMAT-STRING and any
  OTHER-FORMAT-ARGS. That text is then justified across as many lines
  as necessary to present it to the user.  The overall effect is that
  output to STREAM is generated that resembles

     label: text text text text text text text text text text text
            and even more text text text text text text text text
"
  (let* ((words (split '(#\Space #\Tab #\Return #\Newline #\Page)
		       (apply #'format nil format-string other-format-args)))
	 (spaces (make-string (+ (length label) 2) :initial-element #\Space))
	 (format (concatenate 'string "~a: ~{~<~%" spaces "~1:;~a~>~^ ~}~%")))
    (format stream format label words)))

(defun usage-header (stream label
		     &optional format-string &rest other-format-args)
  (cond
    (format-string
     (hanging-par stream label format-string other-format-args))
    (t
     (princ label stream)
     (terpri stream))))

(defun usage-footer (stream &optional format-string &rest other-format-args)
  (format stream "~%~{~<~%~1:;~a~>~^ ~}~%"
	  (split '(#\Space #\Tab #\Return #\Newline #\Page)
		      (apply #'format nil format-string other-format-args))))

(defun spec-cli* (&rest forms)
  "A series of forms... see SPEC-CLI."
  (let ((name "app")
	(desc (make-hash-table))
	summary tail argopts flagopts aliases arglist styles)
    (iterate (for f in forms)
	     (unless (listp f)
	       (error "SPEC-CLI needs a list of forms"))
	     (case (car f)
	       (:summary (setf summary (cdr f)))
	       (:name (setf name (cadr f)))
	       (:tail (setf tail (cdr f)))
	       (:alias (push (cdr f) aliases))
	       ((:arg :args) (iterate (for x in (cdr f))
				 (collect x into y)
				 (finally (setf arglist y))))
	       ((:style :styles)
		(iterate (for x in (cdr f))
			 (collect x into y)
			 (finally (setf styles y))))
	       (:flagopt (push (cadr f) flagopts)
			 (setf (gethash (cadr f) desc) (cddr f)))
	       (:argopt (push (cadr f) argopts)
			(setf (gethash (cadr f) desc) (cddr f)))
	       ))
    (format *error-output* "name ~s~%summary ~s~%tail ~s~%~
                            flagopts ~s~%argopts ~s~%aliases ~s~%~
                            styles ~s~%arglist ~s~%"
	    name summary tail flagopts argopts aliases styles arglist)
    (maphash (lambda (k v) (format *error-output* "desc of ~s is ~s~%" k v)) 
	     desc)
    (apply #'usage-header *error-output* name summary)
    (when tail
      (apply #'usage-footer *error-output* tail))))

(defmacro spec-cli (&rest forms)
  "Using a series of forms specifying a complete command-line interface
  presented to the user, blah blah blah...

  \(:NAME \"…\"\) provides a name for the application.  Eventually,
  we might tease the name under which we were invoked out of the running
  Lisp environment, but for now, every well-behaved application should
  supply its name

  \(:SUMMARY \"…\" …\) provides a short summary of the application.
  The first string and any subsequent arguments are applied to the
  FORMAT function (those arguments are only evaluated when a suitable
  help message is generated).  The resulting summary text will be
  further reformatted and justified to take as many lines as necessary
  on output; breaking will occur on any whitespace characters embedded
  within the result of the call to FORMAT.  Whitespace includes
  spaces, tabs, newlines, and carriage returns, so don't bother using
  them, they will be silently disregarded when usage messages are
  generated.  Examples:

     \(:SUMMARY \"stimulate the beaded hamster\"\)

     \(:SUMMARY \"do the thing with the thing and the other things ~
               and report things found \(version ~a, ~a\)\"
               \(get-version-string\) \(get-release-date\)\)

  Combined with \(:NAME …\), the use of \(:SUMMARY \"…\" …\) will generate
  text formatted in the following way:

     name: summary text summary text summary text summary text
	   and even more summary text summary text summary text
	   as little or as much as it takes…

  \(:TAIL \"…\" …\) provides further text to appear at the bottom of a
  usage page, perhaps supplying copyright, author information, or anything
  else that isn't important enough to appear in the summary."
  (apply #'spec-cli* (iterate (for f in forms)
			      (collect f))))
