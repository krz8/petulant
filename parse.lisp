(in-package #:petulant)

(defun cb (&rest args)
  "A test function for debugging PARSE \(and SIMPLE\)
that just echoes its arguments back.  You can use this as the callback
for those two functions."
  (format t "cb~{ ~s~}~%" args))

(defun partials-fn (argopts flagopts aliases &optional styles)
  "When STYLES contains :PARTIAL, return a function that implements
partial matching for all the options seen in ARGOPTS, FLAGOPTS, and
ALIASES; otherwise, #'IDENTITY is returned and no partial matching is
supported.

When partial matching is desired, ARGOPTS FLAGOPTS ALIASES and STYLES
are taken in the same format as PARSE.  A function is returned
that takes a single string as an option or argument appearing on a
command-line, recognizing unambiguous partial matches of options as
they appear in the three supplied lists, and returning a possibly new
string that PARSE should process as if it were the original word
from the command-line."
  (with-stylehash styles
    (cond
      ((stylep :partial)
       (let ((dict (make-dict :loose (stylep :streq)))
	     (str= (str=-fn)))
	 (labels ((maybe-add (o) (unless (dict-word-p dict o)
				   (dict-add dict o))))
	   (mapc #'maybe-add (append argopts flagopts))
	   (mapc (lambda (alist) (mapc #'maybe-add (cdr alist)))
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

(defun build-alias-hash (aliases &optional styles)
  "Returns a hash table initialized with the option alias list ALIASES.
In that alist, the CDR of each element are all the strings that are
mapped to the CAR of the element.  STYLES is used to determine how to
compare strings within the hash table."
  (with-stylehash styles
    (let ((hash (make-hash-table :test (equal-fn))))
      (mapc (lambda (entry)
	      (let ((to (car entry)))
		(mapc (lambda (from) (setf (gethash from hash) to))
		      (cdr entry))))
	    aliases)
      hash)))

(defun aliases-fn (aliases &optional styles)
  "Given ALIASES, a list that maps one or more strings to an intended
option, this creates the function that performs that mapping according
to STYLES.  The returns function takes an option string as parsed by
SIMPLE, and returns a string to use in its stead.

ALIASES is an association list of strings, where the CAR of each entry
is an intended option, and the CDR is a list of strings that are
mapped to the intended option.

STYLES is a keyword or list of keywords influencing the matching
between command-line options and the list of aliases seen here."
  (with-stylehash styles
    (let ((hash (build-alias-hash aliases styles)))
      (lambda (x) (aif (gethash x hash)
		       it
		       x)))))

(defun build-argopt-hash (argopts &optional styles)
  "Returns a hash table initialized with the options listed in
ARGOPTS.  Each key simply maps to T; this hash is treated as a set
function.  STYLES is used to initialize the hash table's equality
test."
  (with-stylehash styles
    (let ((hash (make-hash-table :test (equal-fn))))
      (mapc (lambda (argopt) (setf (gethash argopt hash) t))
	    argopts)
      hash)))

(defun argopt-p-fn (argopts &optional styles)
  "Given ARGOPTS, a list of strings denoting options that take
arguments, this returns a function that can be used by Petulant to
test if an ambiguous option consumes arguments or not.

STYLES is a keyword or list of keywords influencing the comparison
between options provided by the calling application and options
supplied by the user.  In order of priority, :STR= prevents case
folding, :STREQ directs case-insensitive matching, and :UP :DOWN
and :KEY all imply :STREQ."
  (with-stylehash styles
    (let ((hash (build-argopt-hash argopts styles)))
      (lambda (x) (gethash x hash)))))

(defun hack-option-fn (&optional styles)
  "Compose a new function that calls other functions to transform
\(hack\) a single argument that is an option name.  These other
functions are based on the contents of STYLES, and might change the
case of the option, they might replace it with a symbol from the
keyword package, they might substitute aliases or recognize partial
matches.  The results of passing an option string through the composed
function is an object (string or keyword) ready for the supplied
callback function."
  (with-stylehash styles
    (let ((funcs nil))
      (when (stylep :down)
	(push #'string-downcase funcs))
      (when (stylep :up)
	(push #'string-upcase funcs))
      (when (stylep :key)
	(push (lambda (x) (intern x "KEYWORD")) funcs))
      (when (null funcs)
	(push #'identity funcs))
      (apply #'compose funcs))))

(defun parse (fn &key argopts flagopts aliases arglist styles)
  "PARSE examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.

FN is a function supplied by the caller, which is called for each
option or argument identified by PARSE.  Each call to FN has three
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
options that PARSE handles, even those with arguments; it is
merely a hint that

   \(parse … :argopts '\(\"f\" \"file\"\) … \)

FLAGOPTS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  This argument has no effect on
PARSE unless :PARTIAL appears in STYLES.  See :PARTIAL below.

   \(parse … :flagopts '\(\"verbose\" \"debug\" \"trace\"\) … \)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is a list of lists.  Every element of ALIASES is a list naming the
primary option first, followed by all aliases for it.  For example, in
the call below, both \"/sleep\" and \"/wait\" would be recognized by
PARSE, but processed as if \"/delay\" were seen.

  \(parse … :aliases '\(\(\"alpha\" \"transparency\"\)
			   \(\"delay\" \"sleep\" \"wait\"\)
			   \(\"file\" \"f\"\)\) … \)

ARGLIST causes PARSE to parse a specified list of strings, instead
of the default command-line that was supplied to the application.
These strings are parsed exactly as if they appeared on the
command-line, each string corresponding to one \"word\".

   \(parse … :arglist '\(\"-xv\" \"-f\" \"foo.tar\"\) … \)

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
  (with-stylehash styles
    (let ((hack-fn (hack-option-fn styles)))
      (flet ((cb (x y z)
	       (case x
		 (:opt (funcall fn x (funcall hack-fn y) z))
		 (:arg (funcall fn x y z)))))
	(simple #'cb
		:argoptp-fn (argopt-p-fn argopts styles)
		:chgname-fn (compose (aliases-fn aliases styles)
				     (partials-fn argopts flagopts
						  aliases styles))
		:arglist arglist
		:styles styles)))))
