(in-package #:petulant)

(defclass context ()
  ((appname :initarg :appname :initform "" :accessor appname
	    :documentation "A string that names our application.")
   (summary-fn :initarg :summary-fn :initform *no-doc-fn* :accessor summary-fn
	       :documentation "A closure that returns a string
               describing the application.  This string will
               eventually be reformatted across as many lines of
               output as necessary.")
   (tail-fn :initarg :tail-fn :initform *no-doc-fn* :accessor tail-fn
	    :documentation "A closure that returns a string containing
	    extra information about the application.  This string will
	    eventually be reformatted across as many lines of output
	    as necessary.")
   (opthash :initarg :opthash :initform nil :accessor opthash
	    :documentation "A hash table that maps strings naming
	    options to Petulant type specifications.")
   (dochash :initarg :dochash :initform nil :accessor dochash
	    :documentation "A hash table that maps strings naming
	    options to closures that return strings describing the
	    options.")
   (alihash :initarg :alihash :initform nil :accessor alihash
	    :documentation "A hash table that maps strings naming
	    aliases to strings naming options.  Typically, this is a
	    many-to-few mapping.")
   (stylehash :initarg :stylehash :initform nil :accessor stylehash
	      :documentation "A hash table representing all style
              keywords, express and implied, during the context of a
              call to Petulant according to STYLES-TO-HASH."))
  (:documentation "Captures a description of the current definitions
  of options, styles, documentations, and everything else that goes
  into Petulant.  This structure might be filled out completely \(when
  a full specification is supplied\), or minimally \(when a low level
  functional interface is selected\).  In either setting, an instance
  of this class becomes the current context of a call through
  Petulant."))

(defun styles-to-hash (styles)
  "Given STYLES, which is a keyword or a list of keywords, populate a
hash with a complete list of those keywords and any other keywords
implied by those supplied as well as the running system.  Not only
does STYLES-TO-HASH effectively \"convert\" the supplied STYLES list
into a hash, it \"fleshes out\" the list of styles so that it
determines all explicit and implicit aspects of Petulant's processing
based on what was originally supplied by the caller.

First, all keywords in STYLES are added to the returned hash.

Next, we ensure that one of :UNIX or :WINDOWS always appears in the
hash, but never both.  When one of these are not already present in
the original list of keywords, CL:*FEATURES* is consulted \(or,
possibly, some other platform-specific feature\) to determine which
will appear in the hash.

If :KEY appears in the hash, and neither :UP nor :DOWN appear in
STYLES, then :UP is added to the hash.  In other words, unless
overridden with :DOWN, :KEY implies :UP.  When :KEY so appears, option
strings will be converted to keyword symbols; otherwise, they are left
as strings.

If :UP or :DOWN appear in the hash, and neither :STREQ nor :STR=
appear then :STREQ is added to the hash.  In other words, :UP
and :DOWN both imply :STREQ unless :STR isalready present.  When :UP
appears in the hash, all lowercase letters appearing in an option
string is mapped to uppercase; conversely, when :DOWN appears, the
opposite conversion is applied.

If neither :STREQ nor :STR= appear in the hash, but :WINDOWS does,
then :STREQ is added to the hash, otherwise :STR= is added. In other
words, :WINDOWS implies :STREQ while :UNIX implies :STR=, but either
is easily overridden.

When :STREQ is present in the hash, string comparisons between known
options and those appearing on a command-line will be performed
independent of case, using STRING-EQUAL or EQUALP.  When :STR= is
present in the hash, those comparisons are performed with STRING= or
EQUAL."
  (let ((hash (make-hash-table :test #'eq)))
    (labels ((set? (&rest flags) (some (lambda (f) (gethash f hash)) flags))
	     (set! (&rest flags)
	       (mapc (lambda (f)
		       (unless (keywordp f)
			 (error "PETULANT: Option processing styles must be ~
                                 specified as keyword values, which ~s is ~
                                 not." f))
		       (setf (gethash f hash) f))
		     flags)))
      (apply #'set! (ensure-list styles))
      (unless (set? :windows :unix)
	(set! (if (featurep :windows) :windows :unix)))
      (unless (set? :up :down)
	(when (set? :key)
	  (set! :up)))
      (unless (set? :str= :streq)
	(set! (or (and (set? :up :down :windows) :streq)
		  :str=))))
    hash))

(defparameter *context* (make-instance 'context
				       :stylehash (styles-to-hash nil)
				       :alihash (make-hash-table)
				       :opthash (make-hash-table)
				       :dochash (make-hash-table))
  "An instance of CONTEXT captures everything we are processing within
any given call to the Petulant API.  Typically, new bindings are
established through the WITH-CONTEXT-SIMPLE and WITH-CONTEXT-FULL
macros.  *CONTEXT* has a default value that is an intance of CONTEXT
not because any processing can actually be carried out with it, but
simply so that the rest of Petulant need not test for binding to NIL
values all over the place; the slots of that default *CONTEXT* are
just barely valid.")

(defun stylep (key)
  "Returns true if KEY appears in the style hash of the current
*CONTEXT*, otherwise NIL."
  (gethash key (stylehash *context*)))

(defun mkhash ()
  "Return a new hash table with an equality test that reflects the
current *CONTEXT*."
  (make-hash-table :test (or (and (stylep :streq) #'equalp)
			     #'equal)))

(defun aliases-to-hash (aliases hashtable)
  "Given a set of option ALIASES, add them to the supplied HASHTABLE.
ALIASES is a list of alias specifications. Each list is, itself, a
list of strings.  The first string in the list is the option, and all
subsequent strings are aliases for that option.  It is up to the
caller to supply a hash table with the proper equality test."
  (mapc (lambda (spec)
	  (let ((option (car spec)))
	    (mapc (lambda (alias) (setf (gethash alias hashtable) option))
		  (cdr spec))))
	aliases)
  hashtable)

(defun make-context-simple (argopts flagopts aliases styles)
  "Creates a Petulant context from arguments typically supplied to one
of the simpler, less-specified interfaces.  ARGOPTS is a list of
strings, naming the options that are known to take arguments, while
FLAGOPTS is a list of strings naming the options that are only flags
\(not taking arguments\).  ALIASES is a list-of-lists, each sublist
providing alternate option strings that are considered aliases to the
first string in the sublist.  STYLES is a keyword, or list of
keywords, that influence how Petulant processes a command-line."
  (let ((stylehash (styles-to-hash styles)))
    (let ((opthash (mkhash)) (dochash (mkhash)) (alihash (mkhash)))
      (mapc (lambda (o) (setf (gethash o opthash) '(:string *)
			      (gethash o dochash) *no-doc-fn*))
	    argopts)
      (mapc (lambda (o) (setf (gethash o opthash) '(:flag)
			      (gethash o dochash) *no-doc-fn*))
	    flagopts)
      (aliases-to-hash aliases alihash)
      (make-instance 'context
		     :opthash opthash :dochash dochash :alihash alihash
		     :stylehash stylehash))))

(defun make-context-full (appname summary-fn tail-fn optlist aliases styles)
  "Creates a Petulant context from arguments supplied to the \"full
specification\" interface of Petulant.  These values supplied here are
typically generated by the CLI:SPEC macro \(see petulant.lisp\), and a
simplified shorthand is used by the caller.  APPNAME is a string
naming an application, while SUMMARY-FN and TAIL-FN are closures that
generate a string to be used for the primary documentation and the
extra \(tail\) documentation of an app.  OPTLIST is a list of
canonical option specifications, each being a list of the option
string, corresponding Petulant type, and documentation closure.
ALIASES is a list of alias specifications, where each spec is a list
of strings; the first string naming an option for which all other
strings in the list are aliases.  STYLES is a keyword, or list of
keywords, that influence how Petulant processes a command-line."
  (let ((stylehash (styles-to-hash styles)))
    (let ((opthash (mkhash)) (dochash (mkhash)) (alihash (mkhash)))
      (mapc (lambda (optspec)
	      (destructuring-bind (option type docfn) optspec
		(setf (gethash option opthash) type
		      (gethash option dochash) (or docfn *no-doc-fn*))))
	    optlist)
      (aliases-to-hash aliases alihash)
      (make-instance 'context
		     :appname appname
		     :summary-fn (or summary-fn *no-doc-fn*)
		     :tail-fn (or tail-fn *no-doc-fn*)
		     :opthash opthash :dochash dochash :alihash alihash
		     :stylehash stylehash))))

(defmacro with-context-simple ((argopts flagopts aliases style)
			       &body body)
  "Used to establish a new *CONTEXT* for simple calls to lower-level
Petulant functions."
  `(let ((*context* (make-context-simple ,argopts ,flagopts ,aliases ,style)))
     ,@body))

(defmacro with-context-full ((appname summary-fn tail-fn optspecs
				      aliases styles)
			     &body body)
  "Used to establish a new *CONTEXT* for fully-specified calls to
Petulant."
  `(let ((*context* (make-context-full ,appname ,summary-fn ,tail-fn
				       ,optspecs ,aliases ,styles)))
     ,@body))

(defun str<-fn ()
  "Returns the function comparing strings under the styles set in the
current *CONTEXT*."
  (if (stylep :streq) #'string-lessp #'string<))

(defun str=-fn ()
  "Returns the function testing string equality under the styles set
in the current *CONTEXT*."
  (if (stylep :streq) #'string-equal #'string=))

(defun equal-fn ()
  "Returns the function testing objects \(typically strings\) for
equality under the styles set in the current *CONTEXT*.  This must be
used instead of STR=-FN when creating hash tables, for example."
  (if (stylep :streq) #'equalp #'equal))

(defun maybe-chgopt-case-fn ()
  "Returns a function that might rewrite a supplied option \(as found
on a command-line\).  If the current *CONTEXT* contains :UP or :DOWN,
the returned function will map its string argument to upper or
lowercase, respectively.  Otherwise, the returned function simply
returns its argument unchanged."
  (cond
    ((stylep :down) #'string-downcase)
    ((stylep :up)   #'string-upcase)
    (t              #'identity)))

(defun maybe-chgopt-key-fn ()
  "Returns a function that might rewrite a supplied option \(as found
on a command-line, and as already processed by MAYBE-CHGOPT-CASE-FN\)
from a string to a keyword, if the current *CONTEXT* contains :KEY.
Otherwise, the function simply returns its argument unchanged."
  (cond
    ((stylep :key) (lambda (string) (intern string "KEYWORD")))
    (t             #'identity)))

(defun optargp-fn ()
  "Based on the current *CONTEXT*, return a function yielding true
when its supplied option string is recognized to require an argument
value.  Specifically, when a supplied option exists in the option
hash of our current *CONTEXT* defined for any type *but* a flag."
  (let ((opthash (opthash *context*)))
    (lambda (option) (aand (gethash option opthash)
			   (not (eq :flag (car it)))))))

(defun aliases-fn ()
  "Construct and return a function that, when passed a string naming
an option, either returns that option, or returns the option for which
it is an alias.  Case sensitivity is handled in the underlying hash
table itself, so it isn't necessary to worry about :UP :DOWN :STREQ
or :STR= here."
  (let ((alihash (alihash *context*)))
    (lambda (opt) (or (gethash opt alihash)
		      opt))))

(defun partials-fn ()
  "Construct and return a function that takes a string naming an
option, returning that string or possibly another string to be used as
the actual option name.  When :PARTIAL appears in the style hash of
the current *CONTEXT*, the minimum unique strings for every option and
aliases in *CONTEXT* are computed, and the returned function compares
its argument to this list and if a match is found, the actual option
is returned \(e.g., supplying \"f\" could return \"file\"\).  Like
ALIASES-FN, there's no need to worry about :STREQ :STR= :UP or :DOWN
here.

When :PARTIAL does not so appear, the returned function always returns
its argument unchanged."
  (cond
    ((stylep :partial)
     (let ((dict (make-dict :test (or (and (stylep :streq) #'equalp)
				      #'equal)))
	   (str= (str=-fn)))
       (labels ((maybe-add (opt) (unless (dict-word-p dict opt)
				   (dict-add dict opt))))
	 (maphash-keys #'maybe-add (opthash *context*))
	 (maphash-keys #'maybe-add (alihash *context*)))
       (let ((minwords (minwords dict)))
	 (lambda (option) 
	   (block nil
	     (let ((len (length option)))
	       (mapc (lambda (partial)
		       (destructuring-bind (min max opt) partial
			 (when (and (<= min len max)
				    (funcall str= option (subseq opt 0 len)))
			   (return opt))))
		     minwords)
	       option))))))
    (t
     #'identity)))
