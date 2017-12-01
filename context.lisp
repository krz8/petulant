(in-package #:petulant)

(defparameter *no-doc-fn* (constantly "")
  "A closure that always returns an empty string.  Useful when dealing
with aspects of a command-line that may or may not be documented.")

(defun styles-to-hash (styles)
  "Given STYLES, which is a keyword or a list of keywords, populate
a hash with a complete list of those keywords and any other keywords
implied by those supplied as well as the running system.  Not only
does STYLES-TO-HASH convert the STYLES list to a hash \(to speed
lookups\), it \"fleshes out\" the list of styles so that it determines
all explicit and implicit aspects of Petulant's processing based on
what was originally supplied by the caller.

First, all keywords in STYLES are added to the returned hash.

Next, we ensure that one of :UNIX or :WINDOWS always appears in the
hash, but never both.  When one of these are not already present in
the original list of keywords, CL:*FEATURES* is consulted to determine
which will appear in the hash.

If :KEY appears in the hash, and neither :UP nor :DOWN appear in
STYLES, then :UP is added to the hash.  In other words, unless
overridden with :DOWN, :KEY implies :UP.

If :UP or :DOWN appear in the hash, and neither :STREQ nor :STR=
appear, then :STREQ is added to the hash.  In other words, :UP
and :DOWN both imply :STREQ unless :STR= is already present.

If neither :STREQ nor :STR= appear in the hash, but :WINDOWS does,
then :STREQ is added to the hash, otherwise :STR= is added.  In other
words, :WINDOWS implies :STREQ while :UNIX implies :STR=, but either
is easily overridden."
  (let ((hash (make-hash-table)))
    (labels ((set? (&rest flags) (some (lambda (f) (gethash f hash)) flags))
	     (set! (&rest flags)
	       (mapc (lambda (f)
		       (unless (keywordp f)
			 (error "Option processing styles must be specified ~
                                 as keyword values, which ~s is not." f))
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
              call to Petulant according to STYLES-TO-HASH.")  ))

(defparameter *context* (make-context "" nil nil nil nil nil))

(defun stylep (key)
  "Returns true if KEY appears in the style hash of the current
context, otherwise NIL."
  (gethash key (stylehash *context*)))


;; now how is that going to work? when we initialize, we'd want to
;; call make-pethash or something similar, but we can't since we
;; haven't set up the very styles in the context that we need in order
;; to call make-pethash.
;;
;; Hmm...
;;
;; make hash table inits to nil
;; write a proper constructor that sets things up from local context


(defstruct (spec-context (:conc-name nil))
  "A structure for capturing the context of an invocation of
Petulant's API.  Depending on which function in Petulant is called,
different information is supplied from the caller and different
information is needed within Petulant \(e.g., Should we process
options in a Unix or Windows style? Should string matching be
sensitive to case? Does \"foo\" take an argument?\)


.
There is a mix of hash tables, closures, strings, and lists that are
all managed in a call to SPEC* and its friends.  This structure
ensures we don't go nuts passing around five, six, or seven arguments
across so many functions.  Instead, SPEC* will create an instance of
this structure based on its arguments, and the functions in the SPEC*
family will simply operate on this structure.  Slots are provided for
capturing the original list of options and other values supplied to
SPEC*, as well as the hash tables and other computed values needed for
the duration of its call.  CONC-NAME is NIL, thus, slots of a
SPEC-CONTEXT are retrieved through a function whose name is the slot
name \(e.g., \(opthash foo\) returns the value of the slot named
OPTHASH in FOO\)."
  appname		      ; application name
  summaryfn		      ; closure providing app summary
  tailfn		      ; closure providing extra info
  options		      ; original list of options
  aliases		      ; original list of aliases
  arguments		      ; original list of command-line args
  opthash		      ; maps options to Petulant types
  dochash		      ; maps options to documentation closures
  alihash)		      ; maps aliases to options

(defun make-pethash ()
  "Referencing whatever styles are in effect via the most recent
WITH-STYLEHASH, return a new hash table with the appropriate test of
equality baked in.  These hash tables are meant to use option strings
as their keys."
  (make-hash-table :test (equal-fn)))

(defun make-context (appname summaryfn tailfn options aliases args)
  "Create a new instance of SPEC-CONTEXT and return it, with most of
its slots filled in to support the SPEC* family of functions.  It is
expected that this is called within a WITH-STYLEHASH context.  While
most slots are simply set with the arguments supplied, the slots that
are hashes are initialized by working through the OPTIONS and ALIASES
lists.  Specifically, every option named in OPTIONS appears in the
OPTHASH, the value of which is the type specification for the option.
Every option also appears in the DOCHASH, mapped to a closure
providing a documentation string for the named option.  The ALIHASH
maps alternative option names to their proper option name."
  (let ((opthash (make-pethash))
	(dochash (make-pethash))
	(alihash (make-pethash)))
    (mapc (lambda (optspec)
	    (let ((opt (car optspec)))
	      (setf (gethash opt opthash) (cadr optspec)
		    (gethash opt dochash) (or (caddr optspec) *no-doc-fn*))))
	  options)
    (mapc (lambda (alispec)
	    (let ((opt (car alispec)))
	      (mapc (lambda (a) (setf (gethash a alihash) opt))
		    (cdr alispec))))
	  aliases)
    (make-spec-context :appname appname :summaryfn (or summaryfn *no-doc-fn*)
		       :tailfn (or tailfn *no-doc-fn*) :options options
		       :aliases aliases :arguments args :opthash opthash
		       :dochash dochash :alihash alihash)))

(defparameter *context* (make-context "" nil nil nil nil nil)
  "SPEC* binds this to a new context while it executes, so that the
rest of the functions in the SPEC* family have easy access to all the
different bits of information with which we've been invoked.  Doing
this saves us an argument from nearly every single function.  Its
default contents are valid but empty.")

