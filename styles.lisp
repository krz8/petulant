;;;; styles of option processing
(in-package #:petulant)

(defun styles-to-hash (styles)
     "Given STYLES, which is a keyword or a list of keywords, populate a
hash with a complete list of keywords and any other keywords implied
by those supplied and the running system.  Not only does
STYLES-TO-HASH convert the STYLES list to a hash \(to speed lookups\),
it \"fleshes out\" the list of styles so that it determines all
explicit and implicit aspects of Petulant's processing based on what
was originally supplied by the caller.

First, all keywords in STYLES are added to the returned hash.

Next, we ensure that one of :UNIX or :WINDOWS always appears in the
hash.  When one of these are not already present, CL:*FEATURES* is
consulted to determine which of those keywords will appear in the
hash.

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

(defvar *stylehash* (styles-to-hash nil)
  "*STYLEHASH* is typically bound by WITH-STYLEHASH for the body of a
function, ensuring that a style or list of styles set by a caller is
maintained as a hash for fast lookup of the keywords that indicate
various option processing styles supported by Petulant.  Its global
binding should be a default style hash, typically reflecting the local
operating environment.  Don't modify this value, instead bind
*STYLEHASH* to something useful \(typically via WITH-STYLEHASH\).")

(defmacro with-stylehash (styleval &body body)
  "WITH-STYLEHASH takes a style keyword, a list of style keywords, or
a hash as returned by a previous STYLES-TO-HASH, and ensures that
*STYLEHASH* is bound to such a hash for the duration of BODY."
  (let ((var (gensym)))
    `(let* ((,var ,styleval)
	    (*stylehash* (cond
			   ((hash-table-p ,var)
			    ,var)
			   ((keywordp ,var)
			    (styles-to-hash ,var))
			   ((listp ,var)
			    (styles-to-hash ,var))
			   (t
			    (error "The value supplied for styles, ~s, must ~
                                    be a hash table, a list, or NIL." ,var)))))
	    ,@body)))

(defun stylep (key)
  "Returns true when KEY is among the currently selected set of option
processing styles in *STYLEHASH*.  KEY might be :UP or :WINDOWS or :STREQ
or what-have-you."
  (gethash key *stylehash*))

(defun str=-fn ()
  "Returns the function to use for testing strings for equality under
the current style."
  (if (stylep :streq) #'string-equal #'string=))

(defun str<-fn ()
  "Returns the function to use for comparing strings under the current
style."
  (if (stylep :streq) #'string-lessp #'string<))

(defun equal-fn ()
  "Returns the function to use for comparing objects \(typically
strings\) under the current style.  Use this instead of STR=-FN when
creating hash tables, for example."
  (if (stylep :streq) #'equalp #'equal))
