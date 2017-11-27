(in-package #:petulant)

;;; The main point of the CLI:SPEC macro is to parse a number of
;;; different forms the caller can provide, including shortcuts and
;;; abbreviations.  CLI:SPEC is also where we intend to detect any
;;; errors as well.  From the caller's input, we should wind up with
;;; very regular forms for the CLI:SPEC* function to operate on.  We
;;; push as much of the error and warning activity to this macro as we
;;; can, so that CLI:SPEC* can proceed "trusting" that it input is
;;; correct and *very* regular.
;;;
;;; By regular, we mean that everything for a given type follows the same
;;; form by the time it gets to CLI:SPEC*.  Consider option specifications,
;;; for example.  Regardless of the different :ARGOPT and :FLAGOPT forms,
;;; the unprovided types, partially specified, and fully specified
;;; types, the optional documentation, CLI:SPEC* is provided a simple list
;;; where the first element is always the name of an option, the second
;;; element is always a full type specification, and the third element is
;;; always a closure providing documentation.
;;;
;;; Closures are used to capture format control strings and optional
;;; arguments to document both the options as well as the application
;;; itself.  There's two reasons for this.  One is that we cannot
;;; predict what arguments are needed for each bit of documentation;
;;; okay, that's not a big deal, we could solve that by capturing a
;;; list of everything provided (not just the format control string,
;;; but other arguments, functions, and expressions that might be
;;; provided as well).  The other reason, though, is that it allows us
;;; to capture the caller's full specification, including those other
;;; functions to be called, into the closure without quoting and later
;;; re-evaluating those arguments.  It would be wrong to evaluate
;;; everything when CLI:SPEC is called, so we have to ensure that
;;; evaluation is delayed until actually needed, but it's crazy to try
;;; and quote everything now and later pass it all to EVAL or
;;; something like that.  Closures are the natural way to capture a
;;; current state (set of bindings) for possible continuation later.
;;;
;;; Oh, and just to not lose track of this: we use (:FOO …) forms
;;; for the same reason DEFCLASS and other big macros do.  Traditional
;;; keywords don't fit our needs; for example, we need to provide more
;;; than one flag option or argument-taking option, thus ruling out
;;; traditional keyword-based lambda lists.  We could use keywords
;;; with list oriented arguments, but I want to make this easier for
;;; the caller.  Instead, we accept multiple subforms as seen here, in
;;; the same manner as DEFCLASS.  It also will make it so easy to
;;; extend this in the future with other forms when necessary.

(defmacro spec (&rest forms)
  "Using a series of forms specifying a complete command-line
interface presented to the user, blah blah blah…

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
trying any fancy formatter control, just stick to informative
strings.\)

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
  command interpreter is supported.  Note that Petulant's type
  specification of \(:STRING N\) has a slightly different meaning than
  Common Lisp's \(STRING N\).

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

  Hint: If you aren't certain what to use, follow this rule: If you
  want integers, use :INTEGER, otherwise use :REAL.

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
  point value that cannot be negative.  \(:REAL * 1\) indicates a
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

  \(cli:spec … \(:argopt \"color\" :key\) …\)
  $ app --color=red
  CLI:SPEC returns :RED as the argument of \"color\".

- The pseudo-type :ONE-OF can be used to introduce a list of strings
  that the user may supply as an argument to the option.  The list
  can be specified as strings or symbols; any match from the user
  counts.  The comparison is not sensitive to case, and a symbol in
  the keyword package will be returned, regardless of the specification.

   \(:argopt \"channel\" \(:one-of :red :green :blue\)\)

   \(:argopt \"xfer\" \(:one-of \"rectlin\" \"sigmoid\" \"thresh\" \"lin\"\)\)

- A pseudo-type :READ can be specified that will call the Lisp reader
  to parse the supplied argument string.  This can be exploited to
  accept other data types and expressions \(such as arrays and
  lists\).  HOWEVER this can also be risky in that the running Lisp
  environment is opened up to whatever the reader can be tricked into
  evaluating.  While something as obvious as \"\(ERASE-DISK\)\" won't
  do the trick, it isn't inconceivable that a clever user could trick
  some Lisp implementation' reader into undesirable side effects.
  While it is a handy development tool, using the :READ psuedo-type in
  an executable released into the wild is probably not a great idea.

  That said, here is an example of using the :READ pseudo-type:

  1. \(cli:spec … \(:argopt \"foo\" :read\) …\)

  2. $ app '--foo=#\(1 22 333 44 5\)'

  3. CLI:SPEC returns an array \(vector\) of five items as the
     argument value of the \"foo\" option.

  Another example:

  1. \(cli:spec … \(:argopt \"foo\" :read\) …\)

  2. C:\\Users\\krz> app /foo:\(erase-disk\)

  3. CLI:SPEC returns a list of one item, the symbol ERASE-DISK, as the
     argument of the \"foo\" option.

\(:ALIAS \"option\" \"alias\" [\"alias\" …]\) establishes one or more
aliases for a given option. Multiple instances of :ALIAS for the same
option accumulate. \(aka :ALIASES\)

   \(:alias \"dry-run\" \"n\"\)
   \(:alias \"alpha\" \"fade\" \"transparency\"\)

\(:STYLE style [style …]\) supplies one or more style options, as
documented in CLI:PARSE, to influence the parsing of the
command-line. Multiple instances of :STYLE accumulate. \(aka :STYLES\)

   \(:style :key :unix\)

\(:ARG \"command-line-arg\" [\"command-line-arg\"]\) supplies one or
more strings to be used instead of the application's actual
command-line.  Multiple instances of :ARG accumulate. \(aka :ARGS\)"
  ;; CLI:SPEC provides hand-holding.  So much hand-holding.  I figure
  ;; if someone is using CLI:SPEC and not CLI:GET or CLI:PARSE, they
  ;; want all the functionality (including the kitchen sink).  So,
  ;; we'll give it to them, catching as many problem situations as we
  ;; can.  If the caller can get through CLI:SPEC without warnings or
  ;; errors, there's no reason for them to expect anything but
  ;; success.
  (let ((keypkg (find-package :keyword))
	name summary tail options aliases styles args)
    (macrolet ((wrn (x &rest y) `(warn ,(strcat "CLI:SPEC: " x) ,@y))
	       (err (x &rest y) `(error ,(strcat "CLI:SPEC: " x) ,@y)))
      (labels
	  ((name (form)
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
			   `(list ,opt '(:flag) nil))
			  ((stringp x)
			   `(list ,opt '(:flag)
				  (lambda () (format nil ,@(cddr form)))))
			  (t
			   (err "In (:FLAGOPT ~s ...), if a third element ~
                                 is provided, it must be a FORMAT control ~
                                 string." opt))))
		      options)
		)))
	   (num-num-form (opt form)
	     (destructuring-bind (type &optional min max &rest x)
		 form
	       (cond
		 (x
		  (err "In (:ARGOPT ~s ...), ~s can only take up to two ~
                        arguments." opt type))
		 ((and (or (null max) (eq '* max))
		       (or (null min) (eq '* min)))
		  `(,type * *))
		 ((and (numberp max) (or (null min) (eq '* min)))
		  `(,type * ,max))
		 ((and (numberp min) (or (null max) (eq '* max)))
		  `(,type ,min *))
		 ((and (numberp min) (numberp max))
		  `(,type ,min ,max))
		 (t
		  (err "In (:ARGOPT ~s ...), the arguments to ~s can only be ~
                      a number or *." opt type)))))
	   (num-form (opt form)
	     (destructuring-bind (type &optional x &rest y)
		 form
	       (cond
		 (y
		  (err "In (:ARGOPT ~s ...), ~s can only take zero or one ~
                        arguments." opt type))
		 ((or (null x) (eq '* x))
		  `(,type *))
		 ((numberp x)
		  `(,type ,x))
		 (t
		  (err "In (:ARGOPT ~s ...), the argument to ~s can only be ~
                      a number or *." opt type)))))
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
			`(list ,opt ',type ,docs))
		      options))))
	   (alias (form)
	     (cond
	       ((null (cdr form))
		(err "In (~s), an option and at least one alias must ~
                      be supplied." (car form)))
	       ((null (cddr form))
		(err "In (~s ~s), at least one alias must be supplied."
		     (car form) (cadr form)))
	       ((not (stringp (cadr form)))
		(err "In (~s ...), the option must be a string." (car form)))
	       ((not (every #'stringp (cdr form)))
		(err "In (~s ~s ...), all aliases must be strings."
		     (car form) (cadr form)))
	       (t
		(push (cdr form) aliases))))
	   (style (form)
	     (cond
	       ((null (cdr form))
		(err "In (~s), at least one style keyword must be supplied."
		     (car form)))
	       ((not (every (lambda (x)
			      (and (symbolp x)
				   (eq keypkg (symbol-package x))))
			    (cdr form)))
		(err "In (~s ...), all styles must be keyword symbols."
		     (car form)))
	       (t
		(mapc (lambda (x) (push x styles))
		      (cdr form)))))
	   (arg (form)
	     (cond
	       ((null (cdr form))
		(err "In (~s), at least one argument must be supplied."
		     (car form)))
	       (t
		(mapc (lambda (x) (push (stringify x) args))
		      (cdr form))))))
	(mapc (lambda (f)
		(if (listp f)
		    (case (car f)
		      (:name (name f))
		      (:summary (summary f))
		      (:tail (tail f))
		      (:flagopt (flagopt f))
		      (:argopt (argopt f))
		      ((:alias :aliases) (alias f))
		      ((:style :styles) (style f))
		      ((:arg :args) (arg f))
		      (otherwise (err "~s is not a recognized form." f)))
		    (err "~s must be a list." f)))
	      forms))
      (unless name
	(wrn "(:NAME ...) missing, using (:NAME \"nemo\") for now.")
	(setf name "nemo")))
    ;; The difference between OPTIONS and ALIASES below is simply
    ;; this: OPTIONS needs a form that can be evaluated, (LIST ...),
    ;; because it might include a lambda form that documents the
    ;; option.  ALIASES, STYLES, and so forth are typically simple
    ;; (maybe nested) lists of constants (strings, keywords), so they
    ;; can be handled by a simpler quoted form.  That's all there is
    ;; to it.  You know, no matter how many times I use it, `',foo
    ;; always feels like I'm abusing something...
    `(petulant::spec* ,name
		      ,(or summary '(constantly ""))
		      ,(or tail '(constantly ""))
		      ,(if options `(list ,@options) 'nil)
		      ,(if aliases `',aliases 'nil)
		      ,(if styles `',styles 'nil)
		      ,(if args `',(nreverse args) 'nil))))

#|

;;; This form is for debugging, I'm leaving it in here because we
;;; might need it again later.  Primarily, it demonstrates arguments
;;; to the various documentation forms aren't actually evaluated until
;;; needed: you should see a three second difference between the
;;; timestamp printed by CLI:SPEC* and any documentation form to
;;; CLI:SPEC that uses (GET-UNIVERSAL-TIME).  Secondarily, it pprints
;;; its other options, so we can see just what CLI:SPEC is passing us.
;;; Of course, MACROEXPAND is equally useful in this role, but it was
;;; nice to just see it all in one place.

#+nil
(defun cli:spec* (name summary tail options aliases styles args)
  (macrolet ((showq (thing)
	       `(show (string-downcase (symbol-name ',thing)) ,thing)))
    (flet ((show (label value)
	     (princ label)
	     (pprint value)
	     (terpri)))
      (format t "cli:spec* starting at ~d~%pausing three seconds~%"
	      (get-universal-time))
      (sleep 3)
      (showq name)
      (if summary
	  (format t "summary ~s~%" (funcall summary))
	  (format t "no summary~%"))
      (if tail
	  (format t "tail ~s~%" (funcall tail))
	  (format t "no tail~%"))
      (showq options)
      (showq aliases)
      (showq styles)
      (showq args))))

(defun make-pethash (styles)
  "Given a STYLES specification (that may or may not have already been
canonicalized), return a new hash table with the appropriate test of
equality baked in."
  (with-styles-canon (styles styles)
    (make-hash-table :test (eq=-fn styles))))

(defun sort-out-options (opthash dochash alihash options aliases)
  "Work through OPTIONS and ALIASES, as supplied to CLI:SPEC*, and
update the option hash, option documentation hash, and alias hash
accordingly.  Every option appears in the OPTHASH, the value of which
is the type specification for the option.  Those options which have
documentation closures also appear in the DOCHASH.  The ALIHASH maps
alternative names for aliases to their proper option name."
  (mapc (lambda (optspec)
	  (let ((opt (car optspec)))
	    (setf (gethash opt opthash) (cadr optspec)
		  (gethash opt dochash) (caddr optspec))))
	options)
  (mapc (lambda (alispec)
	  (let ((opt (car alispec)))
	    (mapc (lambda (a) (setf (gethash a alihash) opt))
		  (cdr alispec))))
	aliases))

(defun cli:spec* (nane summaryfn tailfn options aliases styles args)
  "Typically invoked from the CLI:SPEC macro.  NAME is a string
identifying the running application.  SUMMARYFN and TAILFN are
closures that generate the strings that appear in a usage message.
OPTIONS is a list of option specifications, ALIASES is a list of
alternate names for those options, STYLES is a list of keyword
controlling various nuances of our command line parsing, and ARGS is a
list of strings to use instead of the command line.

Each element in the OPTIONS list must have the form \(OPTION TYPE
DOCFN\) where OPTION is a string, TYPE is a list providing a fully
qualified Petulant type specification, and DOCFN is a closure that
generates documentation for this option.

Each element in the ALIAS list is, itself, a list of strings.  The
first string in that list is an option that appears in the OPTIONS
list, and all remaining strings in the list are alternative names for
it.

STYLES is a list of keywords directing the processing of the command
line according to system (Unix or Windows), case sensitivity, string
conversion, keyword conversion, and so on.

ARGS, when not nil, is a list of strings to use instead of the
application's actual command line."
  (with-styles-canon (styles styles)
    (let ((opthash (make-pethash styles))
	  (dochash (make-pethash styles))
	  (alihash (make-pethash styles))
	  (results (make-pethash styles)))
      (labels ((argp (option) (gethash option opthash)))
	(sort-out-options opthash dochash alihash options aliases)
	))))


;;; WWI
;;; Hit parse.lisp, and change CLI:PARSE to work with hashes instead of
;;; lists.  Then create a wrapper that takes the original lists and
;;; generates hashes for use by the other function.
;;;
;;; We probably want the new wrapper to be CLI:PARSE, and the function
;;; doing the actual work and consuming hashes to be CLI:PARSE*


;; (defun label-option (option type styles &optional (padding "  "))
;;   "Given a string naming an OPTION, some kind of type specification
;; going with it (:FLAG appearing for plain options, and something else
;; for options that take arguments), and a styles list, return a string
;; to be used in a usage message for this option.  The string is always
;; padded with two spaces at its beginning.

;;    \(label-option \"alpha\" :string :unix\)
;; => \"  --alpha=VAL\"
;;    \(label-option \"beta\" :string :windows\)
;; => \"  /beta:VAL\"
;;    \(label-option \"gamma\" :flag :unix\)
;; => \"  --gamma\"
;;    \(label-option \"delta\" :flag :windows\)
;; => \"  /delta\"
;;    \(label-option \"e\" :flag :unix\)
;; => \"  -e\"
;;    \(label-option \"z\" :string :unix\)
;; => \"  -z VAL\""
;;   (with-styles-canon (styles styles)
;;     (let ((winp (member :windows styles))
;; 	  (shortp (< (length option) 2))
;; 	  (flagp (eq :flag type)))
;;       (strcat padding
;; 	      (cond (winp   "/")
;; 		    (shortp "-")
;; 		    (t      "--"))
;; 	      option
;; 	      (cond (flagp  "")
;; 		    (winp   ":VAL")
;; 		    (shortp " VAL")
;; 		    (t      "=VAL"))))))

;; (defun widest-option-label (opthash styles)
;;   "Given the type hash of all options to our application, a hash
;; containing only those options that take arguments, and the influencing
;; styles of our application, format each of them as they would appear in
;; a usage message via LABEL-OPTION, and return the length of the longest
;; string \(e.g., \" --config=VAL\"\)."
;;   (with-styles-canon (styles styles)
;;     (reduce #'max
;; 	    (maphash/c (lambda (k v) (length (label-option k v styles)))
;; 		       opthash))))

;; (defun fmt (format-args)
;;   "Format the list FORMAT-ARGS as if they were arguments to the FORMAT
;; utility, returning the resulting string.  The first element of
;; FORMAT-ARGS must be a string, and any subsequent elements are as
;; called for in the first string."
;;   (apply #'format nil format-args))

;; (defun canonicalize-type (type-spec)
;;   "Try to put TYPE-SPEC into a canonical form, undoing the
;; abbreviations and shortcuts that are commonly accepted.  If TYPE-SPEC
;; is a keyword, then it is reformed to a list with * in the position of
;; its arguments.  If it is a list, but it takes more arguments than are
;; provided, * is appended for each of the missing arguments."
;;   (when type-spec
;;     (destructuring-bind (&optional x y z)
;; 	(ensure-list type-spec)
;;       (case x
;; 	((:integer :float :ratio :rational :real) ; two arguments
;; 	 (list x (or y '*) (or z '*)))
;; 	(:string			; one argument
;; 	 (list x (or y '*)))
;; 	(otherwise
;; 	 (list x))))))

;; (defun option-type (type-spec)
;;   "This function returns text that at least somewhat describes the
;; supplied type for use in a help or usage message.  In addition to
;; obvious types like :FLOAT, :INTEGER, :RATIO, :RATIONAL, and :REAL, the
;; following pseudo-types are supported:

;; :KEY is not really a type, but instead, represents the intent to take
;; the supplied string, trim whitespace from either end, convert it to
;; upper case, and intern the result as a symbol in the keyword package.

;; :READ is not really a type, but instead, represents the desire to call
;; the Lisp READ-FROM-STRING on the supplied argument and take the result
;; as-is.  This could be used for reading lists from the command-line or
;; other weirdness.  This could lead to very unexpected behavior \(you
;; know how users are\), so use this pseudo-type with great care.

;; :STRING is slightly different than the built-in Lisp STRING type.
;; When its single optional argument appears, it is taken as a maximum
;; length for the string."
;;   (destructuring-bind (&optional d0 d1 d2)
;;       (canonicalize-type type-spec)
;;     (apply #'strcat
;; 	   (case d0
;; 	     ((:float :integer :ratio :rational :real)
;; 	      (list "This option takes"
;; 		    (case d0
;; 		      (:integer " an integer")
;; 		      (:float " a floating point value")
;; 		      (:rational " a rational number (like 6 or 11/3)")
;; 		      (:ratio " a ratio (like -1/3 or 11/3)")
;; 		      (:real " a number"))
;; 		    (cond
;; 		      ((and (eq d1 '*) (eq d2 '*))
;; 		       "")
;; 		      ((eq d2 '*)
;; 		       (format nil " no less than ~a" d1))
;; 		      ((eq d1 '*)
;; 		       (format nil " no more than ~a" d2))
;; 		      (t
;; 		       (format nil " between ~a and ~a" d1 d2)))
;; 		    ". "))
;; 	     (:string
;; 	      (if (eq '* d1)
;; 		  '("")
;; 		  (list (format nil "This option takes a string no more than ~
;;                                      ~d characters in length." d1))))
;; 	     (:key
;; 	      '("This option takes a single short alphanumeric word, starting"
;; 		"with a letter, containing no whitespace or other symbols. "))
;; 	     (otherwise
;; 	      '(""))))))

;; (defparameter *ws* '(#\Space #\Tab #\Newline #\Return #\Page)
;;   "A list of common whitespace characters.")

;; (defun pad (string minlength &optional (minpad 2))
;;   "Return a new string that is STRING but with spaces appended in
;; order to make its length equal to MINLENGTH.  At least MINPAD spaces
;; appears at the right of STRING, no matter how long the resulting
;; string is.  This is used to set off a tag in a paragraph with a
;; hanging tag, as in an option in a usage message.

;;    \(PAD \"foo\" 8) => \"foo     \"
;;    \(PAD \"foo\" 5) => \"foo  \"
;;    \(PAD \"foo\" 2) => \"foo  \"

;;    \(PAD \"blah\" 3 0\) => \"blah\" 
;;    \(PAD \"blah\" 3 1\) => \"blah \" 
;;    \(PAD \"blah\" 3 2\) => \"blah  \" 
;;    \(PAD \"blah\" 4 0\) => \"blah\" 
;;    \(PAD \"blah\" 4 1\) => \"blah \" 
;;    \(PAD \"blah\" 4 2\) => \"blah  \" 
;;    \(PAD \"blah\" 5 0\) => \"blah \" 
;;    \(PAD \"blah\" 5 1\) => \"blah \" 
;;    \(PAD \"blah\" 5 2\) => \"blah  \" 
;;    \(PAD \"blah\" 6 0\) => \"blah  \" 
;;    \(PAD \"blah\" 6 1\) => \"blah  \" 
;;    \(PAD \"blah\" 6 2\) => \"blah  \""
;;   (let ((str (string-right-trim *ws* string)))
;;     (strcat str (make-string (max minpad (- minlength (length str)))
;; 			     :initial-element #\Space))))

;; (defun hanging-par (label text &optional stream indentlength)
;;   "Presents a hanging paragraph onto STREAM or *STANDARD-OUTPUT* if
;; STREAM is not provided.  The exdented text, starting at the beginning
;; of the first line, is supplied by the LABEL string.  The TEXT string
;; is broken up on whitespace boundaries and flowed onto the remainder of
;; the line until the right margin is encountered.  Remaining words are
;; placed on as many subsequent lines as necessary, each of those lines
;; indented by spaces.  The number of spaces used to indent all remaining
;; lines is given by INDENTLENGTH; if that argument is not provided, the
;; width of LABEL is used instead."
;;   (let* ((spaces (make-string (or indentlength (length label))
;; 			      :initial-element #\Space))
;; 	 (words (split *ws* text))
;; 	 (format (strcat "~a~{~<~%" spaces "~1:;~a~>~^ ~}~%")))
;;     (format (or stream *standard-output*) format label words)))

;; (defun option-text (option dochash)
;;   "Return a string that may be empty describing OPTION.  Looks for a
;; closure in the dochash, and if one exists, executes it (we assume it
;; came from CLI:SPEC, and therefore returns a string).  Otherwise,
;; returns an empty string."
;;   (or (aand (gethash option dochash) (funcall it))
;;       ""))

;; (defun option-aliases (option aliases styles)
;;   (with-styles-canon (styles styles)
;;     (apply #'format nil
;; 	   "~#[~;An alias for this option is ~s.~
;;              ~;Aliases for this option are ~s and ~s.~
;;              ~;Aliases for this option are ~@{~#[~; and~] ~s~^,~}.~]"
;; 	   (cdr (assoc option aliases :test (str=-fn styles))))))

;; (defun usage-option (option opthash dochash aliases styles tagwidth stream)
;;   "Format a full description of the string named OPTION onto the
;; supplied STREAM.  OPTHASH is the hash mapping options to their types,
;; and DOCHASH is the hash mapping options to closures that provide
;; descriptions.  TAGWIDTH provides a specific width for the lefthand
;; column in which the options appear."
;;   (with-styles-canon (styles styles)
;;     (let* ((type (gethash option opthash))
;; 	   (label (pad (label-option option type styles) tagwidth)))
;;       (hanging-par label
;; 		   (strcat (option-text option dochash)
;; 			   " " (option-type type)
;; 			   " " (option-aliases option aliases styles))
;; 		   stream tagwidth))))

;; (defun usage-header (appname summary namewidth stream)
;;   "Given an string APPNAME and its possibly long SUMMARY (which can be
;; NIL), format the pair as a hanging paragraph onto STREAM.  SUMMARY is
;; a closure that generates the text to be formatted, or NIL."
;;   (let ((namewidth (min namewidth (+ 3 (length appname)))))
;;     (if (null summary)
;; 	(format stream "~a:~%" appname)
;; 	(hanging-par (pad (strcat appname ":") namewidth)
;; 		     (funcall summary)
;; 		     stream namewidth))
;;     (terpri stream)))

;; (defun usage-footer (tail styles stream)
;;   "If TAIL is not NIL, call it to obtain text, and render it onto the
;; named output stream or *STANDARD-OUTPUT*, wrapping the text at the
;; same right margin as USAGE-HEADER and USAGE-OPTION."
;;   (with-styles-canon (styles styles)
;;     (flet ((par (text)
;; 	     (let ((words (split *ws* text)))
;; 	       (when (not (zerop (length (car words))))
;; 		 (format stream "~{~<~%~1:;~a~>~^ ~}~%~%" words)))))
;;       (par (strcat (if (and (member :unix styles)
;; 			    (foldp styles))
;;               "Options are case-insensitive (-x and -X are equivalent). "
;; 		       "")
;; 		   (if (and (member :windows styles)
;; 			    (not (foldp styles)))
;;               "Options are case sensitive (/x and /X are different). "
;; 		       "")
;; 		   (if (member :partial styles)
;;               "Options may be abbreviated to their shortest unique specifier. "
;; 		       "")))
;;       (when tail
;; 	(par (funcall tail))))))

;; (defun usage (appname summary tail opthash dochash aliases styles
;; 	      &key (stream *standard-output*) (maxappwidth 18) (maxoptwidth 16))
;;   "Display a usage message on the supplied stream, describing all the
;; options the application supports on its command-line.  MAXAPPWIDTH and
;; MAXOPTWIDTH can be used to supply maximum indentation of the SUMMARY
;; and each option's description \(though longer texts will be formatted
;; reasonably\); use those keywords to change the default sizes."
;;   (with-styles-canon (styles styles)
;;     (let ((optwidth (min (+ 2 (widest-option-label opthash styles))
;; 			 maxoptwidth))
;; 	  (printed nil))
;;       (usage-header appname summary maxappwidth stream)
;;       (mapc (lambda (option)
;; 	      (usage-option option opthash dochash aliases styles
;; 			    optwidth stream)
;; 	      (setf printed t))
;; 	    (sort (hash-table-keys opthash) #'string<))
;;       (when printed
;; 	(terpri stream))
;;       (usage-footer tail styles stream))))

;; (defun sort-out-options (optspecs)
;;   "Given specifications for options taking arguments and options that
;; are only flags, return two values: a list of all the options taken by
;; the application, and a hash mapping all options to their types
;; \(including the pseudo-type :FLAG indicating the option takes no
;; argument\)."
;;   (let ((opthash (make-hash-table))
;; 	(dochash (make-hash-table)))
;;     (mapc (lambda (spec) (destructuring-bind (option type docfn)
;; 			     spec
;; 			   (setf (gethash option opthash) type
;; 				 (gethash option dochash) docfn)))
;; 	  optspecs)
;;     (values opthash dochash)))

;; #+nil
;; (defun cli:spec* (name summary tail optspecs aliases styles args)
;;     "Receives a parsed specification of command-line options and
;; arguments from the CLI:SPEC macro."
;;     (multiple-value-bind (opthash dochash)
;; 	(sort-out-options optspecs)
;;       (usage name summary tail opthash dochash aliases styles)))

|#
