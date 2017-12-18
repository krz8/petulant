(in-package #:petulant)

(defparameter *options* (make-hash-table)
  "Holds a hash table mapping options seen on the command-line to
  their decoded values.  This hash table reflects the most recent
  successful call to CLI:SPEC.")

(defparameter *arguments* nil
  "Holds a list of command-line argument strings \(not otherwise
  associated with options\), or NIL if there are none.  This list
  reflects the most recent successful call to CLI:SPEC.")

(defvar *usage* '("?" "help")
  "A list of option strings that, when encountered by CLI:SPEC, will
  trigger its full usage message to be generated for the end-user.  If
  this happens, CLI:SPEC will return :USAGE, CLI:*OPTIONS* will
  contain an empty hash, and CLI:*ARGUMENTS* will be an empty list.")

(defvar _shush_ nil
  "When true, suppresses certain warnings.  For testing.")

;;; The main point of the CLI:SPEC macro is to parse a number of
;;; different forms the caller can provide, including shortcuts and
;;; abbreviations.  CLI:SPEC is also where we intend to detect any
;;; errors as well.  We push as much of the error and warning activity
;;; to this macro as we can, so that CLI:SPEC* can proceed "trusting"
;;; that it input is correct and *very* regular.
;;;
;;; By regular, we mean that everything for a given type follows the
;;; same form by the time it gets to CLI:SPEC*.  Consider option
;;; specifications, for example.  Regardless of the different :ARGOPT
;;; and :FLAGOPT forms, the omitted types, partial types, and fully
;;; specified types, the optional documentation, CLI:SPEC* is provided
;;; a simple list where the first element is always the name of an
;;; option, the second element is always a full type specification,
;;; and the third element is always a closure providing documentation.
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
interface presented to the end-user, a command-line (supplied by
either the caller or by the Lisp environment) will be parsed.  The
value returned by CLI:SPEC to the caller is one of the following:

   T - A command-line was found and parsed, results are available in
   two specials variables after SPEC returns.  CLI:*OPTIONS* is a hash
   table mapping options to the values of the arguments supplied to
   them; options without values \(i.e., flags\) will be mapped to the
   value :FLAG.  CLI:*ARGUMENTS* is a list \(that may be empty\) of
   strings representing arguments from the command-line that are not
   associated with options.

   NIL - An error was encountered during the parse of the
   command-line.  The application should consider this attempt at
   using its command-line as a failure; either unknown options, or
   option arguments of the wrong type, were encountered.  An error
   message will have been generated for the end-user.  CLI:*OPTIONS*
   and CLI:*ARGUMENTS* are unchanged.

   :USAGE - The application was invoked with one of the options in
   CLI:*USAGE*.  A usage message for the end-user will have been
   generated.  CLI:*OPTIONS* and CLI:*ARGUMENTS* are unchanged.

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
  not present in the command-line argument using :KEY.  Thus, the user
  specifies \"red\" but the application receives :RED.

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
documented in CLI:PROCESS, to influence the parsing of the
command-line. Multiple instances of :STYLE accumulate. \(aka :STYLES\)

   \(:style :key :unix\)

\(:ARG \"command-line-arg\" [\"command-line-arg\" ...]\) supplies one
or more strings to be used instead of the application's actual
command-line.  Multiple instances of :ARG accumulate. \(aka :ARGS\)"
  ;; CLI:SPEC provides hand-holding.  So much hand-holding.  I figure
  ;; if someone is using CLI:SPEC and not CLI:COLLECT or CLI:PROCESS, they
  ;; want all the functionality (including the kitchen sink).  So,
  ;; we'll give it to them, catching as many problem situations as we
  ;; can.  If the caller can get through CLI:SPEC without warnings or
  ;; errors, there's no reason for them to expect anything but
  ;; success.
  (let ((keypkg (find-package :keyword))
	name summary tail options aliases styles args)
    (macrolet ((wrn (x &rest y) `(unless _shush_
				   (warn ,(strcat "SPEC: " x) ,@y)))
	       (err (x &rest y) `(error ,(strcat "SPEC: " x) ,@y)))
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
			   (setf docs nil))) ; paranoia
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
    ;; to it, it's purely an aesthetical thing.  You know, no matter
    ;; how many times I use it, `',foo always feels like I'm abusing
    ;; something...
    `(multiple-value-bind (rstatus ropts rargs)
	 (petulant::spec* ,name ,summary ,tail
			  ,(if options `(list ,@options) 'nil)
			  ,(if aliases `',aliases 'nil)
			  ,(if styles `',styles 'nil)
			  ,(if args `',(nreverse args) 'nil))
       (cond
	 ((not rstatus))
	 ((eq :usage rstatus))
	 (t (setf *options* ropts
		  *arguments* rargs)))
       rstatus)))

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
(defun spec* (name summary-fn tail-fn options aliases styles args)
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
      (if summary-rn
	  (format t "summary ~s~%" (funcall summary-fn))
	  (format t "no summary~%"))
      (if tail-fn
	  (format t "tail ~s~%" (funcall tail-fn))
	  (format t "no tail~%"))
      (showq options)
      (showq aliases)
      (showq styles)
      (showq args)
      (values t nil nil))))

(defparameter *usage-option-padding* "  "
  "A string providing the left padding \(that is, the indentation\)
for all options presented in a usage message.  Typically, this is a
string of two spaces.")

(defun label-option (option type)
  "Given a string naming an OPTION, along with a canonical Petulant
TYPE specifier \(a list starting with a keyword\), return a string to
be used in a usage message for this option.  The string is always
padded with two spaces at its beginning.

   \(label-option \"alpha\" '\(:string *\)\)  ; when style includes :UNIX
=> \"  --alpha=VAL\"
   \(label-option \"beta\" '\(:string *\)\)   ; when style includes :WINDOWS
=> \"  /beta:VAL\"
   \(label-option \"gamma\" '\(:flag\)\)      ; when style includes :UNIX
=> \"  --gamma\"
   \(label-option \"delta\" '\(:flag\)\)      ; when style includes :WINDOWS
=> \"  /delta\"
   \(label-option \"e\" '\(:flag\)\)          ; when style includes :UNIX
=> \"  -e\"
   \(label-option \"z\" '\(:string *\)\)      ; when style includes :UNIX
=> \"  -z VAL\""
  (let ((winp (stylep :windows))
	(shortp (< (length option) 2))
	(flagp (eq :flag (car type))))
    (strcat *usage-option-padding*
	    (cond (winp   "/")
		  (shortp "-")
		  (t      "--"))
	    option
	    (cond (flagp  "")
		  (winp   ":VAL")
		  (shortp " VAL")
		  (t      "=VAL")))))

(defun widest-option-label ()
  "Work through the current hash-table mapping options to Petulant types,
format each of them as they would appear in a usage message via
LABEL-OPTION, returning the length of the longest string that
represents an option in a usage message \(e.g., \" --config=VAL\"\).
Even if there are no options defined for the application, the length
of the option indentation is returned as a sort of \"minimal
length\"."
  (if (zerop (hash-table-count (opthash *context*)))
      (length *usage-option-padding*)
      (reduce #'max
	      (collecting (lambda (k v) (length (label-option k v)))
			  (opthash *context*)))))

(defun option-text (option)
  "Return a string that may be empty describing OPTION.  Executes the
appropriate closure in the dochash of the current context."
  (funcall (gethash option (dochash *context*))))

(defun option-type (type-spec)
  "This function returns text that at least somewhat describes the
supplied type for use in a help or usage message.  Petulant type
specifications include real ones like :STRING, :FLOAT, :INTEGER,
:RATIO, :RATIONAL, :REAL, and so on.  We also support pseudo-types
like :FLAG, :READ, :KEY, :ONE-OF, etc."
  (destructuring-bind (&optional d0 d1 d2)
      type-spec
    (case d0
      ((:float :integer :ratio :rational :real)
       (strcat "This option takes"
	       (case d0
		 (:integer " an integer")
		 (:float " a floating point value")
		 #+nil (:rational " a rational number (like 6 or 11/3)")
		 (:rational " a rational number")
		 #+nil (:ratio " a ratio (like -1/3 or 11/3)")
		 (:ratio " a ratio")
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
	   ""
	   (format nil "This option takes a string no more than ~d ~
                        characters in length." d1)))
      (:key
       "This option takes a single short alphanumeric word, starting with ~
        a letter, containing no whitespace or other symbols.")
      (:one-of
       (apply #'format nil
	      "~#[~;This option takes only the value ~a. ~
                  ~;This option may take the values ~a and ~a. ~
                  ~;This option takes one of the values ~
                    ~@{~#[~; and~] ~a~^,~}.~]"
	      (sort (cdr type-spec) (str<-fn))))
      (:read
       "This option takes a lisp expression.")
      (:flag
       "")
      (otherwise			; should we warn?
       ""))))

(defun option-aliases (option)
  "Returns a string describing aliases for the named option, from the
current context."
  (let ((eqfun (str=-fn))
	aliases)
    (maphash (lambda (k v)
	       (when (funcall eqfun option v)
		 (push (strcat (cond
				 ((stylep :windows)
				  "/")
				 ((< 1 (length k))
				  "--")
				 (t
				  "-")) k)
		       aliases)))
	     (alihash *context*))
    (apply #'format nil
	   "~#[~;An alias for this option is ~a. ~
            ~;Aliases for this option are ~a and ~a. ~
            ~;Aliases for this option are ~@{~#[~; and~] ~a~^,~}.~]"
	   (sort aliases (str<-fn)))))

(defun usage-option (option tagwidth stream)
  "Format a full description of the string named OPTION onto the
supplied STREAM, using the hashes in the current context.  TAGWIDTH
provides a specific width for the lefthand column in which the options
appear."
  (let* ((type (gethash option (opthash *context*)))
	 (label (pad (label-option option type) tagwidth)))
    (hanging-par label
		 (strcat (option-text option)
			 " " (option-type type)
			 " " (option-aliases option))
		 :stream stream :indentlength tagwidth)))

(defun usage-header (namewidth stream)
  "Format an introduction to a list of options for the current
application, using the current context, onto STREAM.  NAMEWIDTH gives
us the width of the space that the application will be squeezed into,
creating a left margin for the application summary."
  (let* ((name (appname *context*))
	 (namewidth (min namewidth (+ 3 (length name)))))
    (hanging-par (pad (strcat name ":") namewidth)
		 (funcall (summary-fn *context*))
		 :stream stream :indentlength namewidth)
    (terpri stream)))

(defun usage-footer (stream)
  "Render the tail information, if any, of the application onto
STREAM, along with any extra advice about case and abbreviations."
  (par (strcat
	(funcall (tail-fn *context*))
	(if (and (stylep :unix) (stylep :streq))
	    " Options are case-insensitive (-x and -X are equivalent). "
	    "")
	(if (and (stylep :windows) (stylep :str=))
	    " Options are case-sensitive (/x and /X are different). "
	    "")
	(if (stylep :partial)
	    " Options may be abbreviated to their shortest unique name. "
	    ""))
       :stream stream))

(defun usage (&key (stream *standard-output*) (maxappwidth 18)
		    (maxoptwidth 16))
  "Display a usage message on the supplied stream, describing the
application and its options as represented by the current context.
MAXAPPWIDTH and MAXOPTWIDTH can be used to supply maximum indentation
of the SUMMARY and each option's description \(though longer texts
will be formatted reasonably\); use those keywords to change the
default sizes."
  (let ((optwidth (min maxoptwidth (+ 2 (widest-option-label))))
	(printed nil))
    (usage-header maxappwidth stream)
    (mapc (lambda (option)
	    (usage-option option optwidth stream)
	    (setf printed t))
	  (sort (hash-table-keys (opthash *context*))
		(str<-fn)))
    (when printed
      (terpri stream))
    (usage-footer stream)))

(defun decode (kind name valstr)
  "Attempt to decode the string VALSTR according to the KIND of item
encountered on the command-line \(:ARG or :OPT\) and its name \(a
string\), using the current *CONTEXT* for type information.  Returns
two values: a decoded value of VALSTR and an indicator that is true
when the decoding was successful."
  (labels ((err (fmt &rest args)
	     (apply #'error (strcat "CLI:SPEC: " fmt) args)))
    (case kind
      (:opt
       (let ((type (gethash name (opthash *context*))))
	 (case (car type)
	   ;; flags are trivial, they get a "value" of :flag to denote
	   ;; them in *OPTIONS*
	   (:flag (values :flag t))
	   ;; Strings are also very easy, they can't "fail", since we
	   ;; either truncate them or return them.
	   (:string (cond
		      ((eq '* (cadr type))
		       (values valstr t))
		      ((< (cadr type) (length valstr))
		       (values (subseq valstr 0 (cadr type)) t))
		      (t
		       (values valstr t))))
	   ;; Okay, we have no idea what this type is, just fail out of here.
	   (t (err "unknown argument type ~s" type)
	      (values nil nil)))))
      (t
       (values name t)))))

(defun spec* (name summary-fn tail-fn options aliases styles args)
  "Typically invoked from the CLI:SPEC macro.

NAME is a string identifying the running application.

SUMMARY-FN and TAIL-FN are closures that generate the strings that
appear in a usage message, or NIL.

OPTIONS is a list of option specifications.  Each specification is,
itself, a list of three elements.  The first element is a string
naming the option.  The second element is a Petulant type
specification \(e.g., flags, integers, strings\).  The third element
is a closure yielding a string that documents the option, or NIL.

ALIASES is a list of alternative names for the options.  Each element
of ALIASES is a list, where the first element is a string names an
option appearing in OPTIONS, and all subsequent strings are its
aliases.

STYLES is a keyword, or list of keywords, that control various nuances
of our command line parsing, including option formats \(Unix or
Windows\), case sensitivity, string conversion, keyword conversion,
and so on.

ARGS is a simple list of strings to be processed as a command line,
rather than the application's actual command line."
  (block nil
    (with-context-full (name summary-fn tail-fn options aliases styles args)
      (let ((ropts (mkhash))
	    (rargs nil)
	    (renamer (optname-fn)))
	(process*
	 (lambda (kind name valstr)
	   (when (and (eq :opt kind)
		      (or (string-equal "help" name)
			  (string= "?" name)))
	     (usage)
	     (return (values :usage nil nil)))
	   (multiple-value-bind (value goodp)
	       (decode kind name valstr)
	     (cond
	       ((not goodp)
		(return (values nil nil nil)))
	       ((eq :opt kind)
		(setf (gethash (funcall renamer name) ropts) value))
	       (t
		(push value rargs))))))
	(values t ropts (nreverse rargs))))))
