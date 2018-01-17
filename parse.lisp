(in-package #:petulant)

(defun isolate-switches (string)
  "Given a string that begins with at least one Windows CLI switch
character, return a list of strings that exist between slashes,
skipping leading, multiple, and trailing slashes.  Arguments to
switches introduced with a colon are preserved with the switch.

If this looks like SPLIT-SEQUENCE, well, you're not wrong.
ISOLATE-SWITCHES exists because one day, this is going to require some
kind of weird escape processing and probably a state machine of some
kind.  In the meantime, this mini-split-sequence hack is good enough.

    \(isolate-switches \"/a\"\)                 => \(\"a\"\)
    \(isolate-switches \"/ab\"\)                => \(\"ab\"\)
    \(isolate-switches \"/a/bc\"\)              => \(\"a\" \"bc\"\)
    \(isolate-switches \"/a/bc:de\"\)           => \(\"a\" \"bc:de\"\)
    \(isolate-switches \"/a/bc:de/f\"\)         => \(\"a\" \"bc:de\" \"f\"\)
    \(isolate-switches \"/a/bc:de/f/\"\)        => \(\"a\" \"bc:de\" \"f\"\)
    \(isolate-switches \"///a///bc:de///f//\"\) => \(\"a\" \"bc:de\" \"f\"\)
    \(isolate-switches \"\"\)                   => \(\"\"\)
    \(isolate-switches \"/\"\)                  => \(\"/\"\)
    \(isolate-switches \"//\"\)                 => \(\"//\"\)"
  (cond
    ((zerop (length string))                       '(""))
    ((not (position #\/ string :test #'char/=))    (list string))
    (t
     (let ((str (string-trim "/" string))
	   (i 0) (res))
       (awhile (position #\/ str :start i)
	 (push (subseq str i it) res)
	 (setq i (position #\/ str :start it :test #'char/=)))
       (when i
	 (push (subseq str i) res))
       (nreverse res)))))

(defun canonicalize-switch-args (strings)
  "Given a list of strings that represents command line options and
arguments passed in a Windows environment, break up combined switches
and return a new list of strings that is easier to parse.  The original
set of STRINGS is broken down via ISOLATE-SWITCHES.

Strings like \"\", \"/\", \"//\", and so on are special, we preserve
them as they are.

    \(canonicalize-switch-args '\(\"abc\" nil \"\" \"def\"\)\)
    ⇒ \(\"abc\" \"\" \"def\"\)
    \(canonicalize-switch-args '\(\"abc\" \"//\" \"def\"\)\)
    ⇒ \(\"abc\" \"//\" \"def\"\)
    \(canonicalize-switch-args '\(\"/abc\" \"def\"\)\)
    ⇒ \(\"/abc\" \"def\"\)
    \(canonicalize-switch-args '\(\"/a/bc\" \"def\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\"\)
    \(canonicalize-switch-args '\(\"/a/bc\" \"def\" \"/ef:gh\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\" \"/ef:gh\"\)
    \(canonicalize-switch-args '\(\"/a/bc\" \"def\" \"/ef\" \"gh\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\" \"/ef\" \"gh\"\)
    \(canonicalize-switch-args '\(\"/a/bc\" \"def\" \"/e/f:g/h\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\" \"/e\" \"/f:g\" \"/h\"\)"
  (let ((result))
    (labels ((collect (x) (push x result)))
      (mapc (lambda (sw)
	      (cond
		((null sw))
		((and (> (length sw) 0) (char= #\/ (char sw 0)))
		 (mapc (lambda (s) (collect (slashify s)))
		       (isolate-switches sw)))
		(t
		 (collect sw))))
	    strings))
    (nreverse result)))


(defun parse-windows (fn cmdline &key
				   (chgopt-fn #'identity)
				   (optargp-fn (constantly nil)))
  "This is the low level parser for Windows-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.  At least consider PARSE instead of
PARSE-WINDOWS.

PARSE-WINDOWS works through CMDLINE, a proper list of strings,
calling FN for each switch or argument encountered on that supplied
command-line.  Each call to FN has three arguments.  The first
argument is either :ARG or :OPT, indicating if a standalone
argument \(that is, an argument unassociated with any switch\) or if a
switch was encountered on the supplied command-line.

When the first argument to FN is :ARG, its second argument is a string
that is the command-line argument.  The third argument is NIL.

When the first argument to FN is :OPT, its second argument is the
switch encountered on the command-line.  If this switch has its own
argument, the third argument is that string; otherwise, it is NIL.

CHGOPT-FN is a function always applied to any detected switch.  It
can be used to change the switch, perhaps implementing aliases or
abbreviations for a known switch.  It is called with a string that is
the switch detected on the command-line, and it should return either
that string or a new string to use in its place.

OPTARGP-FN is a function that may be called when a switch is detected
on the command-line.  When it is ambiguous whether a given switch
takes an argument, this function is called with the detected switch.
If it returns true, the switch is processed as if it takes an
argument; otherwise, it is considered a simple \"flag\" and no
argument is associated with it.

Consider the example command-line:

    C:\\FOO> app /file:data.txt /foo bar /v

Sometimes, a switch can be unambiguously detected to take an option.
In the example above, \"file\" is one such switch; likewise, \"v\" can
be seen not to take an argument.  However, \"foo\" is ambiguous, it
might take the argument \"bar\", or it may simply be a flag \(leaving
\"bar\" as a standalone argument\).  Resolving these ambiguities is
the responsibility of SWARGP-FN.

Generally speaking, the calls to FN proceed from the head to the
tail of CMDLINE, and from left to right within each string of
CMDLINE.  This is useful to know in testing, but callers probably
should not rely on any specific ordering."
  (do* ((av (canonicalize-switch-args cmdline) (cdr av))
	(str (car av) (car av))
	(len (or (and str (length str)) 0)))
       ((null av) t)
    (labels ((optargp (x) (funcall optargp-fn x))
	     (chgopt (x) (funcall chgopt-fn x))
	     (opt! (o &optional a) (funcall fn :opt o a))
	     (arg! (a) (funcall fn :arg a nil))
	     (advance () (setf av (cdr av))))
      (acond
	((zerop len)					     ; nil ""
	 (arg! str))
	((string= str "//")				     ; "//"
	 (loop (unless (advance)
		 (return))
	    (arg! (car av))))
	((char/= (char str 0) #\/)			     ; "foo"
	 (arg! str))
	((position #\: str)				     ; "/foo:…"
	 (opt! (chgopt (subseq str 1 it))
	       (unless (= it (1- len))		             ; "/foo:xyz"
		 (subseq str (1+ it)))))
	(t
	 (let ((sw (chgopt (subseq str 1))))
	   (cond
	     ((optargp sw)                                    ; "/foo" "xyz"
	      (opt! sw (cadr av))
	      (advance))  ; skip next arg that we just used
	     (t                                              ; "/foo"
	      (opt! sw)))))))))

(defun parse-unix (fn cmdline &key
				(chgopt-fn #'identity)
				(optargp-fn (constantly nil)))
  "This is the low level parser for Unix-style \(POSIX\) command
lines.  If you're an end-user of Petulant, you might want to consider
calling a higher level function; this one is mostly for implementation
of other Petulant functionality.  At least consider PARSE instead of
PARSE-UNIX.

PARSE-UNIX works through CMDLINE, a proper list of strings,
calling FN for each option or argument encountered on that supplied
command-line.  Each call to FN has three arguments.  The first
argument is either :ARG or :OPT, indicating if a standalone
argument \(that is, an argument unassociated with any option\) or if an
option was encountered on the supplied command-line.

When the first argument to FN is :ARG, its second argument is a string
that is the command-line argument.  The third argument is NIL.

When the first argument to FN is :OPT, its second argument is the
option encountered on the command-line.  If this switch has its own
argument, the third argument is that string; otherwise, it is NIL.

CHGOPT-FN is a function always applied to any detected option.  It
can be used to change the option, perhaps implementing aliases or
abbreviations for a known option.  It is called with a string that is
the detected option from the command-line, and it should return either
that string or a new string to use in its place.

OPTARGP-FN is a function that may be called when a option is
detected on the command-line.  When it is ambiguous whether a given
option takes an argument, this function is called with the detected
option.  If it returns true, the option is processed as if it takes an
argument; otherwise, it is considered a simple \"flag\" and no
argument is associated with it.

Consider the example command-line:

    $ app --file=data.txt --foo bar -v

Sometimes, a option can be unambiguously detected to take an option.
In the example above, \"file\" is one such option; likewise, \"v\" can
be seen not to take an argument.  However, \"foo\" is ambiguous, it
might take the argument \"bar\", or it may simply be a flag \(leaving
\"bar\" as a standalone argument\).  Resolving these ambiguities is
the responsibility of OPTARGP-FN.

Generally speaking, the calls to FN proceed from the head to the
tail of CMDLINE, and from left to right within each string of
CMDLINE.  This is useful to know in testing, but callers probably
should not rely on any specific ordering."
  ;; this all works, but it could use a little refactoring, like we
  ;; did in PARSE-WINDOWS.  A DO* instead of DO might let us get rid
  ;; of some of the worse patterns in here...
  (do ((av cmdline))
      ((null av) t)
    (labels ((argoptp (x) (funcall optargp-fn x))
	     (chgopt (x) (funcall chgopt-fn x))
	     (is- (&rest chars) (apply #'char= #\- chars))
	     (advance () (setf av (cdr av)))
	     (opt! (o &optional a) (funcall fn :opt o a))
	     (arg! (a) (funcall fn :arg a nil))
	     (long (opt)
	       (let* ((o (subseq opt 2))                    ; "--foo…"
		      (i (position #\= o))		    ; "--foo=…"
		      (f (chgopt (if i
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
		 (let ((f (chgopt c)))
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

(defun parse-unix-or-windows-fn (style)
  "Returns either PARSE-UNIX or PARSE-WINDOWS, based on STYLE. STYLE
is NIL, a keyword, or a list of keywords that can be used to select a
particular style of command-line processing.  This implements the
decision tree described in CLI:MAKE-PARSER."
  (let ((stylehash (stylehash *context*)))
    (cond
      ((and (symbolp style) (not (null style)))
       (or (and (eq :windows style)
		#'parse-windows)
	   #'parse-unix))
      ((consp style)
       (or (and (member :windows style)
		#'parse-windows)
	   #'parse-unix))
      ((not (zerop (hash-table-count stylehash)))
       (or (and (gethash :windows stylehash)
		#'parse-windows)
	   #'parse-unix))
      (t
       (or (and (member :windows cl:*features*)
		#'parse-windows)
	   #'parse-unix)))))

(defun app-command-line ()
  "Returns a list of strings representing the command-line with which
the application was invoked.  This is necessarily specific to the Lisp
environment.  The list returned by APP-COMMAND-LINE does _not_ include
the executable name \(also known as the image name or argv[0]\)."
  #+ccl ccl:*command-line-argument-list*
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+acl (cdr (sys:command-line-arguments))
  #-(or ccl sbcl clisp acl)
  (error "Petulant needs to be ported to this Lisp environment."))

(defun make-parser (&key style
		      (chgopt-fn #'identity)
		      (optargp-fn (constantly nil)))
  "This creates a simple low-level parser for command-lines.  If
you're simply using Petulant in your application \(i.e., you aren't
developing or extending Petulant\), you might want to consider calling
a higher level function; CLI:MAKE-PARSER is primarily intended for the
implementation of other Petulant \(or Petulant-like\) functionality.

There is also the simpler function CLI:PARSE which is more convenient
for one-time usage, and there is also CLI:DEFPARSER to provide the
binding of a command-line parser to a symbol in the current package.
See the documentation for those functions for further information.

The parser returned by CLI:MAKE-PARSER takes one mandatory argument, a
function \(typically a closure\) to be called for each option or
argument seen on a command-line.  Additionally, the returned parser
takes two keyword arguments:

- :COMMAND-LINE can be used to supply a proper list of strings to be
  processed as a command line.  The default for this keyword is NIL.
  The keyword :ARGV is a synonym for brevity.
- :USE-OS-COMMAND-LINE can be used to toggle the use of the
  command-line provided to the Lisp environment by the operating
  system.  The default for this keyword is T.  The keyword :OS is a
  synonym for brevity.

With these keywords, the parser can optionally process a command-line
provided by the application \(perhaps from a service definition,
configuration file, or some other source, or even for testing\), but
easily work with the command-line provided to the application by
CLI:APP-COMMAND-LINE.

The returned parser will process specified command-line according to
the dominant style for a specific operating system \(e.g.,
hyphen-based options under Unix, slash-based switches under Windows\).
The supplied function is called for each option/switch \(with or
without an argument\) and every non-option argument identified during
parsing.  Each call receives three arguments.  The first argument is
always :ARG or :OPT.

- When :ARG, the second argument is a non-switch argument string from
  the command line, and the third argument is NIL.
- When :OPT, the second argument is a switch \(usually a string, but
  see OPTNAME-FN below\) found on the command line, eliding its
  leading hyphen or slash, and the third argument is any argument to
  that option or NIL.

Generally speaking, the calls to the supplied function proceed from
the head to the tail of the list of argument strings, and from left to
right within each string of that list.  This is useful to know in
testing, but callers probably should not rely on any specific
ordering.

OPTARGP-FN, if not NIL, is a function.  Certainly, Petulant recognizes
certain long options \(--foo=bar\) and switches \(/foo:bar\) that
unambiguously present an option taking an argument.  However, Petulant
cannot know for certain when a short option takes an option \(e.g., -f
bar\), nor can it discern when long options lacking an equal character
\(e.g., --foo bar\) or a switch lacking a colon character \(e.g., /foo
bar\) take an argument.  To address this, the caller can supply a
function taking the name of the option as a string \(f or foo\) and
returning true or false to indicate if it takes an argument.  To be
clear, ARGOPTP-FN is not called on every option, only those that
Petulant cannot recognize as definitely taking an argument.

CHGOPT-FN, if not NIL, can be used to change a detected switch from
yone value to another, taking a string naming the switch \(option\)
and returning a string to use in its place.  It could be used to
implement aliases or partial matching, for example.  Every detected
switch is passed through this function before processing continues; if
OPTARGP-FN must be called, CHGOPT-FN will be called first.

STYLE is NIL, a keyword, or a proper list of keywords that can be
used to select a particular style of command-line processing.  The
decision tree is:

- Unless STYLE is NIL, its value is considered.  If it is equal to or
  contains the value :WINDOWS, then Windows-style command-line
  processing is used; otherwise, Unix-style processing is selected.
- Unless the current *CONTEXT* lacks a useful \(non-zero\) style hash,
  it is examined.  If it contains :WINDOWS, then Windows-style
  processing is used; otherwise, Unix-style processing is selected.
- Finally, in the event that STYLE is NIL and the current *CONTEXT*
  contains just a placeholder style hash, CL:*FEATURES* is examined.
  If it contains :WINDOWS, then Windows-style processing is used;
  otherwise, Unix-style processing is selected."
  (let ((parser (parse-unix-or-windows-fn style)))
    (lambda (fn &key command-line argv (use-os-command-line t) (os t))
      (aif (or command-line
	       argv
	       (and use-os-command-line os (app-command-line)))
	   (funcall parser fn it
		    :optargp-fn optargp-fn
		    :chgopt-fn chgopt-fn)
	   (error "Petulant: bad usage of :COMMAND-LINE :ARGV ~
                   :USE-OS-COMMAND-LINE or :OS keyword arguments appears in ~
                   call to a parser returned by CLI:MAKE-PARSER.")))))

(defun parse (fn &key command-line argv (use-os-command-line t) (os t)
		   optargp-fn chgopt-fn style)
  "This is a simple, low level parser for command-lines.  If you're
simply using Petulant in your application \(i.e., you aren't
developing or extending Petulant\), you might want to consider calling
a higher level function; CLI:PARSE is mainly for implementation of
other Petulant \(or Petulant-like\) functionality.

CLI:PARSE is intended as a \"one time\" parser.  If there are multiple
command-lines parsed in your application, or even multiple parsings of
the some command-line, you are likely better served by CLI:DEFPARSER.

Arguments to CLI:PARSE are the union of those taken by CLI:MAKE-PARSER
and the parser it returns."
  (funcall (make-parser :optargp-fn optargp-fn
			:chgopt-fn chgopt-fn
			:style style)
	   fn
	   :command-line command-line
	   :argv argv
	   :use-os-command-line use-os-command-line
	   :os os))

(defmacro defparser (name dummy &rest forms)
  "Defines a named low-level Petulant parser. NAME is a symbol naming
the new parser. DUMMY is currently unused and should be NIL \(existing
only to let DEFPARSER follow other similar defining forms\).  FORMS,
seen below, share meaning with the keyword arguments of the same name
in CLI:MAKE-PARSER.  If a given form is seen more than once, the last
instance \"wins\".

    \(cli:defparser foobar \(\)
      \"An optional docstring for the FOOBAR low-level command-line parser.\"
      \(:optargp-fn …\)
      \(:chgopt-fn …\)
      \(:style …\)\)

Continuing that example, FOOBAR may later be called, providing a mandatory
function argument and up to two keywords as described for the function
returned by CLI:MAKE-PARSER.

    \(foobar \(lambda …\)\)
    ;; or
    \(foobar \(lambda …\) :argv '\(\"-v\" \"one\" \"two\"\)\)"
  (declare (ignore dummy))
  (let ((docstring "A Petulant low-level command-line parser.")
	optargp-fn chgopt-fn style)
    (when (stringp (car forms))
      (setf docstring (car forms)
	    forms (cdr forms)))
    (mapc (lambda (form)
	    (case (car form)
	      (:optargp-fn  (setf optargp-fn (cadr form)))
	      (:chgopt-fn  (setf chgopt-fn (cadr form)))
	      (:style      (setf style (cadr form)))
	      (t           (error "CLI:DEFPARSER: (~s …) is not a ~
                                   recognized form." (car form)))))
	  forms)
    `(setf (symbol-function ',name) (make-parser :optargp-fn ,optargp-fn
						 :chgopt-fn ,chgopt-fn
						 :style ,style)
	   (documentation ',name 'function) ,docstring)))
