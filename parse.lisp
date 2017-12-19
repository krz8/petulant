(in-package #:petulant)

(defun isolate-switches (string)
  "Given a string that begins with at least one Windows CLI switch
character, return a list of strings that exist between slashes,
skipping leading, multiple, and trailing slashes.  Arguments to
switches introduced with a colon are preserved with the switch.

If this looks like `SPLIT-SEQUENCE`, well, you're not wrong.
`ISOLATE-SWITCHES` exists because one day, this is going to require some
kind of weird escape processing and probably a state machine of some
kind.  In the meantime, this mini-split-sequence hack is good enough.

    \(ISOLATE-SWITCHES \"/a\"\)                 => \(\"a\"\)
    \(ISOLATE-SWITCHES \"/ab\"\)                => \(\"ab\"\)
    \(ISOLATE-SWITCHES \"/a/bc\"\)              => \(\"a\" \"bc\"\)
    \(ISOLATE-SWITCHES \"/a/bc:de\"\)           => \(\"a\" \"bc:de\"\)
    \(ISOLATE-SWITCHES \"/a/bc:de/f\"\)         => \(\"a\" \"bc:de\" \"f\"\)
    \(ISOLATE-SWITCHES \"/a/bc:de/f/\"\)        => \(\"a\" \"bc:de\" \"f\"\)
    \(ISOLATE-SWITCHES \"///a///bc:de///f//\"\) => \(\"a\" \"bc:de\" \"f\"\)
    \(ISOLATE-SWITCHES \"\"\)                   => \(\"\"\)
    \(ISOLATE-SWITCHES \"/\"\)                  => \(\"/\"\)
    \(ISOLATE-SWITCHES \"//\"\)                 => \(\"//\"\)"
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
set of `STRINGS` is broken down via `ISOLATE-SWITCHES`.

Strings like `\"\"`, `\"/\"`, `\"//\"`, and so on are special, we preserve
them as they are.

    \(CANONICALIZE-SWITCH-ARGS '\(\"abc\" nil \"\" \"def\"\)\)
    ⇒ \(\"abc\" \"\" \"def\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"abc\" \"//\" \"def\"\)\)
    ⇒ \(\"abc\" \"//\" \"def\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"/abc\" \"def\"\)\)
    ⇒ \(\"/abc\" \"def\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"/a/bc\" \"def\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"/a/bc\" \"def\" \"/ef:gh\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\" \"/ef:gh\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"/a/bc\" \"def\" \"/ef\" \"gh\"\)\)
    ⇒ \(\"/a\" \"/bc\" \"def\" \"/ef\" \"gh\"\)
    \(CANONICALIZE-SWITCH-ARGS '\(\"/a/bc\" \"def\" \"/e/f:g/h\"\)\)
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

(defun parse-windows (fn cmdline &optional (swargp-fn (constantly nil))
				           (chgname-fn #'identity))
  "This is the low level parser for Windows-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

`PARSE-WINDOWS` works through `CMDLINE`, a flat list of strings
delivered from some OS-specific wrapper in the Lisp environment
parsing it according to most Windows behaviors.  As switches are
identified, `SWARGP-FN` is called to determine if that switch takes an
argument.

`FN` is called for each switch \(with or without an argument\) and
every non-switch argument.  Each call has three arguments.  The first
is always `:ARG` or `:OPT`.  When `:ARG`, the second argument is a
non-switch argument string from the command line, and the third
argument is NIL.  When `:OPT`, the second argument is a switch \(a
string\) found on the command line, eliding its leading slash, and the
third argument is any argument to that option or NIL.

`SWARGP-FN`, if supplied, is a mechanism for the caller to indicate
when a switch should take an argument.  The default binding of
`SWARGP-FN` always returns NIL, indicating that any ambiguous switch
is assumed not to take an argument.  A non-ambiguous switch with an
argument is one that uses the colon character \(e.g.,
`\"/foo:bar\"`\).

`CHGNAME-FN`, if supplied, can be used to change a detected switch
from one value to another, taking a string and returning a string to
use in its place.  It could be used to implement aliases or partial
matching, for example.  Every detected switch is passed through this
function before processing continues; it is called before `SWARGP-FN`,
for example.

Generally speaking, the calls to `FN` proceed from the head to the
tail of `CMDLINE`, and from left to right within each string of
`CMDLINE`.  This is useful to know in testing, but callers probably
should not rely on any specific ordering."
  (do ((av (canonicalize-switch-args cmdline)))
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

(defun parse-unix (fn cmdline &optional (argoptp-fn (constantly nil))
				        (chgname-fn #'identity))
  "This is the low level parser for Unix-style command lines.  If
you're an end-user of Petulant, you might want to consider calling a
higher level function; this one is mostly for implementation of other
Petulant functionality.

`PARSE-UNIX` works through `CMDLINE`, a flat list of strings from a
command line, parsing it according to most POSIX and GNU behaviors.
As options are identified, `ARGOPTP-FN` is called to determine if that
option takes an argument.

`FN` is called for each option \(with or without an argument\) and
every non-option argument.  Each call has three arguments.  The first
is always `:ARG` or `:OPT`.  When `:ARG`, the second argument is a
non-option argument string from the command line, and the third
argument is NIL.  When `:OPT`, the second argument is an option \(a
string\) found on the command line, eliding any leading dashes, and
the third argument is any argument to that option or NIL.

`ARGOPTP-FN`, if supplied, is a mechanism for the caller to indicate
when an option, long or short, should take the next word in `CMDLINE`
as an argument.  The default binding of `ARGOPTP-FN` always returns
NIL, indicating that any ambiguous option is assumed not to take an
argument.  The only non-ambiguous option with an argument are long
options that use the `\"=\"` character \(e.g., `\"--foo=bar\"`\).

`CHGNAME-FN`, if supplied, can be used to change a detected option from
one value to another, taking a string and returning a string to use in
its place.  It could be used to implement aliases or partial matching,
for example.  Every detected option, long or short, is passed through
this function before processing continues; it is called before
`ARGOPTP-FN`, for example.

Generally speaking, the calls to `FN` proceed from the head to the
tail of `CMDLINE`, and from left to right within each string of
`CMDLINE`.  This is useful to know in testing, but callers probably
should not rely on any specific ordering."
  (do ((av cmdline))
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

(defun parse-unix-or-windows-fn (style)
  "Returns either `PARSE-UNIX` or `PARSE-WINDOWS`, based on `STYLE`.
`STYLE` is NIL, a keyword, or a list of keywords that can be used to
select a particular style of command-line processing.  The decision
tree is described in `CLI:MAKE-PARSER`."
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

(defun argv ()
  "Returns a list of strings representing the command-line from the
environment.  This is necessarily OS specific.  The list returned by
`ARGV` does not include the executable name \(also known as the image
name or argv[0]\)."
  #+ccl ccl:*command-line-argument-list*
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+acl (cdr (sys:command-line-arguments))
  #-(or ccl sbcl clisp acl)
  (error "Petulant needs to be ported to this Lisp environment."))

(defun make-parser (&key argoptp-fn chgname-fn style)
  "This creates a simple low-level parser for command-lines.  If
you're simply using Petulant in your application \(i.e., you aren't
developing or extending Petulant\), you might want to consider calling
a higher level function; `CLI:MAKE-PARSER` is primarily intended for the
implementation of other Petulant \(or Petulant-like\) functionality.

There is also the simpler function `CLI:PARSE` which is more
convenient for one-time usage, and there is also `CLI:DEFPARSER` to
provide the binding of a command-line parser to a global symbol.  See
the documentation for those functions for further information.

The parser returned by `CLI:MAKE-PARSER` takes one mandatory argument,
a function \(typically a closure\) to be called for each option or
argument seen on a command-line.  Additionally, the parser takes two
keyword arguments:

- `:COMMAND-LINE` can be used to supply a proper list of strings to
  be processed as a command line.  The default for this keyword is
  NIL.  The keyword `:ARGS` is a synonym for brevity.
- `:USE-OS-COMMAND-LINE` can be used to toggle the use of the
  command-line provided to the Lisp environment by the operating system.
  The default for this keyword is T.  The keyword `:OS` is a synonym for
  brevity.

With these keywords, the parser can optionally process a command-line
provided by the application \(perhaps from a service definition,
configuration file, or some other source, or even for testing\), but
easily work with the command-line provided to the application by
`CLI:ARGV`.

The returned parser will process specified command-line according to
the dominant style for a specific operating system \(e.g.,
hyphen-based options under Unix, slash-based switches under Windows\).
The supplied function is called for each option/switch \(with or
without an argument\) and every non-option argument identified during
parsing.  Each call receives three arguments.  The first argument is
always `:ARG` or `:OPT`.

- When `:ARG`, the second argument is a non-switch argument string
  from the command line, and the third argument is NIL.
- When `:OPT`, the second argument is a switch \(a string\) found on
  the command line, eliding its leading hyphen or slash, and the third
  argument is any argument to that option or NIL.

Generally speaking, the calls to the supplied function proceed from
the head to the tail of the list of argument strings, and from left to
right within each string of that list.  This is useful to know in
testing, but callers probably should not rely on any specific
ordering.

`ARGOPTP-FN`, if not NIL, is a function.  Certainly, Petulant
recognizes certain long options \(`--foo=bar`\) and switches
\(`/foo:bar`\) that unambiguously present an option taking an
argument.  However, Petulant cannot know for certain when a short
option takes an option \(e.g., `-f` `bar`\), nor can it discern when
long options lacking an equal character \(e.g., `--foo` `bar`\) or a
switch lacking a colon character \(e.g., `/foo` `bar`\) take an
argument.  To address this, the caller can supply a function taking
the name of the option as a string \(`f` or `foo`\) and returning true
or false to indicate if it takes an argument.

`CHGNAME-FN`, if not NIL, can be used to change a detected switch from
yone value to another, taking a string naming the switch \(option\) and
returning a string to use in its place.  It could be used to implement
aliases or partial matching, for example.  Every detected switch is
passed through this function before processing continues; it is called
before `ARGOPTP-FN`, for example.

`STYLE` is NIL, a keyword, or a proper list of keywords that can be
used to select a particular style of command-line processing.  The
decision tree is:

- Unless `STYLE` is NIL, its value is considered.  If it is equal to
  or contains the value `:WINDOWS`, then Windows-style command-line
  processing is used; otherwise, Unix-style processing is selected.

- Unless the current `*CONTEXT*` lacks a useful \(non-zero\) style
  hash, it is examined.  If it contains `:WINDOWS`, then Windows-style
  processing is used; otherwise, Unix-style processing is selected.

- Finally, in the event that `STYLE` is NIL and the current
  `*CONTEXT*` contains just a placeholder style hash, `CL:*FEATURES*`
  is examined.  If it contains `:WINDOWS`, then Windows-style
  processing is used; otherwise, Unix-style processing is selected."
  (let ((parser (parse-unix-or-windows-fn style))
	(argoptp (or argoptp-fn (constantly nil)))
	(chgname (or chgname-fn #'identity)))
    (lambda (fn &key command-line args (use-os-command-line t) (os t))
      (let ((use-os (and use-os-command-line os))
	    (args (or command-line args)))
	(funcall parser
		 fn
		 (or command-line args (and use-os (argv)))
		 argoptp
		 chgname)))))

(defun parse (fn &key command-line args (use-os-command-line t) (os t)
		   argoptp-fn chgname-fn style)
  "This is a simple, low level parser for command-lines.  If you're
simply using Petulant in your application \(i.e., you aren't
developing or extending Petulant\), you might want to consider calling
a higher level function; `CLI:PARSE` is mainly for implementation of
other Petulant \(or Petulant-like\) functionality.

`CLI:PARSE` is intended as a \"one time\" parser.  If there are
multiple command-lines parsed in your application, you are more likely
better served by `CLI:DEFPARSER`.

Keyword arguments to `CLI:PARSE` are described by `CLI:MAKE-PARSER`
and by the parser it returns."
  (funcall (make-parser :argoptp-fn argoptp-fn
			:chgname-fn chgname-fn
			:style style)
	   fn
	   :command-line command-line
	   :args args
	   :use-os-command-line use-os-command-line
	   :os os))

(defmacro defparser (name dummy &rest forms)
  "Defines a named simple, low-level Petulant parser. `NAME` is a
symbol naming the new parser. `DUMMY` is currently unused and should
be NIL.  `FORMS`, seen below, are as documented in `CLI:MAKE-PARSER`.
If a given form is seen more than once, the last instance \"wins\".

    \(cli:defparser foobar \(\)
      \"An optional docstring for the FOOBAR low-level command-line parser.\"
      \(:argoptp-fn …\)
      \(:chgname-fn …\)
      \(:style …\)\)

Continuing that example, `FOOBAR` may later be called, providing a mandatory
function argument and up to two keywords as described for the function
returned by `CLI:MAKE-PARSER`.

    \(foobar \(lambda …\)\)
    ;; or
    \(foobar \(lambda …\) :args '\(\"-v\" \"one\" \"two\"\)\)"
  (declare (ignore dummy))
  (let ((docstring "A Petulant command-line parser.")
	argoptp-fn chgname-fn style)
    (when (stringp (car forms))
      (setf docstring (car forms)
	    forms (cdr forms)))
    (mapc (lambda (form)
	    (case (car form)
	      (:argoptp-fn (setf argoptp-fn (cadr form)))
	      (:chgname-fn (setf chgname-fn (cadr form)))
	      (:style      (setf style (cadr form)))
	      (t           (error "CLI:DEFPARSER: (~s …) is not a ~
                                   recognized form." (car form)))))
	  forms)
    `(setf (symbol-function ',name) (make-parser :argoptp-fn ,argoptp-fn
						 :chgname-fn ,chgname-fn
						 :style ,style)
	   (documentation ',name 'function) ,docstring)))
