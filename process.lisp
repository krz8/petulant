(in-package #:petulant)

(defun cb (&rest args)
  "A test function for debugging parsers that just echoes its
arguments back.  You can use this as the callback for `CLI:PARSE` and
`CLI:PROCESS`."
  (format t "cb~{ ~s~}~%" args))

(defun make-processor* ()
  "Create an option processor from the current `*CONTEXT*`."
  (make-parser :argoptp-fn (argoptp-fn)
	       :chgname-fn (chgname-fn)
	       :argv (argv *context*)))

(defun make-processor (&key argopts flagopts aliases styles argv)
  "`CLI:MAKE-PROCESSOR` constructs and returns a closure that can be
used to process command-lines as described by the optional arguments.
This returned function takes one mandatory argument and two optional
keyword arguments.  The mandatory argument is a function that is
called for each option \(aka switch\) or standalone argument
encountered on a command-line.  The two optional keyword arguments
are:

- `:COMMAND-LINE` can be used to supply a proper list of strings to
  be processed as a command line.  The default for this keyword is
  NIL.  The keyword `:ARGV` is a synonym for brevity.
- `:USE-OS-COMMAND-LINE` can be used to toggle the use of the
  command-line provided to the Lisp environment by the operating system.
  The default for this keyword is T.  The keyword `:OS` is a synonym for
  brevity.

As noted, the function returned by `CLI:MAKE-PROCESSOR` takes a
mandatory argument, a function supplied by the caller, which is
invoked for each option or argument identified by the command-line
processor.  Each call has three arguments.  The first is the keyword
`:OPT` or `:ARG`, indicating whether an option \(switch\) or an
non-option argument was found.  When `:ARG`, the second argument is a
string, an argument from the command-line that was not associated with
an option, and the third argument is NIL.  When `:OPT`, the second
argument is usually a string naming an option \(although see `STYLES`
below\), and the third argument is a string value associated with that
option, or NIL.

`ARGOPTS`, if supplied, is a list of all options \(short or long\)
that require an argument.  While Petulant can automatically recognize
some options that explicitly take an argument \(as in
\"--file=foo.psd\" or \"/file:foo.psd\"\), it needs the hint in
`ARGOPTS` to recognize other patterns on the command-line \(such as
`-f foo.psd`, or `/file foo.psd`\).  Simply place the option (no
leading hyphens or slashes) as a string in this list.  The call below
would recognize both `-f` and `--file` as requiring an argument.
`ARGOPTS` does not limit the options that the processor handles, even
those with arguments; it merely expands the options and switches that
the processor can recognize as requiring arguments.

    \(cli:make-processor :argopts '\(\"f\" \"file\"\) …\)

\(Note that \"f\" in this call to `CLI:MAKE-PROCESSOR` is much better
handled by an alias below, or by the use of `:PARTIAL` in `STYLES`;
its presence here is merely for example.\)

`FLAGOPTS`, if supplied, is a list of all the options \(short or
long\) that do not take an argument.  This argument has no effect on
the processor unless `:PARTIAL` appears in `STYLES`; see `:PARTIAL`
below.

    \(cli:make-processor :flagopts '\(\"verbose\" \"debug\" \"trace\"\) …\)

`ALIASES` can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.
`ALIASES` is a list of lists.  Every element of `ALIASES` is a list
naming the primary option first, followed by all aliases for it.  For
example, in the call below, `/delay` `/sleep` and `/wait` would all be
recognized by the processor, but treated in all three cases as if
\"/delay\" were seen.  Also, the function supplied to the processor by
the caller would only be invoked with `\"delay\"` even when `sleep` or
`wait1 was found on the command-line.

    \(cli:make-processor :aliases '\(\(\"alpha\" \"transparency\"\)
                                   \(\"delay\" \"sleep\" \"wait\"\)
                                   \(\"file\" \"f\"\)\)
                        …\)

`STYLES` is a keyword, or a list of keywords, that influence the
option processor's behavior.  Recognized keywords are as follows;
unrecognized keywords are silently ignored.

- `:STR=`  String matching between `ARGOPTS`, `FLAGOPTS`, `ALIASES`,
  and the command-line being parsed is usually sensitive to
  case except when running under Windows.  `:STR=` exists
  solely to override any folding semantics implied by
  `:WINDOWS`, `:UNIX`, `:UP`, `:DOWN`, `:KEY`, and the local
  Lisp environment.  Overrides `:STREQ`.  Its name is meant
  to be evocative of `STRING=`.

- `:STREQ` Ensures that string matching between `ARGOPTS`, `FLAGOPTS`,
  `ALIASES`, and the command-line being parsed is insensitive
  to case \(unless `:STR=` is present\).  Its name is meant
  to be evocative of `STRING-EQUAL`.

- `:UP` All option names presented to the option processor will be
  converted to upper case.  Implies `:STREQ`.

- `:DOWN` All option names presented to FN will be converted to lower
  case.  Implies `:STREQ`.

- `:KEY` All option names presented to the option processor will be
  converted to symbols in the keyword package.  Implies `:UP`.

- `:PARTIAL` Support partial matches of options.  When present,
  Petulant will support unambiguous partial matches of options \(as
  they appear in `ARGOPTS`, `FLAGOPTS`, and `ALIASES`\), as if they
  were aliases.  For example, if `ARGOPTS` contained `beat`, then
  `:PARTIAL` would trigger recognition of `b`, `be`, and `bea`
  for `beat`.  But if `FLAGOPTS` also contained `bop`, then `b`
  would no longer be recognized as an alias for `beat`, and `bo`
  would be treated as an alias for `bop`.

- `:UNIX` Disregard the current running system, and process the
  command-line arguments as if in a Unix environment.  Also implies
  `:STR=`.

- `:WINDOWS` Disregard the current running system, and process the
  command-line arguments as if in a Windows environment.  Also implies
  `:STREQ`.

    \(cli:make-processor :styles '\(:unix :key\) …\)"
  (with-context-simple (argopts flagopts aliases styles argv)
    (let ((renamer (optname-fn))
	  (parser (make-processor*)))
      ;; duplicate the lambda-list of the processor from MAKE-PARSER
      ;; so that we can make this look like it's the actual processor
      ;; returned by MAKE-PROCESSOR*.  Yuck, I hate being redundant.
      ;;
      ;; Maybe we should have a opt renamer function in the base
      ;; parser that goes unused except by process and friends.
      ;;
      ;; Think so. XXX
      
      (lambda (fn &key command-line argv (use-os-command-line t) (os t))
	(funcall parser
		 (lambda (x y z)
		   (funcall fn
			    x
			    (if (eq x :opt)
				(funcall renamer y)
				y)
			    z))
		 :use-os-command-line use-os-command-line
		 :os os
		 :command-line command-line
		 :argv argv)))))


;;; ----------------------------------------

(defun process* (fn)
  "Invoke the simple argument parser using functions that provide
common functionality based on the current *CONTEXT*.  Think of it like
this: PARSE provides the understanding of the command-line and
CONTEXT provides the \"usual\" understanding of flag options, argument
options, styles, and the like."
  (parse fn
	 :argoptp-fn (argoptp-fn)
	 :chgname-fn (compose (aliases-fn) (partials-fn))
	 :args (args *context*)))

(defun process (fn &key argopts flagopts aliases arglist styles)
  "CLI:PROCESS examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.

FN is a function supplied by the caller, which is invoked for each
option or argument identified by CLI:PROCESS.  Each call to FN has three
arguments.  The first is the keyword :OPT or :ARG, indicating whether
an option \(switch\) or an non-option argument was found.  When :ARG,
the second argument is a string, an argument from the command-line
that was not associated with an option, and the third argument is NIL.
When :OPT, the second argument is usually a string naming an option
\(although see STYLES below\), and the third argument is a string
value associated with that option, or NIL.

ARGOPTS, if supplied, is a list of all options \(short or long\) that
require an argument.  While Petulant can automatically recognize some
options that explicitly take an argument \(as in \"--file=foo.psd\" or
\"/file:foo.psd\"\), it needs the hint in ARGOPTS to recognize other
patterns on the command-line \(such as \"-f\" \"foo.psd\", or
\"/file\" \"foo.psd\"\).  Simply place the option (no leading hyphens
or slashes) as a string in this list.  The call below would recognize
both \"-f\" and \"--file\" as requiring an argument.  ARGOPTS does not
limit the options that PROCESS handles, even those with arguments; it
merely expands the options and switches that PROCESS can recognize as
requiring arguments.

   \(cli:process … :argopts '\(\"f\" \"file\"\) …\)

\(Note that \"f\" in that call to CLI:PROCESS is better handled by an
alias below, or by the use of :PARTIAL in STYLES; its presence here is
merely for example.\)

FLAGOPTS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  This argument has no effect on
CLI:PROCESS unless :PARTIAL appears in STYLES; see :PARTIAL below.

   \(cli:process … :flagopts '\(\"verbose\" \"debug\" \"trace\"\) …\)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is a list of lists.  Every element of ALIASES is a list naming the
primary option first, followed by all aliases for it.  For example, in
the call below, \"/delay\" \"/sleep\" and \"/wait\" would all be
recognized by CLI:PROCESS, but processed as if \"/delay\" were seen.
Also, the FN supplied by the caller would only be invoked with
\"delay\" even when \"sleep\" or \"wait\" was found on the
command-line.

  \(cli:process … :aliases '\(\(\"alpha\" \"transparency\"\)
                           \(\"delay\" \"sleep\" \"wait\"\)
                           \(\"file\" \"f\"\)\) …\)

ARGLIST causes CLI:PROCESS to process a specified list of strings,
instead of the default command-line that was supplied to the
application.  These strings are parsed exactly as if they appeared on
a command-line, each string corresponding to one \"word\".

   \(cli:process … :arglist '\(\"-xv\" \"-f\" \"foo.tar\"\) …\)

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
	    ALIASES\), as if they were aliases.  For example, if
	    ARGOPTS contained \"beat\", then :PARTIAL would trigger
	    aliases of \"b\", \"be\", and \"bea\" for \"beat\".  But
	    if FLAGOPTS also contained \"bop\", then \"b\" would no
	    longer be recognized as an alias for \"beat\", and \"bo\"
	    would be treated as an alias for \"bop\".

   :UNIX    Disregard the current running system, and process the
	    command-line arguments as if in a Unix environment.
            Also implies :STR=.

   :WINDOWS Disregard the current running system, and process the
	    command-line arguments as if in a Windows environment.
	    Also implies :STREQ."
  (with-context-simple (argopts flagopts aliases styles arglist)
    (let ((renamer (optname-fn)))
      (process*
       (lambda (x y z)
	 (funcall fn
		  x
		  (if (eq x :opt)
		      (funcall renamer y)
		      y)
		  z))))))
