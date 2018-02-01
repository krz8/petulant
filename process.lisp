(in-package #:petulant)

(defun cb (&rest args)
  "A test function for debugging parsers that just echoes its
arguments back.  You can use this as the callback for CLI:PARSE and
CLI:PROCESS."
  (format t "cb~{ ~s~}~%" args))

(defun make-processor (&key argopts flagopts aliases styles)
  "CLI:MAKE-PROCESSOR constructs and returns a closure that processes
command-lines as described by the keyword arguments.  This returned
function takes one mandatory argument and two keyword arguments of its
own.  The mandatory argument is a function that is called for each
option \(aka switch\) or standalone argument encountered on a
command-line.  The two optional keyword arguments are:

- :COMMAND-LINE can be used to supply a proper list of strings to
  be processed as a command line.  The default for this keyword is
  NIL.  The keyword :ARGV is a synonym for brevity.
- :USE-OS-COMMAND-LINE can be used to toggle the use of the
  command-line provided to the Lisp environment by the operating system.
  The default for this keyword is T.  The keyword :OS is a synonym for
  brevity.

As noted, the function returned by CLI:MAKE-PROCESSOR takes a
mandatory argument, a function supplied by the caller, which is
invoked for each option or argument identified by the command-line
processor.  Each call has three arguments.  The first is the keyword
:OPT or :ARG, indicating whether an option \(switch\) or an
non-option argument was found.  When :ARG, the second argument is a
string, an argument from the command-line that was not associated with
an option, and the third argument is NIL.  When :OPT, the second
argument is usually a string naming an option \(although see STYLES
below\), and the third argument is a string value associated with that
option, or NIL.

ARGOPTS, if supplied, is a list of all options \(short or long\) that
require an argument.  While Petulant can automatically recognize some
options that explicitly take an argument \(as in \"--file=foo.psd\" or
\"/file:foo.psd\"\), it needs the hint in ARGOPTS to recognize other
patterns on the command-line \(such as \"-f\" \"foo.psd\", or
\"/file\" \"foo.psd\"\).  Simply place the options that take
arguments (without leading hyphens or slashes) as strings in this
list.  For example, the call below would recognize both \"-f\" and
\"--file\" as requiring an argument.  ARGOPTS does not limit the
options that the processor handles, even those with arguments; it
merely expands the options and switches that the processor can
recognize as requiring arguments.

    \(cli:make-processor :argopts '\(\"f\" \"file\"\) …\)

\(Note that \"f\" in this particular use of CLI:MAKE-PROCESSOR is much
better handled by an alias below, or by the use of :PARTIAL in STYLES;
its presence here is merely for example.\)

FLAGOPTS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  Unless :PARTIAL appears in STYLES, this
has no real effect as flag options are recognized automatically by
Petulant.

    \(cli:make-processor :flagopts '\(\"verbose\" \"debug\" \"trace\"\) …\)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is a list of lists.  Every element of ALIASES is a list naming the
primary option first, followed by all aliases for it.  For example, in
the call below, \"/delay\" \"/sleep\" and \"/wait\" would all be
recognized by the processor, but treated in all three cases as if
\"/delay\" were seen.  Also, the function supplied to the processor by
the caller would only be invoked with \"delay\" even when \"sleep\" or
\"wait\" was found on the command-line.

    \(cli:make-processor :aliases '\(\(\"alpha\" \"transparency\"\)
                                   \(\"delay\" \"sleep\" \"wait\"\)
                                   \(\"file\" \"f\"\)\)
                        …\)

STYLES is a keyword, or a list of keywords, that influence the
option processor's behavior.  Recognized keywords are as follows;
unrecognized keywords are silently ignored.

- :STR= String matching between ARGOPTS, FLAGOPTS, ALIASES, and the
  command-line being parsed is usually sensitive to case except when
  running under Windows.  :STR= exists solely to override any folding
  semantics implied by :WINDOWS, :UNIX, :UP, :DOWN, :KEY, and the
  local Lisp environment.  Overrides :STREQ.  Its name is meant to be
  evocative of STRING=.

- :STREQ Ensures that string matching between ARGOPTS, FLAGOPTS,
  ALIASES, and the command-line being parsed is insensitive
  to case \(unless :STR= is present\).  Its name is meant
  to be evocative of STRING-EQUAL.

- :UP All option names presented to the option processor will be
  converted to upper case.  Implies :STREQ.

- :DOWN All option names presented to FN will be converted to lower
  case.  Implies :STREQ.

- :KEY All option names presented to the option processor will be
  converted to symbols in the keyword package.  Implies :UP.

- :PARTIAL Support partial matches of options.  When present,
  Petulant will support unambiguous partial matches of options \(as
  they appear in ARGOPTS, FLAGOPTS, and ALIASES\), as if they
  were aliases.  For example, if ARGOPTS contained \"beat\", then
  :PARTIAL would trigger recognition of \"b\", \"be\", and \"bea\"
  for \"beat\".  But if FLAGOPTS also contained \"bop\", then \"b\"
  would no longer be recognized as an alias for \"beat\", and \"bo\"
  would be treated as an alias for \"bop\".

- :UNIX Disregard the current running system, and process the
  command-line arguments as if in a Unix environment.  Also implies
  :STR=.

- :WINDOWS Disregard the current running system, and process the
  command-line arguments as if in a Windows environment.  Also implies
  :STREQ.

    \(cli:make-processor :styles '\(:unix :key\) …\)"
  (with-context-simple (argopts flagopts aliases styles)
    (let ((parser (make-parser :optargp-fn (optargp-fn)
			       :chgopt-fn (maybe-chgopt-case-fn)))
	  (keyify-fn (maybe-chgopt-key-fn)))
      (lambda (fn &key command-line argv (use-os-command-line t) (os t))
	(funcall parser
		 (lambda (x y z)
		   (funcall fn x (or (and (eq :opt x)
					  (funcall keyify-fn y))
				     y)
			    z))
		 :command-line command-line
		 :argv argv
		 :use-os-command-line use-os-command-line
		 :os os)))))

(defun process (fn &key argopts flagopts aliases styles
		        command-line argv (use-os-command-line t) (os t))
  "CLI:PROCESS examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.  CLI:PROCESS is
mostly meant for \"one time\" parsing as it creates a new command line
processor on eveery invocation; if you have multiple command-lines to
process, you are better served by CLI:MAKE-PROCESSOR.

CLI:PROCESS combines the arguments of both CLI:MAKE-PROCESSOR and the
processor/parser it generates.  That is, it takes from the caller:

- FN, a function that will be invoked with three arguments for each
  option (aka switch) found on the command-line and for each standalone
  argument found on the command-line.  The three arguments are described
  in greater details in CLI:MAKE-PROCESSOR, but briefly, they are:
    1. The kind of thing encountered on the command-line, :ARG or :OPT.
    2. The argument or switch (option) itself.
    3. NIL, or a string containing the argument to the option in (2).

- :ARGOPTS, a proper list of strings naming the options (switches) that
  CLI:PROCESS should recognize as requiring an argument.

- :FLAGOPTS, a proper list of strings naming the options (switch) that
  do not take an argument.

- :ALIASES, a proper list of lists.  Each sublist is a proper list of
  strings that name aliases (alternative names) for the first string
  in that list.

- :STYLES, a keyword or list of keywords overriding default behaviors.
  These keywords can force the interpretation of the command-line according
  to Unix or Windows conventions regardless of the running system, can
  force case-sensitivity, can force all options to upper or lower case,
  or even remap them to keyword values.

- :COMMAND-LINE, a proper list of strings that should be used instead
  of the command-line provided to the application by the underlying
  operating system.  :ARGV is a synonym for this keyword.

- :USE-OS-COMMAND-LINE, whose default value is T, instructing Petulant
  to use the command-line provided by the operating system.  :OS is
  a synonym for this keyword.

See CLI:MAKE-PROCESSOR or even CLI:MAKE-PARSER for more details.

blah blah blah

FN is a function supplied by the caller, which is invoked for each
option or argument identified by CLI:PROCESS.  Each call to FN has three
arguments.  The first is the keyword :OPT or :ARG, indicating whether
an option \(switch\) or a non-option argument was found.  When :ARG,
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

ARGV causes CLI:PROCESS to process a specified list of strings,
instead of the default command-line that was supplied to the
application.  These strings are parsed exactly as if they appeared on
a command-line, each string corresponding to one \"word\".

   \(cli:process … :argv '\(\"-xv\" \"-f\" \"foo.tar\"\) …\)

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

  ;;; wait who gets which arguments again?
  
  (funcall (make-processor fn :argopts argopts :flagopts flagopts
			   :aliases aliases :style styles)
	   fn
	   
    )
  (let ((renamer (optname-fn)))
    (process*
     (lambda (x y z)
       (funcall fn
		x
		(if (eq x :opt)
		    (funcall renamer y)
		    y)
		z)))))
