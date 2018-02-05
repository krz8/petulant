(in-package #:petulant)

(defun cb (&rest args)
  "A test function for debugging parsers that just echoes its
arguments back.  You can use this as the callback for parsers and
processors."
  (format t "cb~{ ~s~}~%" args))

(defun make-processor (&key argopts flagopts aliases styles)
  "CLI:MAKE-PROCESSOR constructs and returns a closure that processes
command-lines as described by the keyword arguments.  This returned
function takes one mandatory argument and one optional keyword
argument.  The mandatory argument is a function that is called for
each option \(aka switch\) or standalone argument encountered on a
command-line.  The optional keyword arguments is :ARGV, taking a
proper list of strings to be used as a command-line instead of those
with which the application was invoked.

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
list.  For example, the call below would generate a processor that
recognizes both \"-f\" and \"--file\" as requiring an argument; all
other options would be assumed to be simple flags.  ARGOPTS does not
limit the options that the processor handles, even those with
arguments; it merely ensures that certain options are interpreted
correctly in ambiguous contexts.

    \(cli:make-processor :argopts '\(\"f\" \"file\"\)
                        …\)

\(Note that \"f\" in this particular use of CLI:MAKE-PROCESSOR is much
better handled by an alias below, or by the use of :PARTIAL in STYLES;
its presence there is merely for example.\)

FLAGOPTS, if supplied, is a list of all the options \(short or long\)
that do not take an argument.  Unless :PARTIAL appears in STYLES, this
has no real effect as flag options are recognized automatically by
Petulant.

    \(cli:make-processor :flagopts '\(\"verbose\" \"debug\" \"trace\"\)
                        …\)

ALIASES can be used to supply one or more alternative options that,
when encountered, are considered aliases for another option.  ALIASES
is a list of lists.  Every element of ALIASES is a list naming the
primary option first, followed by all aliases for it.  For example, in
the call below, \"/delay\" \"/sleep\" and \"/wait\" would all be
recognized by the processor, but treated in all three cases as if
\"/delay\" were seen.  The function supplied to the processor by the
caller would be invoked with \"delay\" even when \"sleep\" or \"wait\"
was found on the command-line.

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

    \(cli:make-processor :styles '\(:unix :key\)
                        …\)"
  (with-context-simple (argopts flagopts aliases styles)
    (let* ((keyify (maybe-chgopt-key-fn))
	   (parser (make-parser :optargp (optargp-fn)
				:chgopt (compose (partials-fn) (aliases-fn)
						 (maybe-chgopt-case-fn)))))
      (lambda (fn &key argv)
	(funcall parser
		 (lambda (x y z)
		   (funcall fn x
			    (or (and (eq :opt x)
				     (funcall keyify y))
				y)
			    z))
		 :argv argv)))))

(defun process (fn &key argopts flagopts aliases styles argv)
  "CLI:PROCESS examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.  CLI:PROCESS is
mostly meant for \"one time\" parsing as it creates a new command line
processor on every invocation; if you have multiple command-lines to
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

- :ARGV, a proper list of strings to be used instead of the actual
  command-line argument with which the application was invoked.

See CLI:MAKE-PROCESSOR or even CLI:MAKE-PARSER for more details."
  (funcall (make-processor :argopts argopts :flagopts flagopts
			   :aliases aliases :styles styles)
	   fn
	   :argv argv))
