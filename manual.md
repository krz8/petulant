Petulant Manual
===============

Overview
--------

[Homepage][home] [Manual][manual]

Petulant is a command-line parser that can be used when delivering
Common Lisp applications under both Windows and Unix, supporting
_native_ CLI styles.  That means:

- Windows users don't have to learn GNU or POSIX hyphen-based options,
- Unix users don't need to worry about Windows slash-based switches, and
- Developers only need one body of code to support both environments.

As I begin to develop native executable applications for both Windows
and Unix-like environment (hat tip to the wonderful [Roswell][]
project), I saw a “one size fits all” problem.  I looked through
several other command-line option and argument parsers for CL, and
they all seem to expect their users to follow POSIX (or GNU) style
command-line parsing.

Sure, that's fine for people who use Unix-derived systems, but I can
easily imagine the frustration and irritation of Windows users
encountering such software.  For example, if someone insisted that I
had to use Windows switch syntax in my commands under FreeBSD in order
to use their software, I'd be mightily annoyed, too, _no matter how
much better_ someone else claimed it was.

Hence, **Petulant**.

[home]:    https://krz8.github.io/petulant        "Petulant Homepage"
[manual]:  https://krz8.github.io/petulant/manual "Petulant Manual"
[roswell]: https://github.com/roswell/roswell     "Roswell Project Homepage"



Some Words About Options and Arguments
--------------------------------------

Let me apologize in advance.  My long-term operating system of choice
has always been Unix and Unix-like systems.  It's only in recent years
that I've needed to live under Windows, and even more recent that I've
needed to deliver Lisp applications in it.

So, while I know the proper terms are “options” and “switches” under
Unix and Windows (respectively), if I refer to “options” in the
documentation where I should have said “switches,” I apologize and
assure you this isn't some kind of favoritism.  It's merely habit.
Likewise, I'll probably be using the word “option” in some generic
sense that covers both operating systems.

Finally, “argument” has a meaning that's a bit sensitive to context.
Programs have arguments, and so do their options.  The former are the
arguments that aren't associated with an option, while the latter are.
Simple as that.

Petulant doesn't require a specification to parse a command-line.
This might be a surprise for people coming from a getopt-style
background, but it's true.  On its own, Petulant recognizes simple
switch flags, single character (_aka_ short) options, and full word
(_aka_ long) options.  It also recognizes long options that take
arguments when they use equal signs, as well as switches that use
colons.  For example, these two lines can be parsed in their
respecting operating systems without any specification or other help:

    foo -v -abc --conf=test.json /some/other/place
    foo /v /a/b/c /conf:test.json X:\some\other\place

It's only when arguments need to be associated with short options,
long options without an equals sign, or switches without colons, that
you need to help Petulant out a bit and toss it a clue.

    foo -xvfpath
    foo -xvf path
    foo -xv -f path
    foo -xv --input path
    foo /x/v /input path
    


Functionality
-------------

Petulant provides its functionality in layers, in order to suit
some very different contexts in which it could be used.

- [The Basic Functional Interface](#basic)

  A simple parser works through a command-line, invoking functions
  provided by the caller.  The calls to that function indicate what
  was found by the parser (not entirely unlike a stream event parser).

- [A Higher Functional Interface](#func)

  A second functional interface, a bit more powerful, is also
  available.  At this level, the caller supplies just a single
  function for processing option and argument events.  The caller also
  can specify that options are mapped from strings to keywords, that
  partial matching of options is supported and so forth.

- [The Data-Oriented Interface](#data)

  No longer a functional interface to parsing, here you can simply
  call Petulant and get back one easy-to-read data structure
  representing the entire command-line.  Small to medium sized
  applications will probably use this most, as it's the easiest
  to work with for most projects.

- [The Specification-Driven Interface](#spec)

  For those that like writing out entire static specifications of
  their command-line options and argument processing needs, here you.
  This most resembles what people expect if they come from
  getopt-style experiences.  However, while it's beneficial in
  applications with potentially extensive command-lines, the other
  interfaces are much easier to use when all that's needed are a
  handful of flags, or while a tool is still in development.

Also, after working through the available functionality below, if
there's something that you'd like to see added to Petulant, feel free
to write me and ask.



<a name="basic"></a>

The Basic Functional Interface
------------------------------

A simple, non-validating option and argument parser is provided
through the `simple-parse-cli` function.  The caller provides a
'function that is invoked with whatever Petulant finds on the command
line.  A second _optional_ function may also be supplied, to help
Petulant recognize more options that should take arguments.

This basic function is used in the implementation of the rest of
Petulant.  For that reason, it's documented and exported, as it might
be used by a caller to implement other functionality.  For most option
and argument processing, though, one of the other three interfaces are
recommended.

### API

Function **simple-parse-cli** fn _&key_ arglist sw-arg-p-fn style

This is the low level parser for command-lines.  If you're an end-user
of Petulant, you might want to consider calling a higher level
function; this one is mainly for implementation of other Petulant
functionality.

**simple-parse-cli** works through an argument list, a flat list of
strings representing the command-line.  It parses this list according
to the dominant style for a specific operating system (e.g.,
slash-based switches under Windows, and hyphen-based options under
Unix).

**fn** is called for each option (with or without an argument) and
every non-option argument identified during parsing.  Each call to
**fn** has three arguments.  The first is always **:arg** or **:opt**.
When **:arg**, the second argument is a non-switch argument string
from the command line, and the third argument is **nil**.  When
**:opt**, the second argument is a switch (a string) found on the
command line, eliding its leading slash, and the third argument is any
argument to that option or **nil**.

Generally speaking, the calls to **fn** proceed from the head to the tail
of the list of argument strings, and from left to right within each
string of that list.  This is useful to know in testing, but callers
probably should not rely on any specific ordering.

**arglist**, if supplied, is a list of strings to parse.  By default,
**simple-parse-cli** will parse a list of strings provided by the Lisp
environment representing the command-line with which the application
was started.

**optarg-p-fn**, if supplied, is a function.  Petulant recognizes
certain long options (`"--foo=bar"`) and switches (`"/foo:bar"`) that
unambiguously present an option taking an argument.  However, Petulant
cannot know for certain when a short option (`"-f" "bar"`) takes an
option, nor can it discern when a long option (`"--foo" "bar"`) or a
switch (`"/foo" "bar"`) lacking extra punctuation takes an argument.
The caller can supply a function taking the name of the option as a
string (`"f"` or `"foo"`) and returning true or false to indicate if
it takes an argument.

**style** can be used to select a particular style of command-line
processing.  By default, **simple-parse-cli** will choose the style
based on the current operating system environment.  However, the
caller can force a particular style by supplying `:unix` or
`:windows`, or by supplying a list containing `:unix` or `:windows`,
to this argument.

### Usage

Here is an example showing the argument list as received in a Unix
environment, and the simple binding for `fn` we supply to print its
arguments.  For this example, imagine the Lisp environment is hosted
on Unix and the argument list is
`("-a" "--beta" "--input=file" "some" "-v" "thing")`.

```cl
(simple-parse-cli #'(lambda (&rest args) (print args)))
=> NIL
(:OPT "a" NIL)
(:OPT "beta" NIL)
(:OPT "input" "file")
(:ARG "some" NIL)
(:OPT "v" NIL)
(:ARG "thing" NIL)
```

To ignore the command-line with which the application was invoked, the
`:arglist` and `:style` keywords can be used to force a particular
interpretation of options and arguments, no matter the current
environment.  This would yield the same results as the previous
example, and would do so on both Unix and Windows systems.

```cl
(simple-parse-cli #'(lambda (&rest args) (print args))
		  :arglist '("/a" "/beta" "/input:file" "some" "/v" "thing")
		  :styles :windows)
```

In the early stages of development, it may not always be clear what
options and arguments an application will need.  In order to not
spend too much time on the problem, it's useful to adopt a simple
approach to parsing command-lines, where anything is accepted and
only options we recognize are processed; everything else can be
safely ignored.  One wouldn't ship a final application in this state,
but for exploratory programming, for early development, for private
hacks, this proves sufficient.

Imagine being at a state in an application's development where it was
known that some sort of verbosity flag was needed.

```cl
(defparameter *verbose* nil)

(defun opts-and-args (kind name value)
  (when (and (eq kind :opt) (string-equal name "v"))
    (setf *verbose* t)))
```

Just the following call would be sufficient in such an application.
`-v` seen under Unix, and `/v` under Windows, would lead to
`*verbose*` becoming true.  All other options and arguments are
silently ignored by the `opts-and-args` function, as written above.

```cl
(simple-parse-cli #'opts-and-args)
```

Some applications take multiple instances of a switch to incrememnt
and decrement debugging verbosity.  Here, we'll implement one such
where `v` (verbose) and `q` (quiet) are used in just that manner.

```cl
(defparameter *verbose* 0)

(defun opts-and-args (kind name value)
  (case kind
    (:arg t)
    (:opt (cond
	    ((string-equal name "q")
	     (decf *verbose*))
	    ((string-equal name "v")
	     (incf *verbose*))))))
```

Obviously, we are reaching the point where a few well placed macros
would make our life much more succinct.  As our focus is on the
functionality and not its presentation, though, we'll stick with this
approach for just one more example.

Imagine our application has defined `*apphome*` as a directory where
all of its installed data lives, including some default configuration
file.  We will still take the `q` and `v` options from the command
line, but we'll also process a `c` option to specify an alternative
configuration file.  Two arguments will be taken from the command-line
as well that specify input and output files.  A first pass at
implementing this using the basic API might look like this:

```cl
(defparameter *verbose* 0)
(defparameter *inpath* nil)
(defparameter *outpath* nil)
(defparameter *confpath* (merge-pathnames "config.yaml" *apphome*)

(defun opts-and-args (kind x y)
  (case kind
    (:arg (cond
            ((null *inpath*)
             (setf *inpath* (pathname x)))
            ((null *outpath*)
             (setf *outpath* (pathname x)))))
    (:opt (cond
            ((string-equal x "c")
	     (setf *confpath* (pathname y)))
	    ((string-equal x "q")
	     (decf *verbose*))
	    ((string-equal x "v")
	     (incf *verbose*))))))
```

The call to `simple-parse-cli` changes slightly to include a function
for `optarg-p-fn` which recognizes `c` as taking an option:

```cl
(simple-parse-cli #'opts-and-args
                  :optarg-p-fn #'(lambda (x) (string-equal "c" x)))
```

Further functionality is obvious at this point.  One thing to consider
is our use of `string-equal` above, to get case insensitivity.  This
would be common under Windows, but it might surprise some Unix users
that `-c` and `-C` would be the same.  If we used the `string=`
function instead, the surprise might then be on the Windows users.
There's no single right answer, but your author humbly suggests that
unless you are reimplementing `ls`, case insensitivity is rarely a
real problem.

Hopefully, `opts-and-args` suggests why this is known as the _basic_
or _simple_ API.  If you prefer it, more power to you!  But if you
don't want to write your own implementations of common functionality
like case-insensitivity, partial matching of options, or option
aliases, you can use one of the other interfaces to Petulant, below.




<a name="func"></a>

A Higher Functional Interface
-----------------------------

foo



<a name="data"></a>

The Data-Oriented Interface
---------------------------

foo



<a name="spec"></a>

The Specification-Driven Interface
----------------------------------

foo
