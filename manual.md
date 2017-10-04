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
several other command line option and argument parsers for CL, and
they all seem to expect their users to follow POSIX (or GNU) style
command line parsing.

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



A Word About Nomenclature
-------------------------

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



Functionality
-------------

Petulant provides its functionality in layers, in order to suit
some very different contexts in which it could be used.

- [The Basic Functional Interface](#basic)

  A simple parser works through a command line, invoking functions
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
  representing the entire command line.

- [The Specification-Driven Interface](#spec)

  For those that like writing out entire static specifications of
  their command line options and argument processing needs, here you.
  This most resembles what people expect if they come from
  getopt-style experiences.  However, while it's beneficial in
  applications with potentially extensive command lines, the other
  interfaces are much easier to use when all that's needed are a
  handful of flags, or while a tool is still in development.



<a name="basic"></a>

The Basic Functional Interface
------------------------------

foo



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
