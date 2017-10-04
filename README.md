Petulant
========

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



Functionality
-------------

Here's what you get:

- No need for specifications to process simple sets of options,
- If you really like writing out specifications, though, Petulant works
  with those, too,
- A functional interface to CLI parsing, and
- A data interface to CLI parsing.


### What's Supported Currently

- Windows
  - Switches: `foo /a /b /c`
  - Switch combination: `foo /a/bc /def`
  - Switch arguments with colon: `foo /file:foo.bar /v/r`
  - Switch arguments without colon: `foo /file foo.bar /v/r`
  - Double slash termination (nonstandard, I know):
    `foo /a/b // /these are /not options /either`
- POSIX
  - Short options: `foo -a -b -c arg1 arg2`
  - Combinations: `foo -a -bc arg1 arg2`
  - Option arguments: `foo -ab -f file -d2 arg1 arg2`
  - Historical option arguments: `foo -xvf junk.tar arg1 arg2`
  - Hysterical option combinations with arguments: `foo -abd2 arg1 arg2`
- GNU
  - Long options with an equal sign: `foo -ab --input=file --delay=2 arg1 arg2`
  - Long options without an equal sign:
    `foo -ab --input file --delay 2 arg1 arg2`
  - Out-of-order non-option arguments: `foo -ab arg1 -f file arg2`
  - Double dash termination: `foo -ab -- -these --are -not --options`
- Additionally, there's
  - Case-sensitive and case-insensitive option parsing.
  - Mapping options to keywords.


### What's Coming Next

- Abbreviated long options: `foo --in file`
- Windows abbreviations: `foo /a/b /in:file /d2 arg1 arg2`
- Addition to [Quicklisp][], when I feel Petulant is good enough.

[Quicklisp]: https://www.quicklisp.org/beta/ "The Quicklisp Project Homepage"



### Things I Don't Know

- Detecting an alternative `SwitChar` (switch character) in use
  under Windows CMD and PowerShell.
- Accessing the actual command line string under Windows.  (More
  details on this in the manual).

I'd welcome the help, if you know how either of those work.


### Future Work

Certainly, there are yet more styles out there that are in wide use
(or, were in wide use in the past).  I'm not against these. I'm
listing them here to be up front about what Petulant does and doesn't
do at present.  (Most of these are easy for a caller to implement in
the meantime, with the exception of **3** below).

1. Korn shell automatic numeric parsing
2. Korn shell option subargument support
3. `-f`/`+f` toggling
4. `--no-foo` negation
5. Auto-parsing option argument types (numeric, strings, booleans, etc)

I will say, however, that often I saw those features being useful in
one context, but generally going unused or unnoticed everywhere else.
That's why I just haven't gotten around to them.  If you need one,
feel free to convince me they're a good idea!

On the other hand, pull requests are always welcome, and a much faster
way to get them added.  _(hint, hint)_ Petulant includes a pretty good
set of tests via [FiveAM][], so it's easy to see if your improvements
work out or not.

[FiveAM]: https://common-lisp.net/project/fiveam/



Credits
-------

- [Roswell][], which inspired my need for Petulant,
- [FiveAM][], which kept me sane during development,
- [Iterate][], which is simply a joy to use compared to LOOP's extended forms,
  and
- [Alexandria][] and [Anaphora][], which (try to) save me from re-inventing
  wheels.

[Alexandria]: https://common-lisp.net/project/alexandria/
              "Alexandria Package Homepage"
[Anaphora]:   https://common-lisp.net/project/anaphora/
              "Anaphora Package Homepage"
[Iterate]:    https://common-lisp.net/project/iterate/
              "Iterate Package Homepage"


License
-------

Petulant is available under the [MIT License][].

Copyright © 2017 Robert S. Krzaczek

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
“Software”), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[MIT License]: https://opensource.org/licenses/MIT
               "The MIT Open Source License"
