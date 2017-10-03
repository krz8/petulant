Overview
--------

Petulant is a command-line parser that can be used when delivering
Common Lisp applications under both Windows and Unix, supporting
*native* CLI styles.  That means

- Windows users don't have to learn GNU or POSIX hyphen-based options,
- Unix users don't need to worry about Windows slash-based switches, and
- Developers only need one body of code to support both environments.

As I begin to develop native executable applications for both Windows
and Unix-like environment (hat tip to the wonderful [Roswell][]
project), I ran into a "one size fits all" problem.  I looked through
several other command line option and argument parsers for CL, and
they all seem to expect their users to follow POSIX (or GNU) style
command line parsing.

[roswell]: https://github.com/roswell/roswell

Now, that might be fine for people who use Unix-derived systems, but I
can easily imagine the frustration and irritation of Windows users
encountering such software.  If someone insisted that I have to adapt
to using Windows switch syntax in all of my commands, I'd be mightily
annoyed.  Petulant is my response.



Functionality
-------------

Here's what you get:

- No need for specifications to process simple sets of options,
- But if you really like writing out specifications, Petulant works with
  those, too,
- A functional interface to CLI parsing, and
- A data interface to CLI parsing.

### What's Supported Currently
  
- POSIX style short options: `foo -a -b -c arg1 arg2`
- POSIX style combinations: `foo -a -bc arg1 arg2`
- POSIX style option arguments: `foo -ab -f file -d2 arg1 arg2`
- Historical option arguments: `foo -xvf junk.tar arg1 arg2`
- Hysterical option combinations with arguments: `foo -abd2 arg1 arg2`
- GNU long options with `=`: `foo -ab --input=file --delay=2 arg1 arg2`
- GNU long options without `=`: `foo -ab --input file --delay 2 arg1 arg2`
- GNU out-of-order arguments: `foo -ab arg1 -f file arg2`
- GNU double dash termination: `foo -ab -- -these --are -not --options`

### What's Coming Next

- Abbreviated long options: `foo --in file`
- Addition to [Quicklisp][], as soon as I feel Petulant is mature enough.

[Quicklisp]: https://www.quicklisp.org/beta/

### Future

Certainly, there are yet more styles out there that are in wide use
(or, were in wide use at one point).  I'm not against these, I'm only
listing them here to be up front about what Petulant does and doesn't
do at present.  (Most of these are easy for a caller to implement in
the meantime.)

- Korn shell automatic numeric parsing
- Korn shell option subargument support
- `-f`/`+f` toggling
- `--no-foo` negation
- Types (numeric, strings, booleans, etc) for arguments

Pull requests are always welcome.  Petulant includes a pretty good set
of tests via [FiveAM][], so it's easy to see if your improvements work
out or not.

[FiveAM]: https://common-lisp.net/project/fiveam/



Credits
-------

- [Roswell][], which inspired my need for Petulant,
- [FiveAM][], which kept me sane during development,
- [Iterate][], which is just a joy to use compared to LOOP's extended forms,
- [Alexandria][], which saves me from re-inventing wheels, and
- [Anaphora][], which helps me break my bad NIH habits.

[Alexandria]: https://common-lisp.net/project/alexandria/
[Anaphora]: https://common-lisp.net/project/anaphora/
[Iterate]: https://common-lisp.net/project/iterate/



License
-------

Petulant is available under the [MIT License][].

[MIT License]: https://opensource.org/licenses/MIT

Copyright © 2017 Robert S. Krzaczek

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
