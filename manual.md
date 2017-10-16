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

Petulant exists so that developers can deliver applications in
Unix and Windows environments, supporting users who don't want to
give up the established command-line semantics of their respective
systems.

[home]:    https://krz8.github.io/petulant        "Petulant Homepage"
[manual]:  https://krz8.github.io/petulant/manual "Petulant Manual"
[roswell]: https://github.com/roswell/roswell     "Roswell Project Homepage"



Some Words About Options and Arguments
--------------------------------------

Let me apologize in advance.  My long-term operating system of choice
has always been Unix and Unix-like systems.  It's only in recent years
that I've needed to live under Windows, and even more recently that I've
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
(_aka_ long) options.  It recognizes long options that take arguments
when they use equal signs, as well as switches that use colons.  For
example, these two lines can be parsed in their respecting operating
systems without any specification or other help:

    foo -v -abc --conf=test.json /some/other/place
    foo /v /a/b/c /conf:test.json X:\some\other\place

It's only when arguments need to be associated with short options,
long options without an equals sign, or switches without colons, that
you need to help Petulant out a bit and toss it a clue.

    foo -xvfpath
    foo -xvf path
    foo /x/v /f path
    foo -xv --input path
    foo /x/v /input path
    


Functionality
-------------

Petulant provides its functionality in layers, in order to suit
some very different contexts in which it could be used.

- [The Functional Interface](#func)

  This was originally the intended way to use Petulant.  The caller
  supplies a single function for processing option and argument
  events.  Optionally, the caller can also specify aliases for options
  and switches.  Petulant provides not only option and argument
  processing, but also supports case folding, mapping option strings
  to keywords, and partial unique matching of specified options.

- [The Data-Oriented Interface](#data)

  Here you can simply call Petulant and get back one easy-to-read data
  structure representing the entire command-line.  Small to medium
  sized applications will probably use this most, as it's the simplest
  to work with for most projects.

- [The Specification-Driven Interface](#spec)

  For those that like writing out entire static specifications of
  their command-line options and argument processing needs, here you
  go.  This most resembles what people expect if they come from
  getopt-style experiences.  However, while it's beneficial in
  applications with potentially extensive command-lines, the other
  interfaces are much easier to use when all that's needed are a
  handful of flags, or while a tool is still in development.

- [The Basic Functional Interface](#basic)

  A simple parser works through a command-line, invoking functions
  provided by the caller.  The calls to that function indicate what
  was found by the parser (not entirely unlike a stream event parser).
  This is provided so that a developer might provide their own
  command-line parsing implementation.

Also, after working through the available functionality below, if
there's something that you'd like to see added to Petulant, feel free
to write me and ask.


<a name="func"></a>

The Functional Interface
------------------------

A option and argument parser is provided through the `parse-cli`
function.  The caller provides a function that is called back with
whatever options and arguments Petulant teases from the command line,
in left-to-right order.  This is the main interface to Petulant,
the functionality in the data-oriented and the specification-based
interfaces are built on top of this.

### API

Function **parse-cli** fn _&key_ optargs optflags aliases arglist styles

**parse-cli** examines the command-line with which an application was
invoked.  According to given styles and the local environment,
options (aka switches) and arguments are recognized.

**fn** is a function supplied by the caller, which is called for each
option or argument identified by **parse-cli**.  Each call to **fn**
has three arguments.  The first is the keyword **:opt** or **:arg**,
indicating whether an option (aka switch) or an non-option argument
was found.  When **:arg**, the second argument is a string, an
argument from the command-line that was not associated with an option,
and the third argument is **nil**.  When **:opt**, the second argument
is usually a string naming an option (although see **styles** below),
and the third argument is a string value associated with that option,
or **nil**.

**optargs**, if supplied, is a list of all options (short or long)
that require an argument.  While Petulant can automatically
recognize some options that explicitly take an argument (as in
“--file=foo.psd” or “/file:foo.psd”), it needs the hint in
**optargs** to recognize other patterns (such as “-f” “foo.psd”, or
“/file” “foo.psd”).  Simply place the option (no leading
hyphens or slashes) as a string in this list.  The call below would
recognize both “-f” and “--file” as requiring an argument.
(Note that “f” in the list is better handled by an alias below,
or by the use of **:partial** in **styles**; its presence here is merely
for example.) **optargs** does not limit the options that **parse-cli**
handles, even those with arguments; it is merely a hint that

```cl
(parse-cli fn :optargs '("delay" "file"))
````

**optflags**, if supplied, is a list of all the options (short or
long) that do not take an argument.  This argument has no effect on
**parse-cli** unless **:partial** appears in **styles**.  See
**:partial** below.

```cl
(parse-cli fn :optflags '("verbose" "debug" "trace"))
```

**aliases** can be used to supply one or more alternative options
that, when encountered, are considered aliases for another option.
**aliases** is a list of lists.  Every element of **aliases** is a
list naming the primary option first, followed by all aliases for it.
For example, in the call below, both “/sleep” and “/wait” would be
recognized by **parse-cli**, but processed as if “/delay” were seen.

```cl
(parse-cli fn :aliases '(("alpha" "transparency")
                         ("delay" "sleep" "wait")))
```

**arglist** causes **parse-cli** to parse a specified list of strings,
instead of the default command-line that was supplied to the
application.  These strings are parsed exactly as if they appeared on
the command-line, each string corresponding to one “word”.

```cl
(parse-cli fn :arglist '("-xv" "-f" "foo.tar"))
```

**styles** is a keyword, or a list of keywords, that influence
Petulant's behavior.  Recognized keywords are as follows;
unrecognized keywords are silently ignored.

- **:str=**  
  String matching between **optargs**, **optflags**, **aliases**, and
  the command-line being parsed is sensitive to case.  This exists
  solely to override any folding semantics implied by **:windows**,
  **:unix**, **:up**, **:down**, **:key**, and the local Lisp
  environment.  Overrides **:streq**.  Its name is meant to be
  evocative of **string=**.
- **:streq**  
  String matching between **optargs**, **optflags**, **aliases**, and
  the command-line being parsed is insensitive to case.  Its name is
  meant to be evocative of **string-equal**.
- **:up**  
  All option names presented to **fn** will be converted to upper
  case.  Implies **:streq**.
- **:down**  
  All option names presented to **fn** will be converted to lower
  case.  Implies **:streq**.
- **:key**  
  All option names presented to **fn** will be converted to symbols in
  the keyword package.  Implies **:up**.
- **:partial**  
  Support partial matches of options.  When present, Petulant will
  support unambiguous partial matches of options (as they appear in
  **optargs**, **optflags**, and **aliases**).  For example, if
  **optargs** contained “beat”, then **:partial** would trigger
  aliases of “b”, “be”, and “bea” for “beat”.  But, if **optflags**
  also contained “bop” then “b” would no longer be recognized, instead
  “be” and “bo” would become the minimum length unambiguous matches
  for “beat” and “bop”.
- **:unix**  
  Disregard the current running system, and process the command-line
  arguments as if in a Unix environment.
- **:windows**  
  Disregard the current running system, and process the command-line
  arguments as if in a Windows environment.  Also implies **:streq**.

### Usage

The basic call to **parse-cli** is as simple as the following example.
With it, the supplied function **myfun** will be called once for each
option and for each argument encountered on the command-line.  That
command-line, in turn, will be parsed under Unix or Windows
conventions, according to the presence of the **:windows** feature in
the Lisp runtime environment.

```cl
(parse-cli #'myfun)
```

To work through the usage of Petulant, we'll imagine developing an
application that needs to parse its command-line for the remainder of
this section.  We'll start with something dirt simple and add more
capabilities to it by using features provided by Petulant.

In the early stages of application development, we often don't know
exactly what the final command-line will eventually look like.  It's
convenient to just recognize the options and arguments we expect, and
ignore the rest for the time being.  Consider supporting a traditional
verbosity flag in the application.

```cl
(defvar *verbose* nil)

  ;; somewhere in a main function
  (parse-cli (lambda (kind item extra)
               (declare (ignore extra))
               (when (and (eq kind :opt) (string= item "v"))
                 (setf *verbose* t))))
```

If the application were deployed on a Unix system, the first line
below would work as you might expect.  Likewise, under Windows, the
second line would be supported.

```text
$ myapp -v
C:\Users\krz> myapp /v
```

Adding two arguments (not options) to the command-line is fairly
simple.  We will break out the lambda form into its own named function
now, since we're going to be adding to this in later examples.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (declare (ignore extra))
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t)))))))
    (parse-cli #'handler)))
```

And now command-lines like the following are supported.  The
application can determine when zero, one, or both command-line
arguments are provided, as well as (perhaps) print diagnostic
information when extra verbosity is selected.

```text
$ myapp -v data.csv report.tex
C:\Users\krz> myapp /v data.csv report.tex
```

Alternatively, some applications enjoy options that have opposite
effects on each other.  Here, we can create a verbosity level for an
application, where every instance of a **v** option increases the
verbosity and every instance of a **q** option decreases it.

```cl
(defvar *verbose* 0)

(defun args ()
  (flet ((handler (kind item extra)
           (declare (ignore extra))
           (when (eq kind :opt)
             (cond
               ((string= "v" item) (incf *verbose*))
               ((string= "q" item) (decf *verbose*))))))
    (parse-cli #'handler)))
```

Though we won't spend much time on error processing in most of the
remaining examples, it should be obvious how to detect and act on
various error situations.  Imagine that our application requires its
first argument, but that its second one is optional; additionally, we
want to report when unknown options are supplied.  A brutally simple
approach might be something like this:

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (declare (ignore extra))
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler)
    (unless *input*
      (error "at least one argument must be supplied"))))))
```

Now, let's update the application to take another command-line
option called **config** which should name a configuration file.
The basic support for a long option, using an **=** or **:**
character under Unix or Windows, is already present in Petulant.
Note that all we've done in this example is to start using the
**extra** argument in the function provided to **parse-cli**.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)
(defvar *config* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t))
                     ((string= "config" item) (setf *config* extra))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler)
    (unless *input*
      (error "at least one argument must be supplied"))))))
```

And at this point, we can handle command-lines like the following.

```text
$ myapp -v --config=test.yml data.csv report.tex
C:\Users\krz> myapp /config:test.yml /v data.csv report.tex
```

Here we see the problem that Petulant faces with recognizing
options and switches that take arguments: without the **=**
or **:** characters, as in the next example, Petulant has no
way of knowing that **config** takes an argument but **v**
does not.

```text
$ myapp -v --config test.yml data.csv report.tex
C:\Users\krz> myapp /config test.yml /v data.csv report.tex
```

Dealing with this is what the **optargs** keyword argument solves.
Petulant already knows that any option in the form **--option=foo** or
any switch **/switch:foo** obviously takes a value **foo**.  Here, we
use **optargs** to tell Petulant all the options that take an argument
even when appearing without the extra decoration.  Note that the rest
of the code is unchanged, all we've done is add an argument to
**parse-cli**.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)
(defvar *config* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t))
                     ((string= "config" item) (setf *config* extra))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler :optargs '("config"))
    (unless *input*
      (error "at least one argument must be supplied"))))))
```

Another useful feature is the ability to accept options as aliases
for one another.  Consider that the application takes some value
specifying a delimiter of some kind; the trouble being that some
of the app's audience thinks of it as as “delimiter” while others
think of it as a “separator.”  Petulant allows one to be established
as an alias of the other, so that the option processing code only
needs to concern itself with one primary option string.

We'll add that to our code now, along with a second alias that allows
the user to call the app with a “color” option, along with “rgb”
and “hue” names as well.  It's a little contrived, sure, but it
demonstrates the forms of multiple sets of aliases.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)
(defvar *config* nil)
(defvar *delim* ",")
(defvar *color* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t))
                     ((string= "color" item) (setf *color* extra))
                     ((string= "config" item) (setf *config* extra))
                     ((string= "delimiter" item) (setf *delim* extra))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler
               :optargs '("config" "delimiter" "color")
               :aliases '(("delimiter" "separator")
                          ("color" "rgb" "hue")))
    (unless *input*
      (error "at least one argument must be supplied"))))))
```

At this point, the supported command-line is becoming rich but
awkward.  Not many users _want_ to type long options like
**delimiter** and would prefer to abbreviate their option to simply
**--delim** or perhaps even **/d** if there are no other similarly
named options.  This is supported as well, with just a few more hints.

In order to support partial unambiguous option matching, Petulant will
need to know all the options that the application can process.  No
longer just the argument-bearing options, but also the options that
standalone as simple flags.  With this information, Petulant can
determine how many letters are shared between options, and therefore
what the minimum unique abbreviation is for each option.  We do this
by supplying an **optflags** argument, as well as a **:partial** style
argument.

Since we'll be working with full names, and we expect the shortest
viable abbreviations to be automatically processed, we'll rename the
**v** option to **verbose**.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)
(defvar *config* nil)
(defvar *delim* ",")
(defvar *color* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "verbose" item) (setf *verbose* t))
                     ((string= "color" item) (setf *color* extra))
                     ((string= "config" item) (setf *config* extra))
                     ((string= "delimiter" item) (setf *delim* extra))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler
               :optargs '("config" "delimiter" "color")
               :optflags '("verbose")
               :aliases '(("delimiter" "separator")
                          ("color" "rgb" "hue"))
               :styles :partial)
    (unless *input*
      (error "at least one argument must be supplied"))))))
```

Alas, to reach this extra functionality, we had to give up one of
Petulant's advantages: we can no longer skip over a specification of
all the options to be processed.

As written above, the application can now process command-lines like
the following.  Note that we have options **color** and **config**
now.  Whereas the rest of the options are unique at their very first
letter (which means **-d:** could stand for **--delimiter=:**, as would
**--delim=:**), it will take at least three characters in order to tell
the **color** and **config** options apart.  For that reason, neither
**/c** nor **/co** will be sufficient, a user would need to supply
**/col** or **/con** to get the option recognized.

These command-lines are now all valid.

```text
$ app -vd:
$ app -v -d : --conf=foo --hue=red
$ app -v -d: --conf foo -h red
$ app -v --delim=: --conf foo -h red

C:\Users\krz> APP /V/D:
C:\Users\krz> APP /V /D : /CONFIG:FOO /HUE RED
C:\Users\krz> APP /V /D:: /CONF FOO /H:RED
C:\Users\krz> APP /V /DELIM:: /CONF FOO /H RED
```

That example demonstrates something else that Petulant handles.  When
running under Windows, command-line string comparisons are insensitive
to case; however, in a Unix environment, case still matters.  This
behavior can be modified in a few ways.

**:streq** and **:str=** can be used to explicitly set the whether or
not Petulant matches options in a case insensitive or sensitive
manner, respectively.  To remember their meaning, recognize that their
names imply the relationship between **string-equal** and **string=**.

Also, **:windows** and **:unix** can be added to the styles list to
direct Petulant to process a command-line with Windows style switch
syntax, or Unix style option processing.  Only when the styles list
contains neither of these two keywords will Petulant choose a default
based on the running system.  (Yes, this is a runtime determination,
not performed at compilation time.)

To more easily see the differences in the styles argument, an **arglist**
argument is supported with **parse-cli** as well.  When used, Petulant
will use its value as the list of strings to parse, rather than taking
the strings from the Lisp environment.  A handy setup for trying
out various calls of **parse-cli** can be seen here; it is also very
useful when writing test cases to run in either environment.

```text
CL-USER> (parse-cli (lambda (&rest args) (format t "saw ~s~%" args))
                    :optargs '("config" "delimiter" "color")
                    :optflags '("verbose")
                    :aliases '(("delimiter" "separator")
                               ("color" "rgb" "hue"))
                    :styles '(:partial :unix)
                    :arglist '("-vd:" "--conf=foo" "--hue" "red" "input.csv"))
saw (:OPT "verbose" NIL)
saw (:OPT "delimiter" ":")
saw (:OPT "config" "foo")
saw (:OPT "color" "red")
saw (:ARG "input.csv" NIL)
T
CL-USER>
```

In addition to the **:str=** and **:streq** styles, and the
**:windows** and **:unix** styles that imply them, there are yet more
common transformations that Petulant can do via the **styles**
argument.  Two of them are case folding, where Petulant will change an
option name to all caps or all lower case (these imply **:streq** as
well).  As long as you aren't re-implementing **ls**(1), this is
rarely a bad thing in practice, and helps to work around easily
avoidable mistakes by the user.

```text
CL-USER> (parse-cli (lambda (&rest args) (format t "saw ~s~%" args))
                    :optargs '("config" "delimiter" "color")
                    :optflags '("verbose")
                    :aliases '(("delimiter" "separator")
                               ("color" "rgb" "hue"))
                    :styles '(:partial :unix :down)
                    :arglist '("-Vd:" "--cOnF=foo" "--Hue" "red" "input.csv"))
saw (:OPT "verbose" NIL)
saw (:OPT "delimiter" ":")
saw (:OPT "config" "foo")
saw (:OPT "color" "red")
saw (:ARG "input.csv" NIL)
T
CL-USER>
```

Finally, Petulant can call the function supplied to **parse-cli** with
its middle argument turned into a symbol in the keyword package.  This,
too, implies case folding and case insensitivity.  Other arguments still
need to be specified with strings; the use of **:key** only affects the
call to the supplied function.

```text
CL-USER> (parse-cli (lambda (&rest args) (format t "saw ~s~%" args))
                    :optargs '("config" "delimiter" "color")
                    :optflags '("verbose")
                    :aliases '(("delimiter" "separator")
                               ("color" "rgb" "hue"))
                    :styles '(:partial :unix :key)
                    :arglist '("-Vd:" "--cOnF=foo" "--Hue" "red" "input.csv"))
saw (:OPT :VERBOSE NIL)
saw (:OPT :DELIMITER ":")
saw (:OPT :CONFIG "foo")
saw (:OPT :COLOR "red")
saw (:ARG "input.csv" NIL)
T
CL-USER>
```

Including **:key** in any styles might make a handler marginally
simpler and more efficient, but it usually won't yield tremendous
gains unless there are _many_ string comparisons being performed in
the supplied function.  Instead, the main advantage of **:key** is
stylistic, enabling **case** statements and similar flexibility in an
option handler.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)
(defvar *config* nil)
(defvar *delim* ",")
(defvar *color* nil)

(defun args ()
  (flet ((handler (kind item extra)
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (case item
                     (:verbose (setf *verbose* t))
                     (:color (setf *color* extra))
                     (:config (setf *config* extra))
                     (:delimiter (setf *delim* extra))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler
               :optargs '("config" "delimiter" "color")
               :optflags '("verbose")
               :aliases '(("delimiter" "separator")
                          ("color" "rgb" "hue"))
               :styles '(:partial :key))
    (unless *input*
      (error "at least one argument must be supplied"))))))
```




<a name="data"></a>

The Data-Oriented Interface
---------------------------

foo



<a name="spec"></a>

The Specification-Driven Interface
----------------------------------

foo
