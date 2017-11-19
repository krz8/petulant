(in-package #:petulant)

(defun get-cli (&key argopts flagopts aliases arglist styles)
  "GET-CLI takes the same keyword arguments, and offers the same
functionality, as the function it wraps, PARSE-CLI.  See that
function for complete documentation.  Unlike PARSE-CLI, however,
it returns a single list of things found on the command-line, in
the order that they were found.  Each element of this list is,
itself, a list of two or three values.  If the first element is
:ARG, then the second argument is a string naming an argument not
associated with an option on the command-line.  Otherwise, the first
element is :OPT, the second argument is the option \(as a string or
keyword symbol, according to the STYLES argument\), and if the option
has an argument, that string appears as the third element.

All arguments and options to GET-CLI share the same name and carry the
same functionality as they appear in PARSE-CLI."
  (let (results)
    (parse-cli (lambda (&rest args) (push args results))
	       :argopts argopts :flagopts flagopts :aliases aliases
	       :arglist arglist :styles styles)
    (nreverse results)))
