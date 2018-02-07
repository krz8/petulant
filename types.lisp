(in-package #:petulant)

;;; We add type functionality to Petulant here.  This, in turn, is
;;; used to both extend the simple interface, as well as provide part
;;; of the full interface.

;;; An option can be specified in two ways.  The easy way just names
;;; the option itself and omits any type information.
;;;
;;;     "foobar"
;;;
;;; Or, it can be spceified in a proper list, providing the option
;;; name, an optional description, its primary type (as a symbol in
;;; the keyword package), and any arguments to that type.  Such as,
;;;
;;;     ("verbose" :flag)
;;;     ("name" :string)
;;;     ("name" :string 50)
;;;     ("iterations" :integer)
;;;     ("iterations" :integer 0)
;;;     ("volume" :real 0.0 10.0)
;;;     ("volume" "But this one goes to eleven." :real 0.0 11.0)
;;;
;;; Flags are simple, either appear or they don't on a command-line,
;;; and they don't take arguments.
;;;
;;; Strings have one argument, giving a maximum length to the
;;; string.  If an option's value is longer, it is silently truncated
;;; to this maximum length.  If no argument is supplied, its default
;;; value is '* which indicates any length string is acceptable (no
;;; truncation is performed).
;;;
;;; Numeric types (integer, ratio, rational, real) have two optional
;;; arguments.  The first gives the minimum value, and the second
;;; gives the maximum.  Both default to '* which imply no limits on
;;; the value.  So, "no negative" values might be represented with
;;; (:real 0 *) or just (:real 0).

(defun canonicalize-string (x)
  )

(defun canonicalize-type (spec)
  "Given a type specifier that might be complete, incomplete, or even
a shortcut notation, return a type specifier that is complete,
documented, and validated.  In other words, this function is
responsible for converting anything we get from the client into a
regular and safely deconstructed type specification, or NIL.

       \(canonicalize-type \"foo\"\)
    => \(\"foo\" \"\" :string *\)
       \(canonicalize-type \"foo\" :string\)
    => \(\"foo\" \"\" :string *\)
       \(canonicalize-type \"foo\" :string 50\)
    => \(\"foo\" \"\" :string 50\)
       \(canonicalize-type \"foo\" \"Blah blah blah\"\)
    => \(\"foo\" \"Blah blah blah\" :string *\)
       \(canonicalize-type \"foo\" \"Blah blah blah\" :string\)
    => \(\"foo\" \"Blah blah blah\" :string *\)
       \(canonicalize-type \"foo\" \"Blah blah blah\" :string 50\)
    => \(\"foo\" \"Blah blah blah\" :string 50\)"
  (cond
    ((listp spec)
     (destructuring-bind (&optional opt type x y)
	 spec
       (let ((opt (stringify opt)))
	 (cond
	   ((zerop (length opt))
	    (wrn "NIL option name encountered (ignoring it)."))
	   (())))))
    ((stringp spec)
     (list spec :string '*))
    (t
     (wrn "Unrecognized type ~s (ignoring it)." spec))))	; nil
