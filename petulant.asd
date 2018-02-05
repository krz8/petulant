(defsystem "petulant"
  :description "a command line parser (CLI) supporting both Unix and Windows"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("alexandria" "anaphora" "iterate")
  :in-order-to ((test-op (test-op "petulant-test")))

  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/petulant/"
  :long-description "Petulant provides option and argument processing
under both Windows and Unix-like systems.  Code using Petulant accepts
a command line like `/a /b/c /out:foo \\bar\\baz` under Windows, as well
as `-a -bc --out=foo /bar/baz` under Unix-like systems, with no change
to the application.  Also, Petulant generally requires less
specification of its expected options than typical getopt(3)-like
libraries, although \"kitchen sink\" specification of option types and
validation is also supported.  Both functional and data interfaces are
provided to the caller."

  :serial t
  :components ((:file "pkg")
	       (:file "trie")
	       (:file "misc")
	       (:file "context")
	       (:file "scan")
	       (:file "process")
	       (:file "collect")
	       (:file "spec")))
