(defsystem "petulant"
  :description "a command line parser (CLI) for both Windows and Unix"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("alexandria" "anaphora" "iterate")
  :in-order-to ((test-op (test-op "petulant-test")))

  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cl@gmail.com"
  :homepage "http://github.com/krz8/petulant/"
  :long-description "Petulant provides option and argument processing
under both Windows and Unix-like systems.  Code using Petulant accepts
a command line like /a /b/c /out:foo \\bar\\baz under Windows, as well
as -a -bc --out=foo /bar/baz under Unix-like systems.  Also, Petulant
generally requires much less specification of its expected options
than typical getopt(3)-like libraries (and in many cases, no
specification at all is necessary).  Both functional and data
interfaces are provided to the caller."

  :components ((:file "pkg")
	       (:file "misc" :depends-on ("pkg"))
	       (:file "simple" :depends-on ("pkg" "misc"))
	       (:file "get" :depends-on ("pkg" "simple"))
	       (:file "trie" :depends-on ("pkg"))
	       (:file "parse" :depends-on ("pkg" "trie"))
	       (:file "petulant" :depends-on ("pkg" "misc" "parse"))))
