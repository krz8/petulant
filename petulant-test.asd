(defsystem "petulant-test"
  :description "petulant-test: sanity checking the petulant cli parser"
  :version "0.1.0"
  :license "MIT"
  :depends-on ("petulant" "fiveam" "anaphora" "petulant")

  :author "Bob Krzaczek"
  :mailto "RobertKrzaczek+cli@gmail.com"
  :homepage "http://github.com/krz8/petulant/"

  :components ((:file "petulant-test"))
  :perform (test-op (o s)
	     (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* '#:all :petulant-test))))
