(defpackage #:petulant-test
  (:use #:cl #:5am #:iterate #:petulant)
  (:export #:all #:misc #:trie #:styles #:parse #:process #:spec))

;; We're not using :IMPORT-FROM any longer.  We intend Petulant
;; functions to be qualified with their package name.  That's why we
;; provide CLI as a nickname for PETULANT, so that client code can
;; simply use (cli:process ...)  and so on.  So, in here, we'll
;; implement our testing either by those symbols, or by explicitly
;; invoking unexported symbols (e.g., cli:split).  Overall, the idea
;; is to leave as much of as the default packaging in place (that is
;; to say, without modification by :IMPORT-FROM), to raise the fidelity
;; of the tests to something like the actual end user's environment.

;; Hush up certain warnings while we're testing.  Right now, this just
;; affects CLI:SPEC when :NAME is not supplied, but we might add other
;; warnings in the future.  I read somewhere that FiveAM has some
;; machinery to intercept output streams and even conditions, which
;; would be so much better than this hack... but it suffices for now.

(setf cli::_shush_ t)

;; On with the show...

(in-package #:petulant-test)
(def-suite all :description "all petulant tests")

(def-suite misc :description "petulant misc utilities" :in all)
(in-suite misc)

(test stringify
  (is (equal "foo" (cli::stringify "foo")))
  (is (equal "" (cli::stringify nil)))
  (is (equal "42" (cli::stringify 42)))
  (is (equal "z" (cli::stringify #\z)))
  (is (equalp "FOO" (cli::stringify :foo))))

(test slashify
  (is (equal "/foo" (cli::slashify "foo")))
  (is (equal "" (cli::slashify "")))
  (is (equal "/FOO" (cli::slashify "/FOO"))))

(test split
  (let ((abc '(#\a #\b #\c))
	(stnr '(#\Space #\Tab #\Newline #\Return)))
    (is (equal '("") (cli::split abc "")))
    (is (equal '("hello," "world") (cli::split stnr "hello, world")))
    (is (equal '("hello," "world") (cli::split stnr "  hello, world")))
    (is (equal '("hello," "world") (cli::split stnr "  hello,  world")))
    (is (equal '("hello," "world") (cli::split stnr "  hello,  world  ")))
    (is (equal '("hello," "world") (cli::split stnr "hello,  world  ")))))

(test wc/make-setfs
      (is (equalp nil
		  (cli::wc/make-setfs 0 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0))
		  (cli::wc/make-setfs 1 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1))
		  (cli::wc/make-setfs 2 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1) c (char foo 2))
		  (cli::wc/make-setfs 3 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1) c (char foo 2))
		  (cli::wc/make-setfs 4 'foo '(a b c)))))

(test wc/make-case-clause
      (is (equalp '(0 nil)
		  (cli::wc/make-case-clause 0 'foo '(a b c))))
      (is (equalp '(1 (setf a (char foo 0)))
		  (cli::wc/make-case-clause 1 'foo '(a b c))))
      (is (equalp '(2 (setf a (char foo 0) b (char foo 1)))
		  (cli::wc/make-case-clause 2 'foo '(a b c))))
      (is (equalp '(t (setf a (char foo 0) b (char foo 1) c (char foo 2)))
		  (cli::wc/make-case-clause 3 'foo '(a b c))))
      (is (equalp '(t (setf a (char foo 0) b (char foo 1) c (char foo 2)))
		  (cli::wc/make-case-clause 4 'foo '(a b c)))))

(test with-chars
      (let ((str "abcde"))
	(cli::with-chars (a b c) str
	  (is (char= #\a a))
	  (is (char= #\b b))
	  (is (char= #\c c))))
      (let ((str "fg"))
	(cli::with-chars (f g h) str
	  (is (char= #\f f))
	  (is (char= #\g g))
	  (is (null h))))
      (let ((str ""))
	(cli::with-chars (f g h) str
	  (is (null f))
	  (is (null g))
	  (is (null h)))))

(test collecting
  (let ((hash (make-hash-table))
	(str "aCeG")
	(seq #(#\b #\D #\f #\H))
	(list '(12 23 34 45)))
    (is (equal nil
	       (sort (cli::collecting (lambda (k v) (* k v)) hash) #'<)))
    (is (equal nil
	       (cli::collecting #'char-downcase "")))
    (is (equal nil
	       (cli::collecting #'char-upcase #())))
    (is (equal nil
	       (cli::collecting (lambda (x) (* 2 x)) nil)))
    (setf (gethash 1 hash) 10
	  (gethash 2 hash) 20
	  (gethash 3 hash) 30
	  (gethash 4 hash) 40)
    (is (equal '(10 40 90 160)
	       (sort (cli::collecting (lambda (k v) (* k v)) hash) #'<)))
    (is (equal '(#\a #\c #\e #\g)
	       (cli::collecting #'char-downcase str)))
    (is (equal '(#\B #\D #\F #\H)
	       (cli::collecting #'char-upcase seq)))
    (is (equal '(24 46 68 90)
	       (cli::collecting (lambda (x) (* 2 x)) list)))))

#+nil
(test hanging-par
  (let ((eol (format nil "~%")))
    (is (equal (cli::strcat "signs: Enoch begat Uruk begat Nineveh begat Babylon begat Sodom begat" eol "Jericho begat Cairo begat Port Said begat Bangkok begat Saigon begat" eol "Vegas begat Hamburg begat New York City begat Mega City One begat" eol "Neo Tokyo." eol)
	       (with-output-to-string (s)
		 (cli::hanging-par
		  "signs: "
		  (cli::strcat "      Enoch begat Uruk begat Nineveh "
			       "begat Babylon begat Sodom begat      "
			       "Jericho begat Cairo begat Port Said  "
			       "begat Bangkok begat Saigon begat     "
			       "Vegas begat Hamburg begat New York   "
			       "City begat Mega City One begat Neo   "
			       "Tokyo")
		  :stream s))))))



(def-suite styles :in all)
(in-suite styles)

(defmacro with-stylehash ((&rest styles) &body body)
  `(let ((hash (cli::styles-to-hash ',styles)))
     (labels ((gh (x) (gethash x hash)))
       (macrolet ((has (&rest flags)
		    `(progn ,@(mapcar (lambda (f) `(is-true (gh ,f)))
				      flags)))
		  (hasnot (&rest flags)
		    `(progn ,@(mapcar (lambda (f) `(is-false (gh ,f)))
				      flags))))
	 ,@body))))

(test styles-to-hash
  (with-stylehash ()
    #+windows (has :windows :streq)
    #+windows (hasnot :unix :str= :up :down :key)
    #-windows (hasnot :windows :streq :up :down :key)
    #-windows (has :unix :str=))
  (with-stylehash (:windows)
    (has :windows :streq)
    (hasnot :unix :str= :up :down :key))
  (with-stylehash (:unix)
    (hasnot :windows :streq :up :down :key)
    (has :unix :str=))
  (with-stylehash (:windows :up)
    (has :windows :streq :up)
    (hasnot :unix :str= :down :key))
  (with-stylehash (:unix :down)
    (has :unix :streq :down)
    (hasnot :windows :str= :up :key))
  (with-stylehash (:unix :key)
    (has :unix :streq :up :key)
    (hasnot :windows :str= :down))
  (with-stylehash (:unix :key :down)	 ; valid but not sane
    (has :unix :streq :down :key)
    (hasnot :windows :str= :up))
  (with-stylehash (:windows :str= :down) ; valid but not sane
    (has :windows :str= :down)
    (hasnot :unix :streq :up :key)))

(test with-stylehash-1
  (let ((sh (cli::stylehash cli::*context*)))
    (is (= 2 (hash-table-count sh)))
    (is-true (or (gethash :unix sh)
		 (gethash :windows sh)))
    (is-false (and (gethash :unix sh)
		   (gethash :windows sh)))
    (is-true (or (gethash :str= sh)
		 (gethash :streq sh)))
    (is-false (and (gethash :str= sh)
		   (gethash :streq sh)))))

(test with-stylehash-2
  (cli::with-context-simple (nil nil nil :unix nil)
     (let ((sh (cli::stylehash cli::*context*)))
       (is-true (hash-table-p sh))
       (is-true (gethash :unix sh))
       (is-false (gethash :windows sh))
       (is-false (gethash :streq sh))
       (is-true (gethash :str= sh))
       (is-false (gethash :down sh))
       (is-false (gethash :up sh))
       (is-false (gethash :key sh)))))

(test with-stylehash-3
  (cli::with-context-simple (nil nil nil '(:unix :key :junk) nil)
     (let ((sh (cli::stylehash cli::*context*)))
       (is-true (hash-table-p sh))
       (is-true (gethash :unix sh))
       (is-false (gethash :windows sh))
       (is-true (gethash :streq sh))
       (is-false (gethash :str= sh))
       (is-false (gethash :down sh))
       (is-true (gethash :up sh))
       (is-true (gethash :key sh))
       (is-true (gethash :junk sh)))))

(test windowsp
  (cli::with-context-simple (nil nil nil :windows nil)
    (is-true (cli::stylep :windows)))
  (cli::with-context-simple (nil nil nil :unix nil)
    (is-false (cli::stylep :windows)))
  #+windows (is-true (cli::stylep :windows))
  #-windows (is-false (cli::stylep :windows)))

(test foldp
  (macrolet ((foldp () `(cli::stylep :streq)))
    #+windows (is-true (foldp))
    #-windows (is-false (foldp))
    (cli::with-context-simple (nil nil nil :unix nil)
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil :windows nil)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil :streq nil)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil :str= nil)
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil '(:streq :unix) nil)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:str= :windows) nil)
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil '(:up :unix) nil)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:unix :down) nil)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:key :unix) nil)
      (is-true (foldp)))
    ;; very weird, but still valid
    (cli::with-context-simple (nil nil nil '(:up :str=) nil)
      (is-false (foldp)))))




(def-suite parse :description "simple parser stuff" :in all)
(in-suite parse)

(test isolate-switches
  (is (equalp '("a") (cli::isolate-switches "/a")))
  (is (equalp '("ab") (cli::isolate-switches "/ab")))
  (is (equalp '("a" "bc") (cli::isolate-switches "/a/bc")))
  (is (equalp '("a" "bc:de") (cli::isolate-switches "/a/bc:de")))
  (is (equalp '("a" "bc:de" "f") (cli::isolate-switches "/a/bc:de/f")))
  (is (equalp '("a" "bc:de" "f") (cli::isolate-switches "/a/bc:de/f/")))
  (is (equalp '("a" "bc:de" "f") (cli::isolate-switches "///a//bc:de///f//")))
  (is (equalp '("") (cli::isolate-switches "")))
  (is (equalp '("/") (cli::isolate-switches "/")))
  (is (equalp '("//") (cli::isolate-switches "//")))
  (is (equalp '("///") (cli::isolate-switches "///"))))

(test canonicalize-switch-args
  (is (equalp '("abc" "" "def")
	      (cli::canonicalize-switch-args '(nil "abc" "" "def"))))
  (is (equalp '("abc" "/" "def")
	      (cli::canonicalize-switch-args '("abc" nil "/" "def"))))
  (is (equalp '("abc" "//" "def")
	      (cli::canonicalize-switch-args '("abc" "//" nil "def"))))
  (is (equalp '("abc" "///" "def")
	      (cli::canonicalize-switch-args '("abc" "///" "def" nil))))
  (is (equalp '("/abc" "def")
	      (cli::canonicalize-switch-args '("/abc" "def"))))
  (is (equalp '("/a" "/bc" "def")
	      (cli::canonicalize-switch-args '("/a/bc" "def"))))
  (is (equalp '("/a" "/bc" "def" "/ef:gh")
	      (cli::canonicalize-switch-args '("/a/bc" "def" "/ef:gh"))))
  (is (equalp '("/a" "/bc" "def" "/ef" "gh")
	      (cli::canonicalize-switch-args '("/a/bc" "def" "/ef" "gh"))))
  (is (equalp '("/a" "/bc" "def" "/e" "/f:g" "/h")
	      (cli::canonicalize-switch-args '("/a/bc" "def" "/e/f:g/h")))))

;; These are broken up because, when you reuse values in the tests,
;; it's not immediately in five-am to know which specific test fails.
;; Yes, using different test values in every test would mitigate this,
;; but come on, who has time for that?  So, we'll just do a1 a2 a3 for
;; specific related functionality and a. b. c. for more general
;; related functionality.  Pass this stuff, and you're almost home
;; free, the rest of Petulant is easy by comparison!

(test windows-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a") #'cb)
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test windows-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c") #'cb)
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test windows-a3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "/de") #'cb)
      (is (= (length res) 4))
      (is (equalp '(:opt "de" nil) (car res)))
      (is (equalp '(:opt "c" nil) (cadr res)))
      (is (equalp '(:opt "b" nil) (caddr res)))
      (is (equalp '(:opt "a" nil) (cadddr res))))))

(test windows-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "d") #'cb)
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "//" "/d") #'cb)
      (is (equalp '((:arg "/d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/alpha" "/b/c" "/beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "/beta" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-c4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "/beta" "//" "/foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "/foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-d1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "/beta:fuzz" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" "fuzz")
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-d2					  ; gnu-ish
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-switches '("/a" "/b/c" "foo" "/beta:fuzz" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:opt "beta" "fuzz")
		    (:arg "foo" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-e1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-switches '("/x/v/f" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-switches '("/x/v/f:foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-switches '("/x/v" "/file:foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-switches '("/x/v" "/file" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-f1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-switches '("/x/v" "/f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-f2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-switches '("/x/v/f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-f3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-switches '("/x/v" "/file:") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-f4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-switches '("/x/v" "/file") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a") #'cb)
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test unix-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "-bc") #'cb)
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test unix-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "-bc" "d") #'cb)
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "-bc" "--" "-d") #'cb)
      (is (equalp '((:arg "-d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "--alpha" "-bc" "--beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "--a" "-bc" "--beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "--a" "-bc" "--beta" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "--a" "-bc" "--beta" "--" "--foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "--foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-d1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "-bc" "--beta=fuzz" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" "fuzz")
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-d2					  ; gnu-ish
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::do-posix '("-a" "-bc" "foo" "--beta=fuzz" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:opt "beta" "fuzz")
		    (:arg "foo" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-e1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "F")))
      (cli::do-posix '("-XVF" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "F" "foo")
		    (:opt "V" nil)
		    (:opt "X" nil))
		  res)))))

(test unix-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-posix '("-xvffoo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-posix '("-xv" "--file=foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-posix '("-xv" "--file" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-f1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-posix '("-xv" "-f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-f2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::do-posix '("-xvf") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-f3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-posix '("-xv" "--file=") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-f4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::do-posix '("-xv" "--file") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-1
  (let ((res (with-output-to-string (s)
	       (cli:parse #'(lambda (x y z)
				     (format s "|~s ~s ~s" x y z))
				 :arglist '("/a" "/beta" "/input:file"
					    "some" "/v" "thing")
				 :style :windows))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test parse-2
  (let ((res (with-output-to-string (s)
	       (cli:parse #'(lambda (x y z)
				     (format s "|~s ~s ~s" x y z))
				 :arglist '("-a" "--beta" "--input=file"
					    "some" "-v" "thing")
				 :style :unix))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test parse-3
  (let ((*verbose* nil))
    (labels ((opts-and-args (kind name value)
	       (declare (ignore value))
	       (when (and (eq kind :opt) (string-equal name "v"))
		 (setf *verbose* t))))
      (cli:parse #'opts-and-args
			:arglist '("-v") :style :unix)
      (is (not (null *verbose*))))))

(test parse-4
  (let ((*verbose* 0))
    (labels ((opts-and-args (kind name value)
	       (declare (ignore value))
	       (case kind
		 (:arg t)
		 (:opt (cond
			 ((string-equal name "q")
			  (decf *verbose*))
			 ((string-equal name "v")
			  (incf *verbose*)))))))
      (cli:parse #'opts-and-args
			:arglist '("/v" "/q/v" "/v/q/v" "/q/q/q" "/v/v")
			:style :windows)
      (is (= 1 *verbose*)))))

(test parse-5
  (let* ((*apphome* (make-pathname :directory '(:absolute "opt" "app")))
	 (*verbose* 0)
	 (*inpath* nil)
	 (*outpath* nil)
	 (*confpath* (merge-pathnames "config.yaml" *apphome*)))
    (labels ((opts-and-args (kind x y)
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
			  (incf *verbose*)))))))
      (cli:parse #'opts-and-args
			:arglist '("-c" "/foo/altconf.yml"
				   "-v" "one.dat" "-q" "two.dat" "-v")
			:argoptp-fn #'(lambda (x) (string-equal "c" x))
			:style :unix)
      (is (= 1 *verbose*))
      (is (equalp (pathname "one.dat") *inpath*))
      (is (equalp (pathname "two.dat") *outpath*))
      (is (equalp (pathname "/foo/altconf.yml") *confpath*)))))



(def-suite trie :description "trie support" :in all)
(in-suite trie)

(test make-trie
  (let ((x0 (cli::make-trie))
	(x1 (cli::make-trie :loose t)))
    (is-true (typep x0 'cli::trie))
    (is-true (typep x1 'cli::trie))
    (is-true (hash-table-p (cli::trie-table x0)))
    (is (equal 'equal (hash-table-test (cli::trie-table x0))))
    (is-true (hash-table-p (cli::trie-table x1)))
    (is (equal 'equalp (hash-table-test (cli::trie-table x1))))))

(test make-similar-trie
  (let ((x0 (cli::make-similar-trie (cli::make-trie)))
	(x1 (cli::make-similar-trie (cli::make-trie :loose t))))
    (is-true (hash-table-p (cli::trie-table x0)))
    (is (equal 'equal (hash-table-test (cli::trie-table x0))))
    (is-true (hash-table-p (cli::trie-table x1)))
    (is (equal 'equalp (hash-table-test (cli::trie-table x1))))))

;; The first time trie-at is called for a given position, it
;; should create it anew.  All subsequent times, it should return
;; what was created (kind of a caching or memoization game).

(test trie-at
  (let ((root (cli::make-trie)))
    (is-true (zerop (hash-table-count (cli::trie-table root))))
    (let ((sub1 (cli::trie-at root 1)))
      (is-true (= 1 (hash-table-count (cli::trie-table root))))
      (is (eq 'cli::trie (type-of sub1)))
      (is (eq sub1 (cli::trie-at root 1)))
      (is-true (= 1 (hash-table-count (cli::trie-table root))))
      (let ((sub2 (cli::trie-at root 2)))
	(is-true (= 2 (hash-table-count (cli::trie-table root))))
	(is (eq 'cli::trie (type-of sub2)))
	(is (eq sub2 (cli::trie-at root 2)))
	(is-true (= 2 (hash-table-count (cli::trie-table root))))
	(is (not (eq sub1 (cli::trie-at root 2))))
	(is-true (= 2 (hash-table-count (cli::trie-table root))))
	(is (not (eq sub2 (cli::trie-at root 1))))
	(is-true (= 2 (hash-table-count (cli::trie-table root))))))))

(test make-dict
  (let ((x0 (cli::make-dict))
	(x1 (cli::make-dict :loose t)))
    (is-true (typep x0 'cli::trie))
    (is-true (typep x1 'cli::trie))
    (is-true (typep x0 'cli::dict))
    (is-true (typep x1 'cli::dict))
    (is-true (hash-table-p (cli::trie-table x0)))
    (is (equal 'equal (hash-table-test (cli::trie-table x0))))
    (is-true (hash-table-p (cli::trie-table x1)))
    (is (equal 'equalp (hash-table-test (cli::trie-table x1))))
    (is-true (zerop (cli::dict-count x0)))
    (is-true (zerop (cli::dict-count x1)))
    (is-false (cli::dict-term-p x0))
    (is-false (cli::dict-term-p x1))))

(test dict-add
  (let ((d0 (cli::make-dict))
	(d1 (cli::make-dict :loose t)))
    (is-false (cli::dict-word-p d0 "foo"))
    (is-false (cli::dict-word-p d1 "foo"))
    (cli::dict-add d0 "foo")
    (cli::dict-add d1 "foo")
    (is-true (cli::dict-word-p d0 "foo"))
    (is-true (cli::dict-word-p d1 "foo"))
    (is-false (cli::dict-word-p d0 "Foo"))
    (is-true (cli::dict-word-p d1 "Foo"))
    (is-false (cli::dict-word-p d0 "beta"))
    (is-false (cli::dict-word-p d0 "bets"))
    (is-false (cli::dict-word-p d0 "bet"))
    (cli::dict-add d0 "bet")
    (is-false (cli::dict-word-p d0 "beta"))
    (is-false (cli::dict-word-p d0 "bets"))
    (is-true (cli::dict-word-p d0 "bet"))
    (cli::dict-add d0 "bet")	        ; do it again, nothing should change
    (is-false (cli::dict-word-p d0 "beta"))
    (is-false (cli::dict-word-p d0 "bets"))
    (is-true (cli::dict-word-p d0 "bet"))
    (cli::dict-add d0 "bets")
    (is-false (cli::dict-word-p d0 "beta"))
    (is-true (cli::dict-word-p d0 "bets"))
    (is-true (cli::dict-word-p d0 "bet"))
    (cli::dict-add d0 "beta")
    (is-true (cli::dict-word-p d0 "beta"))
    (is-true (cli::dict-word-p d0 "bets"))
    (is-true (cli::dict-word-p d0 "bet"))
    (cli::dict-add d0 "beta")		; do it again, nothing should change
    (is-true (cli::dict-word-p d0 "beta"))
    (is-true (cli::dict-word-p d0 "bets"))
    (is-true (cli::dict-word-p d0 "bet"))
    (is-false (cli::dict-word-p d0 "a"))
    (is-false (cli::dict-word-p d0 "at"))
    (is-false (cli::dict-word-p d0 "atom"))
    (cli::dict-add d0 "atom")
    (is-false (cli::dict-word-p d0 "a"))
    (is-false (cli::dict-word-p d0 "at"))
    (is-true (cli::dict-word-p d0 "atom"))
    (cli::dict-add d0 "at")
    (is-false (cli::dict-word-p d0 "a"))
    (is-true (cli::dict-word-p d0 "at"))
    (is-true (cli::dict-word-p d0 "atom"))))

(test minwords
  (let ((dict (cli::make-dict)))
    (mapc (lambda (w) (cli::dict-add dict w))
	  '("zebra" "atom" "beta" "bets" "ignore" "at" "beat" "input"
	    "fee" "fi" "fo" "fum" "alpha"))
    (is (equal '((2 5 "alpha") (3 4 "atom") (3 4 "beat")
		 (2 3 "fee") (2 3 "fum")
		 (2 6 "ignore") (2 5 "input") (1 5 "zebra"))
	       (sort (cli::minwords dict) #'string-lessp :key #'caddr)))))



(def-suite process :description "process support" :in all)
(in-suite process)

(defmacro with-partials ((argopts flagopts aliases styles) &body body)
  `(cli::with-context-simple (,argopts ,flagopts ,aliases ,styles nil)
     (let ((fn (cli::partials-fn)))
       (labels ((match? (x y) (funcall (cli::str=-fn) x (funcall fn y))))
	 ,@body))))

(test partials-fn-1
  (with-partials ('("file" "config" "find")
		   '("verbose" "input" "ignore")
		   '(("verbose" "debug"))
		   :windows)
    (is-false (match? "file" "f"))
    (is-false (match? "file" "fi"))
    (is-false (match? "file" "fil"))
    (is-true (match? "file" "file"))
    (is-false (match? "find" "fin"))
    (is-true (match? "find" "find"))
    (is-false (match? "config" "c"))
    (is-false (match? "config" "conf"))
    (is-true (match? "config" "config"))
    (is-false (match? "verbose" "v"))
    (is-false (match? "verbose" "verb"))
    (is-true (match? "verbose" "verbose"))
    (is-false (match? "verbose" "d"))
    (is-false (match? "verbose" "deb"))
    (is-false (match? "verbose" "debug"))
    (is-false (match? "input" "i"))
    (is-false (match? "input" "in"))
    (is-true (match? "input" "input"))
    (is-false (match? "ignore" "i"))
    (is-false (match? "ignore" "ign"))
    (is-true (match? "ignore" "ignore"))))

(test partials-fn-2
  (with-partials ('("file" "config" "find")
		   '("verbose" "input" "ignore")
		   '(("verbose" "debug"))
		   '(:windows :partial))
    (is-false (match? "file" "f"))
    (is-false (match? "file" "fi"))
    (is-true (match? "file" "fil"))
    (is-true (match? "file" "file"))
    (is-true (match? "find" "fin"))
    (is-true (match? "find" "find"))
    (is-true (match? "config" "c"))
    (is-true (match? "config" "conf"))
    (is-true (match? "config" "config"))
    (is-true (match? "verbose" "v"))
    (is-true (match? "verbose" "verb"))
    (is-true (match? "verbose" "verbose"))
    (is-true (match? "debug" "d"))
    (is-true (match? "debug" "deb"))
    (is-true (match? "debug" "debug"))
    (is-false (match? "input" "i"))
    (is-true (match? "input" "in"))
    (is-true (match? "input" "input"))
    (is-false (match? "ignore" "i"))
    (is-true (match? "ignore" "ign"))
    (is-true (match? "ignore" "ignore"))))

;; We'll actually do the testing via COLLECT rather than PROCESS, as the
;; results from the former are much easier to test.  The COLLECT
;; wrapper is so simple, it can't really break.

(test collect
  (labels ((wrap (styles &rest args)
	     (cli:collect :argopts '("file" "config" "find")
			  :flagopts '("verbose" "input" "ignore")
			  :aliases '(("verbose" "debug"))
			  :styles styles
			  :arglist args)))
    (is (equalp nil (wrap nil nil)))
    (is (equalp nil (wrap :windows nil)))
    (is (equalp nil (wrap '(:windows :partial) nil)))
    (is (equalp '((:opt "verbose" nil)
    		  (:arg "foobar" nil))
    		(wrap :windows "/VERBOSE" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:arg "foobar" nil))
    	       (wrap :unix "--verbose" "foobar")))
    (is (equalp '((:opt "verbose" nil)
    		  (:arg "foobar" nil))
    		(wrap :windows "/Debug" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "--verb" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "-v" "foobar")))
    (is (equalp '((:opt "verbose" nil)
    		  (:opt "config" "cfile")
    		  (:arg "foobar" nil))
    		(wrap '(:windows) "/Debug" "/Config:cfile" "foobar")))
    (is (equalp '((:opt "verbose" nil)
    		  (:opt "config" "cfile")
    		  (:arg "foobar" nil))
    		(wrap '(:windows) "/Debug" "/Config" "cfile" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "--debug" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "-d" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:opt "config" "cfile")
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "-vc" "cfile" "foobar")))
    (is (equal '((:opt "verbose" nil)
    		 (:opt "config" "cfile")
    		 (:arg "foobar" nil))
    	       (wrap '(:unix :partial) "-dc" "cfile" "foobar")))
    (is (equal '((:opt "ignore" nil)
    		 (:arg "alpha" nil)
    		 (:opt "config" "foo.conf")
    		 (:arg "beta" nil)
    		 (:opt "file" "other.dat"))
    	       (wrap '(:unix :partial) "--ig" "alpha" "-cfoo.conf" "beta"
    		     "--file=other.dat")))
    (is (equal '((:opt "ignore" nil)
    		 (:arg "alpha" nil)
    		 (:opt "config" "foo.conf")
    		 (:arg "beta" nil)
    		 (:opt "file" "other.dat"))
    	       (wrap '(:unix :partial) "--ig" "alpha" "-c" "foo.conf" "beta"
    		     "--file" "other.dat")))
    ;; keep writing a few more here every day
    ))



(def-suite spec :description "big kahuna" :in all)
(in-suite spec)

(test spec-macro
  (is (equalp '(cli::spec*		; form
		"nemo"			; name
		nil			; summary
		nil			; tail
		nil			; options
		nil			; aliases
		nil			; styles
		nil) 			; arguments
	      (caddr (macroexpand '(cli:spec)))))
  (is (equalp '(cli::spec* "foo"
		(lambda () (format nil "bar ~d" 42))
		(lambda () (format nil "baz ~d" (get-universal-time)))
		nil nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:summary "bar ~d" 42)
			      (:tail "baz ~d" (get-universal-time))
			      (:name "foo"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list (list "verbose" '(:flag) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:name "foo")
			      (:flagopt "verbose"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list (list "verbose" '(:flag)
		       (lambda () (format nil "bar baz buz"))))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:name "foo")
			      (:flagopt "verbose" "bar baz buz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list (list "verbose" '(:flag)
		       (lambda () (format nil "bar ~a buz" "baz"))))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "file")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "file" :string)
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string 50) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "file" (:string 50))
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string 50) (lambda ()
					      (format nil "bebop"))))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "file" (:string 50) "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					      (format nil "bebop"))))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real * *) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" :real)
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real * *) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real))
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 *) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0))
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real * 100) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real * 100))
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		nil nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		'(("alpha" "transparency" "fade")) nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:alias "alpha" "transparency" "fade")
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		'(("verbose" "debug")
		  ("alpha" "transparency" "fade"))
		nil nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:alias "alpha" "transparency" "fade")
			      (:aliases "verbose" "debug")
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		'(("verbose" "debug")
		  ("alpha" "transparency" "fade"))
		'(:windows) nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:alias "alpha" "transparency" "fade")
			      (:style :windows)
			      (:aliases "verbose" "debug")
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		'(("verbose" "debug")
		  ("alpha" "transparency" "fade"))
		'(:key :windows) nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:alias "alpha" "transparency" "fade")
			      (:style :windows)
			      (:style :key)
			      (:aliases "verbose" "debug")
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 100) nil))
		'(("verbose" "debug")
		  ("alpha" "transparency" "fade"))
		'(:partial :key :windows) nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 100))
			      (:alias "alpha" "transparency" "fade")
			      (:style :windows)
			      (:aliases "verbose" "debug")
			      (:argopt "file" "bebop")
			      (:style :key)
			      (:name "foo")
			      (:style :partial)
			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
  (is (equalp '(cli::spec* "foo" nil nil
		(list
		 (list "verbose" '(:flag) (lambda ()
					    (format nil "bar ~a buz" "baz")))
		 (list "file" '(:string *) (lambda ()
					     (format nil "bebop")))
		 (list "alpha" '(:real 0 99) nil))
		'(("verbose" "debug")
		  ("alpha" "transparency" "fade"))
		'(:partial :key :windows) nil)
	      (caddr (macroexpand '(cli:spec
			      (:argopt "alpha" (:real 0 99))
			      (:alias "alpha" "transparency" "fade")
			      (:style :windows)
			      (:aliases "verbose" "debug")
			      (:argopt "file" "bebop")
			      (:name "foo")
			      (:style :key :partial)
			      (:flagopt "verbose" "bar ~a buz" "baz")))))))

;;; Okay, problem is that when :KEY is in effect, opthash has a string
;;; in it, but the value passed to decode is a keyword.
;;;
;;; Need to build up opthash differently?  Can't do it in SPEC since we don't
;;; know when the :STYLE spec arrives, so it's got to be in SPEC*.

(defmacro bigspec-1 (&rest args)
  `(cli:spec (:name "notpax")
	     (:summary "This is a fake summary of a fake application. ~
                           So much fake.  Wow.  Not sure how I'll test this ~
                           except by eyeballing it.")
	     (:tail "Forty-two is ~r." 42)
	     (:style :key :windows :partial)
	     (:argopt "alpha" (:real 0 100) "Blah-de-blah blah.")
	     (:alias "alpha" "transparency" "fade")
	     (:flagopt "verbose")
	     (:flagopt "dryrun")
	     (:alias "dryrun" "n")
	     (:argopt "volume" (:integer 0 11) "This one goes to eleven.")
	     (:args ,@args)))

(test spec-1
  (is (eq t (bigspec-1 "one" "two" "three")))
  (is (equal '("one" "two" "three") cli:*arguments*))
  (is (zerop (hash-table-count cli:*options*))))

(test spec-2
  (is (eq t (bigspec-1 "one" "/Verbose" "two" "/N" "three")))
  (is (equal '("one" "two" "three") cli:*arguments*))
  (is (= 2 (hash-table-count cli:*options*)))
  (is-true (gethash :verbose cli:*options*))
  (is-true (gethash :dryrun cli:*options*)))

(test spec-3
  (is (eq t (bigspec-1 "one" "/volume" "10" "two" "/n" "three")))
  (is (equal '("one" "two" "three") cli:*arguments*))
  (is (= 2 (hash-table-count cli:*options*)))
  (is (= 10 (gethash :volume cli:*options*)))
  (is-true (gethash :dryrun cli:*options*)))
