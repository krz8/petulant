(defpackage #:petulant-test
  (:use #:cl #:5am #:iterate #:petulant)
  (:export #:all #:trie #:misc #:context #:scan #:simple #:spec))

;; We're not using :IMPORT-FROM any longer.  We intend Petulant
;; functions to be qualified with their package name.  That's why we
;; provide CLI as a nickname for PETULANT, so that client code can
;; simply use (cli:simple ...)  and so on.  So, in here, we'll
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



(def-suite trie :description "trie support" :in all)
(in-suite trie)

(test make-trie
  (let ((x0 (cli::make-trie))
	(x1 (cli::make-trie :test #'equalp)))
    (is-true (typep x0 'cli::trie))
    (is-true (typep x1 'cli::trie))
    (is-true (hash-table-p (cli::trie-table x0)))
    (is (equal 'equal (hash-table-test (cli::trie-table x0))))
    (is-true (hash-table-p (cli::trie-table x1)))
    (is (equal 'equalp (hash-table-test (cli::trie-table x1))))))

(test make-similar-trie
  (let ((x0 (cli::make-similar-trie (cli::make-trie)))
	(x1 (cli::make-similar-trie (cli::make-trie :test #'equalp))))
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
	(x1 (cli::make-dict :test #'equalp)))
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
	(d1 (cli::make-dict :test #'equalp)))
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



(def-suite context :in all)
(in-suite context)

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
  (cli::with-context-simple (nil nil nil :unix)
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
  (cli::with-context-simple (nil nil nil '(:unix :key :junk))
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
  (cli::with-context-simple (nil nil nil :windows)
    (is-true (cli::stylep :windows)))
  (cli::with-context-simple (nil nil nil :unix)
    (is-false (cli::stylep :windows)))
  #+windows (is-true (cli::stylep :windows))
  #-windows (is-false (cli::stylep :windows)))

(test foldp
  (macrolet ((foldp () `(cli::stylep :streq)))
    #+windows (is-true (foldp))
    #-windows (is-false (foldp))
    (cli::with-context-simple (nil nil nil :unix)
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil :windows)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil :streq)
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil :str=)
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil '(:streq :unix))
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:str= :windows))
      (is-false (foldp)))
    (cli::with-context-simple (nil nil nil '(:up :unix))
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:unix :down))
      (is-true (foldp)))
    (cli::with-context-simple (nil nil nil '(:key :unix))
      (is-true (foldp)))
    ;; very weird, but still valid
    (cli::with-context-simple (nil nil nil '(:up :str=))
      (is-false (foldp)))))



(def-suite scan :description "simple scanner stuff" :in all)
(in-suite scan)

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
;; it's not immediately apparent in Five-AM to know which specific
;; test fails.  Yes, using different test values in every test would
;; mitigate this, but come on, who has time for that?  So, we'll just
;; do a1 a2 a3 for specific related functionality and a. b. c. for
;; more general related functionality.  This is the tough stuff: pass
;; this stuff, and you're almost home free, the rest of Petulant is
;; easy by comparison!

(test windows-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a"))
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test windows-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/b/c"))
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test windows-a3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/b/c" "/de"))
      (is (= (length res) 4))
      (is (equalp '(:opt "de" nil) (car res)))
      (is (equalp '(:opt "c" nil) (cadr res)))
      (is (equalp '(:opt "b" nil) (caddr res)))
      (is (equalp '(:opt "a" nil) (cadddr res))))))

(test windows-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/b/c" "d"))
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/b/c" "//" "/d"))
      (is (equalp '((:arg "/d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/alpha" "/b/c" "/beta"))
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test windows-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-windows #'cb '("/a" "/b/c" "/beta" "foo" "bar"))
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
      (cli::scan-windows #'cb '("/a" "/b/c" "/beta" "//" "/foo" "bar"))
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
      (cli::scan-windows #'cb '("/a" "/b/c" "/beta:fuzz" "foo" "bar"))
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
      (cli::scan-windows #'cb '("/a" "/b/c" "foo" "/beta:fuzz" "bar"))
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
      (cli::scan-windows #'cb '("/x/v/f" "foo" "something")
			  :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::scan-windows #'cb '("/x/v/f:foo" "something")
			  :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::scan-windows #'cb '("/x/v" "/file:foo" "something")
			  :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test windows-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::scan-windows #'cb '("/x/v" "/file" "foo" "something")
			  :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

;;; This comment also appears at unix-f1; if you change something
;;; here or in windows-f1 through windows-f5, you probably ought to update
;;; unix-f1 through unix-f9 as well.
;;;
;;; Generally, "missing" arguments to options are indicated with
;;; either empty strings or NIL, depending on their context.  An empty
;;; string should occur not just with an explicit empty string on the
;;; command-line, but also when the argument appears in the option's
;;; presentation. /foo: and --foo= are the Windows and POSIX ways to
;;; see that happen.  A NIL indicates a *missing* argument, which is
;;; typically when an option is seen, the argument is determined to
;;; appear in the next command-line string, but the command-line ends.
;;;
;;; C:\> app /file:foo.dat               "foo.dat"
;;; C:\> app /file foo.dat               "foo.dat"
;;; C:\> app /file ""                    ""         (does this work in Windows?)
;;; C:\> app /file                       NIL
;;; C:\> app /file:                      ""
;;;
;;; $ app --file=foo.dat                 "foo.dat"
;;; $ app --file foo.dat                 "foo.dat"
;;; $ app --file ''                      ""
;;; $ app --file                         NIL
;;; $ app --file=                        ""
;;; $ app -ffoo.dat                      "foo.dat"
;;; $ app -f foo.dat                     "foo.dat"
;;; $ app -f ''                          ""
;;; $ app -f                             NIL

(test windows-f1
  (let (res)
    (cli::scan-windows
     (lambda (kind key value) (push (list kind key value) res))
     '("/foo" "/file:foo.dat")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "foo.dat")
		  (:opt "foo" nil))
		res))))

(test windows-f2
  (let (res)
    (cli::scan-windows
     (lambda (kind key value) (push (list kind key value) res))
     '("/foo" "/file" "foo.dat")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "foo.dat")
		  (:opt "foo" nil))
		res))))

(test windows-f3
  (let (res)
    (cli::scan-windows
     (lambda (kind key value) (push (list kind key value) res))
     '("/foo" "/file" "")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "")
		  (:opt "foo" nil))
		res))))

(test windows-f4
  (let (res)
    (cli::scan-windows
     (lambda (kind key value) (push (list kind key value) res))
     '("/foo" "/file")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" nil)
		  (:opt "foo" nil))
		res))))

(test windows-f5
  (let (res)
    (cli::scan-windows
     (lambda (kind key value) (push (list kind key value) res))
     '("/foo" "/file:")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "")
		  (:opt "foo" nil))
		res))))

(test unix-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a"))
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test unix-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "-bc"))
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test unix-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "-bc" "d"))
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "-bc" "--" "-d"))
      (is (equalp '((:arg "-d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "--alpha" "-bc" "--beta"))
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "--a" "-bc" "--beta"))
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test unix-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (cli::scan-unix #'cb '("-a" "--a" "-bc" "--beta" "foo" "bar"))
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
      (cli::scan-unix #'cb '("-a" "--a" "-bc" "--beta" "--" "--foo" "bar"))
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
      (cli::scan-unix #'cb '("-a" "-bc" "--beta=fuzz" "foo" "bar"))
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
      (cli::scan-unix #'cb '("-a" "-bc" "foo" "--beta=fuzz" "bar"))
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
      (cli::scan-unix #'cb '("-XVF" "foo" "something")
		       :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "F" "foo")
		    (:opt "V" nil)
		    (:opt "X" nil))
		  res)))))

(test unix-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (cli::scan-unix #'cb '("-xvffoo" "something")
		       :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::scan-unix #'cb '("-xv" "--file=foo" "something")
		       :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test unix-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (cli::scan-unix #'cb '("-xv" "--file" "foo" "something")
		       :optargp #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

;;; This comment also appears at windows-f1; if you change something
;;; here or in unix-f1 through unix-f9, you probably ought to update
;;; windows-f1 through windows-f5 as well.
;;;
;;; Generally, "missing" arguments to options are indicated with
;;; either empty strings or NIL, depending on their context.  An empty
;;; string should occur not just with an explicit empty string on the
;;; command-line, but also when the argument appears in the option's
;;; presentation. /foo: and --foo= are the Windows and POSIX ways to
;;; see that happen.  A NIL indicates a *missing* argument, which is
;;; typically when an option is seen, the argument is determined to
;;; appear in the next command-line string, but the command-line ends.
;;;
;;; C:\> app /file:foo.dat               "foo.dat"
;;; C:\> app /file foo.dat               "foo.dat"
;;; C:\> app /file ""                    ""         (does this work in Windows?)
;;; C:\> app /file                       NIL
;;; C:\> app /file:                      ""
;;;
;;; $ app --file=foo.dat                 "foo.dat"
;;; $ app --file foo.dat                 "foo.dat"
;;; $ app --file ''                      ""
;;; $ app --file                         NIL
;;; $ app --file=                        ""
;;; $ app -ffoo.dat                      "foo.dat"
;;; $ app -f foo.dat                     "foo.dat"
;;; $ app -f ''                          ""
;;; $ app -f                             NIL

(test unix-f1
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("--foo" "--file=foo.dat")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "foo.dat")
		  (:opt "foo" nil))
		res))))

(test unix-f2
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("--foo" "--file" "foo.dat")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "foo.dat")
		  (:opt "foo" nil))
		res))))

(test unix-f3
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("--foo" "--file" "")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "")
		  (:opt "foo" nil))
		res))))

(test unix-f4
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("--foo" "--file")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" nil)
		  (:opt "foo" nil))
		res))))

(test unix-f5
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("--foo" "--file=")
     :optargp (lambda (x) (string= x "file")))
    (is (equalp '((:opt "file" "")
		  (:opt "foo" nil))
		res))))

(test unix-f6
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("-vffoo.dat")
     :optargp (lambda (x) (string= x "f")))
    (is (equalp '((:opt "f" "foo.dat")
		  (:opt "v" nil))
		res))))

(test unix-f7
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("-vf" "foo.dat")
     :optargp (lambda (x) (string= x "f")))
    (is (equalp '((:opt "f" "foo.dat")
		  (:opt "v" nil))
		res))))

(test unix-f8
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("-vf" "")
     :optargp (lambda (x) (string= x "f")))
    (is (equalp '((:opt "f" "")
		  (:opt "v" nil))
		res))))

(test unix-f9
  (let (res)
    (cli::scan-unix
     (lambda (kind key value) (push (list kind key value) res))
     '("-vf")
     :optargp (lambda (x) (string= x "f")))
    (is (equalp '((:opt "f" nil)
		  (:opt "v" nil))
		res))))

(test scan-1
  (let ((res (with-output-to-string (s)
	       (cli:scan (lambda (x y z)
			    (format s "|~s ~s ~s" x y z))
			  :argv '("/a" "/beta" "/input:file"
				  "some" "/v" "thing")
			  :style :windows))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test scan-2
  (let ((res (with-output-to-string (s)
	       (cli:scan (lambda (x y z)
			    (format s "|~s ~s ~s" x y z))
			  :argv '("-a" "--beta" "--input=file"
				  "some" "-v" "thing")
			  :style :unix))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test scan-3
  (let ((*verbose* nil))
    (labels ((opts-and-args (kind name value)
	       (declare (ignore value))
	       (when (and (eq kind :opt) (string-equal name "v"))
		 (setf *verbose* t))))
      (cli:scan #'opts-and-args :argv '("-v") :style :unix)
      (is (not (null *verbose*))))))

(test scan-4
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
      (cli:scan #'opts-and-args
		 :argv '("/v" "/q/v" "/v/q/v" "/q/q/q" "/v/v")
		 :style :windows)
      (is (= 1 *verbose*)))))

(test scan-5
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
      (cli:scan #'opts-and-args
		 :argv '("-c" "/foo/altconf.yml"
			 "-v" "one.dat" "-q" "two.dat" "-v")
		 :optargp (lambda (x) (string-equal "c" x))
		 :style :unix)
      (is (= 1 *verbose*))
      (is (equalp (pathname "one.dat") *inpath*))
      (is (equalp (pathname "two.dat") *outpath*))
      (is (equalp (pathname "/foo/altconf.yml") *confpath*)))))



(def-suite simple :description "simple support" :in all)
(in-suite simple)

(defmacro simple-not-really-tar-at-all (fn cmdline)
  `(cli:simple ,fn
		:argopts '("file")
		:flagopts '("verbose" "extract" "create" "update"
			    "list")
		:aliases '(("verbose" "v")
			   ("extract" "x")
			   ("create" "c")
			   ("update" "u")
			   ("list" "t" "toc")
			   ("file" "f"))
		:styles :unix
		:argv ,cmdline))

(test simple-1
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("--extract" "--verbose" "--file=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-2
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-x" "--verbose" "--file=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-3
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-x" "-v" "--file=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-4
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xv" "--file=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-5
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xv" "--file" "foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-6
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xv" "-f" "foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-7
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xvf" "foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-8
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xvffoo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-9
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xtvffoo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-10
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xvffoo.tar" "one" "--toc" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:opt "list" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-11
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xvffoo.tar" "one" "--list" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:opt "list" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-12
  (let (res)
    (simple-not-really-tar-at-all
     (lambda (&rest args) (push args res))
     '("-xvffoo.tar" "one" "-t" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:opt "list" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(defmacro simple-not-really-tar-at-all-2 (fn cmdline)
  `(cli:simple ,fn
		:argopts '("file")
		:flagopts '("verbose" "extract" "create" "update"
			    "list")
		:aliases '(("verbose" "v")
			   ("extract" "x")
			   ("create" "c")
			   ("update" "u")
			   ("list" "t" "toc")
			   ("file" "f"))
		:styles '(:unix :partial)
		:argv ,cmdline))

(test simple-13
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--extract" "--verbose" "--file=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-14
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--ext" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "extract" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-15
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--list" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-16
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--l" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-17
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--t" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-18
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--to" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(test simple-19
  (let (res)
    (simple-not-really-tar-at-all-2
     (lambda (&rest args) (push args res))
     '("--toc" "--verb" "--f=foo.tar" "one" "two"))
    (is (equal '((:opt "list" nil)
		 (:opt "verbose" nil)
		 (:opt "file" "foo.tar")
		 (:arg "one" nil)
		 (:arg "two" nil))
	       (nreverse res)))))

(defmacro poor-partials (fn cmdline)
  `(cli:simple ,fn
		:flagopts '("beta" "beat" "buck")
		:styles '(:unix :partial)
		:argv ,cmdline))

(test simple-20
  (let (res)
    (poor-partials (lambda (&rest x) (push x res))
		   '("--beta" "--beat" "--buck"))
    (is (equal '((:opt "beta" nil)
		 (:opt "beat" nil)
		 (:opt "buck" nil))
	       (nreverse res)))))

(test simple-21
  (let (res)
    (poor-partials (lambda (&rest x) (push x res))
		   '("--beta" "--bet" "--beat" "--buck"))
    (is (equal '((:opt "beta" nil)
		 (:opt "beta" nil)
		 (:opt "beat" nil)
		 (:opt "buck" nil))
	       (nreverse res)))))

(test simple-22
  (let (res)
    (poor-partials (lambda (&rest x) (push x res))
		   '("--beta" "--be" "--beat" "--buck"))
    (is (equal '((:opt "beta" nil)
		 (:opt "be" nil)
		 (:opt "beat" nil)
		 (:opt "buck" nil))
	       (nreverse res)))))

(test simple-23
  (let (res)
    (poor-partials (lambda (&rest x) (push x res))
		   '("--beta" "--bu" "--beat" "--buck"))
    (is (equal '((:opt "beta" nil)
		 (:opt "buck" nil)
		 (:opt "beat" nil)
		 (:opt "buck" nil))
	       (nreverse res)))))

(test simple-24
  (let (res)
    (poor-partials (lambda (&rest x) (push x res))
		   '("--beta" "--b" "--beat" "--buck"))
    (is (equal '((:opt "beta" nil)
		 (:opt "b" nil)
		 (:opt "beat" nil)
		 (:opt "buck" nil))
	       (nreverse res)))))



;; (def-suite spec :description "big kahuna" :in all)
;; (in-suite spec)

;; (test spec-macro
;;   (is (equalp '(cli::spec*		; form
;; 		"nemo"			; name
;; 		nil			; summary
;; 		nil			; tail
;; 		nil			; options
;; 		nil			; aliases
;; 		nil			; styles
;; 		nil) 			; arguments
;; 	      (caddr (macroexpand '(cli:spec)))))
;;   (is (equalp '(cli::spec* "foo"
;; 		(lambda () (format nil "bar ~d" 42))
;; 		(lambda () (format nil "baz ~d" (get-universal-time)))
;; 		nil nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:summary "bar ~d" 42)
;; 			      (:tail "baz ~d" (get-universal-time))
;; 			      (:name "foo"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list (list "verbose" '(:flag) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:name "foo")
;; 			      (:flagopt "verbose"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list (list "verbose" '(:flag)
;; 		       (lambda () (format nil "bar baz buz"))))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar baz buz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list (list "verbose" '(:flag)
;; 		       (lambda () (format nil "bar ~a buz" "baz"))))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "file")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "file" :string)
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string 50) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "file" (:string 50))
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string 50) (lambda ()
;; 					      (format nil "bebop"))))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "file" (:string 50) "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					      (format nil "bebop"))))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real * *) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" :real)
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real * *) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real))
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 *) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0))
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real * 100) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real * 100))
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		nil nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		'(("alpha" "transparency" "fade")) nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		'(("verbose" "debug")
;; 		  ("alpha" "transparency" "fade"))
;; 		nil nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:aliases "verbose" "debug")
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		'(("verbose" "debug")
;; 		  ("alpha" "transparency" "fade"))
;; 		'(:windows) nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:style :windows)
;; 			      (:aliases "verbose" "debug")
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		'(("verbose" "debug")
;; 		  ("alpha" "transparency" "fade"))
;; 		'(:key :windows) nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:style :windows)
;; 			      (:style :key)
;; 			      (:aliases "verbose" "debug")
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 100) nil))
;; 		'(("verbose" "debug")
;; 		  ("alpha" "transparency" "fade"))
;; 		'(:partial :key :windows) nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 100))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:style :windows)
;; 			      (:aliases "verbose" "debug")
;; 			      (:argopt "file" "bebop")
;; 			      (:style :key)
;; 			      (:name "foo")
;; 			      (:style :partial)
;; 			      (:flagopt "verbose" "bar ~a buz" "baz"))))))
;;   (is (equalp '(cli::spec* "foo" nil nil
;; 		(list
;; 		 (list "verbose" '(:flag) (lambda ()
;; 					    (format nil "bar ~a buz" "baz")))
;; 		 (list "file" '(:string *) (lambda ()
;; 					     (format nil "bebop")))
;; 		 (list "alpha" '(:real 0 99) nil))
;; 		'(("verbose" "debug")
;; 		  ("alpha" "transparency" "fade"))
;; 		'(:partial :key :windows) nil)
;; 	      (caddr (macroexpand '(cli:spec
;; 			      (:argopt "alpha" (:real 0 99))
;; 			      (:alias "alpha" "transparency" "fade")
;; 			      (:style :windows)
;; 			      (:aliases "verbose" "debug")
;; 			      (:argopt "file" "bebop")
;; 			      (:name "foo")
;; 			      (:style :key :partial)
;; 			      (:flagopt "verbose" "bar ~a buz" "baz")))))))

;; (defmacro notpax0 (&rest forms)
;;   `(cli:spec (:name "notpax")
;; 	     (:summary "This is a fake summary of a fake application. ~
;;                            So much fake.  Wow.  Not sure how I'll test this ~
;;                            except by eyeballing it.")
;; 	     (:tail "Forty-two is ~r." 42)
;; 	     (:style :windows)
;; 	     (:argopt "alpha" (:real 0 100) "Blah-de-blah blah.")
;; 	     (:argopt "label" (:string 8) "Just a string that is limited to ~
;;                                            eight characters.")
;; 	     (:alias "alpha" "transparency" "fade")
;; 	     (:flagopt "verbose")
;; 	     (:flagopt "dryrun")
;; 	     (:alias "dryrun" "n")
;; 	     (:argopt "volume" (:integer 0 11) "This one goes to eleven.")
;; 	     ,@forms))

;; (test notpax-1
;;       (is (eq t (notpax0)))
;;       (is (null cli:*arguments*))
;;       (is (zerop (hash-table-count cli:*options*))))

;; (defmacro notpax (&rest args)
;;     `(notpax0 (:argv ,@args)))

;; (test notpax-2
;;       (is (eq t (notpax "one" "two" "three")))
;;       (is (equal '("one" "two" "three") cli:*arguments*))
;;       (is (zerop (hash-table-count cli:*options*))))

;; ;; (defmacro notpax (&rest args)
;; ;;   `(cli:spec (:name "notpax")
;; ;; 	     (:summary "This is a fake summary of a fake application. ~
;; ;;                            So much fake.  Wow.  Not sure how I'll test this ~
;; ;;                            except by eyeballing it.")
;; ;; 	     (:tail "Forty-two is ~r." 42)
;; ;; 	     (:style :windows :partial)
;; ;; 	     (:argopt "alpha" (:real 0 100) "Blah-de-blah blah.")
;; ;; 	     (:argopt "label" (:string 8) "Just a string that is limited to ~
;; ;;                                            eight characters.")
;; ;; 	     (:alias "alpha" "transparency" "fade")
;; ;; 	     (:flagopt "verbose")
;; ;; 	     (:flagopt "dryrun")
;; ;; 	     (:alias "dryrun" "n")
;; ;; 	     (:argopt "volume" (:integer 0 11) "This one goes to eleven.")
;; ;; 	     (:argv ,@args)))

;; ;; (test notpax-1
;; ;;   (is (eq t (notpax "one" "two" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (zerop (hash-table-count cli:*options*))))

;; ;; (test notpax-2
;; ;;   (is (eq t (notpax "one" "/Verbose" "two" "/N" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (= 2 (hash-table-count cli:*options*)))
;; ;;   (is-true (gethash "verbose" cli:*options*))
;; ;;   (is-true (gethash "dryrun" cli:*options*)))

;; ;; (test notpax-3
;; ;;   (is (eq t (notpax "one" "/lab:foobar" "two" "/n" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (= 2 (hash-table-count cli:*options*)))
;; ;;   (is (string= "foobar" (gethash "label" cli:*options*)))
;; ;;   (is-true (gethash "dryrun" cli:*options*)))

;; ;; (test notpax-4
;; ;;   (is (eq t (notpax "one" "/l" "foobar" "two" "/n" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (= 2 (hash-table-count cli:*options*)))
;; ;;   (is (string= "foobar" (gethash "label" cli:*options*)))
;; ;;   (is-true (gethash "dryrun" cli:*options*)))

;; ;; (test notpax-5
;; ;;   (is (eq t (notpax "one" "/LAb" "foobar" "two" "/n" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (= 2 (hash-table-count cli:*options*)))
;; ;;   (is (string= "foobar" (gethash "label" cli:*options*)))
;; ;;   (is-true (gethash "dryrun" cli:*options*)))

;; ;; (test notpax-6
;; ;;   (is (eq t (notpax "one" "/L" "foobar" "two" "/n" "three")))
;; ;;   (is (equal '("one" "two" "three") cli:*arguments*))
;; ;;   (is (= 2 (hash-table-count cli:*options*)))
;; ;;   (is (string= "foobar" (gethash "label" cli:*options*)))
;; ;;   (is-true (gethash "dryrun" cli:*options*)))

;; ;; (defmacro downspec (&rest args)
;; ;;   `(cli:spec (:name "notpax-down")
;; ;; 	     (:summary "This is a fake summary of a fake application. ~
;; ;;                            So much fake.  Wow.  Not sure how I'll test this ~
;; ;;                            except by eyeballing it.")
;; ;; 	     (:tail "Forty-two is ~r." 42)
;; ;; 	     (:style :down :windows :partial)
;; ;; 	     (:argopt "alpha" (:real 0 100) "Blah-de-blah blah.")
;; ;; 	     (:argopt "label" (:string 8) "Just a string that is limited to ~
;; ;;                                            eight characters.")
;; ;; 	     (:alias "alpha" "transparency" "fade")
;; ;; 	     (:flagopt "verbose")
;; ;; 	     (:flagopt "dryrun")
;; ;; 	     (:alias "dryrun" "n")
;; ;; 	     (:argopt "volume" (:integer 0 11) "This one goes to eleven.")
;; ;; 	     (:argv ,@args)))

;; ;; (defmacro keyspec (&rest args)
;; ;;   `(cli:spec (:name "notpax-key")
;; ;; 	     (:summary "This is a fake summary of a fake application. ~
;; ;;                            So much fake.  Wow.  Not sure how I'll test this ~
;; ;;                            except by eyeballing it.")
;; ;; 	     (:tail "Forty-two is ~r." 42)
;; ;; 	     (:style :key :windows :partial)
;; ;; 	     (:argopt "alpha" (:real 0 100) "Blah-de-blah blah.")
;; ;; 	     (:argopt "label" (:string 8) "Just a string that is limited to ~
;; ;;                                            eight characters.")
;; ;; 	     (:alias "alpha" "transparency" "fade")
;; ;; 	     (:flagopt "verbose")
;; ;; 	     (:flagopt "dryrun")
;; ;; 	     (:alias "dryrun" "n")
;; ;; 	     (:argopt "volume" (:integer 0 11) "This one goes to eleven.")
;; ;; 	     (:argv ,@args)))

