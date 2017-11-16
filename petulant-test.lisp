(defpackage #:petulant-test
  (:use #:cl #:5am)
  (:export #:all #:misc #:simple
	   ;; #:string-fixers #:with-chars #:make-optargp	  ; test sets
	   ;; #:unique-substrings #:parse-unix-cli
	   )
  (:import-from #:petulant
		#:stringify #:slashify #:split
		#:wc/make-setfs #:wc/make-case-clause #:with-chars
		#:collecting #:styles-to-hash #:with-stylehash
		#:isolate-switches #:canonicalize-windows-args
		;; #:make-string-fixer #:with-chars #:make-optargp
		;; #:all-truncated-strings #:count-strings #:unique-substrings
		;; #:parse-windows-cli #:parse-unix-cli
		;; #:simple-parse-cli
		))

(in-package #:petulant-test)

(def-suite all :description "all petulant tests")



(def-suite misc :description "petulant misc utilities" :in all)
(in-suite misc)

(test stringify
  (is (equal "foo" (stringify "foo")))
  (is (equal "" (stringify nil)))
  (is (equal "42" (stringify 42)))
  (is (equal "z" (stringify #\z)))
  (is (equalp "FOO" (stringify :foo))))

(test slashify
  (is (equal "/foo" (slashify "foo")))
  (is (equal "" (slashify "")))
  (is (equal "/FOO" (slashify "/FOO"))))

(test split
  (is (equal '("") (split '(#\a #\b #\c) "")))
  (is (equal '("hello," "world")
	     (split '(#\Space #\Tab #\Newline #\Return) "hello, world")))
  (is (equal '("hello," "world")
	     (split '(#\Tab #\Space #\Newline #\Return) "  hello, world")))
  (is (equal '("hello," "world")
	     (split '(#\Tab #\Newline #\Space #\Return) "  hello,  world")))
  (is (equal '("hello," "world")
	     (split '(#\Tab #\Newline #\Return #\Space) "  hello,  world  ")))
  (is (equal '("hello," "world")
	     (split '(#\Space #\Tab #\Newline #\Return) "hello,  world  "))))

(test wc/make-setfs
      (is (equalp nil
		  (wc/make-setfs 0 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0))
		  (wc/make-setfs 1 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1))
		  (wc/make-setfs 2 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1) c (char foo 2))
		  (wc/make-setfs 3 'foo '(a b c))))
      (is (equalp '(setf a (char foo 0) b (char foo 1) c (char foo 2))
		  (wc/make-setfs 4 'foo '(a b c)))))

(test wc/make-case-clause
      (is (equalp '(0 nil)
		  (wc/make-case-clause 0 'foo '(a b c))))
      (is (equalp '(1 (setf a (char foo 0)))
		  (wc/make-case-clause 1 'foo '(a b c))))
      (is (equalp '(2 (setf a (char foo 0) b (char foo 1)))
		  (wc/make-case-clause 2 'foo '(a b c))))
      (is (equalp '(t (setf a (char foo 0) b (char foo 1) c (char foo 2)))
		  (wc/make-case-clause 3 'foo '(a b c))))
      (is (equalp '(t (setf a (char foo 0) b (char foo 1) c (char foo 2)))
		  (wc/make-case-clause 4 'foo '(a b c)))))

(test with-chars
      (let ((str "abcde"))
	(with-chars (a b c) str
	  (is (char= #\a a))
	  (is (char= #\b b))
	  (is (char= #\c c))))
      (let ((str "fg"))
	(with-chars (f g h) str
	  (is (char= #\f f))
	  (is (char= #\g g))
	  (is (null h))))
      (let ((str ""))
	(with-chars (f g h) str
	  (is (null f))
	  (is (null g))
	  (is (null h)))))

(test collecting
  (let ((hash (make-hash-table))
	(str "aCeG")
	(seq #(#\b #\D #\f #\H))
	(list '(12 23 34 45)))
    (is (equal nil
	       (sort (collecting (lambda (k v) (* k v)) hash) #'<)))
    (is (equal nil
	       (collecting #'char-downcase "")))
    (is (equal nil
	       (collecting #'char-upcase #())))
    (is (equal nil
	       (collecting (lambda (x) (* 2 x)) nil)))
    (setf (gethash 1 hash) 10
	  (gethash 2 hash) 20
	  (gethash 3 hash) 30
	  (gethash 4 hash) 40)
    (is (equal '(10 40 90 160)
	       (sort (collecting (lambda (k v) (* k v)) hash) #'<)))
    (is (equal '(#\a #\c #\e #\g)
	       (collecting #'char-downcase str)))
    (is (equal '(#\B #\D #\F #\H)
	       (collecting #'char-upcase seq)))
    (is (equal '(24 46 68 90)
	       (collecting (lambda (x) (* 2 x)) list)))))



(def-suite simple :description "simple parser stuff" :in all)
(in-suite simple)

(def-fixture stylehash (&rest styles)
  (let ((hash (styles-to-hash styles)))
    (labels ((gh (x) (gethash x hash)))
      (macrolet ((has (&rest flags)
		   `(progn ,@(mapcar (lambda (f) `(is-true (gh ,f)))
				     flags)))
		 (hasnot (&rest flags)
		   `(progn ,@(mapcar (lambda (f) `(is-false (gh ,f)))
				     flags))))
	(&body)))))

(test styles-to-hash
  (with-fixture stylehash ()
    #+windows (has :windows :streq)
    #+windows (hasnot :unix :str= :up :down :key)
    #-windows (hasnot :windows :streq :up :down :key)
    #-windows (has :unix :str=))
  (with-fixture stylehash (:windows)
    (has :windows :streq)
    (hasnot :unix :str= :up :down :key))
  (with-fixture stylehash (:unix)
    (hasnot :windows :streq :up :down :key)
    (has :unix :str=))
  (with-fixture stylehash (:windows :up)
    (has :windows :streq :up)
    (hasnot :unix :str= :down :key))
  (with-fixture stylehash (:unix :down)
    (has :unix :streq :down)
    (hasnot :windows :str= :up :key))
  (with-fixture stylehash (:unix :key)
    (has :unix :streq :up :key)
    (hasnot :windows :str= :down))
  (with-fixture stylehash (:unix :key :down)	 ; valid but not sane
    (has :unix :streq :down :key)
    (hasnot :windows :str= :up))
  (with-fixture stylehash (:windows :str= :down) ; valid but not sane
    (has :windows :str= :down)
    (hasnot :unix :streq :up :key)))

(test with-stylehash
  (let ((styles '(:unix :key :junk)))
    (with-stylehash (styles)
      (is-true (hash-table-p styles))
      (is-true (gethash :unix styles))
      (is-false (gethash :windows styles))
      (is-true (gethash :streq styles))
      (is-false (gethash :str= styles))
      (is-false (gethash :down styles))
      (is-true (gethash :up styles))
      (is-true (gethash :key styles))
      (with-stylehash (styles)		; ensure they nest right
	(is-true (hash-table-p styles))
	(is-true (gethash :junk styles))))))

(test isolate-switches
  (is (equalp '("a") (isolate-switches "/a")))
  (is (equalp '("ab") (isolate-switches "/ab")))
  (is (equalp '("a" "bc") (isolate-switches "/a/bc")))
  (is (equalp '("a" "bc:de") (isolate-switches "/a/bc:de")))
  (is (equalp '("a" "bc:de" "f") (isolate-switches "/a/bc:de/f")))
  (is (equalp '("a" "bc:de" "f") (isolate-switches "/a/bc:de/f/")))
  (is (equalp '("a" "bc:de" "f") (isolate-switches "///a//bc:de///f//")))
  (is (equalp '("") (isolate-switches "")))
  (is (equalp '("/") (isolate-switches "/")))
  (is (equalp '("//") (isolate-switches "//")))
  (is (equalp '("///") (isolate-switches "///"))))

(test canonicalize-windows-args
  (is (equalp '("abc" "" "def")
	      (canonicalize-windows-args '(nil "abc" "" "def"))))
  (is (equalp '("abc" "/" "def")
	      (canonicalize-windows-args '("abc" nil "/" "def"))))
  (is (equalp '("abc" "//" "def")
	      (canonicalize-windows-args '("abc" "//" nil "def"))))
  (is (equalp '("abc" "///" "def")
	      (canonicalize-windows-args '("abc" "///" "def" nil))))
  (is (equalp '("/abc" "def")
	      (canonicalize-windows-args '("/abc" "def"))))
  (is (equalp '("/a" "/bc" "def")
	      (canonicalize-windows-args '("/a/bc" "def"))))
  (is (equalp '("/a" "/bc" "def" "/ef:gh")
	      (canonicalize-windows-args '("/a/bc" "def" "/ef:gh"))))
  (is (equalp '("/a" "/bc" "def" "/ef" "gh")
	      (canonicalize-windows-args '("/a/bc" "def" "/ef" "gh"))))
  (is (equalp '("/a" "/bc" "def" "/e" "/f:g" "/h")
	      (canonicalize-windows-args '("/a/bc" "def" "/e/f:g/h")))))

;; These are broken up because, when you reuse values in the tests,
;; it's not immediately in five-am to know which specific test fails.
;; Yes, using different test values in every test would mitigate this,
;; but come on, who has time for that?  So, we'll just do a1 a2 a3 for
;; specific related functionality and a. b. c. for more general
;; related functionality.  Pass this stuff, and you're almost home
;; free, the rest of Petulant is easy by comparison!

(test parse-windows-cli-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a") #'cb)
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test parse-windows-cli-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c") #'cb)
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test parse-windows-cli-a3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "/de") #'cb)
      (is (= (length res) 4))
      (is (equalp '(:opt "de" nil) (car res)))
      (is (equalp '(:opt "c" nil) (cadr res)))
      (is (equalp '(:opt "b" nil) (caddr res)))
      (is (equalp '(:opt "a" nil) (cadddr res))))))

(test parse-windows-cli-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "d") #'cb)
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "//" "/d") #'cb)
      (is (equalp '((:arg "/d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/alpha" "/b/c" "/beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "/beta" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-c4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "/beta" "//" "/foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "/foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-d1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "/beta:fuzz" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" "fuzz")
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-d2					  ; gnu-ish
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-windows-cli '("/a" "/b/c" "foo" "/beta:fuzz" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:opt "beta" "fuzz")
		    (:arg "foo" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-windows-cli-e1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-windows-cli '("/x/v/f" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-windows-cli '("/x/v/f:foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-windows-cli '("/x/v" "/file:foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-windows-cli '("/x/v" "/file" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-f1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-windows-cli '("/x/v" "/f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-f2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-windows-cli '("/x/v/f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-f3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-windows-cli '("/x/v" "/file:") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-windows-cli-f4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-windows-cli '("/x/v" "/file") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-a1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a") #'cb)
      (is (= (length res) 1))
      (is (equalp '(:opt "a" nil) (car res))))))

(test parse-unix-cli-a2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "-bc") #'cb)
      (is (= (length res) 3))
      (is (equalp '(:opt "c" nil) (car res)))
      (is (equalp '(:opt "b" nil) (cadr res)))
      (is (equalp '(:opt "a" nil) (caddr res))))))

(test parse-unix-cli-b1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "-bc" "d") #'cb)
      (is (equalp '((:arg "d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-b2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "-bc" "--" "-d") #'cb)
      (is (equalp '((:arg "-d" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-c1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "--alpha" "-bc" "--beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "alpha" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-c2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "--a" "-bc" "--beta") #'cb)
      (is (equalp '((:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-c3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "--a" "-bc" "--beta" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-c4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "--a" "-bc" "--beta" "--" "--foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "--foo" nil)
		    (:opt "beta" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-d1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "-bc" "--beta=fuzz" "foo" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:arg "foo" nil)
		    (:opt "beta" "fuzz")
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-d2					  ; gnu-ish
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli '("-a" "-bc" "foo" "--beta=fuzz" "bar") #'cb)
      (is (equalp '((:arg "bar" nil)
		    (:opt "beta" "fuzz")
		    (:arg "foo" nil)
		    (:opt "c" nil)
		    (:opt "b" nil)
		    (:opt "a" nil))
		  res)))))

(test parse-unix-cli-e1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "F")))
      (parse-unix-cli '("-XVF" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "F" "foo")
		    (:opt "V" nil)
		    (:opt "X" nil))
		  res)))))

(test parse-unix-cli-e2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-unix-cli '("-xvffoo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "f" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-e3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-unix-cli '("-xv" "--file=foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-e4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-unix-cli '("-xv" "--file" "foo" "something") #'cb #'f?)
      (is (equalp '((:arg "something" nil)
		    (:opt "file" "foo")
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-f1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-unix-cli '("-xv" "-f") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-f2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "f")))
      (parse-unix-cli '("-xvf") #'cb #'f?)
      (is (equalp '((:opt "f" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-f3
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-unix-cli '("-xv" "--file=") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test parse-unix-cli-f4
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res))
	     (f? (x) (string= x "file")))
      (parse-unix-cli '("-xv" "--file") #'cb #'f?)
      (is (equalp '((:opt "file" nil)
		    (:opt "v" nil)
		    (:opt "x" nil))
		  res)))))

(test simple-parse-cli-1
  (let ((res (with-output-to-string (s)
	       (simple-parse-cli #'(lambda (x y z)
				     (format s "|~s ~s ~s" x y z))
				 :arglist '("/a" "/beta" "/input:file"
					    "some" "/v" "thing")
				 :styles :windows))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test simple-parse-cli-2
  (let ((res (with-output-to-string (s)
	       (simple-parse-cli #'(lambda (x y z)
				     (format s "|~s ~s ~s" x y z))
				 :arglist '("-a" "--beta" "--input=file"
					    "some" "-v" "thing")
				 :styles :unix))))
    (is (string-equal "|:OPT \"a\" NIL|:OPT \"beta\" NIL|:OPT \"input\" \"file\"|:ARG \"some\" NIL|:OPT \"v\" NIL|:ARG \"thing\" NIL"
		      res))))

(test simple-parse-cli-3
  (let ((*verbose* nil))
    (labels ((opts-and-args (kind name value)
	       (declare (ignore value))
	       (when (and (eq kind :opt) (string-equal name "v"))
		 (setf *verbose* t))))
      (simple-parse-cli #'opts-and-args
			:arglist '("-v") :styles :unix)
      (is (not (null *verbose*))))))

(test simple-parse-cli-4
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
      (simple-parse-cli #'opts-and-args
			:arglist '("/v" "/q/v" "/v/q/v" "/q/q/q" "/v/v")
			:styles :windows)
      (is (= 1 *verbose*)))))

(test simple-parse-cli-5
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
      (simple-parse-cli #'opts-and-args
			:arglist '("-c" "/foo/altconf.yml"
				   "-v" "one.dat" "-q" "two.dat" "-v")
			:argopt-p-fn #'(lambda (x) (string-equal "c" x))
			:styles :unix)
      (is (= 1 *verbose*))
      (is (equalp (pathname "one.dat") *inpath*))
      (is (equalp (pathname "two.dat") *outpath*))
      (is (equalp (pathname "/foo/altconf.yml") *confpath*)))))



;; #+nil (def-suite string-fixers :description "make-string-fixer" :in util)
;; #+nil (in-suite string-fixers)

;; #+nil
;; (test make-string-fixer-plain
;;   (let ((fn (make-string-fixer)))
;;     (flet ((fn (x) (funcall fn x)))
;;       (is (string= (fn "c") "c"))
;;       (is (string= (fn "D") "D"))
;;       (is (string= (fn "foo") "foo"))
;;       (is (string= (fn "BaR") "BaR"))
;;       (is (string= (fn "BAZ") "BAZ"))
;;       (is (string= (fn "") "")))))
;; #+nil
;; (test make-string-fixer-up
;;   (let ((fn (make-string-fixer :up)))
;;     (flet ((fn (x) (funcall fn x)))
;;       (is (string= (fn "c") "C"))
;;       (is (string= (fn "D") "D"))
;;       (is (string= (fn "foo") "FOO"))
;;       (is (string= (fn "BaR") "BAR"))
;;       (is (string= (fn "BAZ") "BAZ"))
;;       (is (string= (fn "") "")))))

;; #+nil
;; (test make-string-fixer-down
;;   (let ((fn (make-string-fixer :down)))
;;     (flet ((fn (x) (funcall fn x)))
;;       (is (string= (fn "c") "c"))
;;       (is (string= (fn "D") "d"))
;;       (is (string= (fn "foo") "foo"))
;;       (is (string= (fn "BaR") "bar"))
;;       (is (string= (fn "BAZ") "baz"))
;;       (is (string= (fn "") "")))))

;; #+nil
;; (test make-string-fixer-key
;;   (let ((fn (make-string-fixer :key)))
;;     (flet ((fn (x) (funcall fn x)))
;;       (is (eq (fn "c") :c))
;;       (is (eq (fn "D") :d))
;;       (is (eq (fn "foo") :foo))
;;       (is (eq (fn "BaR") :bar))
;;       (is (eq (fn "BAZ") :baz))
;;       (is (null (fn ""))))))

;; #+nil (def-suite make-optargp :description "make-optargp" :in util)
;; #+nil (in-suite make-optargp)

;; #+nil
;; (test make-optargp-plain
;;   (let ((fn (make-optargp '("foo" #\a "B"))))
;;     (flet ((f (x) (funcall fn x)))
;;       (is (not (f #\A)))
;;       (is (not (f "A")))
;;       (is (f #\a))
;;       (is (f "a"))
;;       (is (f #\B))
;;       (is (f "B"))
;;       (is (not (f #\b)))
;;       (is (not (f "b")))
;;       (is (not (f #\c)))
;;       (is (not (f "c")))
;;       (is (not (f #\C)))
;;       (is (not (f "C")))
;;       (is (not (f "cee")))
;;       (is (not (f "FoO")))
;;       (is (f "foo"))
;;       (is (not (f ""))))))

;; #+nil
;; (test make-optargp-up
;;   (let ((fn (make-optargp '("foo" #\a "B") :up)))
;;     (flet ((f (x) (funcall fn x)))
;;       (is (f #\A))
;;       (is (f "A"))
;;       (is (f #\a))
;;       (is (f "a"))
;;       (is (f #\B))
;;       (is (f "B"))
;;       (is (f #\b))
;;       (is (f "b"))
;;       (is (not (f #\c)))
;;       (is (not (f "c")))
;;       (is (not (f #\C)))
;;       (is (not (f "C")))
;;       (is (not (f "cee")))
;;       (is (f "FoO"))
;;       (is (f "foo"))
;;       (is (not (f ""))))))

;; #+nil
;; (test make-optargp-down
;;   (let ((fn (make-optargp '("foo" #\a "B") :down)))
;;     (flet ((f (x) (funcall fn x)))
;;       (is (f #\A))
;;       (is (f "A"))
;;       (is (f #\a))
;;       (is (f "a"))
;;       (is (f #\B))
;;       (is (f "B"))
;;       (is (f #\b))
;;       (is (f "b"))
;;       (is (not (f #\c)))
;;       (is (not (f "c")))
;;       (is (not (f #\C)))
;;       (is (not (f "C")))
;;       (is (not (f "cee")))
;;       (is (f "FoO"))
;;       (is (f "foo"))
;;       (is (not (f ""))))))

;; #+nil
;; (test make-optargp-key
;;   (let ((fn (make-optargp '("foo" #\a "B") :key)))
;;     (flet ((f (x) (funcall fn x)))
;;       (is (f #\A))
;;       (is (f "A"))
;;       (is (f #\a))
;;       (is (f "a"))
;;       (is (f #\B))
;;       (is (f "B"))
;;       (is (f #\b))
;;       (is (f "b"))
;;       (is (not (f #\c)))
;;       (is (not (f "c")))
;;       (is (not (f #\C)))
;;       (is (not (f "C")))
;;       (is (not (f "cee")))
;;       (is (f "FoO"))
;;       (is (f "foo"))
;;       (is (not (f ""))))))

;; (def-suite unique-substrings :description "unique-substrings et. al." :in util)
;; (in-suite unique-substrings)

;; (test all-truncated-strings
;;   (let ((res (all-truncated-strings '("alpha" "coo" "beta" "beat"))))
;;     (is (= (length res) 16))
;;     (is (member "a" res :test #'string=))
;;     (is (member "al" res :test #'string=))
;;     (is (member "alp" res :test #'string=))
;;     (is (member "alph" res :test #'string=))
;;     (is (member "alpha" res :test #'string=))
;;     (is (member "b" res :test #'string=))
;;     (is (member "be" res :test #'string=))
;;     (is (member "bea" res :test #'string=))
;;     (is (member "beat" res :test #'string=))
;;     (is (member "bet" res :test #'string=))
;;     (is (member "beta" res :test #'string=))
;;     (is (member "c" res :test #'string=))
;;     (is (member "co" res :test #'string=))
;;     (is (member "coo" res :test #'string=))))

;; (test count-strings-1
;;   (let ((res (count-strings '("a" "bc" "a" "cd" "a"))))
;;     (is (= (length res) 3))
;;     (is (find '("a" . 3) res :test #'equalp))
;;     (is (find '("bc" . 1) res :test #'equalp))
;;     (is (find '("cd" . 1) res :test #'equalp))))

;; (test count-strings-2
;;   (let ((res (count-strings (all-truncated-strings
;; 			     '("alpha" "coo" "beta" "beat" "ant" "a")) t)))
;;     (is (= (length res) 16))
;;     (is (find '("alpha" . 1) res :test #'equalp))
;;     (is (find '("alph" . 1) res :test #'equalp))
;;     (is (find '("alp" . 1) res :test #'equalp))
;;     (is (find '("al" . 1) res :test #'equalp))
;;     (is (find '("a" . 3) res :test #'equalp))
;;     (is (find '("ant" . 1) res :test #'equalp))
;;     (is (find '("an" . 1) res :test #'equalp))
;;     (is (find '("beat" . 1) res :test #'equalp))
;;     (is (find '("bea" . 1) res :test #'equalp))
;;     (is (find '("be" . 2) res :test #'equalp))
;;     (is (find '("b" . 2) res :test #'equalp))
;;     (is (find '("beta" . 1) res :test #'equalp))
;;     (is (find '("bet" . 1) res :test #'equalp))
;;     (is (find '("coo" . 1) res :test #'equalp))
;;     (is (find '("co" . 1) res :test #'equalp))
;;     (is (find '("c" . 1) res :test #'equalp))
;;     ))

;; (test unique-substrings
;;   (let ((res (unique-substrings '("alpha" "beta" "ant" "beat" "coo" "bop"))))
;;     (is (= (length res) 15))
;;     (is (member "coo" res :test #'string=))
;;     (is (member "co" res :test #'string=))
;;     (is (member "c" res :test #'string=))
;;     (is (member "bop" res :test #'string=))
;;     (is (member "bo" res :test #'string=))
;;     (is (member "beat" res :test #'string=))
;;     (is (member "bea" res :test #'string=))
;;     (is (member "beta" res :test #'string=))
;;     (is (member "bet" res :test #'string=))
;;     (is (member "alpha" res :test #'string=))
;;     (is (member "alph" res :test #'string=))
;;     (is (member "alp" res :test #'string=))
;;     (is (member "al" res :test #'string=))
;;     (is (member "ant" res :test #'string=))
;;     (is (member "an" res :test #'string=))))
