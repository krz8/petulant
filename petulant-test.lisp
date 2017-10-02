(defpackage #:petulant-test
  (:use #:cl #:5am)
  (:export #:all #:util			                  ; suites
	   #:string-fixers #:with-chars #:make-optargp	  ; test sets
	   #:unique-substrings #:parse-unix-cli)
  (:import-from #:petulant
		#:make-string-fixer #:with-chars #:make-optargp
		#:all-truncated-strings #:count-strings #:unique-substrings
		#:parse-unix-cli))

(in-package #:petulant-test)

(def-suite all :description "all petulant tests")
(def-suite util :description "petulant misc utilities" :in all)
(def-suite string-fixers :description "make-string-fixer" :in util)
(in-suite string-fixers)

(test make-string-fixer-plain
  (let ((fn (make-string-fixer)))
    (flet ((fn (x) (funcall fn x)))
      (is (string= (fn "c") "c"))
      (is (string= (fn "D") "D"))
      (is (string= (fn "foo") "foo"))
      (is (string= (fn "BaR") "BaR"))
      (is (string= (fn "BAZ") "BAZ"))
      (is (string= (fn "") "")))))

(test make-string-fixer-up
  (let ((fn (make-string-fixer :up)))
    (flet ((fn (x) (funcall fn x)))
      (is (string= (fn "c") "C"))
      (is (string= (fn "D") "D"))
      (is (string= (fn "foo") "FOO"))
      (is (string= (fn "BaR") "BAR"))
      (is (string= (fn "BAZ") "BAZ"))
      (is (string= (fn "") "")))))

(test make-string-fixer-down
  (let ((fn (make-string-fixer :down)))
    (flet ((fn (x) (funcall fn x)))
      (is (string= (fn "c") "c"))
      (is (string= (fn "D") "d"))
      (is (string= (fn "foo") "foo"))
      (is (string= (fn "BaR") "bar"))
      (is (string= (fn "BAZ") "baz"))
      (is (string= (fn "") "")))))

(test make-string-fixer-key
  (let ((fn (make-string-fixer :key)))
    (flet ((fn (x) (funcall fn x)))
      (is (eq (fn "c") :c))
      (is (eq (fn "D") :d))
      (is (eq (fn "foo") :foo))
      (is (eq (fn "BaR") :bar))
      (is (eq (fn "BAZ") :baz))
      (is (null (fn ""))))))

(def-suite with-chars :description "with-chars" :in util)
(in-suite with-chars)

(test with-chars-long
  (let ((str "abcde"))
    (with-chars (a b c) str
      (is (char= a #\a))
      (is (char= b #\b))
      (is (char= c #\c)))))

(test with-chars-short
  (let ((str "fg"))
    (with-chars (f g h) str
      (is (char= f #\f))
      (is (char= g #\g))
      (is (null h)))))

(test with-chars-empty
  (let ((str ""))
    (with-chars (f g h) str
      (is (null f))
      (is (null g))
      (is (null h)))))

(def-suite make-optargp :description "make-optargp" :in util)
(in-suite make-optargp)

(test make-optargp-plain
  (let ((fn (make-optargp '("foo" #\a "B"))))
    (flet ((f (x) (funcall fn x)))
      (is (not (f #\A)))
      (is (not (f "A")))
      (is (f #\a))
      (is (f "a"))
      (is (f #\B))
      (is (f "B"))
      (is (not (f #\b)))
      (is (not (f "b")))
      (is (not (f #\c)))
      (is (not (f "c")))
      (is (not (f #\C)))
      (is (not (f "C")))
      (is (not (f "cee")))
      (is (not (f "FoO")))
      (is (f "foo"))
      (is (not (f ""))))))

(test make-optargp-up
  (let ((fn (make-optargp '("foo" #\a "B") :up)))
    (flet ((f (x) (funcall fn x)))
      (is (f #\A))
      (is (f "A"))
      (is (f #\a))
      (is (f "a"))
      (is (f #\B))
      (is (f "B"))
      (is (f #\b))
      (is (f "b"))
      (is (not (f #\c)))
      (is (not (f "c")))
      (is (not (f #\C)))
      (is (not (f "C")))
      (is (not (f "cee")))
      (is (f "FoO"))
      (is (f "foo"))
      (is (not (f ""))))))

(test make-optargp-down
  (let ((fn (make-optargp '("foo" #\a "B") :down)))
    (flet ((f (x) (funcall fn x)))
      (is (f #\A))
      (is (f "A"))
      (is (f #\a))
      (is (f "a"))
      (is (f #\B))
      (is (f "B"))
      (is (f #\b))
      (is (f "b"))
      (is (not (f #\c)))
      (is (not (f "c")))
      (is (not (f #\C)))
      (is (not (f "C")))
      (is (not (f "cee")))
      (is (f "FoO"))
      (is (f "foo"))
      (is (not (f ""))))))

(test make-optargp-key
  (let ((fn (make-optargp '("foo" #\a "B") :key)))
    (flet ((f (x) (funcall fn x)))
      (is (f #\A))
      (is (f "A"))
      (is (f #\a))
      (is (f "a"))
      (is (f #\B))
      (is (f "B"))
      (is (f #\b))
      (is (f "b"))
      (is (not (f #\c)))
      (is (not (f "c")))
      (is (not (f #\C)))
      (is (not (f "C")))
      (is (not (f "cee")))
      (is (f "FoO"))
      (is (f "foo"))
      (is (not (f ""))))))

(def-suite unique-substrings :description "unique-substrings et. al." :in util)
(in-suite unique-substrings)

(test all-truncated-strings
  (let ((res (all-truncated-strings '("alpha" "coo" "beta" "beat"))))
    (is (= (length res) 16))
    (is (member "a" res :test #'string=))
    (is (member "al" res :test #'string=))
    (is (member "alp" res :test #'string=))
    (is (member "alph" res :test #'string=))
    (is (member "alpha" res :test #'string=))
    (is (member "b" res :test #'string=))
    (is (member "be" res :test #'string=))
    (is (member "bea" res :test #'string=))
    (is (member "beat" res :test #'string=))
    (is (member "bet" res :test #'string=))
    (is (member "beta" res :test #'string=))
    (is (member "c" res :test #'string=))
    (is (member "co" res :test #'string=))
    (is (member "coo" res :test #'string=))))

(test count-strings-1
  (let ((res (count-strings '("a" "bc" "a" "cd" "a"))))
    (is (= (length res) 3))
    (is (find '("a" . 3) res :test #'equalp))
    (is (find '("bc" . 1) res :test #'equalp))
    (is (find '("cd" . 1) res :test #'equalp))))

(test count-strings-2
  (let ((res (count-strings (all-truncated-strings
			     '("alpha" "coo" "beta" "beat" "ant" "a")) t)))
    (is (= (length res) 16))
    (is (find '("alpha" . 1) res :test #'equalp))
    (is (find '("alph" . 1) res :test #'equalp))
    (is (find '("alp" . 1) res :test #'equalp))
    (is (find '("al" . 1) res :test #'equalp))
    (is (find '("a" . 3) res :test #'equalp))
    (is (find '("ant" . 1) res :test #'equalp))
    (is (find '("an" . 1) res :test #'equalp))
    (is (find '("beat" . 1) res :test #'equalp))
    (is (find '("bea" . 1) res :test #'equalp))
    (is (find '("be" . 2) res :test #'equalp))
    (is (find '("b" . 2) res :test #'equalp))
    (is (find '("beta" . 1) res :test #'equalp))
    (is (find '("bet" . 1) res :test #'equalp))
    (is (find '("coo" . 1) res :test #'equalp))
    (is (find '("co" . 1) res :test #'equalp))
    (is (find '("c" . 1) res :test #'equalp))
    ))

(test unique-substrings
  (let ((res (unique-substrings '("alpha" "beta" "ant" "beat" "coo" "bop"))))
    (is (= (length res) 15))
    (is (member "coo" res :test #'string=))
    (is (member "co" res :test #'string=))
    (is (member "c" res :test #'string=))
    (is (member "bop" res :test #'string=))
    (is (member "bo" res :test #'string=))
    (is (member "beat" res :test #'string=))
    (is (member "bea" res :test #'string=))
    (is (member "beta" res :test #'string=))
    (is (member "bet" res :test #'string=))
    (is (member "alpha" res :test #'string=))
    (is (member "alph" res :test #'string=))
    (is (member "alp" res :test #'string=))
    (is (member "al" res :test #'string=))
    (is (member "ant" res :test #'string=))
    (is (member "an" res :test #'string=))))

(def-suite parse-unix-cli :description "parse UNIX cli" :in all)
(in-suite parse-unix-cli)

(test parse-unix-cli-1
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a"))
      (is (= (length res) 1))
      (is (equalp (car res) '(:opt "a" nil))))))

(test parse-unix-cli-2
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "-bc"))
      (is (= (length res) 3))
      (is (equalp (car res) '(:opt "c" nil)))
      (is (equalp (cadr res) '(:opt "b" nil)))
      (is (equalp (caddr res) '(:opt "a" nil))))))

(test parse-unix-cli-10
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "-bc" "d"))
      (is (equalp res '((:arg "d" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-11
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "-bc" "--" "-d"))
      (is (equalp res '((:arg "-d" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-20
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "--alpha" "-bc" "--beta"))
      (is (equalp res '((:opt "beta" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "alpha" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-21
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "--a" "-bc" "--beta"))
      (is (equalp res '((:opt "beta" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-22
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "--a" "-bc" "--beta" "foo" "bar"))
      (is (equalp res '((:arg "bar" nil)
			(:arg "foo" nil)
			(:opt "beta" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-23
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "--a" "-bc" "--beta" "--" "--foo" "bar"))
      (is (equalp res '((:arg "bar" nil)
			(:arg "--foo" nil)
			(:opt "beta" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-30
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "-bc" "--beta=fuzz" "foo" "bar"))
      (is (equalp res '((:arg "bar" nil)
			(:arg "foo" nil)
			(:opt "beta" "fuzz")
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-31					  ; gnu-ish
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-a" "-bc" "foo" "--beta=fuzz" "bar"))
      (is (equalp res '((:arg "bar" nil)
			(:opt "beta" "fuzz")
			(:arg "foo" nil)
			(:opt "c" nil)
			(:opt "b" nil)
			(:opt "a" nil)))))))

(test parse-unix-cli-40
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-xvf" "foo" "something") '("f"))
      (is (equalp res '((:arg "something" nil)
			(:opt "f" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))

(test parse-unix-cli-41
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-xvffoo" "something") '("f"))
      (is (equalp res '((:arg "something" nil)
			(:opt "f" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))

(test parse-unix-cli-42
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-xv" "--file=foo" "something") '("file"))
      (is (equalp res '((:arg "something" nil)
			(:opt "file" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))

(test parse-unix-cli-43
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-xv" "--file" "foo" "something") '("file"))
      (is (equalp res '((:arg "something" nil)
			(:opt "file" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))

(test parse-unix-cli-50
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-Xv" "--File" "foo" "something") '("file") :down)
      (is (equalp res '((:arg "something" nil)
			(:opt "file" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))

(test parse-unix-cli-51
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-Xv" "--File" "foo" "something") '("file") :up)
      (is (equalp res '((:arg "something" nil)
			(:opt "FILE" "foo")
			(:opt "V" nil)
			(:opt "X" nil)))))))

(test parse-unix-cli-52
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-Xv" "--File" "foo" "something") '("file") :key)
      (is (equalp res '((:arg "something" nil)
			(:opt :file "foo")
			(:opt :v nil)
			(:opt :x nil)))))))

;; support for partial matches are incomplete

(test parse-unix-cli-53
  (let (res)
    (labels ((cb (kind key value) (push (list kind key value) res)))
      (parse-unix-cli #'cb '("-xv" "--fi" "foo" "something") '("file"))
      (is (equalp res '((:arg "something" nil)
			(:opt "fi" "foo")
			(:opt "v" nil)
			(:opt "x" nil)))))))
