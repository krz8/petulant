(defpackage #:petulant-test
  (:use #:cl #:5am)
  (:export #:all #:util			; suites
	   #:make-option-fixer)		; test sets
  (:import-from #:petulant
		#:make-string-fixer #:with-chars #:make-optwitharg-tester))

(in-package #:petulant-test)

(def-suite all :description "all petulant tests")
(def-suite util :description "petulant misc utilities" :in all)
(def-suite string-fixers :description "make-option-fixer" :in util)
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
      (is (f "foo"))
      (is (f #\a))
      (is (f "a"))
      (is (f #\B))
      (is (f "B"))
      (is (not (f "FoO")))
      (is (not (f #\A)))
      (is (not (f "A")))
      (is (not (f #\b)))
      (is (not (f "b")))
      (is (not (f ""))))))
