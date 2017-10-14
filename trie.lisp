;;;; Sparse Trie for our word dictionaries supporting Unicode
(in-package #:petulant)

;;; Historically, a trie was a vector of pointers to other tries,
;;; indexed by a character.  Tries were not especially specific to
;;; characters, but trie applications were nearly all oriented to text
;;; processing.  Back when ASCII ruled the land, a trie was simply a
;;; vector of 128 elements, each of which was either null or pointed
;;; to another trie. When ISO 8859 became commonplace, these vectors
;;; grew to 256 elements, still manageable by most modern systems.
;;;
;;; Tries tend to be wasteful, though, in that each vector is fixed in
;;; size and holds mostly nulls.  This waste becomes significant with
;;; other character sets, and with Unicode, it becomes untenable.  The
;;; range of Unicode code points would require vectors with more than
;;; a million elements in each of them.  Even restricting an
;;; implementation to the range of defined code points only saves you
;;; a factor of ten, still requiring over 100,000 elements _per
;;; vector_ in the trie.
;;;
;;; We'll define a sparse trie in the same way that a sparse matrix is
;;; commonly implemented, as a hash table.  We'll trade a fair amount
;;; of speed doing so, but it's more than worth it for the space
;;; savings.  Taking just a little bit of care, a hash table can be
;;; used in place of a vector-based trie with almost no change to the
;;; calling code.  But, at the bottom of the hierarchy, a trie can
;;; be represented by a hash table; everything from here on up is
;;; artifice to make certain uses easier.
;;;
;;; Rather than a bare hash-table, we'll write a class; it gives us a
;;; handy place for an application to specialize a Sparse Trie for its
;;; own uses through inheritance.

(defclass trie ()
  ((table :accessor trie-table :type hash-table
	  :initarg :table :initform (make-hash-table)
	  :documentation "The underlying hash used to map keys to
	  subtables in the trie."))
  (:documentation "A sparse trie that uses hash tables instead of
  traditional vectors.  Typically, avoid the use of MAKE-INSTANCE and
  instead use MAKE-TRIE in order to keep the TABLE and EQFN slots
  properly in sync."))

(defun make-trie (&key loose)
  "Create a new empty TRIE node.  The underlying hash table will use
  EQUAL to compare keys, unless LOOSE is true, which will cause the
  hash table to use EQUALP instead."
  (make-instance 'trie :table (make-hash-table
			       :test (if loose #'equalp #'equal))))

(defgeneric make-similar-trie (trie)
  (:documentation "Create and return a new trie of the same type as
  the supplied TRIE.  There is no relationship between the supplied
  TRIE and the newly created trie, except that they are the same
  class (or subclass) of trie object.  Even though this could be
  served by a single function in Petulant, we'll make this a generic
  function to accommodate any specializations in the future that might
  need extra functionality."))

(defmethod make-similar-trie ((trie trie))
  "Create and return a new trie of the same type as the supplied TRIE.
  There is no linkage between the supplied TRIE and the newly created
  trie.  For most classes and subclasses of TRIE, this method will
  suffice."
  (make-instance (type-of trie)
		 :table (make-hash-table
			 :test (hash-table-test (trie-table trie)))))

(defgeneric trie-at (trie key)
  (:documentation "Given any kind of TRIE, return the subtrie
  associated with KEY within it.  If KEY is not presently associated
  with anything, a new subtrie of the same type as TRIE is created and
  associated with it."))

(defmethod trie-at ((trie trie) key)
  "Given any kind of TRIE, return a subtrie associated with KEY within
  it.  If KEY is not presently associated with anything, a new empty
  subtrie of the same type as TRIE is created and associated with it.
  For most classes and subclasses of TRIE, this method will suffice."
  (aif (gethash key (trie-table trie))
       it
       (setf (gethash key (trie-table trie))
	     (make-similar-trie trie))))

;;; We plan to use a trie as a way to determine minimal unique
;;; strings.  By this, we mean that given a list of words, what is the
;;; minimum amount of text needed to identify each word in the
;;; original list?  For example, given these words:
;;;
;;;    alpha beat beta ignore input zebra
;;;
;;; The minimum amount of text to recognize "alpha" is 1: "a".  "al",
;;; "alp", and "alph" would also match "alpha", but "ab" would not,
;;; nor would "alphas".  The length for "beat" and "beta" is 3,
;;; meaning we need to match the first three letters in order to
;;; uniquely identify the target word.  Just two would work for
;;; "ignore" and "input", and "zebra" is like "alpha" in that just 1
;;; would be sufficient.
;;;
;;; Certainly, we need to consider matches of the entire substring and
;;; pay close attention to length, but these magic numbers of 1 for
;;; alpha, 3 for beta... how do we compute them?
;;;
;;; There's a few different ways, but here we're going to use a
;;; (sparse) trie.  Each table in the trie will be indexed by a single
;;; character.  In a traditional trie, the zeroth entry in the array
;;; would be used to note when we're at the end of a word (so that "a"
;;; could be distinguished from "ant", for example).  Since we're a
;;; sparse hash, instead we'll just add a member to the trie node.
;;;
;;; We're also going to add a count to each node.  The count tells us
;;; how many words pass through the current node.  When following any
;;; given word through the trie, when we first see the count drop to
;;; 1, that's when we know we've found the minimum unique string
;;; length for the word we're chasing.
;;;
;;; For example, consider such a trie with a single word in it, "at".
;;; At the root of the trie, there would be a single subtrie
;;; associated with #\a; in this root node, the end flag is false, and
;;; the count of all words starting with #\a is 1.  Following #\that
;;; a, we have another node in the tree mapped at #\t.  Now, here, the
;;; count is still 1, but the end of word flag is true.
;;;
;;; 1 . ── a 1 . ── t 1 t
;;;
;;; Now, imagine adding "atom" to the trie.  The root and first #\a
;;; nodes would now have counts of 2, indicating that two words ("at"
;;; and "atom") pass through them.  The end of word flag is still set
;;; at the #\t node, indicating that #\a, #\t is a word.  However, in
;;; the #\t node, there is now a trie at #\o.  The count in this node
;;; is 1 (only "atom" is left), and the end of word flag is false
;;; ("ato" is not a word we've added).  In #\o, though, a trie appears
;;; at #\m; its count is 1, but it's end of word flag is true.
;;;
;;; 2 . ── a 2 . ── t 2 t ── o 1 . ── m 1 t
;;;
;;; Continuing the example, we'll add "add", "beat", "beta", "ignore",
;;; "input", and "zebra" to the trie.  t marks the terminating letters
;;; of stored words, blanks where the letter is just on the path of a
;;; word.  Arrows indicate a transition (pointer) from one trie table
;;; to another.  So, below, the root trie table has four entries, and
;;; all other trie tables contain a single node except at #\a and #\i
;;; and #\b, #\e.
;;;
;;; 8 . ─┬ a 3 . ─┬ d 1 . ── d 1 t
;;;      │        └ t 2 t ── o 1 . ── m 1 t
;;;      ├ b 2 . ── e 2 . ─┬ a 1 . ── t 1 t
;;;      │                 └ t 1 . ── a 1 t
;;;      ├ i 2 . ─┬ n 1 . ── p 1 . ── u 1 . ── t 1 t
;;;      │        └ g 1 . ── n 1 . ── o 1 . ── r 1 . ── e 1 t
;;;      └ z 1 . ── e 1 . ── b 1 . ── r 1 . ── a 1 t
;;;
;;; When on a word's path (e.g., #\a #\t #\o #\m), the nodes with a
;;; flag marked t indicate word terminations ("at" and "atom").  The
;;; first node to drop to 1 along any path indicates we've found a
;;; minimum unique prefix for the word being followed (3 for "atom").

(defclass dict (trie)
  ((termp :accessor dict-term-p :type boolean
	  :initarg :termp :initform nil
	  :documentation "Indicates when a given node (subtrie) in a
	  dictionary terminates the word spelled by the path to this
	  node.")
   (count :accessor dict-count :type integer
	  :initarg :count :initform 0
	  :documentation "Indicates how many words in the dictionary
	  pass through this particular node (subtrie)."))
  (:documentation "A Sparse Trie acting as a dictionary of stored
  words, specialized for Petulant's need to find minimum length unique
  substrings.  Words are added via DICT-ADD."))

(defun make-dict (&key loose)
  "Create a new empty dictionary.  The underlying hash table will use
  EQUAL to compare keys, unless LOOSE is true, which will cause the
  hash table to use EQUALP instead."
  (make-instance 'dict :table (make-hash-table
			       :test (if loose #'equalp #'equal))))

(defgeneric dict-add (dict word)
  (:documentation "Given a WORD, add it to the supplied dictionary."))

;;; Unsurprisingly, adding a word into a dictionary fits into a TCO
;;; definition quite nicely.

(defmethod dict-add ((dict dict) (word string))
  "Given a single WORD represented by a string, add it to the supplied
  dictionary DICT.  All necessary new subtries are added to DICT, all
  nodes along the \"path\" of the WORD are incremented, and the
  termination flag of the node corresponding to the end of that
  \"path\" is set.  DICT is returned."
  (labels ((add (dict word length idx)
	     (let ((subtrie (trie-at dict (char word idx))))
	       (incf (dict-count subtrie))
	       (if (< idx (1- length))
		   (add subtrie word length (1+ idx))
		   (setf (dict-term-p subtrie) t)))))
    (add dict word (length word) 0))
  dict)

#+nil
(defun words (dict)
  "Print all the words in DICT.  Just a debugging exercise."
  (let ((string (make-array 0 :element-type 'character :fill-pointer 0
			    :adjustable t)))
    (labels ((hack (dict)
	       (maphash #'(lambda (k v)
			    (vector-push-extend k string)
			    (when (dict-term-p v)
			      (print string))
			    (hack v)
			    (vector-pop string))
			(trie-table dict))))
      (hack dict))))

(defun minwords (dict)
  "Given the populated dictionary DICT, return a list describing all
the words that can be uniquely abbreviated and their minimum lengths.
Each element of the returned list is a list of three elements used in
evaluating a test string against the words in the dictionary.  The
first element is the minimum length of a test string, and the second
element is the maximum length; a test word shorter than the min or
larger than the max can never match the target word itself, which is
the third element of the list.

Not all words in the dictionary will appear in the returned list.  Only
those that can be usefully shortened while still being uniquely identified
will be returned.

So, if an element in the returned list was \(2 5 \"zebra\"\), any test
string of at least two characters, but no more than five, can be
compared against a substring of \"zebra\", and if it matches, the test
word can be considered a match for \"zebra\".  Thus, if the test word
were \"z\", it cannot be compared, it is too small \(there must have
been another word in the dictionary starting with a \"z\"\). If the
test word were \"zeb\" it would be compared against a substring of
\"zebra\" of the same length, where it would be found to match.  A
test word of six characters, say \"zebras\", cannot be compared as it
is too long."
  (let ((string (make-array 0 :element-type 'character :fill-pointer 0
			    :adjustable t))
	(result))
    (labels ((remainder (dict)
	       (maphash #'(lambda (k v)
			    (vector-push-extend k string)
			    (remainder v))
			(trie-table dict)))
	     (walk (dict)
	       (maphash #'(lambda (k v)
			    (vector-push-extend k string)
			    (cond
			      ((= 1 (dict-count v))
			       (let ((minlen (length string)))
				 (remainder v)
				 ;; when the minimum length is the same as
				 ;; the length of the target word, don't
				 ;; bother pushing this result: it's useless
				 ;; and just clutters up our results
				 (unless (>= minlen (length string))
				   (push (list minlen
					       (length string)
					       (copy-seq string)) result))
				 ;; restoring the fill pointer to MINLEN
				 ;; undoes the call to REMAINDER, and the 1-
				 ;; is effectively VECTOR-POP.
				 (setf (fill-pointer string) (1- minlen))))
			      (t
			       (walk v)
			       (vector-pop string))))
			(trie-table dict))))
      (walk dict))
    result))

;; No, this doesn't draw the nice picture above.  It's just for
;; debugging.

(defun show-dict (root &optional (indent 0))
  (let ((ind (+ 2 (* 3 indent))))
    (when (not (zerop indent))
      (format t "~vt~d ~a~%"
	      ind (dict-count root) (if (dict-term-p root) "T" " ")))
    (maphash #'(lambda (key val)
		 (format t "~vt~a:" ind key)
		 (show-dict val (1+ indent)))
	     (trie-table root))))


;; (defun leader (n m)
;;   "Return the leader for the nth entry in a trie of m items. It's
;;   assumed that n and m are both >= 1."
;;   (cond
;;     ((= m 1) "──")
;;     ((= n 1) "─┬")
;;     ((= m n) " └")
;;     (t       " ├")))

;; (defun hack (dict)
;;   (let ((cntwid (ceiling (log (1+ (dict-count dict)) 10)))
;; 	(items (hash-table-count (trie-table dict)))
;; 	(n 0))
;;     (format t "~v,'0d ~:[.~;T~]" cntwid (dict-count dict) (dict-term-p dict))
;;     (maphash #'(lambda (k v)
;; 		 (declare (ignore v))
;; 		 (format t " ~a ~c " (leader (incf n) items) k)
;; 		 (hack v)
;; 		 (terpri))
;; 	     (trie-table dict))))

;;; 8 . ─┬ a 3 . ─┬ d 1 . ── d 1 t
;;;      │        └ t 2 t ── o 1 . ── m 1 t
;;;      ├ b 2 . ── e 2 . ─┬ a 1 . ── t 1 t
;;;      │                 └ t 1 . ── a 1 t
;;;      ├ i 2 . ─┬ n 1 . ── p 1 . ── u 1 . ── t 1 t
;;;      │        └ g 1 . ── n 1 . ── o 1 . ── r 1 . ── e 1 t
;;;      └ z 1 . ── e 1 . ── b 1 . ── r 1 . ── a 1 t
