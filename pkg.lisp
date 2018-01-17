(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:argv #:make-parser #:parse
	   #:make-processor #:process
	   #:collect #:spec #:*options* #:*arguments*))
