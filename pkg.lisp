(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:argv #:make-scanner #:scan
	   #:make-processor #:process
	   #:collect #:spec #:*options* #:*arguments*))
