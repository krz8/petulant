(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:argv #:make-scanner #:scan
	   #:make-simple #:simple
	   #:collect
	   #:spec #:*options* #:*arguments*))
