(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:argv #:make-parser #:parse #:process #:collect #:spec #:*options* #:*arguments*))
