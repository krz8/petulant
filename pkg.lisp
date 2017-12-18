(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:parse #:process #:collect #:spec #:*options* #:*arguments*))
