(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export #:simple #:parse #:collect #:spec #:*options* #:*arguments*))
