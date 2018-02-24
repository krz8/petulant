(defpackage #:petulant
  (:use #:cl #:anaphora #:iterate #:alexandria)
  (:nicknames #:cli)
  (:export

   #:clispec #:name #:flagopt
   #:style #:partial
   
   #:argv #:make-scanner #:scan
   #:make-simple #:simple
   #:collect
   #:spec #:*options* #:*arguments*))
