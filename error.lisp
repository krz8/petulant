(in-package #:petulant)

(defmacro err (x &rest y)
  `(error ,(strcat "PETULANT: " x) ,@y))

(defmacro wrn (x &rest y)
  `(warn ,(strcat "PETULANT: " x) ,@y))
