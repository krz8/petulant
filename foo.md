Foo.

```cl
(defvar *verbose* nil)
(defvar *input* nil)
(defvar *output* nil)

(defun args ()
  (flet ((handler (kind item extra)
           t))
    (parse-cli #'handler)
    (unless *input*
      (error "at least one argument must be supplied"))))
```

Foo.

```text
(defun args ()
  (flet ((handler (kind item extra)
           (declare (ignore extra))
           (case kind
             (:arg (cond
                     ((null *input*) (setf *input* item))
                     ((null *output*) (setf *output* item))
                     (t (error "too many arguments"))))
             (:opt (cond
                     ((string= "v" item) (setf *verbose* t))
                     (t (error "unknown option: ~a" item)))))))
    (parse-cli #'handler)
    (unless *input*
      (error "at least one argument must be supplied"))))))
```