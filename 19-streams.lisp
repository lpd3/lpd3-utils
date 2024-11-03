;;;; lpd3-utils/19-streams.lisp

(in-package :lpd3-utils)

(defun readlist (&rest args)
  "Intended for use with streams, especially
  user interaction. Takes an arbitrary number
  of args, constructs a string which is 
  a pair of parentheses enclosing all
  the args, then reads from the string. 
  Useful for allowing users to enter
  operations without enclosing them in 
  lists."
  (with-safe-io-syntax ()
    (values
     (read-from-string
      (concatenate 'string
        "("
        (apply #'read-line args)
        ")")))))

(defun prompt (format-control
               &rest format-args)
  "Prompt the user through *query-io*. 
  Takes an optional format control string
  and format args, all of which are used
  to construct the message. Returns the
  user's response."
  (with-safe-io-syntax ()
    (apply 
      #'format 
      *query-io* 
      format-control
      format-args)
    (read *query-io*)))

(defun break-loop (fn quit &rest args)
  "Establishes a program loop for user
  interaction. Takes a function to run
  on each loop, a function that will be the
  signal to quit, and args as for prompt."
  (format *query-io* "Entering break-loop.")
  (loop
    (let ((in (apply #'prompt args)))
      (when (funcall #'quit in)
        (return))
      (format *query-io* "~A~%" 
              (funcall fn in)))))

                         