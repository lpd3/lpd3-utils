;;;; lpd3-utils/06-structures
                            
(in-package :lpd3-utils)
            
(defun mkstr (&rest args)
  "Given a series of objects, 
  coerce and concatenate them into
  a single string. Mainly a 
  helper function for symb."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Given a series of objects, create
  and intern a symbol whose name
  is the coercion to strings and 
  concatenation of the objects. Mainly
  a helper function for with-struct."
  (values (intern (apply #'mkstr args))))

(defmacro with-struct ((name . fields)
                       struct &body body)
  "(with-struct ((name . fields) struct &body body))
  Like with-slota, but for structures. 
  The ``name'' argument is the conc-name,
  which often ends in a hyphen."
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) .gs)))
                       fields)
         ,@body))))