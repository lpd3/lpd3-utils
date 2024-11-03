;;; lpd3-utils/01-evaluation-and-complilation
                                            
(in-package :lpd3-utils)

(defun memoize (fn)
  "Given a function, returns a memoized
  version of the function. Stores prior
  results in a hash table for lookup rather
  than recalculation."
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind
          (value presentp)
          (gethash args cache)
        (if presentp
            value
            (setf
             (gethash args cache)
             (apply fn args)))))))
             
(defconstant +unforced+ (gensym)
  "Symbol used to indicate that a delay
  has not yet been forced.")

(defstruct delay
  "A delay object, used to contain data
  that is to be evaluated lazily."
  (forced closure))

(defmacro delay (expr)
  "Given an expression, place the unevaluated
  expression in a delay struct, to be
  evaluated at a later time."
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced +unforced+)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self)
                       ,expr)))
       ,self)))

(defun force (x)
  "Given an object, if object is a delay,
  evaluate and return the value of its 
  expression, otherwise, just return the
  object."
  (if (delay-p x)
      (if (eq (delay-forced x) +unforced+)
          (funcall (delay-closure x))
          (delay-forced x))
      x))
  
        