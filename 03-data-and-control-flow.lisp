;;;; lpd3-utils/03-data-and-control-flow
                                       
(in-package :lpd3-utils)

(defun fif (if then &optional else)
  "Functional if. Takes an if function
  and a then function and an optional
  else function. Returns a function of 
  one arg that will invoke if on its arg, 
  and then either invoke then or else 
  (or return nil) depending on the 
  result of if."
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (when else
                (funcall else x)))))

(defun lrec (rec &optional base)
  "Used to build a function that recurs
  over a list. Takes a function, rec, 
  of two args, the first is the 
  current car and the second is a 
  a reference to the next recursion.
  Lrec also takes an optional
  base, which can be a function or an 
  object. If it is a function, it must
  take no args and will be called when the
  list is exhausted, otherwise base itself
  or nil is returned."
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall 
                   rec 
                   (car lst)
                   #'(lambda ()
                       (self (cdr lst)))))))
    #'self))
    
;; length for lists could be defined
;; (lrec #'(lambda (x f) (1+ (funcall f))) 0)
                                            
;; can be short-circuited using OR or 
;; AND

(defmacro while (test &body body)
  "(while (test &body body)). Execute
  body while test returns non-nil."
  `(do 
       ()
       ((not (funcall ,test)))
     ,@body))

(defmacro until (test &body body)
  "(until (test &body body). Execute body
  until test returns non-nil."
  `(do
       ()
       ((funcall test))
     ,@body))

(defmacro if3 (test t-case nil-case ?-case)
  "3-way if. Takes a test, a true clause, 
  a nil clause and a ? clause. Test must
  be able to return the symbol ? as well as
  other true values and nil."
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg)
  "Numeric if. Takes an expression, a positive 
  clause, a zero clause, and a negative 
  clause. Expression must return a real number
  that may be positive, negative or zero.
  Branches according to the sign of the
  result."
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond
        ((plusp ,g) ,pos)
        ((minusp ,g) ,neg)
        (t ,zero)))))

(defmacro in (obj &rest choices)
  "Takes an item obj followed by zero or
  more more items choices. Returns obj
  if obj is found in choices, else nil. Short 
  circuits."
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar
              #'(lambda (c)
                  `(eql ,insym ,c))
                choices)))))

(defmacro inq (obj &rest args)
  "Like IN, but first quotes its 
  args."
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))
                      
(defmacro in-if (fn &rest choices)
  "Given a predicate function and a 
  series of choices, returns the first
  element in choices that passes the 
  predicate, else nil."
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                       choices)))))

(defmacro for ((var start stop) &body body)
  "(for ((var start stop) &body body)). 
  var: symbol; start, stop: integers, 
  stop > start. Execute body repeatedly, 
  with var bound to the integers from
  start through stop."
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro allf (val &rest places)
  "Given a value and an arbitrary number
  of places, setf all the places to the 
  value."
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan 
                 #'(lambda (p)
                     (list p gval))
                   places)))))

(defmacro nilf (&rest places)
  "Given an arbitrary number of 
  places, setf all of them to nil."
  `(allf nil ,@places))

(defmacro tf (&rest places)
  "Given an arbitrary number of 
  places, setf all of them to T."
  `(allf t ,@places))

(define-modify-macro %toggle-aux () not
  "Helper modify-macro for toggle.")
  
(defmacro toggle (&rest places)
  "Given an arbitrary number of places, 
  setf them to the opposite of their 
  current (possibly generalized) Boolean
  value. That is, if the current value
  of the place is NIL, set it to T. 
  Otherwise, set it to NIL."
  `(progn
     ,@(mapcar #'(lambda (p)
                   `(%toggle-aux ,p))
                 places)))

(defmacro sort-places (op &rest places)
  "Given a comparison function of two
  args and an arbitrary number of 
  places, (destructively) exchange the 
  values in the places
  so that the order the places appear in
  the arg list matches the order of their
  contents as sorted by the function."
  (let* ((expansions 
           (mapcar 
             #'(lambda (p)
                 (multiple-value-list
                   (get-setf-expansion p)))
               places))
         (store-vars
          (apply
           #'append
           (mapcar #'third expansions))))
    `(let* ,(mapcar 
              #'list
              (mapcan
               #'(lambda (e)
                   (append
                    (first e)
                    (third e)))
                 expansions)
              (mapcan
               #'(lambda (e)
                   (append
                    (second e)
                    (list (fifth e))))
                 expansions))
       ,@(mapcon 
           #'(lambda (rest)
               (mapcar
                #'(lambda (arg)
                    `(unless (,op ,(car rest) ,arg)
                       (rotatef ,(car rest) ,arg)))
                  (cdr rest)))
             temps)
       ,@(mapcar #'fourth expansions))))

(defun most-of (&rest args)
  "Given an arbitrary number of args,
  return T if more than half evalauate to
  non-nil, else return NIL."
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (when a (incf hits)))
    (> hits (/ all 2))))
    
          
     
    


  
     
    


               