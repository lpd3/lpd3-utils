;;;; lpd3-utils/12-conses
                        
(in-package :lpd3-utils)

(defun find2 (fn lst)
  "Given a function and a list, return
  two values: the first item in lst that 
  returns non-nil under fn, and the
  non-nil value. Otherwise, return nil."
  (if (null lst)
      nil
      (let ((value (funcall fn (car lst))))
        (if value
            (values (car lst) value)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Given two items x and y and a list, 
  is x present and found earlier in the
  list than y (which need not be present)?
  If so, returns the tail beginning with
  x.
  Takes a :test kwarg, defaulting to eql."
  (and lst
    (let ((first (car lst)))
      (cond
       ((funcall test y first) nil)
       ((funcall test x first) lst)
       (t
        (before x y (cdr lst) test))))))

(defun after (x y lst &key (test #'eql))
  "Given two items x and y and a list, 
  are both items present in the list 
  and does x appear after y? If so, 
  returns the tail of the list beginning
  with x. Takes 
  a :test kwarg, defaulting to eql."
  (let ((rest (before y x lst test)))
    (and rest (member x rest :test test))))

(defun duplicatep (obj lst &key (test #'eql))
  "Takes an item and a list. If item appears
  more than once in the list, return the 
  tail of the list beginning with the 
  second occurrence of the object. 
  Takes a :test kwarg, defaulting to #'eql"
  (member obj (cdr (member obj lst :test test))
              :test test))

(defun split-if (fn lst)
  "Given a function and a list, returns
  two lists: the initial items that return
  nil under fn, and the rest of the input
  list beginning with the first item that
  returns non-nil under fn."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  "Given a function that returns a real
  number and a list, returns two values: 
  the first item in list that returned the
  maximum value under function, and that
  value."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun mostn (fn lst)
  "Takes a function that returns a real
  number and a list. Returns two values:
  a list containing every item in the 
  input list that tied for producing the
  highest value under function and that 
  value."
  (if (null list)
      (values nil nil)
      (let ((result (list (car lst)))
             (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond
             ((> score max)
              (setf result (list obj)))
             ((= score max)
              (push obj result)))))
        (values (nreverse result) max))))

(defun mapcars (fn &rest lsts)
  "Takes a function of one arg and an arbitrary 
  number of lists. Returns a list which
  is the collected results of applying
  the function to each item in the first
  list, followed by the results from 
  the second list, etc., until the 
  last list is exhausted."
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun mapa-b (fn a b)
  "(mapa-b (fn a b). Returns a list
  containing the results of invoking fn 
  on a, then a+1, ... b."
  (do ((i a (1+ i))
       (results nil (push (funcall fn i) results)))
      ((> i b) (nreverse results))))

(defun map0-n (fn n)
  "(map0-n (fn n)). Return a list
  of the results of invoking fn on 0, 1, 
  2, ..., n."
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "(map1-n (fn n)). Return a list of 
  the results of invoking fn on 1, 2, 3, ...,
  n."
  (mapa-b fn 1 n))

(defun map-> (fn start test succ)
  "Takes a function of one arg, fn, 
  a start value, and two more functions
  of one value, test and succ. Returns 
  a list. Invokes test on start. It test
  fails, returns nil. Else, invokes
  fn on start and puts the result in
  the return list. Then calls succ on
  start to find the next object. Continues
  accumulating results until test returns
  non-nil."
  (do ((i start (funcall succ i))
       (result nil))
      ((funcall test i) (nreverse result))
    (push (funcall fn i) result)))

(defmacro do-tuples (parms source &body body)
  "Given a list of symbols, a list, and a 
  body, repeatedly executes body. Let 
  n = length(parms). Then, on the first
  iteration the parms are bound to elts
  0 through n-1 of list. On the next, 
  parms are bound to elts 1-n of the list.
  Iteration continues until the last 
  elt has been bound to the last parm."
  (if parms
    (let ((src (gensym)))
      `(prog ((,src ,source))
         (mapc #'(lambda ,parms ,@body)
           ,@(map0-n #'(lambda (n)
                           `(nthcdr ,n ,src))
                       (1- (length parms))))))))
                       
(defun %dt-args (len rest src)
  "Helper function for do-tuples*"
  (map0-n
   #'(lambda (m)
       (map1-n
         #'(lambda (n)
             (let ((x (+ m n)))
               (if (>= x len)
                   `(nth ,(- x len) ,src)
                   `(nth ,(1- x) ,rest))))
           len))
     (- len 2)))

(defmacro do-tuples* (parms source &body body)
  "Similar to do-tuples, but ``wraps around'',
  i.e., the last iteration binds the first
  parm to the last element of the list,
  and the rest from the front of the list."
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                     ,@(mapcar #'(lambda (args)
                                   `(,bodfn ,@args))
                                 (%dt-args len rest src))
                       nil)
                  (,bodfn ,@(map1-n #'(lambda (n)
                                          `(nth ,(1- n) 
                                                ,rest))
                                        len))))))))))

(defmacro pull (obj place &rest args)
  "Given an item, a place containing 
  a list, and optional args as for 
  DELETE, destructively remove one
  or more instances of the item from
  the list at place."
  (multiple-value-bind
      (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars vals)
              (,(car store-vars)
               (delete ,g ,reader-form ,@args)))
         ,writer-form))))

(defmacro pull-if (test place &rest args)
  "Given a predicate test, a place 
  containing a list, and optional args
  as for DELETE-IF, destructively remove
  all items that pass the test from the
  list at place."
  (multiple-value-bind
      (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let ((,g ,test)
             ,@(mapcar #'list vars vals)
             (,(car store-vars)
              (delete-if ,g ,reader-form ,@args)))
         ,writer-form))))

(defmacro popn (n place)
  "Given a non-negative integer n and 
  a place containing a list, destructively
  remove the first n items from the list
  at place, returning them in a list."
  (multiple-value-bind
      (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place)
    (with-gensyms (gn glist)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars vals)
              (,glist ,reader-form)
              (,(car store-vars)
               (nthcdr ,gn ,glist)))
         (prog1 
           (subseq ,glist 0 ,gn)
           ,writer-form)))))

(defmacro propmacro (propname)
  "Given a plist keyname, create
  an access macro with the same name."
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest propnames)
  "Given a series of plist keys, create
  access macros with the same names."
  `(progn
     ,@(mapcar #'(lambda (p)
                   `(propmacro ,p))
                 propnames)))
           
             

                                          

           
  
      
  