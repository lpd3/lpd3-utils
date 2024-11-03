;;;; lpd3-utils/13-arrays

(in-package :lpd3-utils)
            
(defmacro with-matrix (pats ar &body body)
  "Similar to destructuring-bind, but 
  for two-dimensiinal arrays. The 
  destructuring pattern is nested lists."
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan
                #'(lambda (pat)
                    (incf row)
                    (setq col -1)
                    (mapcar
                     #'(lambda (p)
                         `(,p 
                             (aref 
                               ,gar
                               ,row
                               ,(incf col))))
                       pat))
                  pats))
         ,@body))))
         
(defmacro with-array (pat ar &body body)
  "Destructuring for arrays of arbitrary
  rank. The destructuring pattern consists
  of a nested list, where each inner list
  begins with a symbol--the var, and then
  the indices of the item to which the 
  symbol will be bound."
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar 
               #'(lambda (p)
                   `(,(car p) 
                     (aref 
                       ,gar
                       ,@(cdr p))))
                 pat)
         ,@body))))
                        
