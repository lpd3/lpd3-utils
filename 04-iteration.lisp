;;;; lpd3-utils/04-iteration
                           
(in-package :lpd3-utils)

(defun %mvdo-rebind-gen (rebinds)
  "Helper function for mv-do*."
  (cond 
    ((null rebinds) nil)
    ((<= (length (car rebinds)) 3)
     (%mvdo-rebind-gen (cdr rebinds)))
    (t
     (cons
      (list
       (if (atom (caar rebinds))
           'setq
           'multiple-value-setq)
       (caar rebinds)
       (third (car rebinds)))
      (%mvdo-rebind-gen (cdr rebinds))))))
            
(defun %mvdo-gen (binds rebinds test body)
  "Helper function for mv-do*."
  (if (null binds)
      (let ((label (gensym)))
        `(progn nil
            ,label
            (if ,(car test)
                (return (progn ,@(cdr test))))
            ,@body
            ,@(%mvdo-rebind-gen rebinds)
            (go ,label)))
      (let ((rec (%mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds))
              (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))     

(defmacro mvdo* (parm-clause test-clause &body body)
  "Like DO*, but allows access to multiple
  values."
  (%mvdo-gen parm-clause test-clause body))
  
(defun %shuffle (x y)
  "Helper function for mvpsetq"
  (cond ((null x) y)
        ((null y) x)
        (t
         (list* (car x) (car y)
                (%shuffle (cdr x) (cdr y))))))

(defmacro mvpsetq (&rest args)
  "Like multiple-value-serq, but bindings are executed
  in parallel. Principally a helper macro
  for mvdo."
  (let* ((pairs (batches args 2 :strict nil))
         (syms (mapcar 
                 #'(lambda (p)
                     (mapcar
                      #'(lambda (x)
                          (gensym))
                        (ensure-list
                         (car p))))
                   pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq 
                      ,@(mapcan 
                          #'(lambda (p s)
                              (%shuffle
                                (ensure-list
                                 (car p))
                                s))
                            pairs
                            syms))
                    (let ((body
                           (rec
                            (cdr ps)
                            (cdr ss))))
                      (let ((var/s (caar ps))
                            (expr (cadar ps)))
                        (if (consp var/s)
                            `(multiple-value-bind
                                 ,(car ss)
                                 ,expr
                               ,body)
                            `(let (( ,@(car ss)
                                     ,expr))
                               ,body)))))))
      (rec pairs syms))))

(defmacro mvdo ((binds &rest result) &body body)
  "Like DO, but permits multiple values."
  (let ((label (gensym))
        (temps (mapcar
                #'(lambda (b)
                    (if (listp (car b))
                        (mapcar
                         #'(lambda (x)
                             (gensym))
                           (car b))
                        (gensym)))
                  binds)))
    `(let ,(mappend #'ensure-list temps)
       (mvpsetq ,@(mapcan
                   #'(lambda (b var)
                       (list var (cadr b)))
                     binds
                     temps))
       (prog ,(mapcar
               #'(lambda (b var)
                   (list b var))
               (mappend 
                 #'ensure-list
                 (mapcar #'car binds))
               (mappend
                 #'ensure-list
                 temps))
         ,label
         (when ,test
           (return (progn ,@result)))
         ,@body
         (mvpsetq ,@(mapcan
                     #'(lambda (b)
                         (when
                           (third b)
                           (list
                            (car b)
                            (third b))))
                       binds))
         (go ,label)))))
             
               
       
                               
                               
    
               


