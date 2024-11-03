;;;; lpd3-utils/15-sequences.lisp

(in-package :lpd3-utils)

(defun %destruc (pat seq &optional (atom? #'atom) (n 0))
  "Helper function for dbind."
  (if (null pat)
      nil
      (let ((rest 
              (cond 
                ((funcall atom? pat) pat)
                ((eq (car pat) '&rest)
                 (cadr pat))
                ((eq (car pat) '&body)
                 (cadr pat))
                (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
            (let ((p (car pat))
                  (rec 
                    (%destruc 
                      (cdr pat)
                      seq
                      atom?
                      (1+ n))))
              (if (funcall atom? p)
                  (cons
                   `(,p (elt ,seq ,n))
                   rec)
                  (let ((var (gensym)))
                    (cons
                     (cons
                      `(,var (elt ,seq ,n))
                      (%destruc
                       p
                       var
                       atom?))
                     rec))))))))

(defun %dbind-aux (binds body)
  "Helper function for dbind."
  (if (null binds)
      `(progn ,@body)
      `(let ,@(mapcar 
                #'(lambda (b)
                    (if (consp (car b))
                        (car b)
                        b))
                  binds)
         ,(%dbind-aux
           (mapcan 
             #'(lambda (b)
                 (when (consp (car b))
                     (cdr b)))
               binds)
           body))))

(defmacro dbind (pat seq &body body)
  "Like destructuring-bind, but can 
  handle any kind of sequence."
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(%dbind-aux 
           (%destruc pat gseq #'atom)
           body))))

(defun %with-places-aux (binds body)
  "Helper function for with-places"
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet
        ,(mapcar 
           #'(lambda (b)
               (if (consp (car b))
                   (car b)
                   b))
             binds)
         ,(%with-places-aux
           (mapcan 
             #'(lambda (b)
                 (when (consp (car b))
                   (cdr b)))
               binds)
           body))))

(defmacro with-places (pat seq &body body)
  "A more general destructuring macro
  for arbitrary sequences. Allows the
  various symbols in the pattern to 
  act like slots in an object, and thus,
  to be setfable. dbind is more efficient,
  and thus preferable, when not 
  changing the values in the seq arg."
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(%with-places-aux
         (%destruc pat gseq #'atom)
         body))))
           






            
                        