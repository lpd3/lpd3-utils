;;;; lpd3-utils/24-miscellany.lisp
                                 
(in-package :lpd3-utils)

(defun %build-compose (fns)
  "Helper fn for build-fn"
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(%rbuild (car fns))
                         ,(rec (cdr fns)))
                        g)))
          (rec fns)))))

(defun %build-call (op fns)
  "Helper fn for build-fn."
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(%rbuild f) ,g))
                        fns)))))

(defun %rbuild (expr)
  "Helper function for build-fn."
  (if (or (atom expr)
          (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (%build-compose (cdr expr))
          (%build-call (car expr) (cdr expr)))))

(defmacro build-fn (expr)
  "General function builder. Takes an 
  expression containing an operation
  to perform on the other args, which 
  could be functions. Alternatively, the
  first element could be 'compose, in which
  case, a composition of the supplied 
  functions is returned."
  `#',(%rbuild expr))

(defmacro abbrev (short long)
  "Given two symbols, the second already
  interned and assigned to an operator, 
  grants the first symbol the same 
  functionality (mostly) as the second.
  Note that the abbreviation will be 
  bound to a macro, and thus cannot be
  directly passed as an argument."
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  "Given an even number of symbols, 
  with those at the zero-based 1st, 3rd, 
  etc. positions assigned to operators, 
  assign macros to the 0th, 2nd, etc 
  symbols to match the operator of the
  second. Abbreviations are macros and 
  cannot be passed as args."
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
                 (batches names 2))))

;;; Continuations follow. These require
;;; the global lexically scoped variable
;;; *cont*

#| Rules for use of the continuation
macros:

1. The parameter list of a function created
   with =defun, if not empty, must contain
   only parameter names.
   
2. Functions which make use of continuations,
   or call other functions that do, must be
   defined with =defun or =lambda.
   
3. Functions defined with =defun or =lambda 
   must either terminate in a call to =values,
   or in a call to a function that ends in
   a call to =values.
   
4. Calls to =bind, =values, =apply, or 
   =funcall must occur in tail position.
   If further action is needed, it must
   be accomplished by nesting.
   
|#
   

(setq *cont* #'identity)

(defmacro =lambda (parms &body body)
  "Use this instead of lambda when 
  creating anonymous functions that
  are continuations."
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  "use this instead of defun when defining
  new functions that employ continuations.
  All functions so defined must end in a 
  call to =values, or to another function
  that ends in a call to =values. Only
  required parameters are accepted."
  (let ((f (intern (concatenate 'string
                     "=" 
                     (symbol-name name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  "``Receives'' values from =values. Like
   multiple-value-bind, it can receive 
   multiple values, however, it cannot
   return multiple values to toplevel."
   `(let ((*cont* #'(lambda ,parms ,@body)))
      ,expr))
      
(defmacro =values (&rest retvals)
  "Used to return a value or values at the
  end of a function defined with =defun."
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  "Use this rather than funcall when 
  invoking passed functions in continuations."
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  "Use this rather than apply when invoking
  a function defined with =defun on 
  args contained in lists."
  `(apply ,fn *cont* ,@args))
  
#| The following macros implement 
nondeterminism. They rely on the continuationd
macros above. |#
       
(defparameter *paths* nil
  "As yet unexplored computational paths.")

(defconstant +failsym+ '@
  "Symbol to be passed when a computational
  path fails.")

(defun fail ()
  "Called when a computational path fails.
  If there are still unexplored paths, pops
  the first one and tries again. Otherwise,
  returns +failsym+."
  (if *paths*
      (funcall (pop *paths*))
      +failsym+))

(defmacro choose (&rest choices)
  "One of two ``amb'' operators. This
  one is easiest to use when the choices
  are plain data."
  (if choices
      `(progn
         ,@(mapcar #'(lambda (c)
                       `(push 
                          #'(lambda ()
                              ,c)
                          *paths*))
                     (reverse (cdr choices)))
         ,(car choices))
      '(fail)))

(defun %cb (fn choices)
  "Helper function for choose-bind"
  (if choices
      (progn
        (when (cdr choices)
          (push 
            #'(lambda ()
                (%cb fn (cdr choices)))
            *paths*))
        (funcall fn (car choices)))
      (fail)))

(defmacro choose-bind (var choices &body body)
  "The second of two ``amb'' operators. This
  one facilitates pushing functions to 
  paths, and running code with a 
  lexical binding "
  `(%cb #'(lambda (,var) ,@body) ,choices))
            
                  