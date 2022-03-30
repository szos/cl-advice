;;;; cl-advice.lisp

(in-package #:cl-advice)

;;;;;;;;;;;;;;
;;; Macros ;;;
;;;;;;;;;;;;;;

(defmacro when-let1 ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym)
                   `(,sym (gensym ,(symbol-name sym))))
          syms)
     ,@body))

;;;;;;;;;;;;;;;
;;; Utility ;;;
;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun argument-list-to-apply-list (argslist)
    "This function is for use a macroexpansion time. It takes an ordinary lambda 
list and parses it into code to generate a list suitable for use with apply"
    ;; This is ugly, and uses recursion - CL spec does not specify TCO. 
    (labels ((mkword (symbol) (intern (symbol-name symbol) :keyword))
             (parse-&optional (args accum)
               (typecase (car args)
                 ((member &rest &key &aux)
                  (case (car args)
                    (&rest (reverse (cons (cadr args) accum)))
                    (&key (parse-&key (cdr args) accum))
                    (&aux (reverse accum))))
                 (cons
                  (case (length (car args))
                    ((1 2) (parse-&optional (cdr args)
                                            (cons (list 'list (caar args))
                                                  accum)))
                    (3 (reverse (cons (list 'when (caddar args)
                                            (cons 'append
                                                  (cons (list 'list (caar args))
                                                        (parse-&optional (cdr args) nil))))
                                      accum)))))
                 (null (reverse accum))
                 (symbol (parse-&optional (cdr args)
                                          (cons (list 'list (car args))
                                                accum)))))
             (parse-&key (args accum)
               (typecase (car args)
                 ((or null (member &aux)) (reverse accum))
                 (cons (case (length (car args))
                         ((1 2)
                          (parse-&key (cdr args)
                                      (cons (list 'list (mkword (caar args))
                                                  (caar args))
                                            accum)))
                         (3
                          (parse-&key (cdr args)
                                      (cons (list 'when (caddar args)
                                                  (list 'list (mkword (caar args))
                                                        (caar args)))
                                            accum)))))
                 (symbol (parse-&key (cdr args)
                                     (cons (list 'list (mkword (car args))
                                                 (car args))
                                           accum)))))
             (parse (args accum)
               (case (car args)
                 (&optional (parse-&optional (cdr args) accum))
                 (&rest (reverse (cons (cadr args) accum)))
                 (&key (parse-&key (cdr args) accum))
                 (&aux (reverse accum))
                 (otherwise
                  (if (and (symbolp (car args))
                           (not (null (car args))))
                      (parse (cdr args) (cons (list 'list (car args)) accum))
                      (reverse accum))))))
      (cons 'append (parse argslist nil))))

  (defun generate-ignore-declarations (argument-list)
    (labels ((collect-aux (arglist accum)
               (if (null arglist)
                   accum
                   (let ((arg (car arglist)))
                     (if (atom arg)
                         (collect-aux (cdr arglist) (cons arg accum))
                         (collect-aux (cdr arglist) (cons (car arg) accum))))))
             (collect-keys (arglist accum)
               (if (null arglist)
                   accum
                   (let ((arg (car arglist)))
                     (cond ((atom arg)
                            (case arg
                              (&aux (collect-aux (cddr arglist) accum))
                              (&allow-other-keys (collect-aux (cddr arglist) accum))
                              (t (collect-keys (cdr arglist) (cons arg accum)))))
                           ((cddr arg)
                            (collect-keys (cdr arglist)
                                          (cons (car arg)
                                                (cons (caddr arg)
                                                      accum))))
                           (t (collect-keys (cdr arglist)
                                            (cons (car arg) accum)))))))
             (collect-after-rest (arglist)
               (let ((arg (car arglist)))
                 (case arg
                   (&key (collect-keys (cdr arglist) nil))
                   (&aux (collect-aux  (cdr arglist) nil))
                   (otherwise nil))))
             (parse (arglist)
               (let ((rest (member '&rest arglist)))
                 (when rest
                   (collect-after-rest (cddr rest))))))
      (unless (eql argument-list :not-provided)
        (let ((ignoring (parse argument-list)))
          (when ignoring
            `((declare (ignore ,@ignoring)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Advisable Function Class ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass advisable-function ()
  ((arglist :initarg :arguments
            :accessor advisable-function-arguments)
   (main    :accessor advisable-function-main)
   (before  :initarg :before
            :accessor advisable-function-before
            :initform nil)
   (around  :initarg :around
            :accessor advisable-function-around
            :initform nil)
   (after   :initarg :after
            :accessor advisable-function-after
            :initform nil))
  (:metaclass c2mop:funcallable-standard-class))

(define-condition advisable-function-initialization-error (error)
  ((advisable-function :initarg :advisable-function
                       :reader
                       advisable-function-initialization-error-advisable-function)
   (main-function :initarg :function
                  :reader advisable-function-initialization-error-function))
  (:report
   (lambda (c s)
     (format s
             "Expected a normal function when initializing object ~A, but got ~A"
             (advisable-function-initialization-error-advisable-function c)
             (advisable-function-initialization-error-function c)))))

;; Define a documentation method for advisable function objects which returns
;; the documentation of all functions except the dispatcher
(macrolet
    ((document-advisable-function (obj)
       `(flet ((document-function (fn)
                 (documentation fn 'function)))
          (append
           (list :before)
           (when (advisable-function-before ,obj)
             (remove nil (mapcar #'document-function
                                 (advisable-function-before ,obj))))
           (list :around)
           (when (advisable-function-around ,obj)
             (remove nil (mapcar #'document-function
                                 (advisable-function-around ,obj))))
           (list :main (document-function (advisable-function-main ,obj)))
           (list :after)
           (when (advisable-function-after ,obj)
             (remove nil (mapcar #'document-function
                                 (advisable-function-after ,obj))))))))
  (defmethod cl:documentation ((obj advisable-function) (doctype (eql t)))
    (document-advisable-function obj))
  (defmethod cl:documentation ((obj advisable-function) (doctype (eql 'function)))
    (document-advisable-function obj)))

(defmethod initialize-instance :around
    ((obj advisable-function) &key main &allow-other-keys)
  "Normalize the function being advised to be a function object"
  (let ((normalized-main (typecase main
                           (symbol (symbol-function main))
                           (otherwise main))))
    (if (and (functionp normalized-main)
             (not (typep normalized-main 'advisable-function)))
        (progn
          (setf (slot-value obj 'main) normalized-main)
          (call-next-method))
        (error 'advisable-function-initialization-error
               :function main
               :advisable-function obj))))

(defmethod initialize-instance :after
    ((obj advisable-function)
     &key dispatcher-generator force-accurate-dispatcher-arglist &allow-other-keys)
  "Set the funcallable instance function. 

If DISPATCHER-GENERATOR is provided it must be a function of arity one which
will be passed the advisable function object and must return a dispatcher
function. 

Otherwise if FORCE-ACCURATE-DISPATCHER-ARGLIST is T and arguments were provided,
then EVAL is used to generate a dispatcher function with the correct argument
list.

Otherwise a generic dispatcher is used which takes a rest argument."
  (c2mop:set-funcallable-instance-function
   obj
   (cond (dispatcher-generator
          (funcall dispatcher-generator obj))
         ((and force-accurate-dispatcher-arglist
               (not (eql (advisable-function-arguments obj) :not-provided)))
          (eval `(lambda ,(advisable-function-arguments obj)
                   "Advisable function dispatcher"
                   ,@(generate-ignore-declarations
                      (advisable-function-arguments obj))
                   (let ((fixed ,(argument-list-to-apply-list
                                  (advisable-function-arguments obj))))
                     (apply-before ,obj fixed)
                     (multiple-value-prog1 (apply-around ,obj fixed)
                       (apply-after ,obj fixed))))))
         (t (lambda (&rest arguments)
              "Advisable function dispatcher"
              (apply-before obj arguments)
              (multiple-value-prog1 (apply-around obj arguments)
                (apply-after obj arguments)))))))

(defun advisable-function-p (object)
  "Check if OBJECT is an advisable function"
  (typep object 'advisable-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice Calling Logic ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-around-caller (function-list)
  (lambda (&rest args)
    (if (cdr function-list)
        (apply (car function-list)
               (cons (generate-around-caller (cdr function-list))
                     args))
        (apply (car function-list) args))))

(defun apply-before (obj args)
  (loop for fn in (advisable-function-before obj)
        do (apply fn args)))

(defun apply-around (obj args)
  (apply (generate-around-caller
          (append (advisable-function-around obj)
                  (list (advisable-function-main obj))))
         args))

(defun apply-after (obj args)
  (loop for fn in (advisable-function-after obj)
        do (apply fn args)))

(defun make-advisable (symbol &key (arguments :not-provided args-provided-p)
                                force-use-arguments)
  "Make the function denoted by SYMBOL an advisable function"
  (check-type symbol (or symbol function))
  (let ((fn (make-instance 'advisable-function
                           :main symbol
                           :arguments arguments
                           :force-accurate-dispatcher-arglist
                           (and args-provided-p force-use-arguments))))
    (if (and (typep symbol 'symbol) fn)
        (restart-case (setf (symbol-function symbol) fn)
          (abort ()
            :report (lambda (s)
                      (format s "Abort conversion of ~A to be advisable"
                              symbol))
            fn))
        fn)))

(define-compiler-macro make-advisable
    (&whole whole symbol &key arguments force-use-arguments)
  (declare (ignore force-use-arguments))
  (if (atom arguments)
      whole
      `(let ((fn (make-instance
                  'advisable-function
                  :main ,symbol
                  :arguments ,arguments
                  :dispatcher-generator
                  (lambda (obj)
                    (lambda ,(cadr arguments)
                      ,(format nil "Advisable function dispatcher for ~A" symbol)
                      ,@(generate-ignore-declarations (cadr arguments))
                      (let ((fixed ,(argument-list-to-apply-list
                                     (cadr arguments))))
                        (apply-before obj fixed)
                        (multiple-value-prog1 (apply-around obj fixed)
                          (apply-after obj fixed))))))))
         (if (and (typep ,symbol 'symbol) fn)
             (restart-case (setf (symbol-function ,symbol) fn)
               (abort ()
                 :report (lambda (s)
                           (format s "Abort conversion of ~A to be advisable"
                                   ,symbol))
                 fn))
             fn))))

(define-condition not-an-advisable-function (error)
  ((fn :initarg :function :reader not-an-advisable-function-function))
  (:report
   (lambda (c s)
     (format s "~A is not an advisable function"
             (not-an-advisable-function-function c)))))

(defun make-unadvisable (symbol)
  (check-type symbol symbol)
  (let ((fn (symbol-function symbol)))
    (if (typep fn 'advisable-function)
        (setf (symbol-function symbol)
              (advisable-function-main fn))
        (error 'not-an-advisable-function :function symbol))))

(defun copy-advice (fn1 fn2)
  "DESTRUCTIVELY Copy all advice from FN1 to FN2"
  (setf (advisable-function-before fn2) (advisable-function-before fn1)
        (advisable-function-around fn2) (advisable-function-around fn1)
        (advisable-function-after  fn2) (advisable-function-after  fn1)))

(defmacro advisable-lambda (argslist &body body)
  (with-gensyms (fobj fixed)
    `(let ((,fobj (make-advisable-function (lambda ,argslist ,@body)
                                           :arguments ',argslist)))
       (install-advisable-function-dispatcher
        ,fobj
        (lambda ,argslist
          ,@(generate-ignore-declarations argslist)
          (let ((,fixed ,(argument-list-to-apply-list argslist)))
            (apply-before ,fobj ,fixed)
            (multiple-value-prog1
                (apply-around ,fobj ,fixed)
              (apply-after ,fobj ,fixed)))))
       ,fobj)))

(defmacro defun-advisable (name argslist &body body)
  "Define a function as an advisable function - works the same as DEFUN."
  (with-gensyms (oldfn)
    `(let ((,oldfn (handler-case (symbol-function ',name)
                     (undefined-function () nil))))
       (defun ,name ,argslist ,@body)
       (make-advisable ',name :arguments ',argslist)
       (when (and ,oldfn
                  (not (eql (advisable-function-arguments ,oldfn) :not-provided))
                  (equal (advisable-function-arguments ,oldfn) ',argslist))
         (copy-advice ,oldfn (symbol-function ',name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implicit Conversion ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *allow-implicit-conversion* t
  "Controls whether variables can implicitly be converted to advisable
functions.  When NIL implicit conversion signals an error.")

(define-condition implicit-conversion-to-advisable-function (error)
  ((function-being-converted :initarg :function :reader function-being-converted))
  (:report
   (lambda (c s)
     (format s "~A is being implicitly converted to an advisable function"
             (function-being-converted c)))))

(defun ensure-advisable-function (symbol &optional arguments force-use-arguments)
  (check-type symbol (or symbol function))
  (let ((fn (if (symbolp symbol)
                (symbol-function symbol)
                symbol)))
    (flet ((convert ()
             (make-advisable symbol
                             :arguments arguments
                             :force-use-arguments force-use-arguments)))
      (cond ((advisable-function-p fn)
             fn)
            (*allow-implicit-conversion*
             (convert))
            (t (restart-case (error 'implicit-conversion-to-advisable-function
                                    :function symbol)
                 (allow-conversion ()
                   (convert))
                 (return-value (value)
                   :report (lambda (stream)
                             (format stream
                                     "Provide a value to return from ensure-advisable-function"))
                   :interactive (lambda ()
                                  (format *query-io* "Enter a value: ")
                                  (multiple-value-list (eval (read))))
                   :test (lambda (condition)
                           (typep condition
                                  'implicit-conversion-to-advisable-function))
                   (return-from ensure-advisable-function value))))))))

(defun ensure-unadvisable-function (symbol)
  (handler-case (make-unadvisable symbol)
    (not-an-advisable-function () nil)))

(defmacro with-implicit-conversion
    ((allow-or-not &optional abort-on-implicit-conversion return-on-abort)
     &body body)
  "Allow or disallow implicit conversions to advisable functions.  

If ALLOWED-OR-NOT is :allowed then conversions are allowed, otherwise they are
disallowed.

If ABORT-ON-IMPLICIT-CONVERSION is T, a handler is established for implicit
conversion errors which immediately returns RETURN-ON-ABORT."
  (with-gensyms (blockname c)
    `(let ((*allow-implicit-conversion* (eql ,allow-or-not :allowed)))
       ,@(if abort-on-implicit-conversion
             `((block ,blockname
                 (handler-bind ((implicit-conversion-to-advisable-function
                                  (lambda (,c)
                                    (declare (ignore ,c))
                                    (return-from ,blockname ,return-on-abort))))
                   (locally ,@body))))
             body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and Removing Advice ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-advice-around (function advice-fn &key allow-duplicates (test 'eql) from-end)
  (let* ((advise (ensure-advisable-function function))
         (list (advisable-function-around advise))
         (loc (member advice-fn list :test test)))
    (unless (symbolp advice-fn)
      (warn "advising ~A with anonymous function" function))
    (if loc
        (if allow-duplicates
            (if from-end
                (setf (cdr (last list)) (list advice-fn))
                (push advice-fn (advisable-function-around advise)))
            (setf (car loc) advice-fn))
        (if from-end
            (setf (cdr (last list)) (list advice-fn))
            (push advice-fn (advisable-function-around advise))))))

(defun add-advice-before (function advice-fn &key allow-duplicates (test 'eql) from-end)
  (let* ((advise (ensure-advisable-function function))
         (list (advisable-function-before advise))
         (loc (member advice-fn list :test test)))
    (unless (symbolp advice-fn)
      (warn "advising ~A with anonymous function" function))
    (if loc
        (if allow-duplicates
            (if from-end
                (setf (cdr (last list)) (list advice-fn))
                (push advice-fn (advisable-function-before advise)))
            (setf (car loc) advice-fn))
        (if from-end
            (setf (cdr (last list)) (list advice-fn))
            (push advice-fn (advisable-function-before advise))))))

(defun add-advice-after (function advice-fn &key allow-duplicates (test 'eql) from-end)
  (let* ((advise (ensure-advisable-function function))
         (list (advisable-function-after advise))
         (loc (member advice-fn list :test test)))
    (unless (symbolp advice-fn)
      (warn "advising ~A with anonymous function" function))
    (if loc
        (if allow-duplicates
            (if from-end
                (setf (cdr (last list)) (list advice-fn))
                (push advice-fn (advisable-function-after advise)))
            (setf (car loc) advice-fn))
        (if from-end
            (setf (cdr (last list)) (list advice-fn))
            (push advice-fn (advisable-function-after advise))))))

(defun add-advice (where function advice-function &key allow-duplicates (test 'eql)
                                                    from-end)
  "Add ADVICE-FUNCTION to FUNCTION. WHERE must be one of :before, :around, or 
:after. If ALLOW-DUPLICATES is true, advice will be added regardless. TEST is used
to check if ADVICE-FUNCTION is already present. When FROM-END is true, advice will
be appended instead of prepended."
  (case where
    (:before (add-advice-before function advice-function
                                :allow-duplicates allow-duplicates
                                :test test :from-end from-end))
    (:around (add-advice-around function advice-function
                                :allow-duplicates allow-duplicates
                                :test test :from-end from-end))
    (:after (add-advice-after function advice-function
                              :allow-duplicates allow-duplicates
                              :test test :from-end from-end))))

(defun replace-advice-before (function old-advice new-advice test if-not-found)
  (let* ((advise (ensure-advisable-function function))
         (loc (member old-advice (advisable-function-before advise) :test test)))
    (if loc
        (setf (car loc) new-advice)
        (case if-not-found
          (:prepend (push new-advice (advisable-function-before advise)))
          (:append (setf (advisable-function-before advise)
                         (append (advisable-function-before advise)
                                 (list new-advice))))
          (otherwise nil)))))

(defun replace-advice-around (function old-advice new-advice test if-not-found)
  (let* ((advise (ensure-advisable-function function))
         (loc (member old-advice (advisable-function-around advise) :test test)))
    (if loc
        (setf (car loc) new-advice)
        (case if-not-found
          (:prepend (push new-advice (advisable-function-around advise)))
          (:append (setf (advisable-function-around advise)
                         (append (advisable-function-around advise)
                                 (list new-advice))))
          (otherwise nil)))))

(defun replace-advice-after (function old-advice new-advice test if-not-found)
  (let* ((advise (ensure-advisable-function function))
         (loc (member old-advice (advisable-function-after advise) :test test)))
    (if loc
        (setf (car loc) new-advice)
        (case if-not-found
          (:prepend (push new-advice (advisable-function-after advise)))
          (:append (setf (advisable-function-after advise)
                         (append (advisable-function-after advise)
                                 (list new-advice))))
          (otherwise nil)))))

(defun replace-advice (where function old-advice new-advice
                       &key (test 'eql) (if-not-found :prepend))
  "Replace OLD-ADVICE with NEW-ADVICE in the advice list for FUNCTION denoted by 
WHERE. TEST is used to find OLD-ADVICE. IF-NOT-FOUND dictates what to do in the 
event OLD-ADVICE is not present. It may be one of :prepend, :append, or nil."
  (case where
    (:before (replace-advice-before function old-advice new-advice
                                    test if-not-found))
    (:around (replace-advice-around function old-advice new-advice
                                    test if-not-found))
    (:after  (replace-advice-after  function old-advice new-advice
                                    test if-not-found))))

(defmacro define-advisory-functions ((to-advise &key (next-arg 'next)) args  &body advice)
  "Define advisable functions and add them to TO-ADVISE. "
  `(progn
     ,@(loop for (type . body) in advice
             for argslist = (if (eq (if (listp type) (car type) type) :around)
                                (cons next-arg args)
                                args)
             collect (cond ((symbolp type)
                            `(apply 'add-advice
                                    (list ,type ',to-advise
                                          (lambda ,argslist ,@body))))
                           ((keywordp (cadr type))
                            (destructuring-bind (where &rest rest) type
                              `(apply 'add-advice (list ,where ',to-advise
                                                        (lambda ,argslist
                                                          ,@body)
                                                        ,@rest))))
                           (t (destructuring-bind (where name &rest rest) type
                                `(progn (defun ,name ,argslist ,@body)
                                        (apply 'add-advice
                                               (list ,where ',to-advise ',name ,@rest)))))))))

(defun list-advice (fn &key (type :all) print)
  "List advice for FN, of type TYPE. When PRINT is true, advice will be printed to
standard output."
  (when-let1 (obj (ensure-advisable-function fn))
    (case type
      (:before
       (let ((fns (advisable-function-before obj)))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (:around
       (let ((fns (advisable-function-around obj)))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (:after
       (let ((fns (advisable-function-after obj)))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (otherwise
       (let ((before (advisable-function-before obj))
             (around (advisable-function-around obj))
             (after  (advisable-function-after  obj)))
         (when print
           (format t "BEFORE:~%~{~A~^~%~}~%AROUND:~%~{~A~^~%~}~%AFTER:~%~{~A~^~%~}"
                   before around after))
         (values before around after))))))

(defun remove-advice-all (fn)
  (when-let1 (obj (ensure-advisable-function fn))
    (setf (advisable-function-before obj) nil
          (advisable-function-around obj) nil
          (advisable-function-after obj) nil)))

(defun remove-nth (list nth)
  (loop for el in list
        for x from 0
        unless (= x nth)
          collect el))

(macrolet ((generate-remove-nth (type)
             (let ((accessor (case type
                               (:before 'advisable-function-before)
                               (:around 'advisable-function-around)
                               (:after  'advisable-function-after))))
               `(defun ,(intern (concatenate 'string "REMOVE-NTH-ADVICE-"
                                             (symbol-name type)))
                    (fn nth)
                  (when-let1 (obj (ensure-advisable-function fn))
                    (setf (,accessor obj) (remove-nth (,accessor obj) nth)))))))
  (generate-remove-nth :before)
  (generate-remove-nth :around)
  (generate-remove-nth :after))

(defun remove-nth-advice (type fn nth)
  "Remove NTH advice advice from FNs advice list of type TYPE."
  (case type
    (:before (remove-nth-advice-before fn nth))
    (:around (remove-nth-advice-around fn nth))
    (:after  (remove-nth-advice-after  fn nth))))

(macrolet ((generate-remove-advice (type checker)
             ;; generate remove-*-advice-if/-not functions 
             (let ((accessor (case type
                               (:before '(advisable-function-before fn))
                               (:around '(advisable-function-around fn))
                               (:after  '(advisable-function-after fn)))))
               `(defun ,(intern (concatenate 'string "REMOVE-" (symbol-name type)
                                             (case checker
                                               (when "-ADVICE-IF-NOT")
                                               (unless "-ADVICE-IF"))))
                    (predicate function from-end start end)
                  (declare (type number start)
                           (type (or number null) end)
                           (type function predicate))
                  (when-let1 (fn (ensure-advisable-function function))
                    (do ((counter start (+ counter 1))
                         (accumulator nil)
                         (list (nthcdr start (if from-end
                                                 (reverse ,accessor)
                                                 ,accessor))
                               (cdr list)))
                        ((or (not list) (and end (= counter end)))
                         (setf ,accessor (reverse accumulator)))
                      (,checker (funcall predicate (car list))
                                (push (car list) accumulator)))))))
           (gen-wrap (suffix)
             ;; generate remove-advice-if/-not functions
             (flet ((generate-remove-call (where)
                      (intern (concatenate 'string "REMOVE-" (symbol-name where)
                                           "-ADVICE-" (symbol-name suffix)))))
               `(defun ,(intern (concatenate 'string "REMOVE-ADVICE-"
                                             (symbol-name suffix)))
                    (predicate type function &key from-end (start 0) end)
                  (case type
                    (:before (,(generate-remove-call 'before)
                              predicate function from-end start end))
                    (:around (,(generate-remove-call 'around)
                              predicate function from-end start end))
                    (:after (,(generate-remove-call 'after)
                             predicate function from-end start end)))))))
  (generate-remove-advice :before when)
  (generate-remove-advice :around when)
  (generate-remove-advice :after  when)
  (generate-remove-advice :before unless)
  (generate-remove-advice :around unless)
  (generate-remove-advice :after  unless)
  (gen-wrap if)
  (gen-wrap if-not))

(defun remove-advice (type fn advice &key (test 'eql))
  (remove-advice-if (lambda (f)
                      (if (eql advice :all)
                          t
                          (funcall test f advice)))
                    type fn))
