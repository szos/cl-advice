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

(defun flatten (l)
  (when l
    (if (atom l)
        (list l)
        (mapcan 'flatten l))))

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
    (let ((ignoring (parse argument-list)))
      (when ignoring
        `((declare (ignore ,@ignoring)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Advisable Function Class ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass advisable-function ()
  ((arglist :initarg :arguments
            :accessor advisable-function-arguments)
   (main :initarg :main
         :accessor advisable-function-main)
   (before :initarg :before
           :accessor advisable-function-before
           :initform nil)
   (around :initarg :around
           :accessor advisable-function-around
           :initform nil)
   (after :initarg :after
          :accessor advisable-function-after
          :initform nil))
  (:metaclass c2mop:funcallable-standard-class))

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

(defun make-advisable-function (function &rest initargs)
  (unless (advisable-function-p function)
    (let ((fn (typecase function
                (function function)
                (symbol (symbol-function function))
                (otherwise (error "~A is not a function" function)))))
      (apply 'make-instance 'advisable-function
             (append (list :main fn) initargs)))))

(defmacro make-install-advisable-function-dispatcher (argslist mainfn)
  (with-gensyms (fobj fixed)
    `(let ((,fobj (make-advisable-function ,mainfn :arguments ',argslist)))
       (c2mop:set-funcallable-instance-function
        ,fobj (lambda ,argslist
                ,@(generate-ignore-declarations argslist)
                (let ((,fixed ,(argument-list-to-apply-list argslist)))
                  (prog2 (apply-before ,fobj ,fixed)
                      (apply-around ,fobj ,fixed)
                    (apply-after ,fobj ,fixed)))))
       ,fobj)))

(defmacro convert-to-advisable (symbol &optional arglist)
  (with-gensyms (rest)
    `(make-install-advisable-function-dispatcher ,(or arglist
                                                      `(&rest ,rest))
                                                 (symbol-function ,symbol))))

(defmacro make-advisable (symbol &optional argslist)
  "Take a quoted symbol and convert the function denoted by it to an advisable 
function. If argslist is provided, it must match the symbols functions argument
list. if argslist is nil, a single &rest argument will be used."
  `(let ((fn (symbol-function ,symbol)))
     (if (typep fn 'advisable-function)
         (error "~A is already an advisable function" ,symbol)
         (setf (symbol-function ,symbol)
               (convert-to-advisable ,symbol ,argslist)))))

(defun make-unadvisable (symbol)
  (let ((fn (symbol-function symbol)))
    (if (typep fn 'advisable-function)
        (setf (symbol-function symbol)
              (advisable-function-main fn))
        (error "~A is not an advisable function" symbol))))

(defun copy-advice (fn1 fn2)
  "DESTRUCTIVELY Copy all advice from FN1 to FN2"
  (setf (advisable-function-before fn2) (advisable-function-before fn1)
        (advisable-function-around fn2) (advisable-function-around fn1)
        (advisable-function-after  fn2) (advisable-function-after  fn1)))

(defmacro advisable-lambda (argslist &body body)
  "Return an advisable function object"
  `(make-install-advisable-function-dispatcher
    ,argslist (lambda ,argslist ,@body)))

(defmacro defun-advisable (name argslist &body body)
  "Define a function as an advisable function - works the same as DEFUN."
  (with-gensyms (oldfn)
    `(let ((,oldfn (handler-case (symbol-function ',name)
                     (undefined-function () nil))))
       (if ,oldfn
           (progn
             (setf (symbol-function ',name)
                   (advisable-lambda ,argslist ,@body))
             (when (equal (advisable-function-arguments ,oldfn) ',argslist)
               (copy-advice ,oldfn (symbol-function ',name))))
           (progn (defun ,name ,argslist ,@body)
                  (make-advisable ',name ,argslist))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and Removing Advice ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-advisable-function (fn)
  (let ((f (typecase fn
             (symbol (symbol-function fn))
             (advisable-function fn))))
    (if (advisable-function-p f)
        f
        (error "~A is not an advisable function" fn))))

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
