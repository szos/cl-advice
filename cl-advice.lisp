;;;; cl-advice.lisp

(in-package #:cl-advice)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (sym)
                   `(,sym (gensym ,(symbol-name sym))))
          syms)
     ,@body))

(defmacro when-let1 ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defun flatten (l)
  (when l
    (if (atom l)
        (list l)
        (mapcan 'flatten l))))

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

(defclass advisable-function ()
  ((main :initarg :main
         :accessor advisable-function-main)
   (before :initarg :before
           :accessor advisable-function-before
           :initform nil)
   (around :initarg :around
           :accessor advisable-function-around
           :initform nil)
   (after :initarg :after
          :accessor advisable-function-after
          :initform nil)))

(defvar *advice-hash* (make-hash-table))

(defun add-advisable-function (name obj &optional overridep)
  (let ((o (gethash name *advice-hash*)))
    (when (or (not o) overridep)
      (setf (gethash name *advice-hash*) obj))))

(defun generate-around-caller (fns)
  (lambda (&rest args)
    (if (cdr fns)
        (apply (car fns) (cons (generate-around-caller (cdr fns)) args))
        (apply (car fns) args))))

(defun apply-before (object args)
  (when-let1 (before-functions (advisable-function-before object))
    (loop for fn in before-functions
          do (apply fn args))))

(defun apply-around (object args)
  (apply (generate-around-caller
          (append (ensure-list (advisable-function-around object))
                  (ensure-list (advisable-function-main object))))
         args))

(defun apply-after (object args)
  (when-let1 (after-functions (advisable-function-after object))
    (loop for fn in after-functions
          do (apply fn args))))

(defmacro defn-adv (name args rule &optional docstring)
  (with-gensyms (obj fixed)
    `(defun ,name ,args
       ,@(when docstring (list docstring))
       ,(generate-decls args)
       (let* ((,obj (gethash ',name *advice-hash*))
              (,fixed ,rule))
         (prog2
             (apply-before ,obj ,fixed)
             (apply-around ,obj ,fixed)
           (apply-after ,obj ,fixed))))))

(defun generate-decls (argslist)
  (list 'declare 
        (cons
         'ignorable
         (flatten
          (loop for var in argslist
                if (and (not (member var '(&rest &optional &key &allow-other-keys)))
                        (symbolp var))
                  collect var
                else if (listp var)
                       collect (append (list (car var))
                                       (cddr var)))))))

(defmacro define-advisable (name argslist rule &body body)
  (with-gensyms (main-fn)
    `(let ((,main-fn (lambda ,argslist ,@body)))
       (add-advisable-function ',name
                               (make-instance 'advisable-function
                                              :main ,main-fn)
                               t)
       (defn-adv ,name ,argslist ,rule ,@(when (and (listp body)
                                                   (stringp (car body)))
                                           (list (car body)))))))

(defmacro make-advisable (name argslist rule)
  (with-gensyms (main-fn obj)
    `(let ((,obj (and (gethash ',name *advice-hash*)
                      (error "~A already denotes an advisable function" ',name)))
           (,main-fn (function ,name)))
       (add-advisable-function ',name
                               (make-instance 'advisable-function
                                              :main ,main-fn))
       (defn-adv ,name ,argslist ,rule))))

(defun push-advice-around (function advice-fn)
  (push advice-fn (advisable-function-around (typecase function
                                               (symbol
                                                (gethash function *advice-hash*))
                                               (advisable-function
                                                function)))))

(defun push-advice-before (function advice-fn)
  (push advice-fn (advisable-function-before (typecase function
                                               (symbol
                                                (gethash function *advice-hash*))
                                               (advisable-function
                                                function)))))

(defun push-advice-after (function advice-fn)
  (push advice-fn (advisable-function-after (typecase function
                                               (symbol
                                                (gethash function *advice-hash*))
                                               (advisable-function
                                                function)))))

(defun add-advice (type fn-name advice-fn &key allow-duplicates (test 'eql))
  (when-let1 (obj (gethash fn-name *advice-hash*))
    (case type
      (:before
       (when (or allow-duplicates
                 (not (member advice-fn (advisable-function-before obj)
                              :test test)))
         (push-advice-before obj advice-fn)))
      (:around
       (when (or allow-duplicates
                 (not (member advice-fn (advisable-function-around obj)
                              :test test)))
         (push-advice-around obj advice-fn)))
      (:after
       (when (or allow-duplicates
                 (not (member advice-fn (advisable-function-after obj)
                              :test test)))
         (push-advice-after obj advice-fn))))
    obj))

(defmacro advise-function ((fn-name &key allow-duplicates test (next-fn-arg 'next))
                           args &body advice)
  `(progn
     ,@(loop for (type . body) in advice
             if (listp type)
               collect `(progn (defun ,(cadr type)
                                   ,(if (eq (car type) :around)
                                        (cons next-fn-arg args)
                                        args)
                                 ,@body)
                               (add-advice ,(car type) ',fn-name
                                           ',(cadr type)
                                           :allow-duplicates ,allow-duplicates
                                           :test ,(or test ''eql)))
             else 
               collect `(add-advice ,type ',fn-name
                                    (lambda
                                        ,(if (eq type :around)
                                             (cons next-fn-arg args)
                                             args)
                                      ,@body)
                                    :allow-duplicates ,allow-duplicates
                                    :test ,(or test ''eql)))))

(defun list-advice (fn &key type print)
  (when-let1 (obj (gethash fn *advice-hash*))
    (case type
      (:before
       (let ((fns (ensure-list (advisable-function-before obj))))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (:around
       (let ((fns (ensure-list (advisable-function-around obj))))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (:after
       (let ((fns (ensure-list (advisable-function-after obj))))
         (when print
           (format t "~{~A~^~%~}" fns))
         fns))
      (otherwise
       (let ((before (ensure-list (advisable-function-before obj)))
             (around (ensure-list (advisable-function-around obj)))
             (after  (ensure-list (advisable-function-after  obj))))
         (when print
           (format t "BEFORE:~%~{~A~^~%~}~%AROUND:~%~{~A~^~%~}~%AFTER:~%~{~A~^~%~}"
                   before around after))
         (values before around after))))))

(defun remove-nth (list nth)
  (loop for el in list
        for x from 0
        unless (= x nth)
          collect el))

(defun remove-nth-advice-before (fn nth)
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-before obj)
          (remove-nth (advisable-function-before obj) nth))))

(defun remove-nth-advice-around (fn nth)
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-around obj)
          (remove-nth (advisable-function-around obj) nth))))

(defun remove-nth-advice-after (fn nth)
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-after obj)
          (remove-nth (advisable-function-after obj) nth))))

(defun remove-nth-advice (type fn nth)
  (case type
    (:before (remove-nth-advice-before fn nth))
    (:around (remove-nth-advice-around fn nth))
    (:after  (remove-nth-advice-after  fn nth))))

(defun remove-advice-before (fn advice &optional (test 'eql))
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-before obj)
          (remove advice (advisable-function-before obj) :test test))))

(defun remove-advice-around (fn advice &optional (test 'eql))
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-around obj)
          (remove advice (advisable-function-around obj) :test test))))

(defun remove-advice-after (fn advice &optional (test 'eql))
  (when-let1 (obj (typecase fn
                    (symbol (gethash fn *advice-hash*))
                    (advisable-function fn)))
    (setf (advisable-function-after obj)
          (remove advice (advisable-function-after obj) :test test))))

(defun remove-advice-all (fn)
  (when-let1 (obj (gethash fn *advice-hash*))
    (setf (advisable-function-before obj) nil
          (advisable-function-around obj) nil
          (advisable-function-after obj) nil)))

(defun remove-advice (type fn advice &key (test 'eql))
  (case type
    (:before (remove-advice-before fn advice test))
    (:around (remove-advice-around fn advice test))
    (:after  (remove-advice-after  fn advice test))
    (:all (remove-advice-all fn))))
