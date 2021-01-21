;;;; cl-advice.lisp

(in-package #:cl-advice)

(defmacro if-let*-eval-all (bindings then-form &body else)
  "An if-let* which preserves BINDINGS in both THEN-FORM and ELSE"
  (let ((vars (loop for (var val) in bindings collect var)))
    `(let* ,bindings
       (if (and ,@vars)
	   ,then-form
	   (progn ,@else)))))

;;;;;;;;;;;;;;;;;;
;;; Conditions ;;;
;;;;;;;;;;;;;;;;;;

(define-condition advisable-function-error (error) ())

(define-condition symbol-not-fbound-error (advisable-function-error)
  ((symbol :initarg :symbol :reader symbol-not-fbound-error-symbol))
  (:report
   (lambda (c s)
     (with-slots (symbol) c
       (format s "Unable to define advice for undefined function ~A" symbol)))))

(define-condition no-advisable-function-found-error (advisable-function-error)
  ((symbol :initarg :symbol :reader no-advisable-function-found-error-symbol)
   (database :initarg :db :reader no-advisable-function-found-error-database))
  (:report
   (lambda (c s)
     (with-slots (symbol database) c
       (format s "No advice registered for symbol ~A in database ~A"
	       symbol database)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advisable Function Object Creation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass advisable-function (advisable-function-before-advice
			      advisable-function-around-advice
			      advisable-function-after-advice)
  ((dispatch :initarg :dispatch :initform nil
	     :accessor advisable-function-dispatch)
   (main   :initarg :main   :initform nil
	   :accessor advisable-function-main)
   (before :initarg :before :initform nil
	   :accessor advisable-function-before)
   (around :initarg :around :initform nil
	   :accessor advisable-function-around)
   (after  :initarg :after  :initform nil
	   :accessor advisable-function-after)))

(defun make-advisable-function-object (dispatch main &key before after around)
  "Make an advisable-function object"
  (make-instance 'advisable-function :dispatch dispatch
				     :main main
				     :before before
				     :around around
				     :after after))

(defun advisable-function-p (name &optional (db *advice-hash-table*))
  "Predicate for checking if NAME denotes an advisable function in DB"
  (and (fboundp name) (gethash name db)))

(defmacro make-advisable (name &key (db '*advice-hash-table*))
  "Create an advisable-function object in DB for the function NAME if one does not
already exist."
  (alexandria:with-gensyms (dispatch dispatch-args d-obj main before around after)
    `(unless (gethash ',name ,db)
       (let ((,dispatch (lambda (&rest ,dispatch-args)
			  ,(format nil "Advice dispatch function for ~A" name)
			  (let* ((,d-obj (gethash ',name ,db))
				 (,main (advisable-function-main ,d-obj))
				 (,before (advisable-function-before ,d-obj))
				 (,around (advisable-function-around ,d-obj))
				 (,after (advisable-function-after ,d-obj)))
			    (prog2 (when ,before
				     (apply ,before (cons nil ,dispatch-args)))
				(if ,around
				    (apply ,around (cons nil ,dispatch-args))
				    (apply ,main ,dispatch-args))
			      (when ,after
				(apply ,after (cons nil ,dispatch-args))))))))
	 (setf (gethash ',name ,db)
	       (make-advisable-function-object
		,dispatch (symbol-function ',name)))))))

(defmacro defadvisable (name args &body body)
  "Define a function and make it advisable. NAME may be either a symbol or a list 
with the car denoting the function name and the cadr denoting the database to store
the advisable-function object in."
  (let ((realname (if (atom name) name (car name)))
	(db (if (atom name) '*advice-hash-table* (cadr name))))
    `(progn
       (defun ,realname ,args ,@body)
       (make-advisable ,realname :db ,db))))

(defvar *advice-hash-table* (make-hash-table)
  "The default database for advisable-function objects")

;;;;;;;;;;;;;;;;;;;;;
;;; Access Macros ;;;
;;;;;;;;;;;;;;;;;;;;;

(defmacro with-advisable-object ((var designator &key (db '*advice-hash-table*))
				 &body body)
  "Look up DESIGNATOR in DB and bind it to VAR. If VAR is nil signal an error, 
otherwise evaluate BODY."
  `(let ((,var (gethash ,designator ,db)))
     (if ,var
	 (progn ,@body)
	 (error 'no-advisable-function-found-error
		:symbol ,designator
		:db ,db))))

(defmacro with-advisable-object-advice ((fn-var obj-var qualifier designator
					 &key (db '*advice-hash-table*))
					&body body)
  "Look up DESIGNATOR in DB and bind VAR to it. Then look up QUALIFIER advice in 
the object and bind FN-VAR to it. Evaluate BODY in the context of these bindings."
  `(with-advisable-object (,obj-var ,designator :db ,db)
     (let ((,fn-var (case ,qualifier
		      (:before (advisable-function-before ,obj-var))
		      (:around (advisable-function-around ,obj-var))
		      (:after (advisable-function-after ,obj-var)))))
       ,@body)))

(defmacro with-inner-advice ((var advice-function) &body body)
  "Bind VAR to the advice wrapped in ADVICE-FUNCTION and evaluate BODY."
  `(let ((,var (funcall (or ,advice-function 'not) t)))
     ,@body))

(defun with-advice-list-worker (fn)
  "Unwrap all advice wrapped by FN and return it as a list. Innermost wrapped 
advice is last."
  (with-inner-advice (var fn)
    (when var
      (cons var (sub-with-advice-list var)))))

(defmacro list-advice (qualifier designator &key (db '*advice-hash-table*))
  "Return a list of advice of type QUALIFIER for the object gotten by looking up
DESIGNATOR in DB."
  (alexandria:with-gensyms (fnvar obj-var)
    `(with-advisable-object-advice (,fnvar ,obj-var ,qualifier ,designator :db ,db)
       (when ,fnvar
	 (cons ,fnvar
	       (with-advice-list-worker ,fnvar))))))

(defmacro with-advice-list ((list-var obj-var qualifier designator
			     &key (db '*advice-hash-table*)) &body body)
  "Bind OBJ-VAR to the result of looking up DESIGNATOR in DB. Then bind LIST-VAR to
the list of advice of type QUALIFIER for object OBJ-VAR. Evaluate BODY in the 
context of these bindings"
  (alexandria:with-gensyms (fnvar)
    `(with-advisable-object-advice (,fnvar ,obj-var ,qualifier ,designator :db ,db)
       (let ((,list-var (when ,fnvar
			  (cons ,fnvar (with-advice-list-worker ,fnvar)))))
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice Function Generation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-defadvice-args-and-decls (arglist body)
  "Parse out argument list, docstring,declarations and body for add-advice."
  (let* ((ignore-args (equal (car arglist) '&ignore))
	 (docstring (when (stringp (car body)) (car body)))
	 (decls (cond ((and docstring
			    (equal (caadr body) 'declare))
		       (cadr body))
		      ((and (listp (car body))
			    (equal (caar body) 'declare))
		       (car body))))
	 (bod (cond ((and docstring (equal (caadr body) 'declare))
		     (cddr body))
		    ((or docstring
			 (and (listp (car body))
			      (equal (caar body) 'declare)))
		     (cdr body))
		    (t body)))
	 (g (when ignore-args (gensym))))
    (if ignore-args
	(list `(&rest ,g)
	      docstring
	      `(declare (ignore ,g) ,@(when (cdr decls) (cdr decls)))
	      bod)
	(list arglist docstring decls bod))))

(defun make-before-*-advice-function (accessor obj args body)
  "Make an advice function which runs before any sub-advice present in OBJ that is
available via ACCESSOR."
  (alexandria:with-gensyms (next-advice poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(let ((,next-advice (,accessor ,obj)))
	 (lambda (,poparg &rest ,restarg)
	   ,@(when docstring (list docstring))
	   (if ,poparg
	       ,next-advice 
	       (destructuring-bind ,argslist ,restarg
		 ,@(when declarations (list declarations))
		 ,@realbody
		 (when ,next-advice 
		   (apply ,next-advice (cons nil ,restarg))))))))))

(defun make-around-*-advice-function (accessor obj args body)
  "Make an advice function which runs around any sub-advice present in OBJ that is
available via ACCESSOR. The macro CALL-NEXT-ADVICE is exposed for usage in BODY."
  (alexandria:with-gensyms (next-advice poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(let ((,next-advice (,accessor ,obj)))
	 (lambda (,poparg &rest ,restarg)
	   ,@(when docstring (list docstring))
	   (if ,poparg
	       ,next-advice 
	       (macrolet ((call-next-advice ()
			    `(apply ,',next-advice (cons nil ,',restarg))))
		 (destructuring-bind ,argslist ,restarg
		   ,@(when declarations (list declarations))
		   ,@realbody))))))))

(defun make-around-around-advice-function (obj args body)
  "Make an advice function which runs around any sub-advice present in OBJ that is
available via ACCESSOR. The macro CALL-NEXT-ADVICE is exposed for usage in BODY."
  (alexandria:with-gensyms (next-advice poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(let ((,next-advice (advisable-function-around ,obj)))
	 (lambda (,poparg &rest ,restarg)
	   ,@(when docstring (list docstring))
	   (if ,poparg
	       ,next-advice 
	       (macrolet ((call-next-advice ()
			    `(apply ,',next-advice (cons nil ,',restarg)))
			  (call-next-advice-with-args (&rest callargs)
			    `(apply ,',next-advice (list nil ,@callargs)))
			  (call-main-function ()
			    `(apply (advisable-function-main ,',obj) ,',restarg))
			  (call-main-function-with-args (&rest callargs)
			    `(apply (advisable-function-main ,',obj)
				    (list ,@callargs))))
		 (destructuring-bind ,argslist ,restarg
		   ,@(when declarations (list declarations))
		   ,@realbody))))))))

(defun make-after-*-advice-function (accessor obj args body)
  "Make an advice function which runs after any sub-advice present in OBJ that is
available via ACCESSOR."
  (alexandria:with-gensyms (next-advice poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(let ((,next-advice (,accessor ,obj)))
	 (lambda (,poparg &rest ,restarg)
	   ,@(when docstring (list docstring))
	   (if ,poparg
	       ,next-advice
	       (progn 
		 (when ,next-advice
		   (apply ,next-advice (cons nil ,restarg)))
		 (destructuring-bind ,argslist ,restarg
		   ,@(when declarations (list declarations))
		   ,@realbody))))))))

(defun make-base-advice-function (args body)
  "Make a base advice function that does not call any sub-advice."
  (alexandria:with-gensyms (poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(lambda (,poparg &rest ,restarg)
	 ,@(when docstring (list docstring))
	 (unless ,poparg
	   (destructuring-bind ,argslist ,restarg
	     ,@(when declarations (list declarations))
	     ,@realbody))))))

(defun make-base-around-advice-function (obj args body)
  "Make a base :around advice function which exposes the macros CALL-MAIN-FUNCTION
and CALL-MAIN-FUNCTION-WITH-ARGS for usage in BODY."
  (alexandria:with-gensyms (poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(lambda (,poparg &rest ,restarg)
	 ,@(when docstring (list docstring))
	 (unless ,poparg
	   (macrolet ((call-main-function ()
			`(apply (advisable-function-main ,',obj) ,',restarg))
		      (call-main-function-with-args (&rest callargs)
			`(apply (advisable-function-main ,',obj)
				(list ,@callargs))))
	     (destructuring-bind ,argslist ,restarg
	       ,@(when declarations (list declarations))
	       ,@realbody)))))))

(defun make-before-advice-function (obj q-qualifier args body)
  (case q-qualifier
    (:before (make-before-*-advice-function 'advisable-function-before
					    obj args body))
    (:around (make-around-*-advice-function 'advisable-function-before
					    obj args body))
    (:after (make-after-*-advice-function 'advisable-function-before
					  obj args body))
    (otherwise (make-base-advice-function args body))))

(defun make-around-advice-function (obj q-qualifier args body)
  (case q-qualifier
    (:before (make-before-*-advice-function 'advisable-function-around
					    obj args body))
    (:around (make-around-around-advice-function obj args body))
    (:after (make-after-*-advice-function 'advisable-function-around
					  obj args body))
    (otherwise (make-base-around-advice-function obj args body))))

(defun make-after-advice-function (obj q-qualifier args body)
  (case q-qualifier
    (:before (make-before-*-advice-function 'advisable-function-after
					    obj args body))
    (:around (make-around-*-advice-function 'advisable-function-after
					    obj args body))
    (:after (make-after-*-advice-function 'advisable-function-after
					  obj args body))
    (otherwise (make-base-advice-function args body))))

(defmacro make-advice-function (obj qualifier q-qualifier args body)
  (case qualifier
    (:before
     (make-before-advice-function obj q-qualifier args body))
    (:around
     (make-around-advice-function obj q-qualifier args body))
    (:after
     (make-after-advice-function obj q-qualifier args body))))

;;;;;;;;;;;;;;;;;;;
;;; Main Macros ;;;
;;;;;;;;;;;;;;;;;;;

(defmacro add-advice ((q-qualifier qualifier) name/db args &body body)
  "Define advice of type QUALIFIER for the function NAME. If an advisable-function
object doesnt exist one will be created. 

Q-QUALIFIER denotes how to wrap pre-existing advice. It can be one of :before 
:after :around or :base. :before and :after will run their advice before and after
pre-existing advice respectively. :around will expose the local macros called 
CALL-NEXT-ADVICE and CALL-NEXT-ADVICE-WITH-ARGS for usage in BODY. If a call to one
of these macros does not occur in BODY then no further advice will be run. :base
will overwrite pre-existing advice.

QUALIFIER denotes what type of advice to create. It can have the value :before 
:after or :around. These will run before, after or around the main function 
respectively. :around advice will expose the local macros CALL-MAIN-FUNCTION and 
CALL-MAIN-FUNCTION-WITH-ARGS. If calls to these macros dont appear in BODY the main
function will not be called. If both Q-QUALIFIER and QUALIFIER are :around, the 
local macros CALL-NEXT-ADVICE, CALL-NEXT-ADVICE-WITH-ARGS, CALL-MAIN-FUNCTION, and
CALL-MAIN-FUNCTION-WITH-ARGS will be exposed to BODY. Advice should only ever call
one of these macros to avoid running advice and the main function multiple times. 

About :around advice - :around advice must be careful to return the correct value. 
The final value of around advice is returned, so if one is doing something after a 
call to CALL-MAIN-FUNCTION or CALL-NEXT-ADVICE, one must properly capture the 
results of that call in order to return them. :before and :after advice does not 
have this problem.

NAME/DB denotes the name and database for advice to be stored in. It can be a 
symbol, in which case it denotes the name of the function to advise and the 
database defaults to *advice-hash-table*, or it can be a list, in which case the 
car denotes the name of the function to advise and the cadr denotes the database.

ARGS must either conform to the arglist of the function denoted by NAME, be of the 
form (&rest rest), or be of the form (&ignore). If &ignore is specified then any 
arguments will not be accessible to the advice. 

BODY is the body of the advice. It may contain a docstring and declarations."
  (alexandria:with-gensyms (obj fn)
    (let ((name (if (atom name/db) name/db (car name/db)))
	  (db (if (atom name/db) '*advice-hash-table* (cadr name/db))))
      `(if (advisable-function-p ',name ,db)
	   (let* ((,obj (or (gethash ',name ,db)
			    (error 'no-advisable-function-found-error
				   :symbol ',name :db ',db)))
		  (,fn (make-advice-function ,obj ,qualifier ,q-qualifier
					     ,args ,body)))
	     (unless (equal (symbol-function ',name)
			    (advisable-function-dispatch ,obj))
	       (setf (symbol-function ',name) (advisable-function-dispatch ,obj)))
	     (setf ,@(case qualifier
		       (:before
			`((advisable-function-before ,obj) ,fn))
		       (:around
			`((advisable-function-around ,obj) ,fn))
		       (:after
			`((advisable-function-after ,obj) ,fn)))))
	   (error 'no-advisable-function-found-error :symbol ',name :db ',db)))))

(defmacro defadvice (qualifier name args &body body)
  "Defadvice defines singular advice. It is the same as add-advice with a 
q-qualifier of :base. "
  `(add-advice (:base ,qualifier) ,name ,args ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice Management ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pop-advice (qualifier name &key (db *advice-hash-table*))
  "Unwrap the next piece of advice of type QUALIFIER for the object gotten by
looking up NAME in DB and store it as the top level advice for the object."
  (with-advisable-object (obj name :db db)
    (let ((inner-advice (funcall (or (case qualifier
				       (:before (advisable-function-before obj))
				       (:around (advisable-function-around obj))
				       (:after (advisable-function-after obj)))
				     'not)
				 t)))
      (when inner-advice 
	(case qualifier
	  (:before
	   (setf (advisable-function-before obj) inner-advice))
	  (:around
	   (setf (advisable-function-around obj) inner-advice))
	  (:after
	   (setf (advisable-function-after obj) inner-advice)))))))

(defun delete-advice (qualifier name &optional (db *advice-hash-table*))
  "Delete advice of type QUALIFIER for function NAME in database db. Qualifiers 
designating advice type will remove that advice from the object. The qualifiers
:ALL and :EVERYTHING will remove all advice. The qualifier :FMAKUNBOUND will delete
the object from the database and unbind the function."
  (flet ((no-advice-p (o)
	   (check-type o advisable-function)
	   (not (or (advisable-function-around o)
		    (advisable-function-before o)
		    (advisable-function-after  o)))))
    (with-advisable-object (obj name :db db)
      (flet ((maybe-reset ()
	       (when (no-advice-p obj)
		 (setf (symbol-function name) (advisable-function-main obj)))))
	(case qualifier
	  (:around
	   (setf (advisable-function-around obj) nil)
	   (maybe-reset))
	  (:before
	   (setf (advisable-function-before obj) nil)
	   (maybe-reset))
	  (:after
	   (setf (advisable-function-after obj) nil)
	   (maybe-reset))
	  ((:all :everything)
	   (setf (symbol-function name) (advisable-function-main obj))
	   (remhash name db))
	  (:fmakunbound
	   (fmakunbound name)
	   (remhash name db)))))))

(defun activate-advice (symbol &key (db *advice-hash-table*))
  "Activate advice for SYMBOL by setting symbol-function to the dispatch function"
  (with-advisable-object (obj symbol :db db)
    (setf (symbol-function symbol) (advisable-function-dispatch obj))))

(defun deactivate-advice (symbol &key (db *advice-hash-table*))
  "Deactivate advice for SYMBOL by setting symbol-function to the main function"
  (with-advisable-object (obj symbol :db db)
    (setf (symbol-function symbol) (advisable-function-main obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Advice Documentation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %advice-documentation (qualifier symbol &key (db *advice-hash-table*))
  (let ((al (list-advice qualifier symbol :db db)))
    (loop for f in al
	  collect (documentation f 'function))))

(defun advice-documentation (symbol &key (db *advice-hash-table*))
  "returns an alist of the documentation for all advices and the main function"
  (with-advisable-object (obj symbol :db db)
    (list (cons :before (%advice-documentation :before symbol :db db))
	  (cons :around (%advice-documentation :around symbol :db db))
	  (cons :main (documentation (advisable-function-main obj) 'function))
	  (cons :after (%advice-documentation :after symbol :db db)))))
