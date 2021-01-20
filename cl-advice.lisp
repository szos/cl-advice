;;;; cl-advice.lisp

(in-package #:cl-advice)

(defmacro if-let*-eval-all (bindings then-form &body else)
  "An if-let* which preserves BINDINGS in both THEN-FORM and ELSE"
  (let ((vars (loop for (var val) in bindings collect var)))
    `(let* ,bindings
       (if (and ,@vars)
	   ,then-form
	   (progn ,@else)))))

(defmacro flet* (bindings &body body)
  `(flet (,(car bindings))
     ,@(if (cdr bindings)
	   `((flet* ,(cdr bindings) ,@body))
	   body)))
;; use trivial-indent to figure out of this, cause its indented wacky.
;; for now use labels

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

(define-condition not-an-advisable-function-error
    (no-advisable-function-found-error)
  ()
  (:report
   (lambda (c s)
     (with-slots (symbol database) c
       (format s "Symbol ~A does not denote an advisable function in database ~A"
	       symbol database)))))

(defclass advisable-function-before-advice ()
  ((before :initarg :before :initform nil
	   :accessor advisable-function-before)
   (inner-before :initarg :inner-before :initform nil
		 :accessor advisable-function-before-inner)))

(defclass advisable-function-around-advice ()
  ((around :initarg :around :initform nil
	   :accessor advisable-function-around)
   (inner-around :initarg :inner-around :initform nil
		:accessor advisable-function-around-inner)))

(defclass advisable-function-after-advice ()
  ((after  :initarg :after  :initform nil
	   :accessor advisable-function-after)
   (inner-after :initarg :inner-after :initform nil
		:accessor advisable-function-after-inner)))

(defclass advisable-function (advisable-function-before-advice
			      advisable-function-around-advice
			      advisable-function-after-advice)
  ((dispatch :initarg :dispatch :initform nil
	     :accessor advisable-function-dispatch)
   (main   :initarg :main   :initform nil
	   :accessor advisable-function-main)))

(defun make-advisable-function-object (dispatch main &key before after around)
  "Make an advisable-function object"
  (make-instance 'advisable-function :dispatch dispatch
				     :main main
				     :before before
				     :around around
				     :after after))

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
  `(with-advisable-object (,obj-var ,designator :db ,db)
     (let ((,fn-var ,(case qualifier
		       (:before `((advisable-function-before ,obj-var)))
		       (:around `((advisable-function-around ,obj-var)))
		       (:after `((advisable-function-after ,obj-var)))
		       (otherwise nil))))
       ,@body)))

(defmacro with-inner-advice ((var advice-function) &body body)
  `(let ((,var (funcall (or ,advice-function 'not) t)))
     ,@body))

(defparameter *advice-hash-table* (make-hash-table)
  "The default database for advisable-function objects")

(defmacro make-advisable (name &key (db '*advice-hash-table*))
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
  (let ((realname (if (atom name) name (car name)))
	(db (if (atom name) '*advice-hash-table* (cadr name))))
    `(progn
       (defun ,realname ,args ,@body)
       (make-advisable ,realname ,db))))

(defun pop-advice (qualifier name &key (db *advice-hash-table*))
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

(defun make-before-*-advice-function (accessor obj args body)
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
  (alexandria:with-gensyms (next-advice poparg restarg)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(let ((,next-advice (,accessor ,obj)))
	 (lambda (,poparg &rest ,restarg)
	   ,@(when docstring (list docstring))
	   (if ,poparg
	       ,next-advice 
	       (macrolet ((call-next-advice ()
			    `(apply ,',next-advice ,',restarg)))
		 (destructuring-bind ,argslist ,restarg
		   ,@(when declarations (list declarations))
		   ,@realbody))))))))

(defun make-after-*-advice-function (accessor obj args body)
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
    (:around (make-around-*-advice-function 'advisable-function-around
					    obj args body))
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

(defun make-advice-function (obj qualifier q-qualifier args body)
  (case qualifier
    (:before
     (make-before-advice-function obj q-qualifier args body))
    (:around
     (make-around-advice-function obj q-qualifier args body))
    (:after
     (make-after-advice-function obj q-qualifier args body))))

(defmacro add-advice ((q-qualifier qualifier) (name db) args &body body)
  (alexandria:with-gensyms (obj fn)
    `(if (advisable-function-p ',name ,db)
	 (let* ((,obj (or (gethash ',name ,db)
			  (error 'no-advisable-function-found-error
				 :symbol ',name :db ',db)))
		(,fn (make-advice-function ,obj ,qualifier ,q-qualifier
					   ,args ,body)))
	   (setf ,@(case qualifier
		     (:before
		      `((advisable-function-before ,obj) ,fn))
		     (:around
		      `((advisable-function-around ,obj) ,fn))
		     (:after
		      `((advisable-function-after ,obj) ,fn)))))
	 (error 'not-an-advisable-function-error :symbol ',name :db ',db))))

;; (defmacro add-advice ((q-qualifier qualifier) (name db) args
;; 		      &body body)
;;   "lets write this out...
;; if we have a qualifier of :around and a q-qualifier of :around, we are in the most 
;; complexe case... we want to first check if theres already :around advice. if not we
;; create it (as we do in defadvice). (actually, we want to create all these functions
;; before we do the checking, cause the function forms need to be generated at 
;; macroexpansion time, but the checks need to happen at runtime.) anyway, if no 
;; :around advice exists we want to add it. but if it does, then we want to expose 
;; the local macros called CALL-NEXT-ADVICE and CALL-NEXT-ADVICE-WITH-ARGS."
;;   (alexandria:with-gensyms (fn obj restarg poparg next-advice)
;;     (destructuring-bind (argslist docstring declarations realbody)
;; 	(generate-defadvice-args-and-decls args body)
;;       `(let* ((,obj (or (gethash ',name ,db)
;; 			(error 'no-advisable-function-found-error
;; 			       :symbol ',name :db ',db)))
;; 	      (,fn ; this is disgusting... how to clean it up? 
;; 		,(case qualifier
;; 		   (:before
;; 		    (case q-qualifier
;; 		      (:before
;; 		       `(let ((,next-advice (advisable-function-before ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(destructuring-bind ,argslist ,restarg
;; 				  ,@(when declarations (list declarations))
;; 				  ,@realbody
;; 				  (when ,next-advice 
;; 				    (apply ,next-advice (cons nil ,restarg))))))))
;; 		      (:around
;; 		       `(let ((,next-advice (advisable-function-before ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(macrolet ((call-next-advice ()
;; 					     `(apply ,',next-advice ,',restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))
;; 		      (:after
;; 		       `(let ((,next-advice (advisable-function-before ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice
;; 				(progn 
;; 				  (when ,next-advice
;; 				    (apply ,next-advice (cons nil ,restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))))
;; 		   (:around
;; 		    (case q-qualifier
;; 		      (:before
;; 		       `(let ((,next-advice (advisable-function-around ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(destructuring-bind ,argslist ,restarg
;; 				  ,@(when declarations (list declarations))
;; 				  ,@realbody
;; 				  (when ,next-advice
;; 				    (apply ,next-advice (cons nil ,restarg))))))))
;; 		      (:around
;; 		       `(let ((,next-advice (advisable-function-around ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(macrolet ((call-next-advice ()
;; 					     `(apply ,',next-advice ,',restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))
;; 		      (:after
;; 		       `(let ((,next-advice (advisable-function-around ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice
;; 				(progn
;; 				  (when ,next-advice
;; 				    (apply ,next-advice (cons nil ,restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))))
;; 		   (:after
;; 		    (case q-qualifier
;; 		      (:before
;; 		       `(let ((,next-advice (advisable-function-after ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(destructuring-bind ,argslist ,restarg
;; 				  ,@(when declarations (list declarations))
;; 				  ,@realbody
;; 				  (when ,next-advice 
;; 				    (apply ,next-advice (cons nil ,restarg))))))))
;; 		      (:around
;; 		       `(let ((,next-advice (advisable-function-after ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(macrolet ((call-next-advice ()
;; 					     `(apply ,',next-advice ,',restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))
;; 		      (:after
;; 		       `(let ((,next-advice (advisable-function-after ,obj)))
;; 			  (lambda (,poparg &rest ,restarg)
;; 			    ,@(when docstring (list docstring))
;; 			    (if ,poparg
;; 				,next-advice 
;; 				(progn
;; 				  (when ,next-advice 
;; 				    (apply ,next-advice (cons nil ,restarg)))
;; 				  (destructuring-bind ,argslist ,restarg
;; 				    ,@(when declarations (list declarations))
;; 				    ,@realbody))))))
;; 		      ))))) ; end disgusting fn gen
;; 	 (setf
;; 	  ,@(case qualifier
;; 	      (:before
;; 	       `((advisable-function-before ,obj) ,fn))
;; 	      (:around
;; 	       `((advisable-function-around ,obj) ,fn))
;; 	      (:after
;; 	       `((advisable-function-after ,obj) ,fn))))
;; 	 (unless (equal (symbol-function ',name)
;; 			(advisable-function-dispatch ,obj))
;; 	   (setf (symbol-function ',name) (advisable-function-dispatch ,obj)))))))

(defun generate-defadvice-args-and-decls (arglist body)
  "Parse out argument list, docstring, and declarations for usage in defadvice"
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

(defmacro defadvice (qualifier name args &body body)
  "Define advice of type QUALIFIER for function NAME. If an advice object doesnt 
exist, one will be created. 

QUALIFIER can be any of :before :around or :after. :around advice will expose the
local macro CALL-MAIN-FUNCTION, which applies the main function to the arguments as
they were recieved. It will also expose the local macro
CALL-MAIN-FUNCTION-WITH-ARGS, which takes a list of arguments to apply the main 
function to.

NAME can be either a symbol denoting a defined function or a list with the symbol
denoting a defined function in its car and a symbol denoting a hash table in its
cadr. This hash table will be where the advisable function object will be stored.

ARGS must either conform to the arglist of the function denoted by NAME, of the 
form (&rest rest), or of the form (&ignore). If &ignore is specified then any 
arguments will not be accessible to the advice. 

BODY is the body of the advice, and may contain a docstring and declaration 
statement."
  (alexandria:with-gensyms (realname db obj restarg fn dispatch-args fboundp
				     main before after around)
    (destructuring-bind (argslist docstring declarations realbody)
	(generate-defadvice-args-and-decls args body)
      `(if-let*-eval-all ((,realname ',(if (atom name) name (car name)))
			  (,fboundp (fboundp ,realname)))
	   (let* ((,db ,(if (atom name) '*advice-hash-table* (cadr name)))
		  (,obj
		    (or
		     (gethash ,realname ,db)
		     (setf (gethash ,realname ,db)
			   (make-advisable-function-object
			    (lambda (&rest ,dispatch-args)
			      ,(format nil "Advice dispatch function for ~A"
				       realname)
			      (let* ((,obj (gethash ,realname ,db))
				     (,main (advisable-function-main ,obj))
				     (,before (advisable-function-before ,obj))
				     (,after (advisable-function-after ,obj))
				     (,around (advisable-function-around ,obj)))
				(prog2 (when ,before
					 (apply ,before ,dispatch-args))
				    (if ,around
					(apply ,around ,dispatch-args)
					(apply ,main ,dispatch-args))
				  (when ,after (apply ,after ,dispatch-args)))))
			    (symbol-function ,realname)))))
		  (,fn
		    ,(if (eql qualifier :around)
			 `(lambda (&rest ,restarg)
			    ,@(when docstring (list docstring))
			    (macrolet ((call-main-function ()
					 `(apply (advisable-function-main ,',obj)
						 ,',restarg))
				       (call-main-function-with-args
					   (&rest callargs)
					 `(apply (advisable-function-main ,',obj)
						 (list ,@callargs))))
			      (destructuring-bind ,argslist ,restarg
				,@(when declarations (list declarations))
				,@realbody)))
			 `(lambda ,argslist
			    ,@(when docstring (list docstring))
			    ,@(when declarations (list declarations))
			    ,@realbody))))
	     (when (not (equal (symbol-function ,realname)
			       (advisable-function-dispatch ,obj)))
	       (setf (symbol-function ,realname)
		     (advisable-function-dispatch ,obj)))
	     (setf ,@(case qualifier
		       (:after `((advisable-function-after ,obj)))
		       (:before `((advisable-function-before ,obj)))
		       (:around `((advisable-function-around ,obj))))
		   ,fn))
	 (error 'symbol-not-fbound-error
		:symbol ',(if (atom name) name (car name)))))))

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

(defun advice-documentation (symbol &key (db *advice-hash-table*))
  "returns an alist of the documentation for all advices and the main function"
  (with-advisable-object (obj symbol :db db)
    (list (cons :before (documentation (advisable-function-before obj) 'function))
	  (cons :around (documentation (advisable-function-around obj) 'function))
	  (cons :main (documentation (advisable-function-main obj) 'function))
	  (cons :after (documentation (advisable-function-after obj) 'function)))))
