;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:symbol-not-fbound-error ; conditions
	   #:no-advisable-function-found-error
	   ;; create advisable functions
	   #:make-advisable
	   #:defadvisable
	   ;; advisable-function object access
	   #:with-advisable-object
	   #:with-advisable-object-advice
	   #:with-inner-advice
	   #:list-advice
	   #:with-advice-list
	   ;; advice creation
	   #:add-advice
	   #:defadvice
	   ;; advice management
	   #:pop-advice 
	   #:advisable-function-p
	   #:delete-advice
	   #:activate-advice
	   #:deactivate-advice
	   #:advice-documentation))
