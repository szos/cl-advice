;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:symbol-not-fbound-error
	   #:no-advisable-function-found-error
	   #:with-advisable-object
	   #:defadvice
	   #:delete-advice
	   #:activate-advice
	   #:deactivate-advice
	   #:advice-documentation))
