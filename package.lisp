;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:advisable-lambda
           #:define-advisory-functions
           #:add-advice
           #:replace-advice
           #:list-advice
           #:remove-advice
           #:remove-nth-advice
	   #:remove-advice-all
	   #:make-unadvisable))
