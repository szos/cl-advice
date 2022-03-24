;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:advisable-function-p
           #:make-advisable
	   #:make-unadvisable
           #:advisable-lambda
           #:defun-advisable

           #:define-advisory-functions
           #:add-advice
           #:replace-advice
           #:list-advice
           #:remove-advice
           #:remove-nth-advice))
