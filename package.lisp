;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:advisable-function-p
           #:make-advisable
	   #:make-unadvisable
           #:advisable-lambda
           #:defun-advisable
           #:ensure-advisable-function
           #:with-implicit-conversion

           #:*allow-implicit-conversion*

           #:define-advisory-functions
           #:add-advice
           #:replace-advice
           #:list-advice
           #:remove-advice
           #:remove-nth-advice

           #:implicit-conversion-to-advisable-function))
