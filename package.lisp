;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export #:advisable-function-p
           #:make-advisable
           #:make-unadvisable
           #:ensure-advisable-function
           #:ensure-unadvisable-function
           #:with-implicit-conversion
           
           #:advisable-lambda
           #:defun-advisable
           
           #:*allow-implicit-conversion*

           #:define-advisory-functions
           #:add-advice
           #:replace-advice
           #:list-advice
           #:remove-advice
           #:remove-nth-advice

           #:implicit-conversion-to-advisable-function
           #:circular-advice-dependency))
