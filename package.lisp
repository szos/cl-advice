;;;; package.lisp

(defpackage #:cl-advice
  (:use #:cl)
  (:export
   #:define-advisable
   #:make-advisable
   #:add-advice
   #:advise-function
   #:list-advice
   #:remove-nth-advice
   #:remove-advice))
