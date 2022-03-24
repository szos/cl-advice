;;;; cl-advice.asd

(asdf:defsystem #:cl-advice
  :description "Portable advice for Common Lisp"
  :author "szos at posteo dot net"
  :license  "LGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:closer-mop)
  :components ((:file "package")
               (:file "cl-advice"))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-advice-tests))))
