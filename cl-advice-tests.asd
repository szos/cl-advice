;;;; cl-advice-tests.asd

(asdf:defsystem #:cl-advice-tests
  :description "Tests for cl-advice"
  :author "szos at posteo dot net"
  :license  "LGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-advice #:fiveam)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :cl-advice-tests :run-tests)))
