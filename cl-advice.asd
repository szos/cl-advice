;;;; cl-advice.asd

(asdf:defsystem #:cl-advice
  :description "Describe cl-advice here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "cl-advice")))
