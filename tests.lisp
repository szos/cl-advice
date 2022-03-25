(defpackage :cl-advice-tests
  (:use :cl :cl-advice :fiveam))

(in-package :cl-advice-tests)

(defun foutput (fun)
  (with-output-to-string (*standard-output*)
    (funcall fun)))

(defun-advisable advisable-main ()
  (format t "main."))

(defun main ()
  (format t "main."))

(defun before ()
  (format t "before."))

(defun after ()
  (format t "after."))

(defun around (next)
  (format t "begin.")
  (funcall next)
  (format t "end."))

(defun before1 ()
  (format t "before1."))

(defun run-tests ()
  (fiveam:run! 'advice-tests))

(def-test advice-tests ()
  (is (string= (foutput 'advisable-main) "main.") "Advisable function")
  (is (cl-advice:advisable-function-p (symbol-function 'advisable-main)) "Is advisable")
  
  (is (string= (foutput 'main) "main.") "Without advice")
  (is (not (cl-advice:advisable-function-p (symbol-function 'main))) "Not advisable")

  (signals error (cl-advice:add-advice :before 'main 'before) "Add advice to non advisable fails") 
  
  (cl-advice:make-advisable 'main)
  (is (cl-advice:advisable-function-p (symbol-function 'main)) "Is advisable")

  (is (string= (foutput 'main) "main.") "Eval after advisable")

  (cl-advice:add-advice :before 'main 'before)
  
  (is (string= (foutput 'main) "before.main.") "Before advice")

  (cl-advice:add-advice :before 'main 'before1)
  (is (string= (foutput 'main) "before1.before.main.") "Add before advice again")

  (cl-advice:replace-advice :before 'main 'before1 'before)
  (is (string= (foutput 'main) "before.before.main.") "Replace before advice")
  
  (cl-advice:remove-advice :before 'main 'before)
  (cl-advice:remove-advice :before 'main 'before1)

  (is (string= (foutput 'main) "main.") "After remove :before advice")

    (cl-advice:add-advice :after 'main 'after)
  (is (string= (foutput 'main) "main.after.") "After advice")

  (cl-advice:remove-advice :after 'main 'after)
  (is (string= (foutput 'main) "main.") "After remove :after advice")

  (cl-advice:add-advice :around 'main 'around)
  (is (string= (foutput 'main) "begin.main.end.") "Around advice")

  (cl-advice:remove-advice :around 'main 'around)
  (is (string= (foutput 'main) "main.") "After remove :around advice")

  ;; all advices
  (cl-advice:add-advice :before 'main 'before)
  (cl-advice:add-advice :after 'main 'after)
  (cl-advice:add-advice :around 'main 'around)

  (is (string= (foutput 'main) "before.begin.main.end.after.") "All advices")
  
  (cl-advice:make-unadvisable 'main)

  (is (string= (foutput 'main) "main.") "After make-unadvisable")
  (is (not (cl-advice:advisable-function-p (symbol-function 'main))))
