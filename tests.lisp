(defpackage :cl-advice-tests
  (:use :cl :cl-advice :fiveam)
  (:export :run-tests))

(in-package :cl-advice-tests)

(defun run-tests ()
  (fiveam:run! 'cl-advice-test-suite))

(def-suite* cl-advice-test-suite)

(defun foutput (fun &rest args)
  (with-output-to-string (*standard-output*)
    (apply fun args)))

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
  )

(defun main-args (x y)
  (format t "main~a." (list x y)))

(defun before-args (x y)
  (format t "before~a." (list x y)))

(defun after-args (x y)
  (format t "after~a." (list x y)))

(defun around-args (next x y)
  (format t "begin~a." (list x y))
  (funcall next x y)
  (format t "end."))

(defun before1-args (x y)
  (format t "before1~a." (list x y)))


(def-test advice-args-tests ()
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "Without advice")
  (is (not (cl-advice:advisable-function-p (symbol-function 'main-args))) "Not advisable")

  (signals error (cl-advice:add-advice :before 'main-args 'before-args) "Add advice to non advisable fails")

  (cl-advice:make-advisable 'main-args)
  (is (cl-advice:advisable-function-p (symbol-function 'main-args)) "Is advisable")

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "Eval after advisable")

  (cl-advice:add-advice :before 'main-args 'before-args)

  (is (string= (foutput 'main-args 'x 'y) "before(X Y).main(X Y).") "Before advice")

  (cl-advice:add-advice :before 'main-args 'before1-args)
  (is (string= (foutput 'main-args 'x 'y) "before1(X Y).before(X Y).main(X Y).") "Add before advice again")

  (cl-advice:replace-advice :before 'main-args 'before1-args 'before-args)
  (is (string= (foutput 'main-args 'x 'y) "before(X Y).before(X Y).main(X Y).") "Replace before advice")

  (cl-advice:remove-advice :before 'main-args 'before-args)
  (cl-advice:remove-advice :before 'main-args 'before1-args)

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :before advice")

  (cl-advice:add-advice :after 'main-args 'after-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).after(X Y).") "After advice")

  (cl-advice:remove-advice :after 'main-args 'after-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :after advice")

  (cl-advice:add-advice :around 'main-args 'around-args)
  (is (string= (foutput 'main-args 'x 'y) "begin(X Y).main(X Y).end.") "Around advice")

  (cl-advice:remove-advice :around 'main-args 'around-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :around advice")

  ;; all advices
  (cl-advice:add-advice :before 'main-args 'before-args)
  (cl-advice:add-advice :after 'main-args 'after-args)
  (cl-advice:add-advice :around 'main-args 'around-args)

  (is (string= (foutput 'main-args 'x 'y) "before(X Y).begin(X Y).main(X Y).end.after(X Y).") "All advices")

  (cl-advice:make-unadvisable 'main-args)

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After make-unadvisable")
  (is (not (cl-advice:advisable-function-p (symbol-function 'main-args))))
)
