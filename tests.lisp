(defpackage :cl-advice-tests
  (:use :cl :fiveam)
  (:import-from :cl-advice
                #:defun-advisable
                #:advisable-function-p
                #:make-advisable
                #:make-unadvisable
                #:add-advice
                #:remove-advice
                #:replace-advice)
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
  (is (advisable-function-p (symbol-function 'advisable-main)) "Is advisable")

  (is (string= (foutput 'main) "main.") "Without advice")
  (is (not (advisable-function-p (symbol-function 'main))) "Not advisable")

  (signals error (add-advice :before 'main 'before) "Add advice to non advisable fails")

  (make-advisable 'main)
  (is (advisable-function-p (symbol-function 'main)) "Is advisable")

  (is (string= (foutput 'main) "main.") "Eval after advisable")

  (add-advice :before 'main 'before)

  (is (string= (foutput 'main) "before.main.") "Before advice")

  (add-advice :before 'main 'before1)
  (is (string= (foutput 'main) "before1.before.main.") "Add before advice again")

  (replace-advice :before 'main 'before1 'before)
  (is (string= (foutput 'main) "before.before.main.") "Replace before advice")

  (remove-advice :before 'main 'before)
  (remove-advice :before 'main 'before1)

  (is (string= (foutput 'main) "main.") "After remove :before advice")

  (add-advice :after 'main 'after)
  (is (string= (foutput 'main) "main.after.") "After advice")

  (remove-advice :after 'main 'after)
  (is (string= (foutput 'main) "main.") "After remove :after advice")

  (add-advice :around 'main 'around)
  (is (string= (foutput 'main) "begin.main.end.") "Around advice")

  (remove-advice :around 'main 'around)
  (is (string= (foutput 'main) "main.") "After remove :around advice")

  ;; all advices
  (add-advice :before 'main 'before)
  (add-advice :after 'main 'after)
  (add-advice :around 'main 'around)

  (is (string= (foutput 'main) "before.begin.main.end.after.") "All advices")

  (make-unadvisable 'main)

  (is (string= (foutput 'main) "main.") "After make-unadvisable")
  (is (not (advisable-function-p (symbol-function 'main))))
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
  (is (not (advisable-function-p (symbol-function 'main-args))) "Not advisable")

  (signals error (add-advice :before 'main-args 'before-args) "Add advice to non advisable fails")

  (make-advisable 'main-args)
  (is (advisable-function-p (symbol-function 'main-args)) "Is advisable")

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "Eval after advisable")

  (add-advice :before 'main-args 'before-args)

  (is (string= (foutput 'main-args 'x 'y) "before(X Y).main(X Y).") "Before advice")

  (add-advice :before 'main-args 'before1-args)
  (is (string= (foutput 'main-args 'x 'y) "before1(X Y).before(X Y).main(X Y).") "Add before advice again")

  (replace-advice :before 'main-args 'before1-args 'before-args)
  (is (string= (foutput 'main-args 'x 'y) "before(X Y).before(X Y).main(X Y).") "Replace before advice")

  (remove-advice :before 'main-args 'before-args)
  (remove-advice :before 'main-args 'before1-args)

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :before advice")

  (add-advice :after 'main-args 'after-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).after(X Y).") "After advice")

  (remove-advice :after 'main-args 'after-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :after advice")

  (add-advice :around 'main-args 'around-args)
  (is (string= (foutput 'main-args 'x 'y) "begin(X Y).main(X Y).end.") "Around advice")

  (remove-advice :around 'main-args 'around-args)
  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After remove :around advice")

  ;; all advices
  (add-advice :before 'main-args 'before-args)
  (add-advice :after 'main-args 'after-args)
  (add-advice :around 'main-args 'around-args)

  (is (string= (foutput 'main-args 'x 'y) "before(X Y).begin(X Y).main(X Y).end.after(X Y).") "All advices")

  (make-unadvisable 'main-args)

  (is (string= (foutput 'main-args 'x 'y) "main(X Y).") "After make-unadvisable")
  (is (not (advisable-function-p (symbol-function 'main-args))))
  )
