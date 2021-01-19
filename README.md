About CL-ADVICE
---------------
This allows you to advise functions in Common Lisp. Please see docstrings for more information.

How To
------
Advice is defined with `defadvice`, like so:
```
(defun test (a b) "add a and b" (+ a b))

(defadvice :before test (&ignore)
  "advice before test"
  (format t "~&running before test~%"))

(defadvice :around test (a b)
  "increment a and b by 1 each before calling main function"
  (format t "~&running around test with arguments ~a and ~a~%" a b)
  (call-main-function-with-args (+ a 1) (+ b 1)))
  
(defadvice :after test (&ignore)
  "advice after test"
  (format t "~&running after test~%"))
```

Advice can be activated and deactivated with `deactivate-advice` and `activate-advice`.

Advice can be deleted with `delete-advice`.

Documentation can be gotten with `advice-documentation`. 
