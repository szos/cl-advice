About CL-ADVICE
---------------
This allows you to advise functions in Common Lisp. The main macros to use are
`add-advice` and `defadvice`. `defadvice` expands into a call to `add-advice`.
`add-advice` is used like so:
`(add-advice (q-qualifier qualifier) name/db args body...)`
It does the following:
Define advice of type QUALIFIER for the function NAME. If an advisable-function
object doesnt exist an error is signalled.

Q-QUALIFIER denotes how to wrap pre-existing advice. It can be one of :before 
:after :around or :base. :before and :after will run their advice before and after
pre-existing advice respectively. :around will expose the local macros called 
CALL-NEXT-ADVICE and CALL-NEXT-ADVICE-WITH-ARGS for usage in BODY. If a call to one
of these macros does not occur in BODY then no further advice will be run. :base
will overwrite pre-existing advice.

QUALIFIER denotes what type of advice to create. It can have the value :before 
:after or :around. These will run before, after or around the main function 
respectively. :around advice will expose the local macros CALL-MAIN-FUNCTION and 
CALL-MAIN-FUNCTION-WITH-ARGS. If calls to these macros dont appear in BODY the main
function will not be called. If both Q-QUALIFIER and QUALIFIER are :around, the 
local macros CALL-NEXT-ADVICE, CALL-NEXT-ADVICE-WITH-ARGS, CALL-MAIN-FUNCTION, and
CALL-MAIN-FUNCTION-WITH-ARGS will be exposed to BODY. Advice should only ever call
one of these macros to avoid running advice and the main function multiple times. 

About :around advice - :around advice must be careful to return the correct value. 
The final value of around advice is returned, so if one is doing something after a 
call to CALL-MAIN-FUNCTION or CALL-NEXT-ADVICE, one must properly capture the 
results of that call in order to return them. :before and :after advice does not 
have this problem.

NAME/DB denotes the name and database for advice to be stored in. It can be a 
symbol, in which case it denotes the name of the function to advise and the 
database defaults to *advice-hash-table*, or it can be a list, in which case the 
car denotes the name of the function to advise and the cadr denotes the database.

ARGS must either conform to the arglist of the function denoted by NAME, be of the 
form (&rest rest), or be of the form (&ignore). If &ignore is specified then any 
arguments will not be accessible to the advice. 

BODY is the body of the advice. It may contain a docstring and declarations.

How To
------
Advice can be defined with `defadvice`, like so:
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
