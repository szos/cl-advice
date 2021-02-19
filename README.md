CL-ADVICE
---------------
A lightweight and portable system for advising function in Common Lisp. An
arbitrary ammount of advice can be added to any advisable function. An advisable
function may be defined with `define-advisable`, while already defined functions
may be made advisable with `make-advisable`. Advice may be added to an advisable
function with `add-advice` and `advise-function`. Advice for a function may be
listed with `list-advice`, and may be removed with `remove-advice` and
`remove-nth-advice`. Advisable functions introduce a hash table lookup for every
function call.

### EXAMPLE ###
```
(define-advisable adder (a b &rest c)
    (cons a (cons b c))
  (+ a b (apply '+ c)))

=> adder

(adder 1 2)

=> 3

(advise-function (adder :next-fn-arg fn)
                 (a b &rest c)
  ((:before adder-before)
   (format t "Adding ~{~A~^, ~}~%" (cons a (cons b c))))
  ((:around adder-around)
   (format t "around advice, calling ~A~%" fn)
   (apply fn (cons a (cons b c)))))

=> (adder-around)

(adder 1 2)

Adding 1, 2
around advice, calling #<FUNCTION (LAMBDA (&REST ARGS)...>
=> 3
```

### ADVICE TYPES ###
There are three types of advice: `:BEFORE`, `:AFTER`, and `:AROUND`. An advice
functions argument list must conform to the argument list of the function being
advised. I.e. the advice function must capable of being applied to the same
arguments as the function being advised. The exception is around advice, which
must take as its first argument the next function to call.

Before and after advice is stored as a list, which is iterated through with
every function being applied to the argument list. Around advice is slightly
different - while it is also stored as a list, the next function will only be
called if the current function calls it. The next function may be the next
around advice or the main function. When funcalling or applying the next
function to the argslist, the next function in the advice list will
automatically be passed in. 


DEFINE-ADVISABLE
----------------
This macro takes a *NAME*, *ARGSLIST*, *RULE*, and function *BODY* and defines
an advisable function. *RULE* dictates how *ARGSLIST* will be parsed into a list
for applying advice to. Example:

```
(define-advisable adder (a b &rest c)
    (cons a (cons b c))
  "sum two or more numbers"
  (+ a b (apply '+ c)))
```

MAKE-ADVISABLE
--------------
This macro will redefine a pre-existing function to be advisable. It will retain
the original function definition. Calling `make-advisable` on an already
advisable function will signal an error.

```
(defun adder (a b &rest c)
  (+ a b (apply '+ c)))
(make-advisable adder (a b &rest c) (cons a (cons b c)))
```

ADD-ADVICE
----------
This function adds advice to an advisable function. It will return the advisable
function object, or if it doesn't exist, it will return nil. The arguments taken
are: `(type fn-name advice-fn &key allow-duplicates (test 'eql))`.
*TYPE* must be a keyword of `:before`, `:around`, or `:after`. *FN-NAME* is the
quoted name of the function to add advice to. *ADVICE-FN* must either be a
function object or a symbol denoting a function. *ALLOW-DUPLICATES* dictates
whether or not a function can appear multiple times in an advice list. *TEST*
must be a function suitable for `member`'s `:test` argument.

```
(add-advice :before 'adder 'adder-before-function)
```

ADVISE-FUNCTION
---------------
This macro defines functions for advising a function. It takes the arguments:
`((fn-name &key allow-duplicates test (next-fn-arg 'next)) args &body advice)`. 
*FN-NAME* is the name of the function to advise. *ALLOW-DUPLICATES* and *TEST*
are as in `add-advice`. *NEXT-FN-ARG* is the argument name to be spliced in to
the argument list of any around advice defined in the body. *ARGS* is the
argument list for all advice functions defined in the body. *ADVICE* is a set
of function definitions. These definitions must be of the form
`(:type form1 form2 ... formn)` or `((:type name) form1 form2 ... formn)`.
When an advice definition is of the first form, an anonymous function is added
with `add-advice`. When the definition is of the second, a function named *NAME*
is defined with `defun` before being added with `add-advice`. 

```
(advise-function (adder) (a b &rest c)
  ((:before adder-before-advice)
   (format t "~&Calling ADDER with arguments ~A, ~A, and ~A~%" a b c)))
```

LIST-ADVICE
-----------
This function takes a symbol denoting a function to list the advice of, and the
key arguments *TYPE* and *PRINT*, and returns a list of advice functions. *TYPE*
should be one of `:before`, `:around`, or `:after`. If it is anything else, all
advice lists are returned with `values`. When *PRINT* is true, the list of
advice is printed to stdout.

REMOVE-NTH-ADVICE
-----------------
This function takes an advice keyword, a symbol denoting a function, and a
number, and removes the nth advice from the specified type of advice for the
function. 

REMOVE-ADVICE
-------------
This function takes a type, a symbol denoting a function, a piece of advice to
remove, and the keyarg `:test`. *TYPE* must be an advice keyword, or `:all`.
The specified peice of advice is removed from the specified advice list when it
matches *TEST*. 
