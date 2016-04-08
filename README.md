# Racket Lisp

Small Lisp Interpreter in Racket. A playground for implementing language constructs and lisp functionalities (macros, recursive functions, TCO, ...)

## Features

* Recursive functions
* Lisp1
* Lexical scoping

## Values

The following values have been implemented:
* var -> immutable variable
* int -> integer
* fun -> lambda
* aunit -> '()
* apair -> cons cell
* closure -> regular closure

### TODO

* bool
* string
* set
* hashmap
* keyword
* symbol

## Special Forms

The following special forms have been implemented:
* ifgreater
* mlet -> let

### TODO

* cond

## Macros

The macro system is just implemented via regular racket functions that get executed before compile time -> macro expansion time.

* ifaunit
* mlet* -> let*

### TODO

* clojure like threading macros -> and ->>
* if (when the bool type is added)

## Core language functions

* map

### TODO

* filter
* remove
* reduce -> foldl/foldr
