#lang racket/base

(require racket/string)
(require br)

;; Convert input file into annotated s-expressions
(define (read-syntax name port)
  ;; Assume our file only contains one string for now
  (define src-line (string-trim (port->string port)))
  ;; Return an s-expression that runs a module with our
  ;; code, and our input as an s-expression. It just happens
  ;; to be a single string s-expression.
  ;;
  ;; aoc2015-start is the name we give our module
  ;; "main.rkt" defines the 'builtin' bindings for our module
  `(module aoc2015-star1 "day01.rkt"
      ,src-line))
(provide read-syntax)

;; Our expander, i.e. run a program with s-expressions above.
;; In our case we do a fixed set of displaying computations of
;; the one s-expression we know we will get.
;;
;; We have to rename our macro to the conventional one called by module,
;; but we can't name it that in here as racket/base has its own. So we
;; export it for when the module is called.
(define-macro (aoc2015-module-begin EXPR)
  #'(#%module-begin
     (displayln (star1 EXPR))
     (displayln (star2 EXPR))))
(provide (rename-out [aoc2015-module-begin #%module-begin]))

;; Starting at floor 0, if ( means go up a stair and ) goes down a stair,
;; return the floor we wind up at.
(define (star1 str)
  (for/sum ([sigil (in-string str)])
    (cond [(eqv? sigil #\() 1]
	  [(eqv? sigil #\)) -1])))
(provide star1)

;; Find the position of the character in the string (representing going up
;; or down a floor) that leads us into the basement (floor -1)
(define (star2 str)
  (for/fold ([curr-pos 0]
	     [curr-floor 0]
	     #:result curr-pos)
      ([sigil (in-string str)]
       #:break (negative? curr-floor))
    (let ([op (if (eqv? sigil #\()
		  add1
		  sub1)])
      (values (add1 curr-pos) (op curr-floor)))))
(provide star2)

;; A special submodule that is only loaded in when we
;; run the module via `raco test <file>` or equivalent.
;; We won't run these when we load the file into a repl
;; or load it as part of a DSL invocation.
(module+ test
  (require rackunit)
  (check-eq? (star1 "(())") 0)
  (check-eq? (star1 "()()") 0)
  (check-eq? (star1 "(((") 3)
  (check-eq? (star1 "(()(()(") 3)
  (check-eq? (star1 "))(((((") 3)
  (check-eq? (star1 "())") -1)
  (check-eq? (star1 "))(") -1)
  (check-eq? (star1 ")))") -3)
  (check-eq? (star1 ")())())") -3)

  (check-eq? (star2 ")))") 1)
  (check-eq? (star2 "()())") 5))

;; As a language module we need to provide these,
;; but we don't care to write these, we just use
;; the ones supplied by br
(provide #%top #%app #%datum #%top-interaction)
