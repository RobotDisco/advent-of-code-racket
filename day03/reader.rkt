#lang racket/base

(require br)

(require syntax/strip-context)

(require peg/peg)
(require "parser.rkt")

(provide (rename-out [literal-read read]
		     [literal-read-syntax read-syntax]))

(define (literal-read port)
  (syntax->datum
   (literal-read-syntax #f port)))

(define (literal-read-syntax path port)
  (with-syntax ([body (parse port)])
    (strip-context
     #`(module aoc2015d03-mod "expander.rkt"
	 body))))

(provide #%top #%app #%datum #%top-interaction)
