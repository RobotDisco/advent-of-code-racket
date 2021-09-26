#lang racket/base

(require peg)

;; I have no idea what I'm doing and I drunkard's-walked
;; my way into getting a PEG Parser working as closely
;; to my brag expectations as possible.
;;
;; TODO Make it look more like https://github.com/rain-1/nand-lang,
;; the author's example of using PEG

(define-peg aoc2015d03-op
  (name res (or (char #\<) (char #\>) (char #\v) (char #\^)))
  `(aoc2015d03-op ,res))

(define-peg/drop aoc2015d03-newline
  (char #\newline))

(define-peg aoc2015d03-program
  (name res (* (or aoc2015d03-newline aoc2015d03-op)))
  `(aoc2015d03-program ,@res))

(require racket/port)
(define (parse port)
  (peg aoc2015d03-program (port->string port)))
(provide parse)
