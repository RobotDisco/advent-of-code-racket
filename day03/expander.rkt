#lang racket/base

(require br)

(define-macro (aoc2015d3-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [aoc2015d3-module-begin #%module-begin]))

(define (fold-funcs f state aoc2015d03-funcs)
  (for/fold ([curr-state state])
      ([aoc2015d03-func (in-list aoc2015d03-funcs)])
    (aoc2015d03-func f curr-state)))

(define-macro (aoc2015d03-program OP-ARG ...)
  #'(begin
      (define init-state-s1 (list
			  ;; per-house gift-delivery count
			  (make-hash '(((0 . 0) . 1)))
			  ;; position (start at 0 0)
			  '(0 . 0)))
      (define init-state-s2 (list
			     ;; per-house gift-delivery count
			     (make-hash '(((0 . 0) . 2)))
			     ;; real santa position
			     '(0 . 0)
			     ;; robo-santa position
			     '(0 . 0)
			     ;; is it robo-santa's turn?
			     #f))
      (define final-state-s1
	(fold-funcs deliver-gift-s1 init-state-s1 (list OP-ARG ...)))
      (define final-state-s2
	(fold-funcs deliver-gift-s2 init-state-s2 (list OP-ARG ...)))
      (define star1-answer
	(hash-count (car final-state-s1)))
      (define star2-answer
	(hash-count (car final-state-s2)))
      (displayln star1-answer)
      (displayln star2-answer)))
(provide aoc2015d03-program)

(define-macro-cases aoc2015d03-op
  [(aoc2015d03-op ">") #'go-east]
  [(aoc2015d03-op "<") #'go-west]
  [(aoc2015d03-op "^") #'go-north]
  [(aoc2015d03-op "v") #'go-south])
(provide aoc2015d03-op)

(require racket/match)

(define (deliver-gift-s1 curr-state deltax deltay)
  (match-let ([(list houses currpos) curr-state])
    (define pos-pair (cons (+ (car currpos) deltax)
			   (+ (cdr currpos) deltay)))
    ;; This is an in-place update, thus returns void
    (hash-set! houses pos-pair (add1 (hash-ref houses pos-pair 0)))
    (list
     houses ; updated in-place
     pos-pair)))

(define (deliver-gift-s2 curr-state deltax deltay)
  (match-let ([(list houses
		     currpos
		     robo-currpos
		     robo-turn) curr-state])
    (define curr-pos (if robo-turn
			 robo-currpos
			 currpos))
    (define pos-pair (cons (+ (car curr-pos) deltax)
			   (+ (cdr curr-pos) deltay)))
    ;; This is an in-place update, thus returns void
    (hash-set! houses pos-pair (add1 (hash-ref houses pos-pair 0)))
    (list
     houses ; updated in-place
     (if robo-turn currpos pos-pair)
     (if robo-turn pos-pair robo-currpos)
     (not robo-turn))))
  
(define (go-east f curr-state)
  ;; Move east and deliver gifts
  (f curr-state 1 0))

(define (go-west f curr-state)
  ;; Move west and deliver gifts
  (f curr-state -1 0))

(define (go-north f curr-state)
  ;; Move north and deliver gifts
  (f curr-state 0 1))

(define (go-south f curr-state)
  ;; Move south and deliver gifts
  (f curr-state 0 -1))
