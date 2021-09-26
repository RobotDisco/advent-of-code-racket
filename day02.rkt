#lang racket/base

;; Specification: https://adventofcode.com/2015/day/2

(require br)

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define (string->dims str)
    (map string->number (regexp-split #rx"x" str)))
  (define box-dims
    (for/list ([box-string (in-lines port)]
	       #:when (non-empty-string? box-string))
      (string->dims box-string)))
  (strip-bindings
   #`(module aoc2015-day2-mod "day02.rkt"
       #,@box-dims)))
(provide read-syntax)


;; TODO summary this
;; the macro does language-specific pre-processing on EXPRS
;; before passing them to the (probably racket/base)-provided #%module-begin 
(define-macro (aoc2015-day2-module-begin . EXPRS)
  #`(#%module-begin
     (let-values ([(paper-area ribbon-length) (paper-and-ribbons 'EXPRS)])
       (displayln paper-area)
       (displayln ribbon-length))))
(provide (rename-out [aoc2015-day2-module-begin #%module-begin]))


;; Do both star questions at the same time since
;; we share some computations and it would be nice
;; not to do them twice.
(define (paper-and-ribbons gift-box-dims)
  (for/fold ([total-gift-box-wrapping-paper-area 0]
	     [total-ribbon-length 0])
      ([curr-box-dims (in-list gift-box-dims)])
    (let (;; Star 1: Wrapping paper required for each box
	  [curr-wrapping-paper-area (box-wrapping-paper-area curr-box-dims)]
	  ;; Star 2: Ribbon length required for each box
	  [curr-ribbon-length (box-ribbon-length curr-box-dims)])
      (values (+ total-gift-box-wrapping-paper-area curr-wrapping-paper-area)
	      (+ total-ribbon-length curr-ribbon-length)))))
(provide paper-and-ribbons)


;;; Helper functions

;; Find the dimensions of the smallest side of a rectangular prism box
(define (smallest-side-dims box-dims)
  ;; use plane area as a proxy for plane "size" since
  ;; a plane with a smaller area will alway have a smaller
  ;; perimeter, for example
  (define (dim-area-less-than dim1 dim2)
    (if (< (apply * dim1) (apply * dim2))
	dim1
	dim2))
  ;; Every combination of dimensions is a side
  (let ([side-dims (combinations box-dims 2)])
    (foldl dim-area-less-than (car side-dims) (cdr side-dims))))


(define (box-ribbon-length box-dims)
  (let (;; ribbon length = length * width * height
	[bow-ribbon-length (apply * box-dims)]
	;; box ribbon = perimeter of smallest area = 2 * (length + width)
	[box-ribbon-length (* 2 (apply + (smallest-side-dims box-dims)))])
    (+ box-ribbon-length bow-ribbon-length)))


(define (box-wrapping-paper-area box-dims)
  (let (;; smallest surface area = smallest area length x smallest area width
	[smallest-surface-area (apply * (smallest-side-dims box-dims))]
	;; box surface area = 2*l*w + 2*w*h + 2*l*h)
	[box-surface-area (apply + (map (λ (x) (apply * 2 x))
					;; Get all 2D combos of 3D box dims
					(combinations box-dims 2)))])
    (+ smallest-surface-area box-surface-area)))

(module+ test
  (require rackunit)

  (check-eq? (box-wrapping-paper-area '(2 3 4)) 58)
  (check-eq? (box-wrapping-paper-area '(1 1 10)) 43)

  (check-eq? (box-ribbon-length '(2 3 4)) 34)
  (check-eq? (box-ribbon-length '(1 1 10)) 14))

(provide #%top #%app #%datum #%top-interaction)
