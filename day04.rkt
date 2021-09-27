#lang racket

(require file/md5)

(define (star str matchlen)
  (let (; Since we wind up dealing with bytes, construct our
	; termination value as a byte string.
	[matchstr (apply bytes (make-list matchlen 48))])
    (for/first ([i (in-naturals)]
		#:when (equal? (subbytes
				; openssl/md5 is faster but requires a library
				(md5
				 (string-append-immutable
				  str
				  (number->string i)))
				0 matchlen)
			       matchstr))
      i)))

(define (star1 str)
  (star str 5))

(define (star2 str)
  (star str 6))

(module+ test 
  (require rackunit)

  (check-equal? (star1 "abcdef") 609043)
  (check-equal? (star1 "pqrstuv") 1048970))

(module+ main
  (let ([input "yzbqklnj"])
    (displayln (star1 input))
    (displayln (star2 input))))
