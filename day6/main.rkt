#lang racket

(define data (file->lines "input.txt"))

(define matrix (map (lambda (x) (filter non-empty-string? (string-split x " ") )) data))

(define (transpose mat) (apply map list mat))

(define transposed (transpose matrix))

;this there has to be a better way to do this
(define (to_op op)
  (cond 
    [(string=? op "+")+]
    [(string=? op "*")*])
  )

(define (calc l)
  (define _l (reverse l))
  (apply (to_op(first _l)) (map string->number (rest _l))))

(apply + (map calc transposed))
