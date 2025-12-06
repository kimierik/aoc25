#lang racket

;(define ranges '( "3-5" "10-14" "16-20" "12-18"))
;(define vals '( 1 5 8 11 17 32))

; i split the files for simplicity
(define ranges (file->lines "input.txt"))
(define vals (map string->number (file->lines "vals.txt")))

(define (second v) (first (cdr v)))

(define (rangefy s) (map string->number (string-split s "-")))

(define total_range (map rangefy ranges))
(define (inrange r v )( cond
                        [(< v (first r)) #f ]
                        [(> v (second r)) #f ]
                        [else #t ]
                       ))

(define (inranges rs v)(cond 
                          [(empty? rs) #f]
                          [(inrange (car rs) v) #t]
                          [else (inranges (rest rs) v)]
                         ))

(count (lambda (n) (inranges total_range n)) vals)
