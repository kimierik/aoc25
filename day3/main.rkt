#lang racket

(define testdata '("987654321111111" "811111111111119" "234234234234278" "818181911112111"))

(define (get_data fname) (file->lines fname))

(define (parse_to_list array) (map string->number (map string(string->list array ))))

; a bit ugly and not optimal
(define (get_biggest data)
  (define first_max (apply max (parse_to_list (substring data 0 (- (string-length data) 1 )))))
  (define findex (string-find data (number->string first_max)))
  (define secondmax (apply max (parse_to_list (substring data (+ findex 1 )))))
  (string->number (string-append (number->string first_max) (number->string secondmax)))
)

(apply + (map get_biggest (get_data "input.txt")))
