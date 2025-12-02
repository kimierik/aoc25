#lang racket

(define (get_data fname) (string-split (read-line (open-input-file fname)) ","))

(define atoi string->number )
(define nts number->string )

(define (id_is_invalid id) 
  (define l (string-length id))
  (cond
    [(not (= (modulo l 2) 0)) #f]
    [else (let 
            (
              [left (substring id 0 (/ l 2))]
              [right (substring id (/ l 2))]
            ) (string=? left right))]
    )
  )

(define (get_invalid_ids r) 
    (apply + 
      (map string->number 
        (filter id_is_invalid (map number->string r))
      )))

(define (get_range_from_string s)
  (define l (string-split s "-"))
  (range (atoi (car l)) (add1 (atoi (car(cdr l)))))
)

(define (get_total_invalids data) (apply + (map get_invalid_ids (map get_range_from_string data))))

(get_total_invalids (get_data "input.txt"))
