#lang racket


(define (get_data fname) 
  (define port (open-input-file fname))
  (define d (read-line port))
  (string-split d ",")
)


(define (id_is_invalid id) 
  (define l (string-length id))
  (cond
    [(not (= (modulo l 2) 0)) #f]
    [else (let 
            (
              [left (substring id 0 (/ l 2))]
              [right (substring id (/ l 2))]
            )
            (if (string=? left right) (println id) '())
           (string=? left right)
           )]
    )

  )

(define (get_invalid_ids r) 
    (apply + 
      (map string->number 
        (filter id_is_invalid 
          (map number->string r)
        )
      )
    )
  )

(define atoi string->number )
(define nts number->string )

(define (get_range_from_string s)
  (define l (string-split s "-"))
  (range (atoi (car l)) (add1 (atoi (car(cdr l)))))
)

(define (get_total_invalids data)
  (apply + (map get_invalid_ids (map get_range_from_string data)))
)


(define data (get_data "input.txt"))
(println data)
(get_total_invalids data)


