#lang racket

(define data (file->lines "input.txt"))

(define (length lst)
  (cond
    [(empty? lst) 0]
    [(cons? lst) (+ 1 (length (rest lst)))]))

(define rows (length data))
(define cols rows)

(define (is_in_range n m) 
  (cond 
       [(>= n m) #f]
       [(< n 0) #f]
       [else #t]))

(define (get_char data x y) (string-ref (list-ref data y) x))
(define (is_char data x y) (if (and (is_in_range x cols) (is_in_range y rows)) (char=? (get_char data x y) #\@) #f))

(define offsets '(
                  (-1 . -1)(0 . -1)(1 . -1)
                  (-1 . 0)          (1 . 0)
                  (-1 . 1) (0 . 1) (1 . 1)
                  )
)
(define identity (lambda (x)x))

(define (get_count data x y)
  (count identity (map (lambda (p)  
         (is_char data (+ (car p) x) (+ (cdr p) y))
         ) offsets))
  )

(define (get_all)
  (map (lambda (y) (map 
                     (lambda (x) (if (and (is_char data x y) (<(get_count data x y)4)) #t #f )) 
                     (range 0 cols)
                   ))
       (range 0 rows)))

(apply + (map (lambda (f) (count identity f)) (get_all)))
