#lang racket

(define input (map string->list (file->lines "input")))

(define height (length input))
(define width (length (car input)))

(define (access x y)
  (list-ref (list-ref input y) x))

(define (in-bounds? x y)
  (cond [(< x 0) #f]
        [(>= x width) #f]
        [(< y 0) #f]
        [(>= y height) #f]
        [else #t]))

(define (roll? x y)
  (if (char=? (access x y) #\@) 1 0))

(define (neighbors x y)
  (for*/sum ([a '(-1 0 1)]
              [b '(-1 0 1)]
              #:unless (and (= a 0) (= b 0)))
    (cond [(in-bounds? (+ x a) (+ y b)) (roll? (+ x a) (+ y b))]
          [else 0])))

(for*/sum ([x width]
            [y height]
            #:when (= (roll? x y) 1))
  (if (< (neighbors x y) 4) 1 0))
