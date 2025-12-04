TODO: unfinished

#lang racket

(define input (map string->list (file->lines "toy-input")))

(define height (length input))
(define width (length (car input)))

(define (access x y)
  (list-ref (list-ref input y) x))

(define (rem x y)
  (list-set input y (list-set (list-ref input y) x #\.)))

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

(define remmed 0)
(define removable (make-hash))
(define (cycle removed removed-last)
  (cond [(= removed-last 0) removed]
        [else (begin
                      (for* ([x width]
                             [y height]
                             #:when (= (roll? x y) 1))
                        (if (< (neighbors x y) 4) (hash-set! removable (list x y) #t) (hash-set! removable (list x y) #f)))
                      (for ([ht removable])
                        (if (hash-ref removable ht) (begin (rem (car ht) (cadr ht)) (set! remmed (add1 remmed))) #f))
                      (hash-clear removable)
                      (cycle (+ removed remmed) remmed))]))

(cycle 0 1)