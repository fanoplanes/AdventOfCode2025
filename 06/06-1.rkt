#lang racket

(define input (map string-split (file->lines "input")))

(define numbers (map (lambda (x) (map string->number x)) (drop-right input 1)))

(define ops (make-hash '(("+" . +) ("*" . *))))

(define operations (map (lambda (x) (hash-ref ops x)) (last input)))

(define (transpose lst)
  (define y-dim (length lst))
  (define x-dim (length (car lst)))
  (for/list ([x x-dim])
    (for/list ([y y-dim])
      (list-ref (list-ref numbers y) x))))

(define transposed-numbers (transpose numbers))

(define ns (make-base-namespace))

(for/sum ([n (length operations)])
         (apply (eval (list-ref operations n) ns) (list-ref transposed-numbers n)))
