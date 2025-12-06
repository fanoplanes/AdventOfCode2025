#lang racket

(define (transpose lst)
  (define y-dim (length lst))
  (define x-dim (length (car lst)))
  (for/list ([x x-dim])
    (for/list ([y y-dim])
      (list-ref (list-ref lst y) x))))

(define input (file->lines "input"))

(define numbers
  (string-join (map (lambda (s) (string-replace s "    " "\n"))
                    (map list->string (transpose (map string->list (drop-right input 1)))))))

(define sepped (map string-split (string-split numbers "\n")))

(define nums (map (lambda (s) (map string->number s)) sepped))

(define ops (make-hash '(("+" . +) ("*" . *))))

(define operations (map (lambda (x) (hash-ref ops x)) (string-split (last input))))

(define ns (make-base-namespace))

(for/sum ([n (length operations)]) (apply (eval (list-ref operations n) ns) (list-ref nums n)))
