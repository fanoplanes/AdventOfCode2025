#lang racket
(require data/integer-set)

(define input (map string-split (string-split (file->string "input") "\n\n")))

(define ranges (map (lambda (l) (map string->number l)) (map (lambda (str) (string-split str "-")) (car input))))

(define sets
  (for/list ([r ranges])
    (make-range (car r) (cadr r))))

(define unioned
  (for/fold ([acc (make-range)])
            ([s sets])
    (union acc s)))

(count unioned)