#lang racket

(define input (map string-split (string-split (file->string "input") "\n\n")))

(define ranges (map (lambda (l) (map string->number l)) (map (lambda (str) (string-split str "-")) (car input))))

(define codes (map string->number (cadr input)))

(define (check-num num)
  (for/or ([r ranges])
    (and (>= num (car r)) (<= num (cadr r)))))

(for/sum ([c codes])
  (if (check-num c) 1 0))