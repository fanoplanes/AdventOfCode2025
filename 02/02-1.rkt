#lang racket

(require racket/list/grouping)
(require data/integer-set)

(define input (string-split (string-trim (file->string "toy-input")) "," ))

(define split-input (map (lambda (str) (string-split str "-")) input))

(define ranges (map (lambda (x) (make-range (string->number (car x)) (string->number (cadr x)))) split-input))

(define (is-repnum? num)
  (define l-n (string->list (number->string num)))
  (define split (list (take l-n (ceiling (/ (length l-n) 2))) (drop l-n (ceiling (/ (length l-n) 2)))))
  (define nums (map (lambda (s) (string->number (list->string s))) split))
  (if (equal? (car nums) (cadr nums)) #t #f))

(for/sum ([i ranges])
  (for/sum ([j i])
    (if (and (is-repnum? j) (even? (string-length (number->string j)))) j 0)))