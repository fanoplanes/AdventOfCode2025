#lang racket

(define input (map string->list (file->lines "input")))

(define rack-no (length input))
(define batt-no (length (car input)))

;; expect 12 digits

(define (maximize rack digit result)
  (define window (drop-right rack (- 12 digit)))
  (define chr (argmax (lambda (ch) (- (char->integer ch) 48)) window))
  (define maxxed (list-tail (member chr rack) 1))
  (define dgt (- (char->integer chr) 48))
  (define updated (+ dgt (* 10 result)))
  (cond [(= digit 12) updated]
        [else (maximize maxxed (add1 digit) updated)]))

(for/sum ([i input])
  (maximize i 1 0))