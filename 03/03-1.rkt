#lang racket

(define input (file->lines "input"))

(define rack-no (length input))
(define batt-no (string-length (car input)))

(define (maximize rack)
  (apply max (for*/list ([i batt-no]
                         [j batt-no]
                         #:when (> j i))
         (+ (- (char->integer (string-ref rack j)) 48)
            (* 10 (- (char->integer (string-ref rack i)) 48))))))

(for/sum ([r input])
  (maximize r))