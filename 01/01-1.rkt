#lang racket

(require racket/list/iteration)

(define input (file->lines "input"))
(define g-i (map (lambda (str) (string-replace (string-replace str "R" "+") "L" "-")) input))
(define nums (map string->number g-i))

(count (lambda (n) (= n 0)) (map (lambda (x) (modulo x 100)) (running-foldl + 50 nums)))