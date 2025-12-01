#lang racket

(require racket/list/iteration)
(require racket/list/grouping)

(define input (file->lines "input"))
(define g-i (map (lambda (str) (string-replace (string-replace str "R" "+") "L" "-")) input))
(define nums (map string->number g-i))

(define (zero-crossing x y)
  (define sum (+ x y))
  (define mod-sum (modulo sum 100))
  (cond [(and (< sum 0) (= y 0)) (- (quotient sum 100))]
        [(< sum 0) (+ 1 (- (quotient sum 100)))]
        [(> sum 100) (quotient sum 100)]
        [(= mod-sum 0) 1]
        [else 0]))

(define run (running-foldl + 50 nums))
(define run-g (map (lambda (x) (modulo x 100)) run))

(define res (for/list ([i nums]
            [j run-g])
  (zero-crossing i j)))

(foldl + 0 res)