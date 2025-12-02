#lang racket

(require racket/list/grouping)
(require data/integer-set)
(require math/number-theory)

(define input (string-split (string-trim (file->string "input")) "," ))

(define split-input (map (lambda (str) (string-split str "-")) input))

(define ranges (map (lambda (x) (make-range (string->number (car x)) (string->number (cadr x)))) split-input))

(define (digit-no num)
  (inexact->exact (floor (+ (log num 10) 1))))

(define (factors num)
  (define nums (divisors num))
  (drop-right nums 1))

(define (split n m)
  (define l-n(string->list (number->string n)))
  (define separated (windows m m l-n))
  (map (lambda (x) (string->number (list->string x))) separated))

(define (string-rep str n)
  (for/fold ([acc ""])
             ([i n])
    (string-append acc str)))

(define (num-rep num n)
  (string->number (string-rep (number->string num) n)))


(define (compare l)
  (define no (length l))
  (if (apply = l)
    (num-rep (car l) no)
    #f))

(for/sum ([i ranges])
  (for/sum ([j i])
    (define res (for/or ([k (factors (digit-no j))])
      (compare (split j k))))
    (if res res 0)))