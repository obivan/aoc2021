#lang racket

(define (zip l r) (for/list ([l l] [r r]) (list l r)))

(define (increase? p) (< (first p) (second p)))

(module+ test
  (require rackunit)

  (define sample '(199 200 208 210 200 207 240 269 260 263))
  (define pairs (zip sample (drop sample 1)))

  (check-equal? 7 (count increase? pairs)))

(define input (map string->number (file->lines "input.txt")))

(define part-one (zip input (drop input 1)))
(displayln (count increase? part-one))

(define part-two (zip input (drop input 3)))
(displayln (count increase? part-two))
