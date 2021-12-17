#lang racket

(require threading)

(define (read-fishes file)
  (~> (file->string file)
      (string-split ",")
      (map (compose string->number string-trim) _)))

(define (count-fishes fishes)
  (for/fold ([counts (make-immutable-hash)])
            ([fish (in-list fishes)])
    (hash-update counts fish add1 0)))

(define (step fish-counts)
  (for*/fold ([counts (make-immutable-hash)])
             ([age (in-inclusive-range 0 8)]
              [count (in-value (hash-ref fish-counts age #f))]
              #:when count)
    (cond
      [(zero? age) (hash-set (hash-set counts 8 count) 6 count)]
      [else (hash-update counts (sub1 age) (curry + count) 0)])))

(define (solution fish-count days)
  (for/fold ([gen fish-count]
             #:result (apply + (hash-values gen)))
            ([_ (in-range days)])
    (step gen)))

(define fish-count (count-fishes (read-fishes "input.txt")))
(displayln (solution fish-count 80))
(displayln (solution fish-count 256))

(module+ test
  (require rackunit)

  (define sample (count-fishes (list 3 4 3 1 2)))

  (check-equal? (solution sample 80) 5934)
  (check-equal? (solution sample 256) 26984457539))
