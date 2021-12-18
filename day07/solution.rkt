#lang racket

(require (only-in math/statistics median)
         threading)

(define (read-positions filename)
  (~> (file->string filename)
      (string-split ",")
      (map (compose string->number string-trim) _)))

(define (solution-one xs)
  (define perfect (median > xs))
  (for/sum ([x (in-list xs)]) (abs (- perfect x))))

(define (solution-two xs)
  (define (sum-1-to n) (* n (add1 n) 1/2))
  (for/fold ([fuel (foldl + 0 (map sum-1-to xs))])
            ([n (in-inclusive-range (apply min xs) (apply max xs))])
    (min fuel
         (foldl + 0 (map (compose sum-1-to abs (curry - n)) xs)))))

(define positions (read-positions "input.txt"))
(displayln (solution-one positions))
(displayln (solution-two positions))

(module+ test
  (require rackunit)

  (define sample (list 16 1 2 0 4 2 7 1 2 14))

  (check-equal? (solution-one sample) 37)
  (check-equal? (solution-two sample) 168))
