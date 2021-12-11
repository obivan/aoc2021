#lang racket

(define (parse-command str)
  (match (string-split str)
    [(list cmd x) (list (string->symbol cmd) (string->number x))]))

(define commands (map parse-command (file->lines "input.txt")))

(define (solution-one commands)
  (for/fold ([x 0] [z 0] #:result (* x z))
            ([instruction commands])
    (match instruction
      [(list 'forward n) (values (+ x n) z)]
      [(list 'down n) (values x (+ z n))]
      [(list 'up n) (values x (- z n))])))

(define (solution-two commands)
  (for/fold ([x 0] [z 0] [aim 0] #:result (* x z))
            ([instruction commands])
    (match instruction
      [(list 'forward n) (values (+ x n) (+ z (* aim n)) aim)]
      [(list 'down n) (values x z (+ aim n))]
      [(list 'up n) (values x z (- aim n))])))

(displayln (solution-one commands))
(displayln (solution-two commands))

(module+ test
  (require rackunit)

  (define sample '((forward 5)
                   (down 5)
                   (forward 8)
                   (up 3)
                   (down 8)
                   (forward 2)))

  (check-equal? 150 (solution-one sample))
  (check-equal? 900 (solution-two sample)))
