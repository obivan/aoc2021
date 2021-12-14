#lang racket

(require "board.rkt")

(define (parse-input file)
  (define sections (string-split (file->string file) "\n\n"))
  (define numbers (map string->number
                       (string-split (first sections) ",")))
  (define fields (for/list ([line (rest sections)])
                   (define rows (string-split line "\n"))
                   (for/list ([row (map string-split rows)])
                     (map string->number row))))
  (values numbers (map make-board fields)))

(define-values (numbers boards) (parse-input "input.txt"))

(define (solution-one boards numbers)
  (for*/fold ([score #f]) ([n (in-list numbers)]
                           [b (in-list boards)])
    (board-mark! b n)
    #:final (board-bingo? b)
    (* n (board-score b))))

(define (solution-two boards numbers)
  (for*/fold ([score #f]) ([n (in-list numbers)]
                           [b (in-list boards)]
                           #:unless (board-bingo? b))
    (board-mark! b n)
    (* n (board-score b))))

(displayln (solution-one boards numbers))
(displayln (solution-two boards numbers))

(module+ test
  (require rackunit)

  (define numbers '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13
                      6 15 25 12 22 18 20 8 19 3 26 1))

  (define boards (map make-board '(((22 13 17 11 0)
                                    (8 2 23 4 24)
                                    (21 9 14 16 7)
                                    (6 10 3 18 5)
                                    (1 12 20 15 19))

                                   ((3 15 0 2 22)
                                    (9 18 13 17 5)
                                    (19 8 7 25 23)
                                    (20 11 10 24 4)
                                    (14 21 16 12 6))

                                   ((14 21 17 24 4)
                                    (10 16 15 9 19)
                                    (18 8 23 26 20)
                                    (22 11 13 6 5)
                                    (2 0 12 3 7)))))

  (check-equal? (solution-one boards numbers) 4512)
  (check-equal? (solution-two boards numbers) 1924))
