#lang racket

(define (parse-input file)
  (define-values (section-sep num-sep board-row-sep)
    (values "\n\n" "," "\n"))
  (define sections (string-split (file->string file) section-sep))
  (define numbers (map string->number
                       (string-split (first sections) num-sep)))
  (define boards (for/list ([line (rest sections)])
                   (define rows (string-split line board-row-sep))
                   (for/list ([row (map string-split rows)])
                     (map string->number row))))
  (values numbers boards))

(define-values (numbers boards) (parse-input "input.txt"))

(define (score board pins)
  (for/sum ([n (flatten board)] [pin (flatten pins)])
    (if pin 0 n)))

(define (bingo? pins)
  (define (has-marked? rows)
    (ormap identity (map (curry andmap identity) rows)))
  (define (transpose l) (apply map list l))
  (or (has-marked? pins)
      (has-marked? (transpose pins))))

(define (find-index board n)
  (for*/first ([(row i) (in-indexed board)]
               [(val j) (in-indexed row)]
               #:when (eq? n val))
    (cons i j)))

(define (mark board pins n)
  (match (find-index board n)
    [#f pins]
    [(cons i j) (let ([row (list-ref pins i)])
                  (list-set pins i (list-set row j #t)))]))

(define (make-pins rows cols)
  (make-list rows (make-list cols #f)))

(define (make-boards-state boards)
  (for/hash ([b (in-list boards)])
    (values b (make-pins 5 5))))

(define (next-move state n board pins)
  (define win-score (* n (score board (mark board pins n))))
  (define new-state (hash-set state board (mark board pins n)))
  (values win-score new-state))

(define (solution-one boards numbers)
  (for*/fold ([answer #f]
              [state (make-boards-state boards)] #:result answer)
             ([n (in-list numbers)]
              [(b p) (in-hash state)] #:final (bingo? (mark b p n)))
    (next-move state n b p)))

(define (solution-two boards numbers)
  (for*/fold ([answer #f]
              [state (make-boards-state boards)] #:result answer)
             ([n (in-list numbers)]
              [(b p) (in-hash state)] #:unless (bingo? p))
    (next-move state n b p)))

(displayln (solution-one boards numbers))
(displayln (solution-two boards numbers))

(module+ test
  (require rackunit)

  (define numbers '(7 4 9 5 11 17 23 2 0 14 21 24 10 16 13
                      6 15 25 12 22 18 20 8 19 3 26 1))

  (define boards '(((22 13 17 11 0)
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
                    (2 0 12 3 7))))

  (check-equal? (solution-one boards numbers) 4512)
  (check-equal? (solution-two boards numbers) 1924))
