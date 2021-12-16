#lang racket

(require threading)

(struct point (x y) #:transparent)
(struct segment (start end) #:transparent)

(define (segment-orthogonal? s)
  (match-define (segment (point x1 y1) (point x2 y2)) s)
  (or (equal? x1 x2) (equal? y1 y2)))

(define (point-delta p1 p2)
  (values (- (point-x p1) (point-x p2))
          (- (point-y p1) (point-y p2))))

(define (segment-points s)
  (match-define (segment p1 p2) s)
  (define-values (dx dy) (point-delta p1 p2))
  (define (step d) (cond [(< 0 d) -1]
                         [(= 0 d) 0]
                         [(> 0 d) 1]))
  (define-values (step-x step-y)
    (values (step dx) (step dy)))
  (for/list ([i (in-inclusive-range 0 (max (abs dx)
                                           (abs dy)))])
    (define x (+ (point-x p1) (* step-x i)))
    (define y (+ (point-y p1) (* step-y i)))
    (point x y)))

(define (read-input file)
  (for/list ([line (file->lines file)])
    (map (compose (curry map string->number)
                  (curryr string-split ","))
         (string-split line " -> "))))

(define (input->segments lst)
  (map (compose (curry apply segment)
                (curry map (curry apply point)))
       lst))

(define (solution segments)
  (define (answer point-map)
    (for/sum ([v (in-hash-values point-map)] #:when (>= v 2)) 1))
  (for*/fold ([point-map (make-immutable-hash)]
              #:result (answer point-map))
             ([s (in-list segments)]
              (p (in-list (segment-points s))))
    (hash-update point-map p add1 0)))

(displayln (~>> (read-input "input.txt")
                (input->segments)
                (filter segment-orthogonal?)
                solution))

(displayln (~>> (read-input "input.txt")
                (input->segments)
                solution))

(module+ test
  (require rackunit)

  (define sample '(([0 9] [5 9])
                   ([8 0] [0 8])
                   ([9 4] [3 4])
                   ([2 2] [2 1])
                   ([7 0] [7 4])
                   ([6 4] [2 0])
                   ([0 9] [2 9])
                   ([3 4] [1 4])
                   ([0 0] [8 8])
                   ([5 5] [8 2])))

  (define segments (input->segments sample))

  (check-equal? (solution (filter segment-orthogonal? segments)) 5)
  (check-equal? (solution segments) 12))
