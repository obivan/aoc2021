#lang racket

(define (read-data filename)
  (for/list ([line (in-list (file->lines filename))])
    (match-define (list digits actual)
      (map string-split (string-split line " | ")))
    (list digits actual)))

(define (solution-one data)
  (for*/sum ([actual (in-list (map second data))]
             [output (in-list actual)])
    (case (string-length output)
      [(2 3 4 7) 1]
      [else 0])))

(displayln (solution-one (read-data "input.txt")))

; 2 -> 1
; 3 -> 7
; 4 -> 4
; 5 -> 2 3 5
; 6 -> 6 9 0
; 7 -> 8
