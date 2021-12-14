#lang racket

(require data/bit-vector
         threading)

(provide (contract-out
          [make-board (-> (and/c (non-empty-listof list?)
                                 equal-length-lists?) board?)]
          [board-pin-at! (-> board? index-pair/c void?)]
          [board-mark! (-> board? exact-integer? void?)]
          [board-score (-> board? exact-nonnegative-integer?)]
          [board-bingo? (-> board? boolean?)]
          [in-board (-> board?
                        (stream/c (cons/c exact-integer? boolean?)))]
          [board-index (-> board? (hash/c exact-integer?
                                          index-pair/c))]))

(define index-pair/c (cons/c exact-nonnegative-integer?
                             exact-nonnegative-integer?))

(define (make-board-iterator b)
  (define index (board-index b))
  (board-iterator index (board-pin-map b) (hash-iterate-first index)))

(struct board (index pin-map pin-map-transposed)
  #:property prop:sequence make-board-iterator)

(struct board-iterator (index pin-map pos)
  #:methods gen:stream
  [(define (stream-empty? it)
     (false? (board-iterator-pos it)))
   (define (stream-first it)
     (define pins (board-iterator-pin-map it))
     (define-values (num map-idx)
       (hash-iterate-key+value (board-iterator-index it)
                               (board-iterator-pos it)))
     (define pinned? (~> (vector-ref pins (car map-idx))
                         (bit-vector-ref (cdr map-idx))))
     (cons num pinned?))
   (define (stream-rest it)
     (define index (board-iterator-index it))
     (define pos (board-iterator-pos it))
     (board-iterator index
                     (board-iterator-pin-map it)
                     (hash-iterate-next index pos)))])

(define (in-board b)
  (sequence->stream b))

(define (make-board rows)
  (define nrows (length rows))
  (define ncols (length (first rows)))
  (board (make-board-index rows)
         (make-pin-map ncols nrows)
         (make-pin-map nrows ncols)))

(define (board-score b)
  (for/sum ([it b])
    (match it [(cons num pinned?) (if pinned? 0 num)])))

(define (board-bingo? b)
  (define (marked? row)
    (equal? (bit-vector-length row) (bit-vector-popcount row)))
  (for/or ([row (in-vector (board-pin-map b))]
           [col (in-vector (board-pin-map-transposed b))])
    (or (marked? row)
        (marked? col))))

(define (board-pin-at! b idx)
  (define row (vector-ref (board-pin-map b) (car idx)))
  (define col (vector-ref (board-pin-map-transposed b) (cdr idx)))
  (bit-vector-set! row (cdr idx) #t)
  (bit-vector-set! col (car idx) #t))

(define (board-mark! b num)
  (define pos (hash-ref (board-index b) num #f))
  (when pos
    (board-pin-at! b pos)))

(define (make-pin-map ncols nrows)
  (define rows (for/list ([_ (in-range nrows)])
                 (make-bit-vector ncols #f)))
  (apply vector-immutable rows))

(define (make-board-index rows)
  (for*/hash ([(row i) (in-indexed (in-list rows))]
              [(val j) (in-indexed (in-list row))])
    (values val (cons i j))))

(define (equal-length-lists? xs)
  (~> (map length xs)
      remove-duplicates
      length
      (equal? 1)))
