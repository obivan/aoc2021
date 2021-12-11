#lang racket

(define (bit-at n i) (bitwise-bit-field n i (add1 i)))

(define (common numbers bit op)
  (define bits (map (curryr bit-at bit) numbers))
  (for/fold ([zeroes 0] [ones 1] #:result (if (op zeroes ones) 0 1))
            ([b bits])
    (cond
      [(eq? 0 b) (values (add1 zeroes) ones)]
      [(eq? 1 b) (values zeroes (add1 ones))])))

(define (decode-power-level diagnostics bit-width op)
  (for/fold ([n 0])
            ([i (in-range bit-width)])
    (bitwise-ior n (arithmetic-shift (common diagnostics i op) i))))

(define (decode-life-support diagnostics bit-width op)
  (for/fold ([remaining diagnostics]
             #:result (first remaining))
            ([i (reverse (range 0 bit-width))])
    #:break (eq? 1 (length remaining))
    (define criteria (common remaining i op))
    (filter (lambda (n) (eq? (bit-at n i) criteria))
            remaining)))

(define (solution-one bitstrings)
  (define bit-width (string-length (first bitstrings)))
  (define diagnostics (map (curryr string->number 2) bitstrings))

  (define gamma (decode-power-level diagnostics bit-width >))
  (define epsilon (decode-power-level diagnostics bit-width <))
  (* gamma epsilon))

(define (solution-two bitstrings)
  (define bit-width (string-length (first bitstrings)))
  (define diagnostics (map (curryr string->number 2) bitstrings))

  (define oxygen (decode-life-support diagnostics bit-width >=))
  (define co2 (decode-life-support diagnostics bit-width <))
  (* co2 oxygen))

(define bitstrings (file->lines "input.txt"))

(displayln (solution-one bitstrings))
(displayln (solution-two bitstrings))

(module+ test
  (require rackunit)

  (define sample '("00100"
                   "11110"
                   "10110"
                   "10111"
                   "10101"
                   "01111"
                   "00111"
                   "11100"
                   "10000"
                   "11001"
                   "00010"
                   "01010"))

  (check-equal? (solution-one sample) 198)
  (check-equal? (solution-two sample) 230))
