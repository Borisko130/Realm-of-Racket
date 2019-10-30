#lang racket
(define LOWER 1)
(define UPPER 100)

(define (start n m)
  (set! LOWER (min n m))
  (set! UPPER (max n m))
  (guess))

(define (guess)
  (quotient (+ LOWER UPPER) 2))

(define (smaller)
  (set! UPPER (max LOWER (sub1 (guess))))
  (guess))

(define (bigger)
  (set! LOWER (min UPPER (add1 (guess))))
  (guess))