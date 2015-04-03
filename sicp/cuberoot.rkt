#lang racket

; 1.8 Newton's cube root method

; Helpers
(define (abs a) (if (< a 0) (- a) a))
(define (cube a) (* a a a))

; Default precision: 0.00000001
(define DELTA 0.00000001)

; Cube root function, alter variable DELTA for more precision
(define (cbrt x)
  (define (cbrt-recursive guess)
    (define (is-good-enough guess)
      (< (abs (- (/ (cube guess) x) 1)) DELTA))
    (define (improve-guess guess)
      (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
    (if (is-good-enough guess)
        guess
        (cbrt-recursive (improve-guess guess))))
  (cbrt-recursive 1.0))

; Test the cube root routine
(cbrt 0.000000001)
(cbrt 1.0)
(cbrt 8.0)
(cbrt 27000.0)
(cbrt 64000000.0)
(cbrt 125000000000000000.0)