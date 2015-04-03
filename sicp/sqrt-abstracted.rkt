#lang racket

; 1.1.8 Abstraction: Newton's square root method

; Helper functions
(define (abs a) (if (< a 0) (- a) a))
(define (square a) (* a a))

; Default precision: 0.00000001
(define DELTA 0.00000001)

; Square root function, alter variable DELTA for more precision
(define (sqrt x)
  (define (sqrt-recursive guess)
    (define (is-good-enough guess)
      (< (abs (- (/ (square guess) x) 1)) DELTA))
    (define (improve-guess guess)
      (define (average a b) (/ (+ a b) 2))
      (average guess (/ x guess)))
    (if (is-good-enough guess)
        guess
        (sqrt-recursive (improve-guess guess))))
  (sqrt-recursive 1.0))

; Test the square root routine
(sqrt 0.00000001)
(sqrt 1.0)
(sqrt 2.0)
(sqrt 40000.0)
(sqrt 9.0)
(sqrt 16.0)
(sqrt 250000000000000000.0)
