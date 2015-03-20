#lang racket

; Finding roots of an equation by half interval method
; Given a,b and we know that f(a) < 0 < f(b) and
; f(x) is continous in the range [a,b] then we can
; use a variation of binary search to figure out the
; root present in [a,b]

; Helper functions
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))
(define (abs x) (if (< x 0) (- x) x))

; Half interval function to find the root of f(x)
(define (half-interval f a b tolerance)
  ; See if the guess is good enough?
  (define (good-enough? guess)
    (< (abs (f guess)) tolerance))
  ; Iterative function
  (define (iter minimum maximum guess)
    (cond ((good-enough? guess) guess)
          ((positive? (f guess)) (iter minimum guess (average minimum guess)))
          (else (iter guess maximum (average guess maximum)))))
  (iter a b (average a b)))

; Tests
(half-interval (lambda (x) (- x 1)) 0.5 50 0.00001)
(half-interval (lambda (x) (- (square x) 1)) 0 50 0.0001)
