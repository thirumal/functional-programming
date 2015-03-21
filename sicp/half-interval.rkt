#lang racket

; Finding roots of an equation by half interval method
; Given a,b and we know that f(a) < 0 < f(b) and
; f(x) is continous in the range [a,b] then we can
; use a variation of binary search to figure out the
; root present in [a,b]

; Helper functions
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2.0))
(define (abs x) (if (< x 0) (- x) x))

; Half interval function to find the root of f(x)
(define (half-interval f a b)
  (let ((a-value (f a))
        (b-value (f b))
        (tolerance 0.00001))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b tolerance))
          ((and (negative? b-value) (positive? a-value))
           (search f b a tolerance))
          (else (error "Values are of not opposite sign" a b)))))

; Modified binary search value
(define (search f a b tolerance)  
  ; See if the guess is good enough?
  (define (good-enough? test-value)
    (< (abs test-value) tolerance))
  ; Iterative function
  (define (iter minimum maximum guess)
    (let ((test-value (f guess)))
    (cond ((good-enough? test-value) guess)
          ((positive? test-value) (iter minimum guess (average minimum guess)))
          (else (iter guess maximum (average guess maximum))))))
  (iter a b (average a b)))

; Tests
; f(x) = x - 1
(half-interval (lambda (x) (- x 1)) 0.5 2)
; f(x) = x^2 - 1
(half-interval (lambda (x) (- (square x) 1)) 0 5)
; f(x) = x^3 + 3x^2 + 3x + 1
(half-interval (lambda (x) (+ (cube x) (* 3 (square x)) (* 3 x) 1)) -2 0)
