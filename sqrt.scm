#lang planet neil/sicp

; 1.1.7 Program to find square root using Newton's method

; Delta value for square root accuracy
(define DELTA 0.00000001)

; Function to find the absolute value of a number
(define (abs x) (if (< x 0) (- x) x))

; Function to find the average of two numbers
(define (average x y) (/ (+ x y) 2))

; Function to find the square of a number
(define (square x) (* x x))

; Function to check if the guess was good enough
; The guess is good enough if
(define (is-good-enough guess x)
  (< (abs (- (/ (square guess) x) 1)) DELTA))

; Function to improve our guess
(define (improve-guess guess x)
  (average guess (/ x guess)))

; New if function, defined from cond
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; Square root recursive function
(define (sqrt-recursive guess x)
  (if (is-good-enough guess x)
      guess
      (sqrt-recursive (improve-guess guess x) x)))

; Wrapper function calling the recursive square root function
; with an initial guess of 1
(define (sqrt x) (sqrt-recursive 1 x))

; Test the square root routine
(sqrt 0.00000001)
(sqrt 1.0)
(sqrt 2.0)
(sqrt 40000.0)
(sqrt 9.0)
(sqrt 16.0)
(sqrt 250000000000000000.0)