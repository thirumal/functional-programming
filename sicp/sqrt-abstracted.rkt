#lang racket

; 1.1.8 Abstraction: Newton's square root method

; Square root function, alter variable DELTA for more precision
; Default precision: 0.00000001
(define (sqrt x)
  ; HELPER for: is-good-enough
  ; Delta value for square root accuracy
  ; Change this if you need more precision
  (define DELTA 0.00000001)
  ; HELPER for: sqrt 
  ; Square root recursive function
  (define (sqrt-recursive guess)
    ; HELPER: Check if the guess was good enough
    (define (is-good-enough guess)
      ; HELPER for: is-good-enough 
      ; Find the absolute value of a number
      (define (abs a) (if (< a 0) (- a) a))
      ; HELPER for: is-good-enough 
      ; Function to find the square of a number
      (define (square a) (* a a))
      ; CODE for: is-good-enough 
      ; Checks if the guess is good enough
      (< (abs (- (/ (square guess) x) 1)) DELTA))
    ; HELPER for: sqrt-recursive 
    ; Function to improve our guess
    (define (improve-guess guess)
      ; HELPER for: improve-guess
      ; Function to find the average of two numbers
      (define (average a b) (/ (+ a b) 2))
      ; CODE for: improve-guess
      ; Improve our guess
      (average guess (/ x guess)))
    ; CODE for sqrt-recursive: 
    ; See if guess is good enough, if so then
    ; that is our square root, else refine and call
    ; this routine again to check the same recursively
    (if (is-good-enough guess)
        guess
        (sqrt-recursive (improve-guess guess))))
  ; CODE for: sqrt
  ; Call the routine with an initial guess of 1
  (sqrt-recursive 1.0))

; Test the square root routine
(sqrt 0.00000001)
(sqrt 1.0)
(sqrt 2.0)
(sqrt 40000.0)
(sqrt 9.0)
(sqrt 16.0)
(sqrt 250000000000000000.0)