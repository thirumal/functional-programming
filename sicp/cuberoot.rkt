#lang planet neil/sicp

; 1.8 Newton's cube root method

; Cube root function, alter variable DELTA for more precision
; Default precision: 0.00000001
(define (cbrt x)
  ; HELPER for: is-good-enough
  ; Delta value for cube root accuracy
  ; Change this if you need more precision
  (define DELTA 0.00000001)
  ; HELPER for: cbrt 
  ; Cube root recursive function
  (define (cbrt-recursive guess)
    ; HELPER for: cbrt-recursive 
    ; Check if the guess was good enough
    (define (is-good-enough guess)
      ; HELPER for: is-good-enough 
      ; Find the absolute value of a number
      (define (abs a) (if (< a 0) (- a) a))
      ; HELPER for: is-good-enough 
      ; Function to find the cube of a number
      (define (cube a) (* a a a))
      ; CODE for: is-good-enough 
      ; Checks if the guess is good enough
      (< (abs (- (/ (cube guess) x) 1)) DELTA))
    ; HELPER for: cbrt-recursive 
    ; Function to improve our guess
    (define (improve-guess guess)
      ; CODE for: improve-guess
      ; Improve our guess
      (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
    ; CODE for: cbrt-recursive 
    ; See if guess is good enough, if so then
    ; that is our cube root, else refine and call
    ; this routine again to check the same recursively
    (if (is-good-enough guess)
        guess
        (cbrt-recursive (improve-guess guess))))
  ; CODE for: cbrt
  ; Call the routine with an initial guess of 1
  (cbrt-recursive 1.0))

; Test the cube root routine
(cbrt 0.000000001)
(cbrt 1.0)
(cbrt 8.0)
(cbrt 27000.0)
(cbrt 64000000.0)
(cbrt 125000000000000000.0)