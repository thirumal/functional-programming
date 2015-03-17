#lang racket

; Exercise 1.30
;
; Product sigma function (recursive)
(define (product-sigma-recr term a next b)
  (if (> a b)
      1
      (* (term a) (product-sigma-recr term (next a) next b))))

; Product sigma function (iterative)
(define (product-sigma term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (* acc (term count)) (next count))))
  (iter 1 a))

; Helper functions
(define (identity x) x)
(define (square x) (* x x))
(define (double x) (* 2 x))
(define (inc x) (+ x 1))

; Factorial function
(define (factorial n) (product-sigma identity 1 inc n))

; Function to approximate PI
; PI   2 * 4 * 4 * 6 * 6 * 8 * ...
; --- = ------------------------
;  4   3 * 3 * 5 * 5 * 7 * 7 * ...
;
(define (approx-pi count)
  (define (top-term x) (square (double x)))
  (define (bottom-term x) (square (+ 1 (double x))))
  (* 4.0 (/ (/ (product-sigma top-term
                              1
                              inc
                              (+ count 1))
               (* 2 (double (+ count 1))))
            (product-sigma bottom-term
                           1
                           inc
                           count))))
; Tests
(approx-pi 10000)
(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 20)