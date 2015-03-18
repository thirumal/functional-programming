#lang racket

; Exercise 1.32

; Sum Sigma from a to b (inclusive)
(define (sum-sigma term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (+ (term count) acc) (next count))))
  (iter 0 a))

; Product PI from a to b (inclusive)
(define (product-pi term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (* (term count) acc) (next count))))
  (iter 1 a))

; Accumulate (Recursive version)
(define (accumulate-recr combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recr combiner null-value term (next a) next b))))

; Accumulate (iterative version)
(define (accumulate combiner null-value term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (combiner (term count) acc) (next count))))
  (iter null-value a))


; Helper functions
(define (identity x) x)
(define (inc x) (+ x 1))
; Tests
(sum-sigma identity 1 inc 10)
(accumulate + 0 identity 1 inc 10)
(accumulate-recr + 0 identity 1 inc 10)
(product-pi identity 1 inc 6)
(accumulate * 1 identity 1 inc 6)
(accumulate-recr * 1 identity 1 inc 6)
