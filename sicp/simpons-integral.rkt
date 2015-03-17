#lang racket

; Helper function
; Iterative sum function
; Exercise 1.30
;
(define (sum term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (+ acc (term count)) (next count))))
  (iter 0 a))

; Simpon's Integration formula
; Excercise 1.29

; (h/3)( f(a)  + 4*f(a + h) + 2*f(a+2h) + 4*f(a+3h) + ... + f(a+nh) )  

(define (simpons-integral f a b n)
  (define h (/ (- b a) n))
  (define (term x) (f (+ a (* x h))))
  (* (/ h 3) (sum (lambda (k) (cond ((or (= k 0) (= k n)) (term k))
                                      ((even? k) (* 2 (term k)))
                                      (else (* 4 (term k)))))
                    0
                    (lambda (x) (+ x 1))
                    n)))

; Tests

(define (cube x) (* x x x))
(define (identity x) x)
(define (square x) (* x x))

(simpons-integral identity 0 1 100) ; 1/2
(simpons-integral square 0 1 100) ; 1/3
(simpons-integral cube 0 1 100) ; 1/4
