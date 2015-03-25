#lang racket

; Procedural representation of cons
; We return a function which takes a single integer
; argument, if the argument is 0 then we return the first
; else we return second part of the pair
(define (mycons1 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Wrong argument passed, only 0 or 1" m)))))

; car is just passing 0 to the returned function
(define (mycar1 z) (z 0))
; cdr is just passing 1 to the returned function
(define (mycdr1 z) (z 1))

; Test for first representation of pairs
(define mypair1 (mycons1 -1 4))
(mycar1 mypair1)
(mycdr1 mypair1)

; Exercise 2.4
; Second alternative procedural representation of pairs
; Here we return a function which takes a selector function
; the function passed should return x when we want the first element
; the function passed should return y when we want the second element
(define (mycons2 x y) (lambda (m) (m x y)))
(define (mycar2 z) (z (lambda (p q) p)))
(define (mycdr2 z) (z (lambda (p q) q)))
; Tests for second representation of pairs
(define mypair2 (mycons2 -1 4))
(mycar2 mypair2)
(mycdr2 mypair2)

; Exercise 2.5
; Third representation of pairs
; Using exponentiation of either 2 or 3
; (cons a b) is (* (expt 2 a) (expt 3 b))
; (car pair) is fetching a from pair
; (cdr pair) is fetching b from pair
(define (mycons3 a b) (* (expt 2 a) (expt 3 b)))
; Helper routine for car and cdr
(define (find-power num a)
  (define (iter res count)
    (if (= (remainder res a) 0)
        (iter (/ res a) (+ count 1))
        count))
  (iter num 0))
(define (mycar3 z) (find-power z 2))
(define (mycdr3 z) (find-power z 3))
; Tests for third representation of pairs
(define mypair3 (mycons3 3 5))
(mycar3 mypair3)
(mycdr3 mypair3)
