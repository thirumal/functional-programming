#lang racket

; Chapter 2: Data abstraction

; Pairs
(define z (cons 1 2))
; (car z) ; first number
; (cdr z) ; second number

; 2.1.1 Arithmetic operations for Rational Numbers

; Helper functions
(define (abs x) (if (< x 0) (- x) x))
(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))

; Constructors
; w/ Error checking, GCD reduction and sign correctionsr
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (= d 0)
        (error "Invalid denominator: " n d)
        (cond ((and (< n 0) (< d 0)) (cons (- (/ n g)) (- (/ d g))))
              ((and (< n 0) (> d 0)) (cons (/ n g) (/ d g)))
              ((and (>= n 0) (> d 0)) (cons (/ n g) (/ d g)))
              (else (cons (- (/ n g)) (- (/ d g))))))))


; Selectors
(define (numer x) (car x))

(define (denom y) (cdr y))

; Operations on rational numbers

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

; Add two rational numbers
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; Subtract two rational numbers
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; Multiply two rational numbers
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

; Divide two rational numbers
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

; Check if two rational numbers are equal
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Tests
(print-rat (make-rat 3 5))
(print-rat (make-rat -3 5))
(print-rat (make-rat 3 -5))
(print-rat (make-rat -3 -5))
(print-rat (make-rat 0 3))
(print-rat (make-rat 0 -3))

; (print-rat (make-rat 4 0))

(define a (make-rat 3 5))
(define b (make-rat 3 5))
(print-rat (add-rat a b))
(print-rat (sub-rat a b))
(print-rat (mul-rat a b))
(print-rat (div-rat a b))
(equal-rat? a b)