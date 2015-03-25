#lang racket

; Exercise 2.6
; Church Numerals

; Helper function
(define (double x) (* 2 x))

; The number zero (applies f 0 times)
(define zero (lambda (f) (lambda (x) x)))
((zero double) 3)

; The number one (applies f 1 times)
(define one (lambda (f) (lambda (x) (f x))))
((one double) 3)

; Function to add one to a church numeral
(define (add-one n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define zero-add-one (add-one zero))
((zero-add-one double) 3)

; Generic n church numeral definition
(define (number n)
  (lambda (f)
    (lambda (x)
      (define (iter res count)
        (if (= count n)
            res
            (iter (f res) (+ count 1))))
      (iter x 0))))

; Tests for generic n church numeral definition
(define number-zero (number 0))
(define number-one (number 1))
(define number-two (number 2))
((number-zero double) 3)
((number-one double) 3)
((number-two double) 3)


; Add two church numerals
(define (add m n)
  (lambda (f)
    (lambda (x)
      ((n f) ((m f) x)))))

; Test for church numerals addition
(define number-three (number 3))
(= ((number-three double) 3)
   (((add number-one number-two) double) 3))