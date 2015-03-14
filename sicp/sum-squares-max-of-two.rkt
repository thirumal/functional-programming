#lang racket

; Excercise: 1.3: Given three numbers a, b and c
; Find the sum of squares of the biggest two
; numbers
(define (sum-square-max-of-two a b c)
    (define (sum-of-squares x y)
        (define (square x) (* x x))
        (+ (square x) (square y)))
    (if (> a b)
        (if (> a c)
            (if (> b c)
                (sum-of-squares a b)
                (sum-of-squares a c))
            (sum-of-squares c a))
        (if (> b c)
            (if (> a c)
                (sum-of-squares b a)
                (sum-of-squares b c))
            (sum-of-squares c b))))

; Tests
(sum-square-max-of-two 3 4 5)
(sum-square-max-of-two 3 4 4)
(sum-square-max-of-two 3 3 4)
(sum-square-max-of-two 4 3 4)
(sum-square-max-of-two 4 4 4)