#lang racket

; Exercise 1.17: Multiplication algorithms
; Also known as Karatsuba multiplication?

; Recursive O(n) algorithm
(define (mul-recr a b)
  (if (= b 0)
      0
      (+ a (mul-recr a (- b 1)))))

; Iterative O(n) algorithm
(define (mul-iter a b)
  (define (iter acc count)
    (if (= count 0)
        acc
        (iter (+ a acc) (- count 1))))
  (iter 0 b))

; HELPER functions
(define (double x) (+ x x))
(define (halve x) (/ x 2))

; Recursive O(log n) algorithm
(define (fast-mul-recr a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul-recr a (halve b))))
        (else (+ a (fast-mul-recr a (- b 1))))))

; Iterative O(log n) algorithm
(define (fast-mul-iter a b)
  (define (iter acc count)
    (cond ((= count 0) acc)
          ((even? count) (iter (double acc) (halve count)))
          (else (iter (+ a acc) (- count 1)))))
  (iter 0 b))

; Tests
(mul-recr 5 0)
(mul-iter 5 0)
(fast-mul-recr 5 0)
(fast-mul-iter 5 0)

(mul-recr 2 5)
(mul-recr 2 5)
(fast-mul-recr 2 5)
(fast-mul-iter 2 5)

(mul-recr 18 7)
(mul-recr 18 7)
(fast-mul-recr 18 7)
(fast-mul-iter 18 7)
