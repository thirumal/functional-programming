#lang racket

; Exercise 1.33

; Filtered Accumulate (Recursive version)
(define (filtered-accumulate-recr filter-satisfy
                                  combiner
                                  null-value
                                  term
                                  a
                                  next
                                  b)
  (if (> a b)
      null-value
      (combiner (if (filter-satisfy count)
                    (term a)
                    null-value)
                (filtered-accumulate-recr filter-satisfy
                                          combiner
                                          null-value
                                          term
                                          (next a)
                                          next b))))

; Filtered Accumulate (Iterative version)
(define (filtered-accumulate filter-satisfy combiner null-value term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (combiner (if (filter-satisfy count)
                            (term count)
                            null-value)
                        acc)
              (next count))))
  (iter null-value a))

; Helper functions
; See exercise 1.23 for more info on primality test
(define (satisfy x) true)
(define (id x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (halve x) (/ x 2))
(define (divides? divisor number) (= (remainder number divisor) 0))
(define (smallest-divisor-opt n)
  (define (next divisor) (if (= divisor 2) 3 (+ 2 divisor)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (find-divisor 2))
(define (prime? n) (= (smallest-divisor-opt n) n))
(define (gcd a b) (if (= b 0) a (gcd b (remainder a b))))

; Tests

; Sum numbers from to 1 to 10
(filtered-accumulate satisfy + 0 id 1 inc 10)
(filtered-accumulate-recr satisfy + 0 id 1 inc 10)

; Factorial of 6 (6!)
(filtered-accumulate satisfy * 1 id 1 inc 6)
(filtered-accumulate-recr satisfy * 1 id 1 inc 6)

; Sum of prime numbers in interval [a,b], a > 1
; Sum of first 100 prime numbers [2,541]
(filtered-accumulate prime? + 0 id 2 inc 541)

; Product of all numbers relatively prime with n and less than n
(define (sum-relatively-prime n)
  (define (relatively-prime? x) (= (gcd x n) 1))
  (filtered-accumulate relatively-prime? * 1 id 2 inc n))

(sum-relatively-prime 10) ; 3, 7 and 9 are relatively prime to 10