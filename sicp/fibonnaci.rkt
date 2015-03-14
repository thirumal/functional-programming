#lang racket

; Tree recursion: Nth Fibonnaci number
(define (fib-recr n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recr (- n 1))
                 (fib-recr (- n 2))))))

; Iterative algorithm: Nth Fibonacci number
(define (fib-iter n)
  (define (iter a b count)
    (if (= count n)
        a
        (iter b (+ a b) (+ count 1))))
  (iter 0 1 0))

; Tests
(fib-recr 0)
(fib-iter 0)

(fib-recr 1)
(fib-iter 1)

(fib-recr 2)
(fib-iter 2)

(fib-recr 3)
(fib-iter 3)

(fib-recr 4)
(fib-iter 4)

(fib-recr 5)
(fib-iter 5)

(fib-recr 8)
(fib-iter 8)

(fib-recr 30)
(fib-iter 30)