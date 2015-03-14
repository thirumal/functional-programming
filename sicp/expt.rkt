#lang racket

; Section 1.2.4 Exponentiation

; Recursive O(n) procedure
(define (expt-recr a n)
  (if (= n 0)
      1
      (* a (expt-recr a (- n 1)))))

; Iterative O(n) procedure
(define (expt-iter a n)
  (define (iter acc count)
    (if (= count n)
        acc
        (iter (* a acc) (+ count 1))))
  (iter 1 0))

; Helper function to find the square of a number
(define (square x) (* x x))

; Recursive O(log n) procedure
(define (fast-expt-recr a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recr a (/ n 2))))
        (else (* a (fast-expt-recr a (- n 1))))))

; Iterative O(log n) procedure
; Also exercise 1.16 
(define (fast-expt-iter a n)
  (define (iter acc count)
    (cond ((= count 0) acc)
          ((even? n) (iter (* acc acc) (/ count 2)))
          (else (iter (* a acc) (- count 1)))))
  (iter 1 n))

; Tests
(expt-recr 10 0)
(expt-iter 10 0)
(fast-expt-recr 10 0)
(fast-expt-iter 15 0)

(expt-recr 5 1)
(expt-iter 7 1)
(fast-expt-recr 8 1)
(fast-expt-iter 9 1)  

(expt-recr 2 4)
(expt-iter 3 4)
(fast-expt-recr 5 4)
(fast-expt-recr 3 5)