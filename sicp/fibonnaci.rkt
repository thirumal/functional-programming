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

; Helper function
(define (square x) (* x x))
(define (halve x) (/ x 2))

; O(log n) algorithm
;
; Explaination for the transformation in algorithm above
; for count is even case:
;
; The transformation (for fibonnaci series use p=0, q=1)
;
; a <- bq + aq + ap
; b <- bp + aq
;
; transforms
;
; < a b p q >
;
; into
;
; < (bq + aq + ap) (bp + aq) p q>
;
; applying the same transformation again, the expression transforms into
;
; < ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p) ((bp + aq)p + (bq + aq + ap)q) p q >
;
; when expanded is nothing but
;
; < (bpq  + aq^2 + bq^2 + aq^2 + apq + bpq + aqp + ap^2) (bp^2 + apq  + bq^2 + aq^2 + apq) p q >
;
; when rearranged looks like
;
; < (b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)) (b(p^2 + q^2) + a(q^2 + 2pq)) p q >
;
; Let's say P = (p^2 + q^2) and Q = (q^2 + 2pq)
;
; < (bQ + aQ + aP) + (pP + aQ) p q > requires two transformations
;
; If I had been using P and Q for my transformations
; I would have achieved above in one single step
;
; Therefore I'll perform the below transformation when I have to
; perform two transformations together
;
; < a b (p^2 + q^2) (q^2 + 2pq) >
;
(define (fast-fib-recr n)
  (define (loop a b p q count)
    (cond ((= count 0) b)
          ((even? count) (loop a
                               b
                               (+ (square p) (square q))
                               (+ (square q) (* 2 p q))
                               (halve count))
                         )
          (else (loop  (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1)
                       )
                )
          )
    )
  (loop 1 0 0 1 n)
  )

; Tests
(fib-recr 0)
(fib-iter 0)
(fast-fib-recr 0)

(fib-recr 1)
(fib-iter 1)
(fast-fib-recr 1)

(fib-recr 2)
(fib-iter 2)
(fast-fib-recr 2)

(fib-recr 3)
(fib-iter 3)
(fast-fib-recr 3)

(fib-recr 4)
(fib-iter 4)
(fast-fib-recr 4)

(fib-recr 5)
(fib-iter 5)
(fast-fib-recr 5)

(fib-recr 8)
(fib-iter 8)
(fast-fib-recr 8)

(fib-recr 30)
(fib-iter 30)
(fast-fib-recr 30)
