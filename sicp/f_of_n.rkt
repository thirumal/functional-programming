#lang racket

; Excercise: 1.11

; Recursive solution
;
(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
         (* 2 (f-r (- n 2)))
         (* 3 (f-r (- n 3))))))

; Iterative solution
;
(define (f-i n)
  (define (find a b c count)
    (if (= count n)
        a
        (find (+ a (* 2 b) (* 3 c)) a b (+ count 1))))
  (if (< n 3)
      n
      (find 2 1 0 2)))

; Tests
(f-i 0)
(f-r 0)

(f-i 1)
(f-r 1)

(f-i 2)
(f-r 2)

(f-i 3)
(f-r 3)

(f-i 4)
(f-r 4)

(f-i 5)
(f-r 5)
