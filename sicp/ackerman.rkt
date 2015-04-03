#lang racket

; Ackerman function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; Tests and concise defintions
; If n == 0 then (A x 0) = 0
;
; (A 0 n) = 2 * n for n > 0
(A 0 4)
;
; (A 1 n) = 2 ^ n for n > 0
(A 1 4)
;
; (A 2 n) = (2 ^ (2 ^ ... (2 ^ 2))) n - 1 times for n> 0
(A 2 4)
