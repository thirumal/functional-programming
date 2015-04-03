#lang planet neil/sicp

; Section 1.2.5: GCD 

; Simple recursive algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Tests
(gcd 14 0)
(gcd 20 12)
(gcd 4 9)
(gcd 9 4)
(gcd 14 7)
