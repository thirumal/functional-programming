#lang racket
; author: Thirumal Venkat

; I'm assuming n >= 0, always
; This keeps the code pretty
; Don't copy and paste into your assignments
; Don't use this implementation for production code
; Goes into infinite loop for n < 0

; Recursive Factorial Algorithm
; Idea: n! = n * (n - 1)! and 0! and 1! is 1
(define (factorial n)
  (if (=  n 0)
      1
      (* n (factorial (- n 1)))))

; Iterative factorial algorithm
;
; C Code:
;
; int fact(int n) {
;   int acc = 1, count = 0;
;   while (count < n) {
;     acc *= count;
;     count++;
;   }
;   return acc;
; }
; ...
; factorial(5);
; ...
;
(define (fact n)
  ; Define a helper routine, this will have
  ; an accumulator and a variable to keep track
  ; of the current count. If count exceeds the
  ; maximum number of iterations (n) then return
  ; from the routine, else increment counter
  ; update the accumulator with the new product
  (define (fact-iter acc count)
    (if (> count n)
        acc
        (fact-iter (* count acc) (+ count 1))))
  (fact-iter 1 1))

; Tests for both recursive and iterative factorial algorithms
(factorial 0)
(fact 0)
(factorial 1)
(fact 1)
(factorial 2)
(fact 2)
(factorial 3)
(fact 3)
(factorial 4)
(fact 4)
(factorial 5)
(fact 5)
(factorial 10)
(fact 10)