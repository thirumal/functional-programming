#lang racket

; Example: Counting Change

; Count the number of ways you can create change when
; you have a good number of supplies of a denomination
; of a coin. You are given the amount of money and a
; list of coins which you can use to create your change
;
; Quirk case of 0 or more coins, 0 money should be 0 way
; This is handled seperately in the main routine
;
(define (count-change money coins)
  (define (count money coins)
    (cond ((= money 0) 1)
          ((or (< money 0) (null? coins)) 0)
          (else (+ (count money (list-tail coins 1))
                   (count (- money (first coins)) coins)))))
  (if (= money 0)
      0
      (count money coins)))

; Tests
; Quirk cases
(count-change 0 (list))
(count-change 0 (list 1 2 3 4))
(count-change 100 (list))
; Simple cases
(count-change 1 (list 1))
(count-change 1 (list 2))
(count-change 4 (list 1 2))
; Large case, and unsorted case
(count-change 300 (list 5 10 20 50 100 200 500))
(count-change 300 (list 500 5 50 100 20 200 10))
; Impossible case
(count-change 301 (list 5 10 20 50 100 200 500))
