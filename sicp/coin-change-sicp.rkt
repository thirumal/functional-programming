#lang racket

; Assume coins to be arranged from large to small
; The first-denomination routine takes the number of
; kinds of coins available and returns the denomination
; of the first kind
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Coin change recursive routine
(define (count money kinds-of-coins)
  (cond ((= money 0) 1)
        ((or (< money 0) (= kinds-of-coins 0)) 0)
        (else (+ (count money (- kinds-of-coins 1))
                 (count (- money (first-denomination
                                  kinds-of-coins))
                        kinds-of-coins)))))

(define (coin-change money) (count money 5))

(coin-change 1)
(coin-change 10)
(coin-change 100)
