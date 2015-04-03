#lang racket

; Definitions
(define DELTA 0.001)
(define pi 3.14159)

; Helper function
(define (cube x) (* x x x))

; Sine function (recursive)
(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (< angle DELTA)
      angle
      (p (sine (/ angle 3.0)))))

; Tests for sine function
(sine 0)
(sine 1)
(sine (/ pi 2))
(sine (/ pi 4))
