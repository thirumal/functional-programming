#lang racket

; Helper functions

(define tolerance 0.00001)
(define (abs x) (if (< x 0) (- x) x))
(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))

; Fixed point function
(define (fixed-point f first-guess)
  ; Function to see if the guess is good enough?
  (define (close-enough? x1 x2)
    (< (abs (- x1 x2)) tolerance))
  ; Iterative guess function
  (define (try guess)
    (let ((next-guess (f guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (try next-guess))))
  ; Make the first guess
  (try first-guess))

; Tests
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; Definining square root of x with this routine
;
; We start out by transformation y -> (x / y) with an intial guess of 1.0
; but unfortunately this does not converge, so we add y on both sides
; and divide the equation by 2. This yields
; y -> (1/2) * (y + (x / y))
;
(define (sqrt x) (fixed-point (lambda (y) (average y ( / x y))) 1.0))
; Some tests for square root function
(sqrt 1)
(sqrt 2)
(sqrt 3)

; We'll attempt the same with the cube root
; If y = f(x) = cbrt(x), then y -> x/y^2, unfortunately this does not converge
; we add y on both sides, even this does not converge (TODO: why???)
; so we add 2y on both sides and divide the equation by 3
; y-> (1/3) * (2y + (x / y^2))
;
(define (cbrt x) (fixed-point (lambda (y)
                                (/ (+ (* 2 y)
                                      (/ x (square y)))
                                   3))
                              1.0))
(cbrt 1)
(cbrt 8)
(cbrt 27)

; Exercise 1.35 golden ratio
(define (phi x) (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
(phi 1.0) ; This should spit out golden ratio

; Exercise 1.36
; Finding solution to x^x = 1000
; Note that log(1) = 0, so cannot start approximation using 1.0
(define (x-root-x x) (fixed-point (lambda (y) (/ (log x) (log y))) 2.0))
(x-root-x 4)
(x-root-x 27)
(x-root-x 256)

; Exercise 1.37
; infinite continued fraction
; f = N1 / (D1 + (N2 / (D2 + (N3 / ... + (Nk / Dk)))...)
; compute this fraction upto K terms starting from 1
(define (cont-frac n d k)
  ; Recursive version (not used)
  (define (recr i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (recr (+ i 1))))))
  ; Iterative version
  (define (iter acc i)
    (if (= i 0)
        acc
        (iter (/ (n i) (+ (d i) acc)) (- i 1))))
  ; Make the call
  (iter 0 k))

; Test for infinite continuous fraction
; 1 / (1 + (1 / (1 + ... = 1/phi, where phi is the golden ratio
(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 50)) ; should spit phi

; Exercise 1.38
; Continued fraction expansion for e - 2
; All Ni = 1, Di = 1, 2,1,1, 4,1,1, 6,1,1, ...
; Let's figure out Di approximation
; Di = if i % 3 == 0 or (i - 1) % 3 == 0 then 1
; else Di = 2*(((i - 2)/3) + 1)
; this is how you arrive at f(2) = 2, f(5) = 4, f(8) = 6...
(define (exp-approx k)
  (+ 2
     (cont-frac (lambda (i) 1.0) ; All Ni = 1
                (lambda (i)
                  (cond ((< i 1) (error "i is less than 1: " i))
                        ((or (= (remainder i 3) 0)
                             (= (remainder (- i 1) 3) 0))
                         1)
                        (else (* 2 ( + 1 (/ (- i 2) 3))))))
                k)))

; Test
(exp-approx 50) ; should be near (exp 1)

; Exercise 1.39
; tangent function approximation using continuous fraction
; approximation
; tanx = x / ( 1 - (x^2 / (3 - x^2 /( 5 - ...))))

(define (tan-cf x k)
  (/ (cont-frac (lambda(i) (- (square x)))
                (lambda (i) (- (* 2 i) 1))
                k)
     (- x)))

; Approximate tan(pi/4), should be near 1
(tan-cf (/ pi 4) 50)
