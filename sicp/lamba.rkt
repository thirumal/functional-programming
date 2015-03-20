#lang racket

; Helper function
(define (sum term a next b)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (+ (term count) acc) (next count))))
  (iter 0 a))

; Our pi/8 sum with lambda functions
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; Integral function defined with lambdas
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; Calling lambda functions
; ((lambda (...params...) (...body...) (...arguments...))

; f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)
(define (square x) (* x x))
; Let's define it the lambda way
(define (f1 x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  ( + 1 (* x y))
  (- 1 y)))
; Let to create local variables
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; Tests
(pi-sum 1 100000)
(integral (lambda (x) (* x x x)) 0 1 0.0001)
((lambda (x) (+ x 1)) 21) ; # 22
(f1 2 1)
(f 2 1)