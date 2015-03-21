#lang racket

; Procedures as Returned Values

; Helper functions
(define (inc x) (+ x 1))
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (log2 x) (/ (log x) (log 2)))
; dx
(define dx 0.00001)

; Fixed point function (see fixed-function.rkt)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) dx))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Average damping
; Used by us during square root and cube root approximations
(define (average-damp f)
  (lambda (x)
    (average x (f x))))

; Defining a function out of the function returned 
(define (damp-x-and-its-square x) ((average-damp square) x))
; Calling the generated function
(damp-x-and-its-square 5) ; (5 + 25) / 2 = 15

; Square root by average damping
(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt-ad 2)

; Cube root by average damping
; Only because we average damp here, we're able to converge
(define (cbrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))
(cbrt-ad 27)

; General Newton's method
;dx already defined

; Derivative Dg(x) = D(g(x)) = (g(x + dx) - g(x))/dx
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

; Testing derivative function
((deriv cube) 5) ; d/dx(x^3) = 3x^2, hence approx: 3*5*5 = 75

; Newton's transform
; x_(n+1) = x_n - g(x_n)/Dg(x_n)
(define (newtons-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

; Finally, Newton's Method
(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

; Redefine square root and cube root, and construct nroot
(define (sqrt-nm x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(define (cbrt-nm x)
  (newtons-method (lambda (y) (- (cube y) x)) 1.0))

(define (nroot x n)
  (newtons-method (lambda (y) (- (expt y n) x)) 1.0))

(sqrt-nm 2)
(cbrt-nm 27)
(nroot 243 5)

; First class procedures
; As seen above, with two different transforms on two different functions
; on the fixed point function we could calculate the sqaure root by two
; different ways. If we want to generalize things even more, then we can
; define a general function which takes the transformation as well

; Fixed point function of transform
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; So now the sqaure root can be defined in two ways as follows
(define (sqrt-ad-ft x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-nm-ft x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newtons-transform
                            1.0))

(sqrt-ad-ft 2)
(sqrt-nm-ft 2)

; Exercise 1.40
; Solve cubic equation: x^3 + ax^2 + bx + c = 0
(define (solve-cubic a b c)
  (define (cubic a b c)
    (lambda (y) (+ (cube y)
                   (* a (square y))
                   (* b y)
                   c)))
  (newtons-method (cubic a b c) 1.0))

; Test for 1.40
(solve-cubic -3 3 -1) ; x is 1
(solve-cubic 3 3 1) ; x is -1

; Exercise 1.41
; Return a function that applies a given function twice
(define (double g)
  (lambda (x) (g (g x))))
; Test for 1.41
((double inc) 1) ;1 + (1 + 1)
; Test for 1.41
(((double (double double)) inc) 5)

; Exercise 1.42
; f and g be one argument functions
; Composition of functions f after g: x |-> f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

; Test for 1.42
((compose square inc) 6)

; Exercise 1.43
; f is a numerical function, n is a positive integer
; write a function to form the nth repeated application of f
; f(f(...(f(x))...))
(define (repeated f n)
  (define (iter result i)
    (if (= i n)
        result
        (iter (compose f result) (+ i 1))))
  (iter f 1))
; Test for 1.43
((repeated square 2) 5)

; Exercise 1.44
; Function smoothing
; If f is a function, dx is some small number
; Define a function that returns a smoothed value at x, given dx
; dx has been defined above
(define (smooth f dx)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

; Repeated smoothening, exercise 1.44 continued
(define (smooth-n-fold f dx n) (repeated (smooth f dx) n))

; Test for 1.44
((smooth sin 0.7) (/ pi 2))
((smooth-n-fold sin 0.7 2) (/ pi 2))

; Exercise 1.45
; Multi fold average damping and finding nth root of x
; For nth root, the number of dampings required
; 2, 3 = 1
; 4, 5, 6, 7, = 2
; 8, 9, .... , 15 = 3
; 16, ..., 31 = 4
; 32, ..., 63 = 5
; 64, ..., 127 = 6
; For n you need floor(log2(n))
;
; Note: I could not test nth-root(2^1024, 1024).
; I was waiting for about a minute or two on execution.
; But, I'm assuming the pattern I've recognized to hold
; good till 1023 will stand for 1024 as well (fingers crossed)
; Maybe I should increase my dx?
;
(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (floor (log2 n)))
                            1.0))
; Tests for Exercise 1.45
(nth-root (expt 2 24) 24)

; Exercise 1.46
; The guess tester function returns #t if guess is good enough
; else it returns #f, improver function improves the guess
(define (iterative-improve guess-tester improver)
  (define (iter old-guess)
    (let ((new-guess (improver old-guess)))
      (if (guess-tester old-guess new-guess)
          new-guess
          (iter new-guess))))
  iter)

; Guess Tester function
(define (good-enough? x y)
  (< (abs (- x y)) dx))

; Square root iterative improvement way
(define (sqrt-ii x)
  ((iterative-improve good-enough?
                      (average-damp (lambda (y) (/ x y))))
   1.0))
(sqrt-ii 2)

; Fixed point function using iterative improvement
(define (fixed-point-ii f first-guess)
  ((iterative-improve good-enough? f) first-guess))

; Testing the fixed-point-ii function
; Square root by average damping
(define (sqrt-ad-ii x)
  (fixed-point-ii (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt-ad-ii 2)
