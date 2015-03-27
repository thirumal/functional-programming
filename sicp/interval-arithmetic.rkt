#lang racket

; Interval Arithmetic
; Exercise 2.7 - 2.16

; Please read this paper about interval arithmetic
;
; Hickey, Timothy, Qun Ju, and Maarten H. Van Emden.
; "Interval arithmetic: From principles to implementation."
; Journal of the ACM (JACM) 48.5 (2001): 1038-1068.
;
; http://dl.acm.org/citation.cfm?id=502106
; (Don't worry you'll find copies of this on Google Scholar as well)

; Constructors and selectors for an interval
(define make-interval cons)
; Exercise 2.7
(define lower-bound car)
(define upper-bound cdr)

; Define a method to pretty print an interval
(define (print-interval x)
  (display "[")
  (display (lower-bound x))
  (display ",")
  (display (upper-bound x))
  (display "]")
  (newline))

; Method to add two intervals
; Intuition: If x varies from [a,b] and y varies from [c,d]
; Number line: -----a---b----c----d------
; Naturally you can observe that
; x + y will vary from [a + c, b + d]
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Exercise 2.8
; Method to subtract two intervals
; Intuition: If x varies from [a,b] and y varies from [c,d]
; x - y varies from [a - d, b - c]
; For simplicity think b > a > d > c
; Number line: ------c----d-----a-----b-----
; We can observe that x - y is [a - d, b - c]
; An alternate representation of x - y
; [a,b] - [c,d] = [a,d] + [-d,-c]
; We can leverage the addition operation we've coded up
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))

; Method to multiply two intervals
; Number line: -----a-----b-----c----d-------
; From above case we can see that our resulting number
; interval would be from [a*c, b*d], which is nothing
; but minimum and the maximum of a*c, a*d, b*c, b*d
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Exercise 2.10, add test for a mixed interval in divisor
; Method to divide two intervals
; Number line: -----c------d-------b-----a--------
; Also figures out if y spans 0 [c,d] has 0 in the middle
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "The bounds of the second interval cannot span a zero: "
             (print-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


; Exercise 2.9
; Width of an interval
; Half the difference between its upper and lower bounds
(define (width inv)
  (/ (- (upper-bound inv) (lower-bound inv)) 2))

; Width is closed under addition and subtraction of intervals
; as the relation holds good all the time
; But not the case of the multiplication or division
; Because the min and the max can be chosen from any argument
; depending on the argument passed

; Exercise 2.11
; Fast multiplication algorithm to multiply two intervals
; From the paper we classify an interval [x,y] into 4 types
; It is to be noted that x <= y in all the below cases
; M: Mixed, x < 0 < y
; Z: Zero, x = y = 0
; P: Positive, x >= 0 and y > 0
; N: Negative, x < 0 and y <= 0
;
; Consider two intervals [a,b] and [c,d] and a < b and c < d
; The nine possibilies of intervals are:
; [a,b] is positive:
; P * P = [a * c, b * d], [5,7] * [3,4]     = [15,28]
; P * M = [b * c, b * d], [5,7] * [-3,4]    = [-21,28]
; P * N = [b * c, a * d], [5,7] * [-3,-2]   = [-21,-10]
; [a,b] is mixed
; M * P = [b * c, b * d], [-3,4] * [5,7]    = [-21,28] 
; M * M = [min(a*d,b*c), max(a*c, b*d)]
; M * N = [b * c, a * c], [-3,4] * [-7,-5]  = [-28,21]
; [a,b] is negative
; N * P = [a * d, b * c], [-3,-2] * [5,7]   = [-21,-10]
; N * M = [a * d, a * c], [-3,-2] * [-5,7]  = [-21,15]
; N * N = [b * d, a * c], [-3,-2] * [-7,-5] = [10,21]
; And lastly, [a,b] or [c,d] is zero
; Z * P = Z * M = Z * N = P * Z = M * Z = N * Z = Z * Z = Z
(define (fast-mul-interval x y)
  ; Tells us if interval is of type zero
  (define (isZ p) (or (zero? (lower-bound p)) (zero? (upper-bound p))))
  ; Tells us if interval type is of type mixed
  (define (isM p) (and (negative? (lower-bound p)) (positive? (upper-bound p))))
  ; Tells us if interval type is of type positive
  (define (isP p) (and (positive? (lower-bound p)) (positive? (upper-bound p))))
  ; Tells us if interval type is of type negative
  (define (isN p) (and (negative? (lower-bound p)) (negative? (upper-bound p))))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((or (isZ x) (isZ y)) (make-interval 0 0))
          ((isP x) (cond ((isP y) (make-interval (* a c) (* b d)))
                         ((isM y) (make-interval (* b c) (* b d)))
                         ((isN y) (make-interval (* b c) (* a d)))
                         (else "unkown interval type: " (print-interval x))))
          ((isM x) (cond ((isP y) (make-interval (* a d) (* b d)))
                         ((isM y) (make-interval (min (* a d)
                                                      (* b c))
                                                 (max (* a c)
                                                      (* b d))))
                         ((isN y) (make-interval (* b c) (* a c)))
                         (else "unkown interval type: " (print-interval x))))
          ((isN x) (cond ((isP y) (make-interval (* a d) (* b c)))
                         ((isM y) (make-interval (* a d) (* a c)))
                         ((isN y) (make-interval (* b d) (* a c)))
                         (else "unkown interval type: " (print-interval x))))
          (else "unkown interval type: " (print-interval x)))))

; Make an interval by using a center of an interval and the error width
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
; Center of the interval
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
; Width has already been defined as part of Exercise 2.9

; Define an interval given center and the percentage error width
; For 5 +/- 5% you pass (make-center-percent 5 5)
(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
    (make-center-width c w)))

; Percentage error width calculated given the interval
(define (percent i)
  (* (/ (width i) (center i)) 100.0))

; Exercise 2.13
;
; Simple approximation to percentage tolerance of the product of
; two intervals defined in terms of tolerance of the factors
;
; Pass two tolerances in terms of percent (out of 100)
; And assume that we have positive intervals
; [a,b] * [c,d] = [a*c, b*d]
;
; We have [a,b] represented using (c1,p1) [c1 - (p1/c1*100), c1 + (p1/c1*100)]
; and [c,d] represented using (c2,p2) = [c2 - (p2/c2*100), c2 + (p2/c2*100)]
;
; Therefore the product [a * c, b * d] will have
;
; = [(c1-(p1/c1*100))*(c2-(p2/c2*100)), (c1+(p1/c1*100))*(c2 +(p2/c2*100))]
;
; = [c1*c2 - ((c1*p2)/(c2*100)) - ((p1*c2)/(c1*100)) + (p1*p2)(c1*c2*100*100),
;    c1*c2 + ((c1*p2)/(c2*100)) + ((p1*c2)/(c1*100)) + (p1*p2)(c1*c2*100*100)]
;
; = [(c1*c2)(1 - (p1 + p2)/100 + p1*p2/100^2),
;    (c1*c2)(1 + (p1 + p2)/100 + p1*p2/100^2)]
;
; With the assumption that percentages are small we can drop p1*p2/100^2
(define (product-of-tolerances p1 p2) (+ (percent i1) (percent i2)))


; Tests for initial exercises
(define inv1 (make-interval -3 -2))
(display "Interval 1: ")
(print-interval inv1)
(display "Center 1: ") ; Exercise 2.9
(center inv1)
(display "Width 1: ")
(width inv1)
(display "Percentage 1: ")
(percent inv1)

(define inv2 (make-interval -7 -5))
(display "Interval 2: ")
(print-interval inv2)
(display "Center 2: ")
(center inv2)
(display "Width 2: ")
(width inv2)
(display "Percentage 2: ")
(percent inv2)

(display "Add Intervals:")
(print-interval (add-interval inv1 inv2))

(display "Subtract Intervals: ")
(print-interval (sub-interval inv1 inv2))

(display "Multiply Intervals: ")
(print-interval (fast-mul-interval inv1 inv2))

(display "Divide Intervals: ")
(print-interval (div-interval inv2 inv1))
; Test for Exercise 2.10
; (div-interval inv2 (make-interval 0 1)) ; Error when uncommented

; Tests for fast multiplication (exercise 2.11)
(newline)
(display "Fast multiplication test start\n")
; P * P
(let ((a (make-interval 5 7))
      (b (make-interval 3 4)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; P * M
(let ((a (make-interval 5 7))
      (b (make-interval -3 4)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; P * N
(let ((a (make-interval 5 7))
      (b (make-interval -3 -2)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; M * P
(let ((a (make-interval -3 2))
      (b (make-interval 5 7)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; M * M
(let ((a (make-interval -5 7))
      (b (make-interval -3 4)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
(let ((a (make-interval -3 4))
      (b (make-interval -5 7)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; M * N
(let ((a (make-interval -5 7))
      (b (make-interval -3 -2)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; N * P
(let ((a (make-interval -3 -2))
      (b (make-interval 5 7)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; N * M
(let ((a (make-interval -3 -2))
      (b (make-interval -5 7)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; N * N
(let ((a (make-interval -3 -2))
      (b (make-interval -7 -5)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; Z * any
(let ((a (make-interval 0 0))
      (b (make-interval 4 5)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; any * Z
(let ((a (make-interval 3 4))
      (b (make-interval 0 0)))
  (equal? (mul-interval a b) (fast-mul-interval a b)))
; Done
(display "Fast multiplication test end\n")

; Test for 2.12
(newline)
(define cw-inv (make-center-width 10 1))
(display "CW interval: ")
(print-interval cw-inv)
(display "CW Center: ")
(center cw-inv)
(display "CW Width: ")
(width cw-inv)
(display "CW Percentage: ")
(percent cw-inv)

(newline)
(define cp-inv (make-center-percent 10 10))
(display "CP interval: ")
(print-interval cp-inv)
(display "CP Center: ")
(center cp-inv)
(display "CP Width: ")
(width cp-inv)
(display "CW Percentage: ")
(percent cp-inv)

; Test for exercise 2.13
(newline)
(define i1 (make-center-percent 5 2))
(display "I1: ")
(print-interval i1)
(display "I1 Percentage: ")
(percent i1)

(define i2 (make-center-percent 6 3))
(display "I2: ")
(print-interval i2)
(display "I2 Percentage: ")
(percent i2)

(define i1_i2 (mul-interval i1 i2))
(display "I1 * I2: ")
(print-interval i1_i2)
(display "I1 * I2 real percentage: ")
(percent i1_i2)
(display "Approximated percentage: ")
(product-of-tolerances i1 i2)

; Exercise 2.14
; Different evaluations of intervals of the same expression leads to different
; results
;
(newline)
(display "Show that different evaluations lead to different results:\n")
(define r1 (make-center-percent 100 5))
(display "R1: ")
(print-interval r1)
(display "R1 Percentage: ")
(percent r1)
(define r2 (make-center-percent 200 2))
(display "R2: ")
(print-interval r2)
(display "R2 Percentage: ")
(percent r2)

; First way of evauluation
; Notice more error than second evaluation
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define res1 (par1 r1 r2))
(display "Res1: ")
(print-interval res1)
(display "Res1 Percentage: ")
(percent res1)

; Second way of evaluation
; Notice: More tightly bound, less error prone
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval (div-interval one r1)
                   (div-interval one r2)))))
(define res2 (par2 r1 r2))
(display "Res2: ")
(print-interval res2)
(display "Res2 Percentage: ")
(percent res2)

; Notice the imperfection when we divide an interval by itself
(define r1byr1 (div-interval r1 r1))
(display "R1/R1: ")
(print-interval r1byr1)
(display "R1/R1 Percentage: ")
(percent r1byr1)

(define r1byr2 (div-interval r1 r2))
(display "R1/R2: ")
(print-interval r1byr2)
(display "R1/R2 Percentage: ")
(percent r1byr2)

; Exercise 2.15
; The observation is correct, if an interval is mentioned in the expression
; more than once the error is more.

; Exercise 2.16
; Dependency issue: http://goo.gl/VXaKpF (Wikipedia article)
; You can see in the text that if an interval is used more than once,
; we can land up into dependency problem. Not all functions can be rewritten
; so that we mention that interval only once in the expression.
; This is a HARD research problem.
