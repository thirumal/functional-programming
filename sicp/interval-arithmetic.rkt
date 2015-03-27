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
; For simplicity think a > b > d > c
; Number line: ------c----d-----b-----a-----
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

; Exercise 2.9, add test for zero divisor
; Method to divide two intervals
; Number line: -----c------d-------b-----a--------
; Also figures out if y spans 0 [c,d] has 0 in the middle
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "The bounds of the second interval cannot span a zero: "
             (print-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


; Exercise 2.9
; Width of an interval
; Half the difference between its upper and lower bounds
(define (width-interval inv)
  (/ (- (upper-bound inv) (lower-bound inv)) 2))

; Width is closed under addition and subtraction of intervals
; as the relation holds good all the time
; But not the case of the multiplication or division
; Because the min and the max can be chosen from any argument
; depending on the argument passed

; Exercise 2.10
;
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

; Tests
(define inv1 (make-interval -3 -2))
(display "Interval 1: ")
(print-interval inv1)

(define inv2 (make-interval -7 -5))
(display "Interval 2: ")
(print-interval inv2)

(display "Add Intervals:")
(print-interval (add-interval inv1 inv2))

(display "Subtract Intervals: ")
(print-interval (sub-interval inv1 inv2))

(display "Multiply Intervals: ")
(print-interval (fast-mul-interval inv1 inv2))

(display "Divide Intervals: ")
(print-interval (div-interval inv2 inv1))
; (div-interval inv2 (make-interval 0 1)) ; Error when uncommented

; Tests for fast multiplication
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
