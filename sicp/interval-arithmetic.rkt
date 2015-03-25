#lang racket

; Interval Arithmetic
; Exercise 2.7 - 2.16

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
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Exercise 2.8
; Method to subtract two intervals
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))

; Method to multiply two intervals
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Exercise 2.9, add test for zero divisor
; Method to divide two intervals
(define (div-interval x y)
  (if (or (= (lower-bound y) 0) (= (upper-bound y) 0))
      (error "The bounds of the second interval cannot be zero: "
             (lower-bound y)
             (upper-bound y))
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


; Tests
(define inv1 (make-interval 3 4))
(display "Interval 1: ")
(print-interval inv1)

(define inv2 (make-interval 16 9))
(display "Interval 2: ")
(print-interval inv2)

(display "Add Intervals:")
(print-interval (add-interval inv1 inv2))

(display "Subtract Intervals: ")
(print-interval (sub-interval inv2 inv1))

(display "Multiply Intervals: ")
(print-interval (mul-interval inv1 inv2))

(display "Divide Intervals: ")
(print-interval (div-interval inv2 inv1))
; (div-interval inv2 (make-interval 0 1)) ; Error when uncommented
