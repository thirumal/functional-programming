#lang racket

; Exercise 2.2: Define abstractions for point and line segment
; Define constructors and selectors for the above abstractions
; Define a routine mid-point which when given a line segment
; will find the mid point on it.

; Helper function
(define (average x y) (/ (+ x y) 2))

; Routine to create, fetch parts of a point
(define make-point cons)
(define x-point car)
(define y-point cdr)

; Routines to create, fetch parts of a line segment
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; Routine to find mid-point of a line segment
(define (mid-point linseg)
  (make-point (average (x-point (start-segment linseg))
                       (x-point (end-segment linseg)))
              
              (average (y-point (start-segment linseg))
                       (y-point (end-segment linseg)))))

; Routine to print a point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; Exercise 2.3 Rectangle area and perimeter
; A rectangle can be represented as a line segment across the diagonal
; Area is l*h
; Perimeter is 2*(l+h)

; Constructor to create a rectangle
(define make-rect make-segment)
; Selectors to get height and width of the rectangle
(define (length rect)
  (abs (- (x-point (end-segment rect))
          (x-point (start-segment rect)))))
(define (height rect)
  (abs (- (y-point (end-segment rect))
          (y-point (start-segment rect)))))

; Another representation of rectangle, using two points one of the ends
; and the midpoint
(define rect-make cons)
(define midpt cdr)
(define othpt car)
(define (length2 rect)
  (* 2 (abs (- (x-point (othpt rect))
               (x-point (midpt rect))))))
(define (height2 rect)
  (* 2 (abs (- (y-point (othpt rect))
               (y-point (midpt rect))))))

; Area of the rectangle and preimeter of the rectangle
; we need to pass length and height of the rectangle be
; whatever the representation.
(define (rect-area l h) (* l h))
(define (rect-peri l h) (* 2 (+ l h)))

; Test for Exercise 2.2
(define linseg (make-segment (make-point 5 -8) (make-point -5 8)))
(print-point (mid-point linseg))

; Test for Excercise 2.3
; First representation of a rectangle
(define myrect (make-rect (make-point 5 -8) (make-point -5 8)))
(rect-area (length myrect) (height myrect))
(rect-peri (length myrect) (height myrect))
; Second representation of a rectangle
(define myrect2 (rect-make (make-point 5 -8) (make-point 0 0)))
(rect-area (length2 myrect2) (height2 myrect2))
(rect-peri (length2 myrect2) (height2 myrect2))
