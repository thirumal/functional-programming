#lang racket

; Procedures as arguments

(define (sum-from-to a b term next)
  (if (> a b)
      0
      (+ (term a) (sum-from-to (next a) b term next))))

(define (sum-from-to-iter a b term next)
  (define (iter acc count)
    (if (> count b)
        acc
        (iter (+ (term count) acc) (next count))))
  (iter 0 a))

(define (sum-of-integers a b) (sum-from-to-iter a
                                                b
                                                (lambda (x) x)
                                                (lambda (x) (+ x 1))))

(define (sum-of-cubes a b) (sum-from-to-iter a
                                             b
                                             (lambda (x) (* x x x))
                                             (lambda (x) (+ x 1))))

(define (pi-sum a b) (sum-from-to-iter a
                                       b
                                       (lambda (x) (/ 1.0 (* x (+ x 2))))
                                       (lambda (x) (+ x 4))))

(define (integral f a b dx) (* dx (sum-from-to-iter (+ a (/ dx 2.0))
                                              b
                                              f
                                              (lambda (x) (+ x dx)))))

; Tests
(sum-of-integers 1 10)
(sum-of-cubes 1 10)
(pi-sum 1 100000) ; Comes near (/ pi 8)

(define (cube x) (* x x x))
(integral cube 0 1 0.0001)
