#lang racket

; Mapping over lists
; Apply a transformation to each element in the list

; Helper functions
(define (square x) (* x x))

; Recursive version
(define (scale-list-recr items factor)
  (if (null? items)
      null
      (cons (* (car items) factor) (scale-list-recr (cdr items) factor))))

; Iterative version (inefficient)
(define (scale-list-iter items factor)
  (define (iter orig-items ret-items)
    (if (null? orig-items)
        ret-items
        (iter (cdr orig-items) (append ret-items
                                       (list (* (car orig-items) factor))))))
  (iter items (list)))

(display "Scaling a list(recursive and iterative):\n")
(scale-list-recr (list 1 2 3 4 5) 10)
(scale-list-iter (list 1 2 3 4 5) 10)

; The ultimate map function given out by Racket
(display "Racket map example:\n")
(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

; Our map function defined in SICP
(define (me-map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (me-map proc (cdr items)))))

(display "Scaling list using map function:\n")
(define (scale-list-map items factor)
  (me-map (lambda (x) (* factor x)) items))
(scale-list-map (list 1 2 3 4 5) 10)

; Exercise 2.21
;
; Square every element in the list
(display "Square every element in the list using map:\n")
(define (square-list1 items)
  (me-map (lambda (x) (* x x)) items))
(square-list1 (list 1 2 3 4 5))

(display "Square every element recursively on a list:\n")
(define (square-list2 items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list2 (cdr items)))))
(square-list2 (list 1 2 3 4 5))

; Exercise 2.22
;
; Iterative square list procedure attempt #1
(display "Square every element in a list, iterative attempt #1:\n")
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items null))
(square-list3 (list 1 2 3 4 5))
; As expected unfortunately, the answer list is in reverse order...

; Attempt #2 with reversing the order inside cons
(display "Square every element in a list, iterative attempt #2:\n")
(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items null))
(square-list4 (list 1 2 3 4 5))
; Unfortunately this will create a mess of pairs
; with car empty instead of cdr, which cannot join together
; to become a list

; Exercise 2.23
;
; for-each is very similar to map, but here are the differences:
; Use map, but don't make use of the return value of the proc
; Also the return value of the entire for-each function can be
; arbitrary, in my case I'm returning #f/false
(define (for-each proc items)
  (define (iter dummy things)
    (if (null? things)
        #f
        (iter (proc (car things)) (cdr things))))
  (iter null items))
; Test for for-each
(display "Test for for-each:\n")
(for-each (lambda (x) (display x)(newline)) (list 57 321 88))
