#lang racket

; Mapping over trees

; Helper functions
(define (not-pair? x) (not (pair? x)))
(define (square x) (* x x))

; First implementation
(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not-pair? tree) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(display "Tree: ")
(define x-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))) x-tree
(display "Scaled Tree: ")
(scale-tree x-tree 10)

; Another implementation using map
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (not-pair? sub-tree)
             (* sub-tree factor)
             (scale-tree-map sub-tree factor)))
       tree))
(display "Mapped scaled tree: ")
(scale-tree-map x-tree 10)  
(newline)

; Exercise 2.30
; Squaring all the elements in a tree
(define (square-tree-1 tree)
  (cond ((null? tree) tree)
        ((not-pair? tree) (square tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))
(display "Square a tree: ")
(square-tree-1 x-tree)

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (not-pair? sub-tree)
             (square sub-tree)
             (square-tree-2 sub-tree)))
       tree))
(display "Squaring a tree with map: ")
(square-tree-2 x-tree)

; Exercise 2.31: More abstraction
(define (tree-map f tree)
  (map (lambda (subtree)
         (if (not-pair? subtree)
             (f subtree)
             (tree-map f subtree)))
       tree))
(define (square-tree-3 tree) (tree-map square tree))
(display "Abstracted map, squaring a tree: ")
(square-tree-3 x-tree)
(newline)

; Exercise 2.32
; Power set of the set (all subsets of the current set)
;
; Given (list 1 2 3)
; we generate something like this
; (list
;    (list)
;    (list 3)
;    (list 2)
;    (list 2 3)
;    (list 1)
;    (list 1 3)
;    (list 1 2)
;    (list 1 2 3))
(define (power-set s)
  (if (null? s)
      (list null)
      (let
          ((rest (power-set (cdr s))))
        (append rest
                (map (lambda (subset)
                       (cons (car s) subset))
                     rest)))))
(display "Subsets of (list 1 2 3): ")
(power-set (list 1 2 3))
