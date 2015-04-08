#lang racket

; This is so that the painters etc. come out for us...
(require (planet soegaard/sicp:2:1/sicp))

; We'll rename einstein as rogers and diagonal-shading as
; wave for our conveiniece
(define wave diagonal-shading)
(define rogers einstein)

; Painting rogers
(display "A picture of Einstein, but Rogers for us:\n")
(paint rogers)
(newline)

(define wave2 (beside wave (flip-vert wave)))
(display "The use of method beside to show a picture beside another:\n")
(paint wave2)

(define wave4 (below wave2 wave2))
(display "Below is used on previous picture to show one below other:\n")
(paint wave4)

; Flipped pairs routine
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(display "The flipped pairs effect (combining above two.):\n")
(paint (flipped-pairs wave))
(newline)

; Right split routine
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
; Test for right split
(display "Right split once:\n")
(paint (right-split rogers 1))
(display "Right splitting twice:\n")
(paint (right-split rogers 2))
(newline)

; Exercise 2.44
; Up split routine
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
; Tests for up-split
(display "Up split once:\n")
(paint (up-split rogers 1))
(display "Up split twice:\n")
(paint (up-split rogers 2))
(newline)

; Corner split routine
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
; Tests for corner split
(display "Corner split once:\n")
(paint (corner-split rogers 1))
(display "Corner split twice:\n")
(paint (corner-split rogers 2))
(newline)

; Square limit, by placing flips of corner-split in 4 directions
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
          (below (flip-vert half) half))))
(display "Picture frame by collating corner splits in different directions:\n")
(paint (square-limit rogers 1))
(paint (square-limit rogers 2))
