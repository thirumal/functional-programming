#lang racket

; Routine to compute the i,j th pascal triangle element
(define (pascal row col)
  (cond ((= col 0) 1)
	((= row col) 1)
	(else (+ (pascal (- row 1) col)
		 (pascal (- row 1) (- col 1))))))

; Tests
(pascal 0 0)
(newline)

(pascal 1 0)
(pascal 1 1)
(newline)

(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(newline)

(pascal 3 0)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
(newline)

(pascal 4 0)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)
(newline)
