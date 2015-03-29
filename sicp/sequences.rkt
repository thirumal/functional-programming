#lang racket

; 2.2.1 Sequences

(define mylist1 (cons 1
                      (cons 2
                            (cons 3
                                  (cons 4 null)))))

(display "List #1: ")
(display mylist1)
(newline)

(define mylist2 (list 1 2 3 4))
(display "List #2: ")
(display mylist2)
(newline)

(display "Are lists equal? ")
(equal? mylist1 mylist2)

(display "With car you can get the first element: ")
(car mylist1)

(display "With cdr you can get the rest of the list: ")
(cdr mylist1)

(display "With cadr you can get the next element\n")
(display "cadr is equivalent to (car (cdr listname)): ")
(cadr mylist1)

; Gets the n'th item in the list [0,...n)
(define (my-list-ref list n)
  (if (= n 0)
      (car list)
      (list-ref (cdr list) (- n 1))))

; Test for the list-ref routine
(newline)
(display "Test for my-list-ref start\n")
(= (my-list-ref mylist1 0) (list-ref mylist1 0))
(= (my-list-ref mylist1 1) (list-ref mylist1 1))
(= (my-list-ref mylist1 2) (list-ref mylist1 2))
(= (my-list-ref mylist1 3) (list-ref mylist1 3))
(display "Test for my-list-ref end\n")
(newline)

; Calculate the length of the list
;
; Recursive version:
;
(define (my-length-recr list)
  (if (null? list)
      0
      (+ 1 (my-length-recr (cdr list)))))

; Calculate the length of the list
;
; Iterative version:
;
(define (my-length-iter list)
  (define (iter acc list)
    (if (null? list)
        acc
        (iter (+ 1 acc) (cdr list))))
  (iter 0 list))

(display "Tests for my-length (recursive and iterative):\n")
(display "List #1: ")
(my-length-recr mylist1)
(display "List #2: ")
(my-length-iter mylist2)
(newline)

; Append the second list after the first
; Recursive version
(define (my-append-recr list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (my-append-recr (cdr list1) list2))))

; Exercise 2.17
; The last element of a (non-empty) list
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(display "Last element in the list: ")
(last-pair mylist1)

; Exercise 2.18: Reverse of a list
(define (reverse list)
  (define (iter res items)
    (if (null? items)
        res
        (iter (cons (car items) res) (cdr items))))
  (iter null list))

(display "Reverse of the list: ")
(reverse mylist1)

; Exercise 2.19
; Coin counting change problem
; Already solved in coin-change.rkt
;
(define (coin-change money coins)
  (define (cc amount coin-values)
    (define no-more? null?)
    (define first-denom car)
    (define except-first-denom cdr)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else (+ (cc amount
                       (except-first-denom coin-values))
                   (cc (- amount (first-denom coin-values))
                       coin-values)))))
  (if (= money 0)
      0
      (cc money coins)))

(display "Coin change problems:\n")
(coin-change 0 (list))
(coin-change 0 (list 1 2 3 4))
(coin-change 100 (list))
; Simple cases
(coin-change 1 (list 1))
(coin-change 1 (list 2))
(coin-change 4 (list 1 2))
; Large case, and unsorted case
(coin-change 300 (list 5 10 20 50 100 200 500))
(coin-change 300 (list 500 5 50 100 20 200 10))
; Impossible case
(coin-change 301 (list 5 10 20 50 100 200 500))

; Exercise 2.20
; Dotted tail notation

; The function below needs at least two arguments
(define (foo x y . z)
  (display "foo => ")
  (display "x: ")
  (display x)
  (display ", y: ")
  (display y)
  (display ", z: ")
  (display z)
  (newline))

(foo 1 2)
(foo 1 2 3 4)

; A function taking zero or more arguments
(define (goo . w)
  (display "goo => ")
  (display w)
  (newline))
(goo)
(goo 1)
(goo 1 2 3 4)

; Exercise 2.20
; A function that takes one or more integers and returns a list
; of all arguments that have the same even-odd parity as the
; first argument
;
; Example:
; (same-parity 1 2 3 4 5 6 7) => 1 3 5 7
; (same-parity 2 3 4 5 6 7) => 2 4 6
;
(define (same-parity x . inputs)
  (define (same-remainder? a b m)
    (= (remainder a m) (remainder b m)))
  (define (iter res items)
    (cond ((null? items) res)
          ((same-remainder? x (car items) 2)
           (iter (cons (car items) res) (cdr items)))
          (else (iter res (cdr items)))))
  (reverse (iter (list x) inputs)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)