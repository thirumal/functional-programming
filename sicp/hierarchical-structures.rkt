#lang racket

; 2.2.2 Hierarchical structures

; Observation #1:
; As the binding of the right most pair's null cannot be changed
; one can only add to an element from the left of the list

(display "Appending 4 to (list 9 10), we get a list: ")
(cons 4 (list 9 10))

(display "Appending (list 9 10) to 4, we get a composite pair: ")
(cons (list 9 10) 4)

(display "Therefore if we cons (list 1 2) and (list 3 4), we get: ")
(define m (cons (list 1 2) (list 3 4)))
m
; The above in a procedural language could have been an ambuigity
; but here we can safely say that we append (list 1 2) to (list 3 4)
; from the left

; We can give the above compositition an abstraction name (Trees)
; If we see the length of the of the above list,
; it'll be the count of all siblings
; A list is a collection of cons and in the end there's a black sheep!
; So don't count the null element
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))
; Test for count-leaves The number of leaves in the above tree
(display "The number of leaves in the above tree: ")
(count-leaves (cons (cons 1 2) (cons 3 4)))

; Another thought.. an indirect way of measuring connecting nodes
; is by measuring the number of null elements in the tree

; Exercise 2.24
; Evaluate (list 1 (list 2 (list 3 4)))
; Hint: You're adding a NULL in the end whenever you wrap
; a list around...
(display "Exercise 2.24: ")
(list 1 (list 2 (list 3 4)))

; Exercise 2.25
; Pick the element 7 from this list
(define list1 (list 1 3 (list 5 7) 9))
; The extra car to select the first element
; out of the singleton list which is present in the end
(car (cdr (car (cdr (cdr list1)))))

(define list2 (list (list 7)))
(car (car (list (list 7))))

(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr ; Removes 1 selects tail
      (car (cdr ; Removes 2 selects tail
            (car (cdr ; Removes 3 selects tail
                  (car (cdr ; Removes 4 selects tail
                        (car (cdr ; Removes 5 selects tail
                              (car (cdr list3)) ; Removes 6 selects tail
                              ))
                        ))
                  ))
            ))
      ))

; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(display "Selecting 7 out of various kinds of lists:\n")
; Experiment 2.26.1: append function
(append x y)
; Experiment 2.26.2: cons on these
(cons x y)
; Experiment 2.26.3: list on these
(list x y)
(newline)

; Exercise 2.27: Deep reversing a list
(display "Reversing and deep reversing this list: ")
(define rev-test (list (list 1 2) (list 3 4)))
rev-test

(define (reverse items)
  (define (iter res things)
    (if (null? things)
        res
        (iter (cons (car things) res) (cdr things))))
  (iter null items))
; Test for normal reverse
(display "Normal reverse: ")
(reverse rev-test)

(define (deep-reverse items)
  (define (iter res things)
    (cond ((null? things)
           res)
          ((not (pair? (car things)))
           (iter (cons (car things) res) (cdr things)))
          (else
           (iter (cons (reverse (car things)) res)(cdr things)))))
  (iter null items))
; Test for deep reverse
(display "Deep reverse: ")
(deep-reverse rev-test)
(newline)

; Exercise 2.28: Fringe - flattens the hierarchial lists
(define (flatten items)
  (cond ((null? items) null)
        ((not (pair? items)) (list items))
        (else (append (flatten (car items)) (flatten (cdr items))))))

; Flatten test
(define flatten-me (list (list 1 2) (list 3 4)))
(display "Original list: ")
flatten-me
(display "Flattened list: ")
(flatten (list flatten-me flatten-me))
(newline)

; Exercise 2.29
; A binary mobile has two branches: left branch and right branch
; Each branch is a rod of certain length, whose end may have
; a weight hanging about or another binary mobile

; Constructor for a binary mobile
(define (make-mobile left right)
  (list left right))

; Constructor for a branch
(define (make-branch br-len structure)
  (list br-len structure))

; a. Write correspond selectors for left-branch, right-branch
; branch-length and branch-structure
;
(define (left-branch x) (car x))
(define (right-branch x) (car (cdr x)))
(define (branch-length x) (car x))
(define (branch-structure x) (car (cdr x)))

; b. With the selectors defined above, get the total-weight of
; a binary mobile
(define (total-weight bm)
  (define (branch-weight b)
    (let ((bs (branch-structure b)))
      (if (not (pair? bs))
          bs
          (total-weight bs))))
  (+ (branch-weight (left-branch bm))
     (branch-weight (right-branch bm))))

(define l-mobile (list (list 4 15) (list 1 20)))
(define r-mobile (list (list 2 5) (list 3 10)))
(define my-mobile (list (list 1 l-mobile) (list 2 r-mobile)))
(display "Binary mobile: ")
my-mobile
(display "Total weight of this binary mobile: ")
(total-weight my-mobile)

; c. See if torque applied by the left branch is equal to the right-branch?
; and also if the left branch has a hanging binary mobile, even it should be
; balanced
; Function to calculate the branch torque of a binary mobile's branch
(define (branch-torque b)
  (let ((bl (branch-length b))
        (bs (branch-structure b)))
    (* bl (if (not (pair? bs))
              bs
              (+ (branch-torque (left-branch bs))
                 (branch-torque (right-branch bs)))))))

; Function to see if the binary mobile is balanced?
(define (is-balanced? bm)
  (if (not (pair? bm))
      true
      (and (is-balanced? (branch-structure (left-branch bm)))
           (is-balanced? (branch-structure (right-branch bm)))
           (= (branch-torque (right-branch my-mobile))
              (branch-torque (left-branch my-mobile))))))
(display "Is my-mobile balanced? ")
(is-balanced? my-mobile)

; d. If the representation of mobiles gets changed, how much of the
; of the implementation needs to be changed?
;
; Only the selectors mate, only the selectors.
