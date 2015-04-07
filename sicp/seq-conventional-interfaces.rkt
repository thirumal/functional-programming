#lang racket

; The above import is for prime? check
(require math/number-theory)

; Helper routines
(define (not-pair? x) (not (pair? x)))
(define (square x) (* x x))
(define (fib n)
  (define (iter b a i)
    (if (= i n)
        a
        (iter a (+ a b) (+ i 1))))
  (iter 1 0 0))

; Use of conventional interfaces

; First let's take a look at a program
; Modified count-leaves procedure
; sum of square of only those leaves which are odd

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not-pair? tree) (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
; Test for sum-odd-squares
(display "Sum of odd leaf squares of a tree: ")
(sum-odd-squares (list 1 2 (list 3 4 5 (list 6 7 8 9) 10 ) 11))

; This can be conceptually thought of as:
; enumerate: tree leaves -> filter: odd? -> map: square -> accumulate: +, 0

; Another program...
; Even fibonnaci numbers
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))
(display "Even fibonnaci numbers: ")
(even-fibs 10)
(newline)

; This can be expressed as
; enumerate: integers -> map: fib -> filter: even? -> accumulate: cons, () 

; These could be expressed as the two recipes given after the programs,
; but we need to structure our programs in such a way, the above hapazard
; way of putting things together will only work for tiny programs

; With this in mind, let's start making those generic procedures

; We have map with us already

; One implementation of the filter procedure (scheme built-in)
(define (filter pred seq)
  (if (null? seq)
      null
      (let ((head (car seq))
            (tail (cdr seq)))
        (if (pred head)
            (cons head (filter pred tail))
            (filter pred tail)))))
; Test for our filter method
(display "Test for filter, get only odd elements: ")
(filter odd? (list 1 2 3 4 5))
(newline)

; Accumulate procedure
(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq) (accumulate f init (cdr seq)))))
; Tests for accumulate procedure:
(display "Test for accumulate procedure:\n")
(display "Sum: ")
(accumulate + 0 (list 1 2 3 4 5))
(display "Product: ")
(accumulate * 1 (list 1 2 3 4 5))
(newline)

; Enumerate interval
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))
; Test for enumerate interval
(display "Enumerate an interval: ")
(enumerate-interval 2 7)

; Procedure to enumerate a tree
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not-pair? tree) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
; Test for enumerating a tree
(display "Enumerate a tree: ")
(enumerate-tree (list 1 (list 2 (list 3 4) 5) 6))

; Putting it all together
; enumerate: tree leaves -> filter: odd? -> map: square -> accumulate: +, 0
(define (sum-odd-squares-2 tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

; enumerate: integers -> map: fib -> filter: even? -> accumulate: cons, () 
(define (even-fibs-2 n)
  (accumulate cons null (map fib (filter even? (enumerate-interval 0 n)))))

; Test for the above routines
(display "Test for modularized, sum odd square leaves of a tree:\n")
(sum-odd-squares-2 (list 1 2 (list 3 4 5 (list 6 7 8 9) 10 ) 11))
(display "Test for modularized, even fibonnaci numbers till n:\n")
(even-fibs-2 10)
(newline)

; enumerate: integers -> map: fib -> map: square -> accumulate: cons, ()
(display "Squares of fibonnaci numbers: ")
(define (list-fib-squares n)
  (accumulate cons null (map square (map fib (enumerate-interval 0 n)))))
(list-fib-squares 10)

; Product of squares of odd numbers in a sequence
; enumerate: integers -> filter: odd? -> map: square -> accumulate *, 1
(display "Product of squares of first odd numbers < 10: ")
(define (sum-squares-odd seq)
  (accumulate * 1 (map square (filter odd? seq))))
(sum-squares-odd (list 1 2 3 4 5))

; Exercise 2.33

; Defining map in-terms of accumulate
(define (map-x f seq)
  (accumulate (lambda (x y) (cons (f x) y)) null seq))
; Test for map-x
(display "Defined map in terms of accumulate: ")
(map-x (lambda (x) (+ x 1)) (list 1 2 3 4))

; Defining append in terms of accumulate
(define (append-x seq1 seq2)
  (accumulate cons seq2 seq1))
; Test for append-x
(display "Append in terms of accumulate: ")
(append-x (list 1 2 3 4) (list 5 6 7 8))

; Defining length in terms of accumulate
(define (length-x seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))
(display "Length in terms of accumulate: ")
(length-x (list 1 2 3 4))
(newline)

; Exercise 2.34

; Horner's rule for evaluating a polynomial
(define (horner-eval x seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              seq))
(display "Horner evaluation of 1 + 3x + 5x^3 + x^5 at x = 2: ")
(horner-eval 2 (list 1 3 0 5 0 1))
(newline)

; Exercise 2.35
; Redefine count leaves as accumulation
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
; Test for the new counting leaves routine
(display "Count leaves in a tree: ")
(count-leaves (list (list 1 2) 3 (list 4 5 (list 6 7)) 8))
(newline)

; Exercise 2.36
; accumulate n: Accumulate n lists
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; Test for accumulate-n
(display "Accumulate n sequences: ")
(accumulate-n + 0 (list (list 1 2 3)
                        (list 4 5 6)
                        (list 7 8 9)
                        (list 10 11 12)))
(newline)

; Eercise 2.37
; v is a vector, m is a matrix,
(define my-vector-1 (list 1 2 3 4))
(display "Vector #1 (v1): ")
my-vector-1
(define my-vector-2 (list 7 8 9 10))
(display "Vector #2 (v2): ")
my-vector-2
(define my-matrix-1 (list (list 1 2 3 4)
                          (list 4 5 6 6)
                          (list 6 7 8 9)))
(display "Matrix #1 (m1): ")
my-matrix-1
(define my-matrix-2 (list (list 1 2 3)
                          (list 4 5 6)
                          (list 6 7 8)
                          (list 9 10 11)))
(display "Matrix #2 (m2): ")
my-matrix-2

; Vector operations
; Dot product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; Test for dot product
(display "Dot product: (v1 . v2): ")
(dot-product my-vector-1 my-vector-2)

; Matrix vector product
; matrix is a list of list
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
; Test
(display "Matrix-vector multiplication (m1 * v1): ")
(matrix-*-vector my-matrix-1 my-vector-1)

; Transpose of a matrix:
(define (transpose m)
  (accumulate-n cons null m))
; Test
(display "Transpose of a matrix: ")
(transpose my-matrix-1)

; Matrix-matrix multiplication
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows) (matrix-*-vector cols rows)) m)))
(display "Matrix-matrix multiplication: ")
(matrix-*-matrix my-matrix-1 my-matrix-2)
(newline)

; Exercise 2.38
; fold-right is another name for accumulate
; fold-left does it from the other way...
(define fold-right accumulate)

(define (fold-left op init seq)
  (define (iter res rest)
    (if (null? rest)
        res
        (iter (op res (car rest)) (cdr rest))))
  (iter init seq))
; Experiments
(display "Start of fold-left and fold-right experiments\n")
(fold-left / 1 (list 1 2 3)) ; yields 1/6
(fold-right / 1 (list 1 2 3)) ; yields 3/2
(fold-left list null (list 1 2 3)) ; yields '(((() 1) 2) 3)
(fold-right list null (list 1 2 3)) ; yields '(1 (2 (3 ())))

; If the operation (op) is commutative then both fold-right and fold-left
; yield the same result (eg. a * b = b * a)
(= (fold-left + 0 (list 1 2 3)) (fold-right + 0 (list 1 2 3)))
(display "End of fold-left and fold-right experiments\n")
(newline)

; Exercise 2.39
; Reversing a list using fold-right and fold-left
(define (reverse-fr seq)
  (fold-right (lambda (x y) (append y (list x))) null seq))
(display "Test for fold-right reverse: ")
(reverse-fr (list 1 2 3 4))

(define (reverse-fl seq)
  (fold-left (lambda (x y) (append (list y) x)) null seq))
(display "Test for fold-left reverse: ")
(reverse-fl (list 1 2 3 4))
(newline)

; Nested mapping
; Generate all pairs (i,j), where 1 <= j < i <= n
(define (generate-pairs n)
  (accumulate
   append
   null
   (map (lambda (i)
          (map (lambda (j) (list i j))
               (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 5))))
(display "Generate pairs: ")
(generate-pairs 5)
(newline)

; The above accumulate with append and null
; and then mapping is so common that we call it flat map
(define (flatmap proc seq)
  (accumulate append null (map proc seq)))
; Intution of what accumulate: append, null does:
; > (accumulate append null (list (list) (list 1 (list 2 3)) (list) (list 4 5)))
; '(1 (2 3) 4 5)

; Intuition:
; What flatmap does in a simple way is when you have a list of lists
; it takes a function f and applies it to each list in the list
; and appends them to form a single list

; Example
(display "Example of flatmapping: ")
(flatmap (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 5))

; Find all pairs less than N such that sum of the pairs is prime
; (i,j) such that i + j is prime and 1 < i,j <= N

; Helper routine #1: Sees if the sum of pairs is prime
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; Helper routine #2: Given a pair (i j) generate a triple (i j i+j)
(define (make-pair-sum pair)
  (let ((head (car pair))
        (tail (cadr pair)))
    (list head tail (+ head tail))))

; Putting all these things together
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap (lambda (i)
                                     (map (lambda (j) (list i j))
                                          (enumerate-interval 1 (- i 1))))
                                   (enumerate-interval 1 n)))))
; Test
(display "Generate all prime sum pairs less than 5:\n")
(prime-sum-pairs 5)
(newline)

; Removing an item from the list
(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))
(display "Removing 3 from (list 1 2 3 4 5): ")
(remove 3 (list 1 2 3 4 5))
(newline)

; Program to return the permutations of a set
; Say we have a set S with elements in it
; For every item x in S, recursively generate sequences
; of permutations of S - x (x removed from S) and finally
; append x to the front of each list
(define (permutation s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permutation (remove x s))))
               s)))
(display "Permutations of S = {1, 2, 3}: ")
(permutation (list 1 2 3))
(newline)

; Exercise 2.40
; Generate pairs (i j) where 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(display "Pairs (i j) where 1 <= j < i <= n:\n")
(unique-pairs 5)
(newline)

; Exercise 2.41
; Generate triplets (i j k) such that 1 <= k < j < i <= n
; and (i + j + k) === s
(define (triplet-sum s n)
  (filter (lambda (x)
            (= s (+ (car x)
                    (cadr x)
                    (car (cdr (cdr x))))))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(display "Generating distinct triplets upto 5 that sum upto 6: ")
(triplet-sum 6 5)
(newline)

; Exercise 2.42
; n-queens problem

(define (queens N)
  ; Empty board
  (define empty-board null)
  ; Method that sees the existing placed queens
  ; till k-1 and determines whether placing a
  ; queen at k is safe or not...
  (define (safe? k positions)
    (let ((me (list-ref positions (- k 1)))
          (others (filter (lambda (col)
                            (not (= k (cdr col))))
                          positions)))
      (define (can-attack? q1 q2)
        (or (= (car q1) (car q2))
            (= (abs (- (car q1) (car q2)))
               (abs (- (cdr q1) (cdr q2))))))
      (define (iter queen list-of-queens)
        (or (null? list-of-queens)
            (and (not (can-attack? queen (car list-of-queens)))
                 (iter queen (cdr list-of-queens)))))
      (iter me others)))
  ; Method that adjoins a new queen at position
  ; (new-row k) along with the rest of the queens
  (define (adjoin-position row col positions)
   (append positions (list (cons row col))))
  ; Method that returns sequence of all ways to place
  ; k queens in the first k columns of the board
  (define (queen-cols k)
    ; If empty board
    (if (= k 0)
        ; return an empty board with no queens
        (list empty-board)
        ; else, filter
        (filter
         ; only those boards who will be safe after
         ; placing a queen at position k
         (lambda (positions) (safe? k positions))
         ; from the set of a new set of boards
         ; built from the existing placed queens
         (flatmap
          ; where we have placed N queens in column k
          ; along with the k-1 queens safely placed on the board
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   ; place a queen at (new-row k) along with
                   ; the rest of the queens
                   (adjoin-position new-row k rest-of-queens))
                 ; for every row in the board (N) for column k
                 ; iterated by new-row using map
                 (enumerate-interval 1 N)))
          ; from the k-1 filled queen columns
          (queen-cols (- k 1))))))
  ; Call the procedure queen-cols on N sized board
  (queen-cols N))
(display "N Queens problem solutions for:\n")
(display "1x1 Board: ")
(queens 1)
(display "2x2 Board: ")
(queens 2)
(display "3x3 Board: ")
(queens 3)
(display "4x4 Board: ")
(queens 4)