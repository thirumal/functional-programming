#lang racket

; Helper function, check for divisibility
; #arg1: divisor
; #arg2: number to be checked
; return: #t if divisible, #f if not
;
(define (divides? divisor number) (= (remainder number divisor) 0))

; Helper function: to find square of a number and halve a number
(define (halve x) (/ x 2))
(define (square x) (* x x))

; Intuition behind stopping our check at sqrt(N):
; If d is a divisor of n, then so is n/d.
; But d and n/d cannot both be greater than sqrt(N) or else
; (d) * (n/d) > N

; Program to find the smallest divisor of a number
; Straightforward O(sqrt(N)) algorithm
; Starts with 2, and keep checking till N is reached
(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n) ; No other divisor apart from n
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ 1 test-divisor)))))
  (find-divisor 2))

; Exercise 1.23
; Optimized algorithm, once the number is not divisible by 2
; We can just skip past all even numbers
(define (smallest-divisor-opt n)
  ; if number is 2, next divisor is 3 or else
  ; next divisor is +2 of current divisor (next odd number)
  (define (next divisor) (if (= divisor 2) 3 (+ 2 divisor)))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (next test-divisor)))))
  (find-divisor 2))

; Test for primality is so simple now
(define (prime? n) (= (smallest-divisor-opt n) n))

;HELPER function: simulate (runtime)
(define (runtime) (current-inexact-milliseconds))

; Timed primality test procedure, prints the time taken to check
; whether a number is prime or not
(define (timed-prime-test n)
  ; Procedure to print three stars and time taken for procedure to run
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  ; Procedure to time the event and display time taken if prime
  (define (start-prime-test n start-time)
    (cond ((prime? n) (report-prime (- (runtime) start-time))))
    (display " "))
  ; Print a newline, display the number
  ; print the time taken for test to run
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; For readibility I have not put this method inside fermat-test procedure
; Helper function: Method to find (a ^ expt % m)
(define (expmod-recr a expt m)
  (cond ((= expt 0) 1)
        ((even? expt) (remainder (square (expmod a (halve expt) m)) m))
        (else (remainder (* a (expmod a (- expt 1) m)) m))))

; This is the iterative version of the same above algorithm
(define (expmod a expt m)
  (define (iter even-acc odd-acc count)
    (cond ((= count 0) odd-acc)
          ((even? count) (iter (remainder (square even-acc) m)
                               odd-acc
                               (halve count)))
          (else (iter even-acc
                      (remainder (* even-acc odd-acc) m)
                      (- count 1)))))
  (iter a 1 expt))

; Fermat's little theorem
; "If n is a prime number and a is any +ve integer < n,
; then a^n % n == a % n"
;
; Fermat test is performed by choosing a random number a, between 1 and n - 1
; inclusive and checking whether the remainder modulo n of nth power of a is
; equal to a.
(define (fermat-test n)
  (define (random-number n) (+ 1 (random (- n 1)))) ; 0 < (random-number n) < n
  (define (try-it a) (= (expmod a n n) a))
  (try-it (random-number n)))

; Fast prime routine
;
; If iterator reaches 0, we assume the number is checked satisfactorily
; If fermat test passes, we go and attempt a check one more time
; If fermat test fails, we know it is not a prime number
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Miller Rabin test

; HELPER function
; New expmod2 function, finds (a ^ expt % m)
; Another thing to note is that 
(define (expmod2 a expt m)
  (define (iter even-acc odd-acc count)
    (cond ((= count 0) odd-acc)
          ; when count is even,
          ; if even-acc != 1 && even-acc != n - 1 && square(even-acc) % n == 1
          ; then signal 0 (not prime), else go for another iteration
          ((even? count) (if (and (not (= even-acc 1))
                                  (not (= even-acc (- m 1)))
                                  (= (remainder (square even-acc) m) 1))
                             0
                             (iter (remainder (square even-acc) m)
                                   odd-acc
                                   (halve count))))
          (else (iter even-acc
                      (remainder (* even-acc odd-acc) m)
                      (- count 1)))))
  (iter a 1 expt))

; Real miller rabin test routine
(define (miller-rabin-test n)
  ; 0 < (random-number n) < n
  (define (random-number n) (+ 1 (random (- n 1))))
  ; Procedure that finds the expmod2 and then runs test on the result
  (define (find-expmod-and-test rnum) (= (expmod2 rnum (- n 1) n) 1))
  (find-expmod-and-test (random-number n)))


; Fast prime routine 2 (uses Miller Rabin test)
;
; If iterator reaches 0, we assume the number is checked satisfactorily
; If fermat test passes, we go and attempt a check one more time
; If fermat test fails, we know it is not a prime number
(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else false)))

; Tests
(timed-prime-test 2)
(timed-prime-test 4)
(timed-prime-test 9)
(timed-prime-test 37)
(timed-prime-test 199)
(timed-prime-test 561)

(newline)

(smallest-divisor 2)
(prime? 2)
(fast-prime? 2 2)
(fast-prime-mr? 2 2)

(newline)

(smallest-divisor 4)
(prime? 4)
(fast-prime? 4 2)
(fast-prime-mr? 4 2)

(newline)

(smallest-divisor 9)
(prime? 9)
(fast-prime? 9 3)
(fast-prime-mr? 9 3)

(newline)

(smallest-divisor 1369) ; 37
(smallest-divisor-opt 1369)
(prime? 37)

(fast-prime? 37 10)
(fast-prime-mr? 37 10)

(newline)

(prime? 199)
(fast-prime? 199 14)
(fast-prime-mr? 199 14)

(newline)

; Primality test on a Carmichaeil number
(prime? 561) ; #Not fooled
(fast-prime? 561 24) ; #Fooled! (Exercise 1.27)
(fast-prime-mr? 561 24) ; #Not fooled

(newline)

; Exercise 1.21
(smallest-divisor-opt 199)
(smallest-divisor-opt 1999)
(smallest-divisor-opt 19999)
