;;; sicp-1h-primality.rkt

#lang racket

(require rackunit
         "simple-table.rkt")

;; Theta(sqrt(n)) order of growth primality test. 
;;
;; Find the smallest divisor >= 2 of n.
;; We try divisors d up to d^2 <= n. 
;;
;; If remainder of n/d is 0, then answer is d.
;; If no divisor such that d^2 <= n is found then
;; n itself is the only such divisor. 
;; And n is therefore prime. 
;;
;; Why? Because if d|n then n/d also | d. It's not
;; possible that both are larger than sqrt(n) because
;; if so then d * d/n would be > n. So one of these
;; divisors must be smaller than sqrt(n). If there isn't
;; one below sqrt(n) then there isn't one bigger than
;; sqrt(n) either.
(define (get-smallest-divisor n)
  (let loop ((d 2))
    (cond ((> (* d d) n)
           n)
          ((= (remainder n d) 0)
           d)
          (else 
           (loop (add1 d))))))

(define (prime? n)
  (= n (get-smallest-divisor n)))

;; Theta(log(n) primality test.
;;
;; This is a probabilistic test based on
;; Fermat's little theorem:
;;
;;    If q is prime then a^q = a mod q.
;;
;; Converse of this is: 
;;
;;    If a^q not= a mod q, then q is not prime.
;;
;; So we choose many a's from 1 to q-1 and try 
;; this, if q passes the test the confidence that
;; q is prime increases.

;; We need a fast mod-exponential to evaluate a^q mod q.
;; Let's write one that's different from the one
;; given in SICP.
;;
;; Computes a^q mod q.
(define (mod-expt a qq)
  ;; Note the difference between qq and q. You don't
  ;; want to take modulus to be q since that is a 
  ;; state variable that gets updated in this tail
  ;; recursive implimentation. The modulus is the
  ;; initial power, qq, not the updated powers q.
  (define (mod-square x) (remainder (* x x) qq))
  (let loop ((a a) (q qq) (result 1))
    (cond ((= q 0)
           result)
          ((odd? q)
           (loop a
                 (sub1 q)
                 (remainder (* a result) qq)))
          (else
           (loop (mod-square a)
                 (/ q 2)
                 result)))))

;; Compare with SICP version.
(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
         (remainder (* base (expmod base (sub1 exp) m)) m))))
 
;; Some tests.
(check-equal? (mod-expt 119 19) (expmod 119 19 19))
(check-equal? (mod-expt 310 18) (expmod 310 18 18))
(check-equal? (mod-expt 640 29) (expmod 640 29 29))
(check-equal? (mod-expt 110 100) (expmod 110 100 100))

;; Choose a random base a and do the test a^q = a mod q.
(define (fermat-test q)
  (let ((a (add1 (random (sub1 q)))))
    (= (mod-expt a q) a)))

;; Do the FLT test a certain number of times. If q passes
;; all these tests it is probably a prime, but if q fails
;; it is for sure not prime.
(define (flt-prime? q trials)
  (cond ((= trials 0)
         true)
        ((fermat-test q)
         (flt-prime? q (sub1 trials)))
        (else
         false)))

;; Charmicheal numbers fool the FLT test.
;; If q is a Charmichael number then
;;
;;    a^q = a mod q for all a < q.
;;

;; Exercise 1.21 ========================================

;; (get-smallest-divisor 199)   ==> 199
;; (get-smallest-divisor 1999)  ==> 1999
;; (get-smallest-divisor 19999) ==> 7



;; Exercise 1.22 ========================================

;; We dont have 'runtime' in Racket but we do have 'time'
;; and 'current-milliseconds'.

(define (timed-prime? q)
  (let ((t1 (current-milliseconds)))
    (if (prime? q)
        (begin
          (displayln
           (string-join
            (list "*** prime:"
                  (number->string (- (current-milliseconds)
                                     t1))
                  "milliseconds.")))
          true)
        false)))

;; (timed-prime? 64111111111111)
;; *** prime: 113 milliseconds.
;; #t

;; Find first 3 primes above N = 1000, 10,000, 100,000, 1,000,000.
;; Test the hypothesis that this procedure is Theta(sqrt(n)).
;; Higher N times should be about sqrt(10) times longer than 
;; lower N times.
;;
;; Computers are a lot faster these days so we must use much
;; larger N to get measurable milliseconds:
;;
;;   N = 10^11, 10^12, 10^13.
;;
;; Refactor this problem and the SICP code to work with simple-table.
(define (first-three-primes-larger-than N)
  (let loop ((N (add1 N)) 
             (count 0) 
             (start-time (current-milliseconds))
             (result '()))
    (cond ((= count 3)
           (reverse result))
          ((prime? N)
           (loop (add1 N)
                 (add1 count)
                 (current-milliseconds)
                 (cons (list N (- (current-milliseconds) start-time))
                       result)))
          (else
           (loop (add1 N)
                 count
                 (current-milliseconds)
                 result)))))

;; Aggregate the results into a table.
;; 
;;   limit       primes       milliseconds
;;
(define (prime-time-table)
  (print-table #:bars true #:head true
   (cons (list "lower limit" "primes" "msecs")
         (map (lambda (N)
                (let ((row (first-three-primes-larger-than N)))
                  (list
                   (number->string N)
                   (string-join (map (lambda (u)
                                       (number->string (car u)))
                                     row)
                                ", ")
                   (number->string (apply + (map cadr row))))))
              (map (lambda (e) 
                     (expt 10 e)) 
                   (list 11 12 13))))))

;; (prime-time-table)
;;    lower limit                                           primes   msecs
;; -----------------------------------------------------------------------
;;   100000000000 |       100000000003, 100000000019, 100000000057 |    27
;;  1000000000000 |    1000000000039, 1000000000061, 1000000000063 |    88
;; 10000000000000 | 10000000000037, 10000000000051, 10000000000099 |   282
;;
;; Now, sqrt(10) ~ 3.16 and we see...
;;
;;    27 * 3.16 = 85.32
;;    88 * 3.16 = 278.0
;;
;; which are very close to the observed times!



;; Exercise 1.23 ========================================

;; Exercise 1.24 ========================================

;; Exercise 1.25 ========================================

;; Exercise 1.26 ========================================

;; Exercise 1.27 ========================================

;; Exercise 1.28 ========================================


