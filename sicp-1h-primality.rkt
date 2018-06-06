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




;; Exercise 1.21 ========================================

;; Exercise 1.22 ========================================

;; Exercise 1.23 ========================================

;; Exercise 1.24 ========================================

;; Exercise 1.25 ========================================

;; Exercise 1.26 ========================================

;; Exercise 1.27 ========================================

;; Exercise 1.28 ========================================


