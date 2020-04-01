;;; sicp1-2f-primality.rkt
;;;
;;; Exercise 1.28.
;;;
;;; A study of the Miller-Rabin prime test, and
;;; of the errors concerning it in SICP.

#lang racket

(require rackunit
         math/base
         "format-table.rkt")

;; SICP pg.74 says...
;;
;;    "...we pick a random number a < n and raise a to the 
;;    (n−1)-st power modulo n using the expmod procedure. 
;;    However, whenever we perform the squaring step in expmod, 
;;    we check to see if we have discovered a “nontrivial square 
;;    root of 1 modulo n,” that is, a number not equal to 1 or n−1 
;;    whose square is equal to 1 modulo n. It is possible to prove 
;;    that if such a nontrivial square root of 1 exists, 
;;    then n is not prime. It is also possible to prove that if 
;;    n is an odd number that is not prime, then, for at least
;;    half the numbers a < n, computing a^(n−1) in this way will
;;    reveal a nontrivial square root of 1 modulo n."
;;
;; This is not true. What SICP meant to say is...
;;
;;    'if n is an odd number that is not prime, then,
;;     for at least half the numbers a < n, computing a^(n-1)
;;     this way (direct-recursion expmod with non-trivial square
;;     root of 1 check) will reveal that n is composite.'
;;
;; Sometimes this process of demonstrating that n is composite
;; uncovers a nontrivial square root, sometimes it does not!
;;
;; Let's see why sometimes it does not reveal a root of 1,
;; no matter what residue 'a' is chosen.

(define (non-trivial-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (modulo (* a a) n) 1)))

(define (non-trivial-root-count n)
  (let loop ((count 0) (a 2))
    (if (= a (- n 1))
        count
        (if (non-trivial-root? a n)
            (loop (+ count 1)
                  (+ a 1))
            (loop count
                  (+ a 1))))))

(define (non-trivial-roots-table)
  (define (make-row type number)
    (list type number (non-trivial-root-count number)))
  (displayln
   (format-table/default
    #:header '("type" "n" "# non-trivial roots")
    (list (make-row "prime" 641)
          (make-row "power of prime" (* 7 7 7 7 7 7))
          (make-row "product of distinct primes" (* 3 7 5 11 13 23))
          (make-row "charmichael" 6601)))))

;; > (non-trivial-roots-table)
;;
;;                       type        n   # non-trivial roots
;; ---------------------------------------------------------
;;                      prime |    641 |                   0
;;             power of prime | 117649 |                   0
;; product of distinct primes | 345345 |                  62
;;                charmichael |   6601 |                   6





;; Use modulo rather than remainder.

;; Does a^2 mod n.
(define (mod-square a n)
  (modulo (* a a) n))

(define (fast-mod-expt a q n)
  (define (mod-square x) (modulo (* x x) n))
  (let loop ((a a) (q q) (result 1))
    (cond ((= q 0) result)
          ((odd? q) (loop a
                           (- q 1)
                           (modulo (* a result) n)))
          (else (loop (mod-square a)
                      (/ q 2)
                      result)))))

;; Factor n-1 = 2^e * k where k is odd.
;; Compute a^k mod n by fast exponentiation.
;; Miller-rabin sequence is a^k, (a^k)^2, (a^k)^4, ... mod n.
(define (miller-rabin-sequence a n)
  (let loop ((e 0) (k (- n 1)))
    (if (odd? k)
        (let ((a^k (fast-mod-expt a k n)))
          (let loop2 ((x a^k) (j 0) (result empty))
            (if (> j e)
                (reverse result)
                (loop2 (mod-square x n)
                       (+ j 1)
                       (cons (if (= x (- n 1))
                                 -1
                                 x)
                             result)))))
        (loop (+ e 1) (/ k 2)))))


(define (expmod base exp m)
  (define (square x)
    (displayln x)
    (remainder (* x x) m))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
         (remainder (* base (expmod base (sub1 exp) m)) m))))

(define (run a n)
  (printf "~a*\n" (expmod a (- n 1) n))
  (miller-rabin-sequence a n))








;; Exercise 1.28 ========================================






