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
;;    half the numbers a < n, computing a n−1 in this way will
;;    reveal a nontrivial square root of 1 modulo n."
;;
;; We will show that this is not true.





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

;; 1000008 has 62 nontrivial roots of 1.

(define (non-trivial-roots-table)
  (displayln
   (format-table/default
    #:header '("type" "n" "# non-trivial roots")
    (list (list "prime" 641 (non-trivial-root-count 641))
          (list "power of prime"
                (* 11 11 11)
                (non-trivial-root-count (* 11 11 11)))
          (list "2x power of prime"
                (* 2 7 7 7 7)
                (non-trivial-root-count (* 2 7 7 7 7)))
          (list "product of distinct primes"
                (* 11 13 23)
                (non-trivial-root-count (* 11 13 23)))
          (list "factorial"
                (* 2 3 4 5 6 7 8)
                (non-trivial-root-count (* 2 3 4 5 6 7 8)))))))

;; > (non-trivial-roots-table)
;;
;;                       type       n   # non-trivial roots
;; --------------------------------------------------------
;;                      prime |   641 |                   0
;;             power of prime |  1331 |                   0
;;          2x power of prime |  4802 |                   0
;; product of distinct primes |  3289 |                   6
;;                  factorial | 40320 |                  30




;; Use modulo rather than remainder.

;; Does a^2 mod n.
(define (mod-square a n)
  (modulo (* a a) n))

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
    (let ((foo (remainder (* x x) m)))
      (displayln foo)
      foo))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
         (remainder (* base (expmod base (sub1 exp) m)) m))))










;; Exercise 1.28 ========================================






