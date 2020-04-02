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
;; no matter what residue 'a' is chosen. Let's find all the
;; non-trivial roots of 1 for some interesting odd numbers
;; and arrange the results in a table.

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
;;
;; Ah, notice that the power of a prime, 7^6, does not hae
;; any non-trivial roots! So what SICP said can't possibly
;; be true: there exist numbers for which no computations
;; with any residue 'a' can reveal a non-trivial root... for
;; the simple fact that such numbers have none to reveal!
;;
;; It is easy to show that a prime has no non-trivial roots.
;; If x is a root of 1 mod p then
;;
;;    x^2 = 1 mod p, or... x^2 - 1 = 0 mod p.
;;
;; Factoring this,
;;
;;    (x + 1)*(x - 1) = 0 mod p.
;;
;; This means that p divides the left-hand side. But since p
;; is prime, it must divide one of the two factors: either
;; p | (x + 1) or p | (x - 1). (Exercise: explain why it can't
;; be both!) So,
;;
;;   x + 1 = 0 mod p, or x - 1 = 0 mod p.
;;
;; and either x = 1 or x = -1 mod p. There are no non-trivial
;; roots.
;;
;; It can be shown that if n is a power of an odd prime then it
;; also has no non-trivial roots. (Likewise if n is two times
;; a power of an odd prime -- but that's even.) So there do exist
;; odd numbers which are composite, yet have no non-trivial roots.
;; The Miller-Rabin test isn't fooled by them (we shall see why),
;; so what SICP says about non-trivial roots can't be true.
;;
;; So then, if n is prime, n will have no non-trivial roots.
;; the logical negation of this is...
;;
;;    if n has a non-trivial root of 1, then n is composite.
;;
;; A-ha! This gives us an idea for a new kind of prime test!
;; All we have to do is rummage through residues 'a' in the
;; non-trivial range 2 < a < n-1, looking for square roots of 1.
;; And if we find one, we have proved that n is composite!
;;
;; But this idea doesn't work. Look at the table above.
;; More experiments with odd composites show that the typical
;; odd number has very few non-trivial roots. It's not practical
;; to do many random trials hoping to run into one. For very
;; large numbers it would be computationally impossible.
;;
;; However, the general idea would be a good one, if there exists
;; some mathematical way of predicting where these roots should be.
;; This concept is the basis of the Euler prime test and the
;; Miller-Rabin prime test.










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
            (if (= j e)
                (reverse (cons x (cons '==> result)))
                (loop2 (mod-square x n)
                       (+ j 1)
                       (cons (if (= x (- n 1))
                                 -1
                                 x)
                             result)))))
        (loop (+ e 1) (/ k 2)))))

(define (expmod-traced base exp m)
  (define sequence empty)
  (define (expmod base exp m)
    (define (square x)
      (set! sequence (cons (if (= x (- m 1)) -1 x) sequence))
      (remainder (* x x) m))
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m)) m))
          (else 
           (remainder (* base (expmod base (sub1 exp) m)) m))))
  (reverse (cons (expmod base exp m)
                 (cons '==> sequence))))

(define (run a n)
  (printf "~a\n" (expmod-traced a (- n 1) n))
  (printf "~a\n" (miller-rabin-sequence a n)))

(run 29 5881) ;; 5881 is prime.
(run 29 46657) ;; 11 also

;; Strictly speaking, 
;; Miller Rabin sequence always looks like
;; (* * * * * ... ==> 1)

;; The check for non-trivial roots combined with
;; Fermat test check is equivalent to Miller-Rabin test.

;;  (* * * * * * * ==> 1)
;;  all stars not 1,-1
;;  composite by miller-rabin
;;  last star is a root of 1, by SICP algo, therefore composite.

;;  (* * * * * * * ==> *)
;;  all stars not 1,-1
;;  composite by miller-rabin.
;;  fails fermat test, by SICP algo, therefore composite.





;; Exercise 1.28 ========================================






