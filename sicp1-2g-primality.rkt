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

;; 1. Introduction.
;; ================

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

(define (mod-square a n)
  (modulo (* a a) n))

(define (non-trivial-root? a n)
  (and (not (= (modulo a n) 1))
       (not (= (modulo a n) (- n 1)))
       (= (mod-square a n) 1)))

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

;; 2. The Miller-Rabin Sequence.
;; =============================

;; Suppose n is an odd prime. By Fermat's little theorem,
;; it must be that
;;
;;    a^n = a mod n
;;
;; for all residues 1 <= a < n-1. We can also write this as
;;
;;    a^(n-1) = 1 mod n, for all 1<=a<n-1
;;
;; Because n-1 is an even number (because we assumed that
;; n is an odd prime), we can factor n-1 an odd part k
;; and powers of 2:
;;
;;   n-1 = k*2^e
;;
;; for some k, some e. Now by Fermat's theorem, it must be that
;;
;;   a^(k*2^e) = 1
;;
;; mod n of course. But this means that
;;
;;   (... ( ( a^k )^2 )^2 ...)^2 = 1.
;;
;; So if you think about this a bit, there MUST BE a square root of 1
;; somewhere in this sequence:
;;
;;   a^k, (a^k)^2, ((a^k)^2)^2, ... (a^k)^(2^(e-1))
;;
;; because how else are you going to get a 1 on the right-hand side
;; of Fermat's theorem when all the terms of this sequence are
;; successive squares? 
;;
;; If n is prime then the roots in this sequence must be trivial.
;; And if we discover a nontrivial root in this sequence... well
;; that must mean that our n is not prime.
;;
;; This is called the Miller-Rabin Sequence.

;; Display n-1 mod n as -1.
(define (prettify-residue a n)
    (if (= a (- n 1)) -1 a))

(define (fast-mod-expt a q n)
  (let loop ((a a) (q q) (result 1))
    (cond ((= q 0)
           result)
          ((odd? q)
           (loop a (- q 1) (modulo (* a result) n)))
          (else
           (loop (mod-square a n) (/ q 2) result)))))

(define (miller-rabin-sequence a n)  
  (let loop-over-powers-of-2 ((e 0) (k (- n 1)))
    (if (odd? k)
        (let ((a^k (fast-mod-expt a k n)))
          (let loop-MR-sequence ((x a^k) (j 0) (result empty))
            (if (= j e)
                (reverse result)
                (loop-MR-sequence (mod-square x n)
                                  (+ j 1)
                                  (cons (prettify-residue x n) result)))))
        (loop-over-powers-of-2 (+ e 1) (/ k 2)))))

;; Some examples of Miller-Rabin sequences.
;;
;; (miller-rabin-sequence 17 641)
;; ==> '(42 482 282 40 318 487 -1)
;; 
;; (miller-rabin-sequence 92 281317)
;; ==> '(1 1)
;;
;; (miller-rabin-sequence 29 5881)
;; ==> '(-1 1 1)
;;
;; 641, 281317 and 5881 are prime and in every case the sequence
;; has to contain a non-trivial root (and it does).

;; 3. The Miller-Rabin Test.
;; =============================

;; If n is prime then a^(n-1) = 1 mod n. Or,
;;
;;    a^(n-1) - 1 = 0 mod n.
;;
;; Or, using the factoring: n-1 = k*2^e where k is odd,
;;
;;    a^(k*2^e) - 1 = 0.
;;
;; Or, again,
;;
;;    (a^k)^(2^e) - 1 = 0 mod n
;;
;; and now it's clear that this can be repeatedly factored
;; as differences of squares. If we do it carefully, we get:
;;
;; (a^k-1)*(a^k+1)*(a^(k*2)+1)*(a^(k*2^2)+1)*...*(a^(k*2^(e-1))+1) = 0
;;
;; Since this is mod n, one of the factors on the left-hand
;; side must be divisible by n. So it must be that...
;;
;;    either           a^k - 1 = 0
;;    or               a^k + 1 = 0
;;    or           a^(k*2) + 1 = 0
;;    or                      ...
;;    or     a^(k*2^(e-1)) + 1 = 0  (all mod n)
;;
;; In other words,
;;
;;    if n is prime then
;;    either            a^k =  1
;;    or                a^k = -1
;;    or            (a^k)^2 = -1
;;    or        (a^k)^(2^2) = -1
;;    or                   ...
;;    or     (a^k)^(2^(e-1) = -1  (all mod n)
;;
;; In other words again,
;;
;;    if n is prime then the either the first term
;;    of the Miller-Rabin sequence is 1 or any term
;;    of the Miller-Rabin sequence is -1.
;;
;; The logical negation of this is:
;;
;;   if the first term of the Miller-Rabin sequence
;;   is not 1, and none of the terms of the Miller-Rabin
;;   sequence are -1 (mod n), then n IS COMPOSITE.
;;
;; If the choice of 'a' gives a sequence that passes
;; this condition, then we can call 'a' a "composite witness".
;; It proves that n is composite. 

(define (miller-rabin-composite-witness? sequence)
  (and (not (= (car sequence) 1))
       (andmap (lambda (u)
                 (not (= u -1)))
               sequence)))

;; Now let's put together a table for a given odd n. We will
;; examine each nontrivial 'a'. Is it a composite witness?
;; Does the Miller-Rabin sequence have a non-trivial square
;; root of 1? And while we are at it we compute the fermat
;; value a^(n-1) which is just the square of the last term
;; of the Miller-Rabin sequence.

(define (has-nontrivial-root? sequence n)
  (let loop ((seq sequence))
    (cond ((null? seq)
           #f)
          ((non-trivial-root? (car seq) n)
           (car seq))
          (else
           (loop (cdr seq))))))

(define (fermat-value sequence n)
  (mod-square (car (reverse sequence)) n))

(define (MR-table n)
  (displayln
   (format-table/default
    #:header (list "a" "MR sequence" "witness?" "has NT root?" "Fermat value")
    (for/list ((a (range 2 (- n 1))))
      (let ((seq (miller-rabin-sequence a n)))
        (list a
              seq
              (miller-rabin-composite-witness? seq)
              (has-nontrivial-root? seq n)
              (fermat-value seq n)))))))

;; Consider odd n = 21.
;;
;; (MR-table 21)
;;
;;  a   MR sequence   witness?   has NT root?   Fermat value
;; ---------------------------------------------------------
;;  2 |     (11 16) |     True |        False |            4
;;  3 |     (12 18) |     True |        False |            9
;;  4 |      (16 4) |     True |        False |           16
;;  5 |     (17 16) |     True |        False |            4
;;  6 |      (6 15) |     True |        False |           15
;;  7 |       (7 7) |     True |        False |            7
;;  8 |       (8 1) |     True |            8 |            1
;;  9 |      (18 9) |     True |        False |           18
;; 10 |      (19 4) |     True |        False |           16
;; 11 |       (2 4) |     True |        False |           16
;; 12 |       (3 9) |     True |        False |           18
;; 13 |      (13 1) |     True |           13 |            1
;; 14 |      (14 7) |     True |        False |            7
;; 15 |     (15 15) |     True |        False |           15
;; 16 |      (4 16) |     True |        False |            4
;; 17 |       (5 4) |     True |        False |           16
;; 18 |      (9 18) |     True |        False |            9
;; 19 |     (10 16) |     True |        False |            4
;;
;; All of the 'a' in the non-trivial range are witnesses
;; that prove n is composite. Yet only two of them, 8 and 13,
;; have non-trivial roots in their Miller-Rabin sequences.
;;
;; Let's look at an n=65. It has a few residues which are
;; not composite witnesses.
;;
;;  (MR-table 65)
;;
;;  a           MR sequence   witness?   has NT root?   Fermat value
;; -----------------------------------------------------------------
;;  2 |   (2 4 16 61 16 61) |     True |        False |           16
;;  3 |   (3 9 16 61 16 61) |     True |        False |           16
;;  4 |  (4 16 61 16 61 16) |     True |        False |           61
;;  5 |  (5 25 40 40 40 40) |     True |        False |           40
;;  6 |  (6 36 61 16 61 16) |     True |        False |           61
;;  7 |  (7 49 61 16 61 16) |     True |        False |           61
;;  8 |      (8 -1 1 1 1 1) |    False |        False |            1
;;  9 |  (9 16 61 16 61 16) |     True |        False |           61
;; ...                  ...        ...            ...            ...
;;
;; The table is big so I cut most of it out. 8, 18, 47 and 57
;; are not composite witnesses. The others from 2 to 63 are.
;; Only two choices for 'a'  reveal non-trivial roots of 1
;; in their associated Miller-Rabin sequences.
;;
;; The following is not too hard to prove:
;;
;;    If n is an odd composite then more than 50% of
;;    the residues a in the non-trivial range 2 <= a <= n-2 
;;    are Miller-Rabin composite witnesses.
;;
;; And that is what SICP means when they say the Miller-Rabin
;; test "can't be fooled". 
;;
;; But can it be that the direct-recursive expmod function
;; somehow finds those few non-trivial roots for more than
;; half the a's? No, because as we shall see by tracing
;; expmod, the recursive steps in expmod are equivalent
;; to computing the Miller-Rabin sequence.

;; 4. Recursion Magic of the expmod Function.
;; ==========================================

;; Goind back to the fast exponentiation mod m algorithm we did
;; in earlier exercises. The algorithm is based on the
;; following recursion relation:
;;
;;    F(a,q) mod m = (F(a, q/2))^2  if q is even
;;                 = a*F(a, q-1)    if q is odd
;;
;; with initial condition:
;;
;;    F(a,0) = 1. 
;;
;; This is a classic and beautiful example of recursion.
;; The problem of computing F(a,q) is decomposed into
;; computing smaller problems F(a,q/2) and F(a,q-1) of
;; the same form. Finally the recursion reaches the initial
;; condition F(a,0), which is really the only think that
;; the process needs to know how to compute as far as F is
;; concerned. Let's expand the recursive calls for F(a,n-1)
;; which is the computation of a^(n-1) mod n...
;;
;; Let n-1 =  k*2^e as before...
;;
;;    F(a, k*2^e) = ( F(a, k*2^(e-1)) )^2            
;;                = ( (F(a, k*2^(e-2)))^2 )^2        
;;                = ( ((F(a, k*2^(e-3)))^2)^2 )^2
;;
;;                ...
;;
;;                = ( (...((F(a, k))^2)^2...)^2 )^2
;;
;;                ...
;;
;; Now the recursion goes off on a tangent computing F(a,k).
;; When it comes back with the value of F(a,k), it will compute
;;
;;                = ( (...((F(a, k))^2)^2...)^2 )^2        (**)
;;
;; Imagine if we had a square function square(x) = (x)^2 that
;; prints out the value of its argument x before it evaluates x^2.
;; If we used this square function on the above expression (**),
;; the following sequence would be printed out...
;;
;;    F(a,k), F(a,k)^2, (F(a,k)^2)^2, ... (...((F(a,k)^2)^2 ...)^2
;;
;; which is
;;
;;    a^k,   a^(k*2),    a^(k*2^2), ...   a^(k*2^(e-1))
;;
;; which is the Miller-Rabin sequence! Amazingly, the Miller-Rabin
;; sequence is hidden in the recursive steps of expmod!
;;
;; Let's modify exmpod to employ a square(x) function that pushes
;; it's argument on a list called 'sequence'. We use multiple-values
;; to return the expmod computation and the sequence of arguments
;; that appeared in square(x).

(define (expmod-traced base exp m)
  (define sequence empty)
  (define (square x)
    (set! sequence (cons (prettify-residue x m) sequence))
    (remainder (* x x) m))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square (expmod base (/ exp 2) m)) m))
          (else 
           (remainder (* base (expmod base (- exp 1) m)) m))))
  (values (expmod base exp m)
          (reverse sequence)))

;; Use that to compute the Fermat value a^(n-1) mod n...

(define (expmod-sequence a n)
  (expmod-traced a (- n 1) n))

;; Let's try this and compare with known Miller-Rabin sequences.
;;
;; n=641, 17^640 mod 641...
;; 
;;    (miller-rabin-sequence 17 641)
;;    ==> '(42 482 282 40 318 487 -1)
;;
;;    (expmod-sequence 17 641)
;;    ==> 1
;;        '(17 289 42 482 282 40 318 487 -1)
;;
;; n=5881, 29^5880 mod 5881...
;;
;;    (miller-rabin-sequence 29 5881)
;;    ==> '(-1 1 1)
;;
;;    (expmod-sequence 9 5881)
;;    ==> 1
;;        '(9 81 239 2442 30 2219 2314 2450 5515 -1 1 1)
;;
;; The Miller-Rabin sequences are there, but at the tail of
;; the expmod lists. What are those other numbers? Recall that
;; expmod goes off into a tangent computing a^k before it
;; can finish off the Miller-Rabin part. That computational
;; tangent involves more square roots, hence the extra numbers
;; at the beginning.
;;
;; So there *is* a difference between the Miller-Rabin sequence
;; and SICP's recursive expmod sequence: the expmod sequence contains
;; some extra terms.
;;
;; Looking at the table for n=21 above, we see that only two of the
;; Miller-Rabin sequences have non-trivial roots. Thus we expect
;; the same for the expmod sequences: only two will show nontrivial
;; roots for n=21, no matter what 'a'. Ah, you may object: but
;; maybe there are nontrivial roots in the OTHER terms of the
;; expmod sequence, the ones that come BEFORE the Miller-Rabin terms.

(define (MR-expmod-table n)
  (displayln
   (format-table/default
    #:header (list "a" "MR seq" "expmod seq" "expmod has NT root?")
    (for/list ((a (range 2 (- n 1))))
      (let-values (((_ seq-expmod) (expmod-sequence a n)))
        (let ((seq-mr (miller-rabin-sequence a n)))
          (list a
                seq-mr
                seq-expmod
                (has-nontrivial-root? seq-expmod n))))))))

;; Finds all square roots of b mod m.
(define (square-roots-of b m)
  (let loop ((a 1) (result empty))
    (cond ((= a m)
           (reverse result))
          ((= (modulo (* a a) m) b)
           (loop (+ a 1) (cons a result)))
          (else
           (loop (+ a 1) result)))))

;; The function 'square-roots-of' is a useful tool for finding
;; interesting examples. Take n=561, a Charmichael number. The
;; square roots of 1 mod 561 are..
;;
;; (square-roots-of 1 561)
;; ==> '(1 67 188 254 307 373 494 560)
;;
;; The square roots of 67 are...
;;
;; (square-roots-of 67 561)
;; ==> '(89 98 166 208 353 395 463 472)
;;
;; And the square roots of 463 are...
;;
;; (square-roots-of 463 561)
;; ==> '(32 100 155 274 287 406 461 529)
;;
;; So we have, say, 100^2 = 463 and 463^2 = 67 and 67^2 = 1 mod 561.
;;
;; Let's compare the Miller-Rabin sequence with the expmod
;; sequence for a=100, n=561.
;;
;; (miller-rabin-sequence 100 561)
;; ==> '(298 166 67 1)
;;
;; (expmod-sequence 100 561)
;; ==> 1
;;     '(100 463 67 1 100 298 166 67 1)
;;
;; Ah! There is a nontrivial root of 1 in the
;; beginning part of the expmod sequence. Let's make
;; a table comparing sequences for every 'a' mod 21.
;;
;; (MR-expmod-table 21)
;;
;; a    MR seq      expmod seq   expmod has NT root?
;; --------------------------------------------------
;;  2 | (11 16) |   (2 4 11 16) |               False
;;  3 | (12 18) |   (3 9 12 18) |               False
;;  4 |  (16 4) |   (4 16 16 4) |               False
;;  5 | (17 16) |   (5 4 17 16) |               False
;;  6 |  (6 15) |   (6 15 6 15) |               False
;;  7 |   (7 7) |     (7 7 7 7) |               False
;;  8 |   (8 1) |     (8 1 8 1) |                   8
;;  9 |  (18 9) |   (9 18 18 9) |               False
;; 10 |  (19 4) |  (10 16 19 4) |               False
;; 11 |   (2 4) |   (11 16 2 4) |               False
;; 12 |   (3 9) |   (12 18 3 9) |               False
;; 13 |  (13 1) |   (13 1 13 1) |                  13
;; 14 |  (14 7) |   (14 7 14 7) |               False
;; 15 | (15 15) | (15 15 15 15) |               False
;; 16 |  (4 16) |   (16 4 4 16) |               False
;; 17 |   (5 4) |   (17 16 5 4) |               False
;; 18 |  (9 18) |   (18 9 9 18) |               False
;; 19 | (10 16) |  (19 4 10 16) |               False
;;
;; Only two expmod sequences have nontrivial roots. We don't
;; see 50% of expmod sequences revealing non-trivial roots.
;;
;; Exercise:
;;
;; Don't make the dumb mistake (like I did) of using a
;; tail-recursive version of fast-exponentiation instead
;; of direct recursion expmod like SICP says to use.
;; Why not? What is the sequence generated by the arguments
;; to square(x) in the tail-recursive version? It's illuminating
;; to see this difference between the direct recursion
;; formulation of a computation and the tail-recursive
;; formulation. The tail-recursive one is more efficient,
;; but doesn't have the magic.

;; 5. Equivalence of Miller-Rabin and SICP idea.
;; =============================================

;; SICP pg.74 continues:
;;
;;    Modify the expmod procedure to signal if it
;;    discovers a nontrivial square root of 1, and
;;    use this to implement the Miller-Rabin test with
;;    a procedure analogous to fermat-test .
;;
;; The question that immediately troubles us is this:
;; why should this be equivalent to the Miller-Rabin test?
;; It's not obvious at all. Yes, we have shown that the
;; expmod sequence contains the Miller-Rabin sequence,
;; but we still have a bit more work to do to demonstrate
;; the equivalence of the SICP idea and the Miller-Rabin idea.
;;
;; We will show this:
;;
;;    The Fermat test combined with the check for
;;    non-trivial square roots of 1 in the expmod
;;    sequence, is equivalent (almost!) to the Miller-
;;    Rabin test.








;; if fermat condition is not met, you wont see any roots
;; in the sequence, trivial or nontrivial!





;;  (* * * * * * * ==> 1)
;;  all stars not 1,-1
;;  composite by miller-rabin
;;  last star is a root of 1, by SICP algo, therefore composite.

;;  (* * * * * * * ==> *)
;;  all stars not 1,-1
;;  composite by miller-rabin.
;;  fails fermat test, by SICP algo, therefore composite.





;; Exercise 1.28 ========================================

;; (define (sicp-miller-rabin-test a n)
;;   (define (expmod base exp m)
;;     (cond ((= exp 0) 1)
;;           ((even? exp)


;;            (remainder (square (expmod base (/ exp 2) m)) m))
;;           (else 
;;            (remainder (* base (expmod base (- exp 1) m)) m))))
;;   (= (expmod a (- n 1) n) 1))





