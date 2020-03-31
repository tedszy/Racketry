;;; sicp1-2f-primality.rkt
;;;
;;; Exercise 1.28.
;;;
;;; A study of the Miller-Rabin prime test, and
;;; a study of the errors concerning it in SICP.

#lang racket

(require rackunit
         math/base
         "format-table.rkt")

;; SICP pg.74 says...

;;    "...we pick a random number a < n and raise a to the 
;;    (n−1)-st power modulo n using the expmod procedure. 
;;    However, whenever we perform the squaring step in expmod, 
;;    we check to see if we have discovered a “nontrivial square 
;;    root of 1 modulo n,” that is, a number not equal to 1 or n−1 
;;    whose square is equal to 1 modulo n. It is possible to prove 
;;    that if such a nontrivial square root of 1 exists, 
;;    then n is not prime. It is also possible to prove that if 
;;    n is an odd number that is not prime, then, for at least
;;    half the numbers a < n, computing a n −1 in this way will
;;    reveal a nontrivial square root of 1 modulo n."
;;
;; We will show that this is not true. We will also explain why
;; the algorithm in SICP SEEMS to work and thus why this incorrect
;; Miller-Rabin formulation evades detection by students working
;; through SICP.









;; Exercise 1.28 ========================================






