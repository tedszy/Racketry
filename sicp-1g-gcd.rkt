;;; sicp-1g-gcd.rkt

#lang racket

(require "check.rkt"
         "simple-table.rkt")

;; Euclidean algorithm. m = q*d + r.
;; m = dividend, q = quotient,
;; d = divisor, r = remainder.
(define (gcd1 a b)
  (define (stringify a b)
    (map number->string (list a (quotient a b) b (remainder a b))))
  (let loop ((a a) 
             (b b) 
             (tt '(("m" "q" "d" "r"))))
    (if (= b 0)
        (begin (print-table (reverse tt) #:head #t)
               a)
        (loop b 
              (remainder a b) 
              (cons (stringify a b) tt)))))

;; > (gcd1 6555 350)
;;    m   q    d    r
;; ------------------
;; 6555  18  350  255
;;  350   1  255   95
;;  255   2   95   65
;;   95   1   65   30
;;   65   2   30    5
;;   30   6    5    0
;; 5

;; Lame's theorem.
;; Take n = min(a, b)
;; Assume the Euclidean algorithm for gcd(a,b) 
;; takes k steps. Then we have,
;;
;;   n >= F(k)
;;
;; But since F(k) -> (p^k)/sqrt(5), then 
;;
;;   n >= p^k * 1/sqrt(5)
;; 
;; so log(n) >= k * some constant.
;; Therefore gcd steps, k, grows like Theta(log(n)).
;;
;; Proof of Lame's theorem.
;;
;; Consider this table and label the sequence
;; (a_(k+1_,b_(k+1)) -> (a_k,b_k) -> (a_(k-1), b_(k-1))
;;
;; (gcd1 2998 191)
;;
;;   m           q    d            r
;; ---------------------------------
;; 2998 a_(k+1) 15  191 b_(k+1)  133
;;  191 a_k      1  133 b_k       58
;;  133 a_(k-1)  2   58 b_(k-1)   17
;;   58          3   17            7
;;   17          2    7            3
;;    7          2    3            1
;;    3          3    1            0
;; 
;; Notice that
;;
;;   191     >= 133 + 58
;;   b_(k+1) >= b_k + b_(k-1).
;;
;; Go down the list of divisors and you can see 
;; that this inequality seems to be always true.
;; So let's prove that it is always true.
;;
;; From the way the GCD algorithm table is organized 
;; we see that 
;;
;;   (1) a_(k-1) = b_k and a_k = b_(k+1).
;;
;;   (2) b_(k-1) = remainder(a_k, b_k).
;;
;; From Euclidean division step k.
;;
;;   a_k     = q*b_k + b_(k-1) >= b_k + b_(k-1).
;;   b_(k+1)                   >= b_k + b_(k-).
;;
;; And that's the inequality we wanted. Now, the 
;; Lame theorem is proved by induction and by
;; application of the above inequality.
;;


;; Verify that the base case for k=1 is true.





;; Exercise 1.20 ========================================




