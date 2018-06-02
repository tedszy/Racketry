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
;; Lame theorem 
;; 
;;   b_n >= F(n)
;;
;; is proved by induction and by application 
;; of the above inequality.
;;
;; Verify that the base case for k=1 is true.
;; 
;;   b >= F(1) = 1. 
;;
;; Which is true, since we assume b >= 1.
;;
;; Assume strong induction hypotheses, that 
;;
;;   b_j >= F(j)
;;
;; is true for all values j <= k. We now show
;; this implies that the case of k+1 is also true.
;;
;; By the strong induction hypotheses, 
;;
;;   b_(k-1) >= F(k-1) 
;;   b_k     >= F(k).
;;
;; Add them:
;;
;;   b_k + b_(k-1) >= F(k) + F(k-1)
;;                 >= F(k+1).
;;
;; But by the inequality we proved earlier, 
;;
;;   b_(k+1) >= F(k+1).
;;
;; QED. Ta-da!



;; Exercise 1.20 ========================================

;; Analyze (gcd2 206 40) from the point of veiw of
;; normal-order evaluation.
;;
;; First let's see the Euclidean algorithm table.
;;
;;  m  q   d  r
;; -------------
;; 206  5  40  6
;;  40  6   6  4
;;   6  1   4  2
;;   4  2   2  0
;;
(define (gcd2 a b)
  (if (= b 0)
      a 
      (gcd2 b (remainder a b))))

;; First of all, b has to be evaluated in order for the 
;; recursion to ever end. And if b is evaluated then the 
;; value of b is known in the expression
;;
;;   (gcd2 b (remainder a b)).
;;
;; (gcd2 206 40)
;;
;; if (= 40 0) => #f
;;              => (gcd2 40 (remainder 206 40))
;; if (= (remainder 206 40) 0) => this must be evaluated 
;;                                 so (remainder 206 40) is known!
;; if (= 6 0)               => (gcd2 6 (remainder 6 4))
;; if (= (remainder 6 4) 0) => 
;; if (= 2 0)               => (gcd2 4 (remainder 4 2))
;; if (= (remainder 4 2) 0) => this has to get evaluated!
;; if (= 0 0) => a, which is now 2.
;; 2.
;;
;; How about applicative-order evaluation? It's similar,
;; but the remainder function calls are evaluated before
;; they are passed to the if-expression.
;;
;; (gcd2 206 40)
;; if (= 40 0) => (gcd2 40 (rem 206 40)) => (gcd2 40 6)
;; if (= 6 0)  => (gcd2 6 (rem 40 6))    => (gcd2 6 4)
;; if (= 4 0)  => (gcd2 4 (rem 6 4))     => (gcd2 4 2)
;; if (= 2 0)  => (gcd2 2 (rem 4 2))     => (gcd2 2 0)
;; if (= 0 0)  => 2
;;
;; I've seen solutions to this problem where the normal-order
;; evaluation ends up to be some long sequence of nested calls
;; to remainder function. I dispute this though: the value
;; of 'b' has to always be known before the recusive call to
;; gcd2. The function call to remainder is deferred, but it
;; does not build up into a long chain of nested calls. So my 
;; answer to 1.20 differs from what you may find elsewhere.

