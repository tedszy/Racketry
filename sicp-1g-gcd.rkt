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







;; Exercise 1.20 ========================================




