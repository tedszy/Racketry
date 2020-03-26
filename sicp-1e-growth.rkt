;;; sicp-1e-growth.rkt

#lang racket

(require "check.rkt"
         "format-table.rkt")

;; n = size of problem, R(n) = resources needed to do n.
;;
;; If R(n) is in between k1*f(n) and k2*f(n) then
;; R(n) has order of growth Theta(f(n)).
;;
;; Recursive factorial.
;; steps(n) ~ Theta(n)
;; space(n) ~ Theta(n)
;;
;; Iterative (tail recursive factorial).
;; steps(n) ~ Theta(n)
;; space(n) ~ Theta(1)
;;
;; Recursive Fibonacci.
;; steps ~ Theta(p^n)
;; space ~ Theta(n)



;; Exercise 1.14 ========================================

;; Make change for M=11 cents (tree diagram).
;; What are the steps and space orders of growth 
;; if M is increased?
;;
;; Need only consider dime, nickel, penny.
;; [**] = number of ways to do **.
;; [0, coins] = 1.
;; [M, null]  = 0.
;; [M<0, coins] = 0
;;
;; [11, dime, nickel, penny]
;;
;; [11, nickel, penny] + [1, dime, nickel, penny]
;;
;; [11, penny] + [6, nickel, penny] 
;;   + [1, nickel, penny] + [-9, dime, nickel, penny]  
;;
;; [11, null] + [10, penny] 
;;   + [6, penny] + [1, nickel, penny]
;;   + [1, penny] + [-4, nickel, penny]
;;   + 0
;;
;; 0 + [10, null] + [9, penny]
;;   + [6, null]  + [5, penny] 
;;   + [1, penny] + [-4, nickel, penny]
;;   + [1, null]  + [0, penny] + 0 
;;   + 0
;;
;; 0 + 0 + [9, null] + [8, penny]
;;   + 0 + [5, null] + [4, penny]
;;   + [1, null] + [0, penny] + 0
;;   + 0 + 1 + 0 
;;   + 0
;;
;; 0 + 0 + 0 + [8, null] + [7, penny]
;;   + 0 + 0 + [4, null] + [3, penny]
;;   + 0 + 1 + 0
;;   + 0 + 1 + 0
;;   + 0
;;
;; 0 + 0 + 0 + 0 + [6, null] + [5, penny]
;;   + 0 + 0 + 0 + 0 + [3, null] + [2, penny]
;;   + 0 + 1 + 0
;;   + 0 + 1 + 0
;;   + 0
;; 
;; Combine constants.
;;
;; 0 + [5, null] + [4, penny]
;;   + 0 + [2, null] + [1, penny]
;;   + 2
;;
;; 0 + [4, null] + [3, penny] 0 + [1, null] + [0, penny] + 2
;;
;; 0 + [3, null] + [2, penny] + 0 + 1 + 2
;;
;; 0 + [2, null] + [1, penny] + 3
;;
;; 0 + [1, null] + [0, penny] + 3
;;
;; 0 + 1 + 3
;;
;; 4, which is actually correct.
;;
;; Seems that steps growth is Theta(n)
;; and space growth is Theta(2^n).


 
;; Exercise 1.15 ========================================

;; Use the the identity sin
;;
;; (x) = 3*sin(x/3) - 4*(sin(x/3))^3
;;
;; and the base case sin(x) = x (!) when x is small enough.
;;
;; How does this work? A large polynomial expression
;; is formed by recursion, and it gets evaluated once
;; my-sine is small enough to be replaced with x.
;; Then the expression can be evaluated.
(define (my-sine x step table)
  (define (poly x) (- (* 3 x) (* 4 (* x x x))))
  (if (<= (abs x) 0.1)
      (begin (displayln (format-table/simple #:separator " | "
                                             (reverse table)))
             x)
      (poly (my-sine (/ x 3.0)
                     (+ step 1)
                     (cons (list (number->string (+ step 1))
                                 "==>"
                                 (number->string x))
                           table)))))

;; (my-sine 12.15 0 '())
;; 1 | ==> |                12.15
;; 2 | ==> |                 4.05
;; 3 | ==> |   1.3499999999999999
;; 4 | ==> |  0.44999999999999996
;; 5 | ==> |                 0.15
;; 6 | ==> | 0.049999999999999996
;; -0.39980345741334
;; Order of growth in steps seems to be Theta(log x).
;; Order of growth in space maybe Theta(2^(log x)) 
;; or just Theta(x).

