;;; sicp-1e-growth.rkt

#lang racket

(require "check.rkt"
         "simple-table.rkt")

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

