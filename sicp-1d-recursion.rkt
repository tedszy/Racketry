;;; sicp-1d-recursion.rkt

#lang racket

(require "check.rkt"
         "simple-table.rkt")

;; Bad recursive procedure for computing Fibonacci numbers.
;; The number of times that the smaller Fibonacci numbers
;; are computed goes like F(n+1) and F grows exponentially.
;; Use base cases F(0)=0 and F(1)=1.
(define (fibonacci1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else 
         (+ (fibonacci1 (- n 1))
            (fibonacci1 (- n 2))))))

;; Better version that is tail-recursive. Function call
;; stack doesn't grow. Let's try to figure out how it
;; works and maybe modify the SICP version a bit by 
;; changing the arguments around.
;;
;; 0 1 (0+1=1) (1+1=2) (1+2=3)
;; a b
;;   a    b      
;;        a       b
;;                a       b
;; and so on!
;;
;; > (fibonacci2 250)
;; 7896325826131730509282738943634332893686268675876375 
(define (fibonacci2 n)
  (let loop ((a 0) (b 1) (n n))
    (if (= n 0)
        a
        (loop b (+ a b) (- n 1)))))

;; Making change for a given sum M. A refactored
;; version of the one in SICP.
;;
;; 1 dollar into change ==> half dollars, quarters,
;;                          dimes, nickels, pennies.
;;
;; Simple database for the kinds of coins. 
;; Can retrieve the pair like so:
;; (assoc 'quarter coin-database) ==> '(quarter . 25)
(define coin-database 
  '((half-dollar . 50)
    (quarter . 25)
    (dime . 10)
    (nickel . 5)
    (penny . 1)))

;; Some rules for base cases.
;; 
;; If the amount M is 0, there is only one way
;; to make a linear combination of coins that equal M.
;; That is, all coefficients are zero. So we should
;; count this as one way.
;;
;; If M < 0 there is no way to make a linear combination
;; of coins equalling M with coefficients >= 0. So we 
;; count this as 0 ways.
;;
;; If there are no coins (length of database is 0) then 
;; there is no way to make change, so we count this as 0 ways.
;;
;; However, what if M=0 and length of database is 0?
;; Can this happen?
;;
;; Now, the reduction step, whereby the problem is
;; reduced to a smaller problem.
;;
;; Suppose our coins are (coin-symbol . value) pairs:
;;
;; coins = '((a . A) (b . B) (c . C) ....)
;;
;; #(M, coins) = number of ways to make linear combinations of
;;               the coins totalling M using all these coins
;;
;; Let (a . A) be the first coin-value pair in the database.
;; Then #(M, coins) = number of combinations without using (a . A)
;;                  + number of combinations using at at least 
;;                    one (a . A). That is, M has been
;;                    decreased by A (the value of coin a).
;;
;; #(M,coins) = #(M, (cdr coins)) + #(M-A, coins)
;;
;; In the second term, we have chosen one coin (a . A). But
;; we can choose more of the same if we like so the choice is
;; over all coins. Again:
;;
;; # of ways = # ways without first + # ways with at least one first.  
(define (ways-to-make-change M)
  (let loop ((M M) (coins coin-database))
    (cond ((= M 0) 1) 
          ((< M 0) 0)
          ((null? coins) 0)
          (else
           (+ (loop M (cdr coins))
              (loop (- M (cdr (car coins))) coins))))))

(check-me ways-to-make-change '((100) . 292))

;; Exercise 1.10 ========================================








;; Exercise 1.11 ========================================


;; Exercise 1.12 ========================================

