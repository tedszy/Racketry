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

;; Making change for a given amount M. A refactored
;; version of the one in SICP. "Making change" is
;; actually finding a linear combination of coins
;; with coefficients >= 0, totalling M. 
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

;; Base cases.
;; 
;; If the amount M is 0, there is only one way
;; to make a linear combination of coins that equals M.
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
;; Now, the recursion step, whereby the problem is
;; reduced to a smaller problem.
;;
;; Suppose our coins are (coin-symbol . value) pairs:
;;
;; coins = '((a . A) (b . B) (c . C) ....)
;;
;; #(M, coins) = number of ways to make linear combinations of
;;               the coins totalling M using all coins
;;
;; Let (a . A) be the first coin-value pair in the database.
;;
;; Then #(M, coins) = number of combinations without coin (a . A)
;;                  + number of combinations using at at least 
;;                    one (a . A). 
;;
;; #(M,coins) = #(M, (cdr coins)) + #(M-A, coins)
;;
;; In the second term, we have chosen one coin (a . A), and that
;; is why M has been decreased by A. But we can choose more of 
;; the same if we like so the choice is over all coins. Again:
;;
;; # of ways = # ways without first coin 
;;             + # ways with at least one first coin.  
;; 
;; This reduces the original problem to a smaller one. In one term
;; the coin database is smaller, in the other term the amount is smaller.
(define (ways-to-make-change M)
  (let loop ((M M) (coins coin-database))
    (cond ((= M 0) 1) 
          ((< M 0) 0)
          ((null? coins) 0)
          (else
           (+ (loop M (cdr coins))
              (loop (- M (cdr (car coins))) coins))))))

(check-me ways-to-make-change '((100) . 292))

;; Exercise 1.11 ========================================

;; f(n) = n if n < 3.
;; f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3.

;; Recursive process.
(define (three-way-fibonacci1 n)
  (cond ((< n 3) n)
        (else 
         (+ (three-way-fibonacci1 (- n 1))
            (* 2 (three-way-fibonacci1 (- n 2)))
            (* 3 (three-way-fibonacci1 (- n 3)))))))

;; Iterative (tail) recursive process.
;;
;; 0 1 2 (0+2*1+3*2 = 8) (1+2*2+3*8 = 29)
;; a b c      
;;   a b     c     
;;     a     b                  c   
;;        
(define (three-way-fibonacci2 n)
  (let loop ((a 0) (b 1) (c 2) (n n))
    (if (= n 0)
        a
        (loop b c (+ c (* 2 b) (* 3 a)) (- n 1)))))



;; Exercise 1.12 ========================================



;; Exercise 1.13 ========================================


