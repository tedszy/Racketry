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
sicp-1d-recursion.rkt﻿> 
(define (fibonacci2 n)
  (let loop ((a 0) (b 1) (n n))
    (if (= n 0)
        a
        (loop b (+ a b) (- n 1)))))

;; 1 dollar into change ==> half dollars, quarters,
;;                          dimes, nickels, pennies.

;; Exercise 1.10 ========================================





;; Exercise 1.11 ========================================


;; Exercise 1.12 ========================================

