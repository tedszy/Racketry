;;; sicp-1g-gcd.rkt

#lang racket

(require "check.rkt"
         "simple-table.rkt")

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


;; Exercise 1.20 ========================================




