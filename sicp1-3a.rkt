;;; sicp1-3a.rkt
;;;
;;; Exercises ... 
;;;
;;; 

;; to do

;; Get rid of check.rkt
;; get rid of format-test/simple
;; put table formatting and function timer in sicp lib.
;; put multiple-value checker in lib


#lang racket

(require rackunit
         "format-table.rkt")

;; Beautiful examples of direct recursion.

;; S(a, b) = a + S(a+1, b)
;;  S(b,b) = 0.

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(check-equal? (sum-integers 0 10) 55)
(check-equal? (sum-integers 1 10) 55)
(check-equal? (sum-integers 100 200)
              (- (sum-integers 1 200)
                 (sum-integers 1 99)))

(define (cube x) (* x x x))

;; C(a, b) = a^3 + C(a+1, b)
;; C(b, b) = 0

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

;; P(a, b) = 1/a*(a+2) + P(a+4, b)
;; P(b, b) = 0
;;
;; 1/1*3 + 1/5*7 + 1/9*11 + ...

(define (sum-pi/8 a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (sum-pi/8 (+ a 4) b))))

;; This would fail if tolerance was 0.001.
(check-= (sum-pi/8 1 100) (/ pi 8) 0.01)

;; Abstract all three into this idea:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Racket has add1 which is like SICP's inc.
;; Also has identity.

(define (sum-cubes-1 a b)
  (sum cube a add1 b))

(define (sum-integers-1 a b)
  (sum identity a add1 b))

;; Not really supposed to use lambdas here because
;; SICP didn't introduce them yet. But still...
(define (sum-pi/8-1 a b)
  (sum (lambda (n) (/ 1.0 (* n (+ n 2))))
       a
       (lambda (n) (+ n 4))
       b))

(check-equal? (sum-cubes-1 1 10) (sum-cubes 1 10))
(check-equal? (sum-integers-1 50 100) (sum-integers 50 100))
(check-= (sum-pi/8-1 1 100) (sum-pi/8 1 100) 0.0001)

;; Numerical integration by literally summing rectangles
;; the way you draw them uder the curve. Area of one
;; rectangle is Height of curve at rectangle
;; midpoint f(x + dx/2) multiplied by rectangle width dx.
;;
;; integral_a^b =  f(a + dx/2)*dx 
;;               + f(a + dx + dx/2)*dx  
;;               + f(a + dx + dx + dx/2)*dx
;;                 ...
;;
;; Or, making the pattern clearer:
;;
;;              =  [  f(a + 0*dx + dx/2
;;                  + f(a + 1*dx + dx/2)
;;                  + f(a + 2*dx + dx/2)
;;                  ... ]*dx

(define (integral F a b dx)
  (define (next x)
    (+ x dx))
  (* dx (sum F (+ a (/ dx 2.0)) next b)))

(check-= (integral cube 0 1 0.001) 0.25 0.001)

;; Exercise 1.29 ========================================


;; Exercise 1.30 ========================================


;; Exercise 1.31 ========================================


;; Exercise 1.32 ========================================


;; Exercise 1.33 ========================================
