;;; chapter-1.rkt
;;;
;;; Exercises from chapter 1.

#lang racket

(require "check.rkt")


;; Exercise 1.1 ========================================

10                       ;; ==> 10
(+ 5 3 4)                ;; ==> 12
(- 9 1)                  ;; ==> 8
(/ 6 2)                  ;; ==> 3
(+ (* 2 4)
   (- 4 6))              ;; ==> 6
(define a 3)             ;; ==> none, a is 3.
(define b (+ a 1))       ;; ==> none, b is 4. 
(+ a b (* a b))          ;; ==> 19
(= a b)                  ;; ==> #f
(if (and (> b a)
         (< b (* a b)))
    b
    a)                   ;; ==> 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))         ;; ==> 16

(+ 2 (if (> b a) b a))   ;; ==> 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))              ;; ==> 16



;; Exercise 1.2 ========================================

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))         ;; ==> -37/150



;; Exercise 1.3 ========================================

;; Procedure f(x,y,z) returns sum of the squares
;; of the two larger arguments.

(define (square a) (* a a))

;; Sum of squares.
(define (sos a b) 
  (+ (square a) (square b)))

(define (sos-larger x y z) 
  (cond ((and (<= x y) (<= x z)) (sos y z))
        ((and (<= y x) (<= y z)) (sos x z))
        (else (sos x y))))

(check-me sos-larger 
          '((1 1 1) . 2)
          '((1 1 3) . 10)
          '((1 2 3) . 13)
          '((3 2 1) . 13)
          '((2 3 1) . 13)
          '((3 3 1) . 18)
          '((3 1 3) . 18)
          '((1 3 1) . 10))


;; Exercise 1.4 ========================================

;; Describe how this works.

(define (a-plus-abs-b a b)
  ((if (> b a) + -) a b))

;; If b > a then you add a and b ==> a + (positive b) ==> a + |b|
;; If b < a then subtract, a - b ==> a - (negative b) ==> a + |b|
;; Clever!



;; Exercise 1.5 ========================================

;; Evaluate then apply ==> applicative order.
;; Expand then reduce  ==> normal order.

;; If you call this function you get an endless loop.
(define (p) (p))

(define (test-evaluation-order x y)
  (if (= x 0) 0 y))

;; Applicative order. Evaluate then apply.
;; (test-evaluation-order 0 (p))
;;                        ^  ^
;;                           This is evaluated ==> infinite loop.

;; Normal order. Expand then reduce.
;; (test-evaluation-order 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t      0 (p)) ;; predicate evaluated first.
;;             ^
;;             This branch is taken, returning 0.
;;
;; So,
;; Applicative order ==> infinite loop.
;; Normal order      ==> 0.


