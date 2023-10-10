;;; sicp1-3c-continued-fractions.rkt
;;;
;;; Exercises 1.37 -- 1.xx
;;;
;;; Continued fractions, phi, 
;;; 
;;;
;;; 
;;; 
;;; 


#lang racket

(require rackunit
         "format-table.rkt")


;; Exercise 1.37 ========================================
;; Part (a).

;; N(k) = numerators of continued fraction.
;; D(k) = denominators of continued fraction.
;; Often N(k) = 1, like for the phi continued fraction.

(define phi (/ (+ 1 (sqrt 5)) 2))

(define (continued-fraction N D n)
  (let loop ((D D)
             (N N)
             (q 0)
             (n n))
    (if (= n 1)
        q
        (loop N D (/ (N n) (+ (D n) q)) (- n 1)))))

;; (displayln
;;  (* 1.0 (continued-fraction (lambda (k) 1) (lambda (k) 1) 12)))
;; (displayln
;;  (- phi 1))
;; =>
;; 0.6180555555555556
;; 0.6180339887498949
;;
;; It takes only 12 terms of the continued fraction
;; to get 4 decimals.

;; Part (b).
;;
;; We wrote a state-variables recursion.
;; Now we will write a purely recursive version.
;;

(define (continued-fraction1 N D n)
  (if (= n 1)
      (/ (N 1) (D 1))
      (/ (N n) (+ (D n) (continued-fraction1 N D (- n 1))))))

;; (displayln
;;  (* 1.0 (continued-fraction1 (lambda (k) 1) (lambda (k) 1) 12)))
''
;; => 0.6180257510729614
;;
;; It works!


;; Exercise 1.38 ========================================

