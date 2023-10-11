;;; sicp1-3c-continued-fractions.rkt
;;;
;;; Exercises 1.37 -- 1.xx
;;;
;;; Continued fractions, phi, euler fraction for e,
;;;
;;; 
;;; It's very easy to write an incorrect continued
;;; fraction recursion scheme when you are testing
;;; it on N_k = D_k = all ones. 


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
  (let loop ((n n)
             (q (/ (N n) (D n))))
    (if (= 1 n)
        q
        (loop (- n 1)
              (/ (N (- n 1)) (+ (D (- n 1)) q))))))

;; (displayln
;;  (* 1.0 (continued-fraction (lambda (k) 1) (lambda (k) 1) 12)))
;; (displayln
;;  (- phi 1))
;; =>
;; 0.6180257510729614
;; 0.6180339887498949
;;
;; It takes only 12 terms of the continued fraction
;; to get 4 decimals.

;; Part (b).
;;
;; We wrote a state-variables recursion.
;; Now we will write a purely recursive version.

(define (continued-fraction1 N D n)
  (let loop ((k 1))
    (if (= k n)
        (/ (N 1) (D 1))
        (/ (N k) (+ (D k) (loop (+ k 1)))))))

;; (displayln
;;  (* 1.0 (continued-fraction1 (lambda (k) 1) (lambda (k) 1) 12)))
;;
;; => 0.6180257510729614
;;
;; It works!
;;
;; (displayln
;;  (* 1.0 (continued-fraction1 (lambda (k) 1) (lambda (k) 1) 12)))


;; Exercise 1.38 ========================================

;; Euler's continued fraction expansion for e-2.
;;
;; N(i) = 1
;; D(i) = 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...

(define (D-euler k)
  (let loop ((d1 1) (d2 1) (j 1) (p 1))
    (if (= j k)
        d2
        (loop d2
              (if (= d1 d2 1)
                  (* 2 p)
                  1)
              (+ j 1)
              (if (= d1 d2)
                  (+ p 1)
                  p)))))

;; (displayln
;;  (for/list ((j (in-range 1 20)))
;;    (D-euler j)))
;; => (1 2 1 1 4 1 1 6 1 1 8 1 1 10 1 1 12 1 1)

(check-= (* 1.0 (continued-fraction
                 (lambda (k) 1.0)
                 D-euler
                 10))
         (- (exp 1) 2)
         0.00001)

;; Finally!


;; Exercise 1.39 ========================================
