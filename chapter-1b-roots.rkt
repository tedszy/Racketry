;;; chapter-1b-roots.rkt
;;;
;;;

#lang racket

(require "check.rkt"
         "simple-table.rkt")

(provide sqrt-table)

;; This is just a manual construction of
;; the table on pg.29 of SICP.
(define (sqrt-approximation-table x n-rows)
  (let loop ((y 1)
             (n-rows n-rows)
             ;; Start the table off with column headers.
             (table (list (list "Guess y"
                                "Quotient x/y"
                                "Average (y + x/y)/2"
                                "float value"))))
    (cond ((= n-rows 0)
           (reverse table))
          (else
           (let* ((q (/ x y))
                  (avg (/ (+ y q) 2)))
             (loop avg
                   (sub1 n-rows)
                   (cons (list (number->string y)
                               (number->string q)
                               (number->string avg)
                               (format-real avg 8))
                         table)))))))

;; (sqrt-table 2 4)
;;
;; Guess y | Quotient x/y | Average (y + x/y)/2 | float value
;;       1 |            2 |                 3/2 |  1.50000000
;;     3/2 |          4/3 |               17/12 |  1.41666667
;;   17/12 |        24/17 |             577/408 |  1.41421569
;; 577/408 |      816/577 |       665857/470832 |  1.41421356 
(define (sqrt-table x n)
  (print-table (sqrt-approximation-table x n)))

;; Roll all the SICP code relating to numerical 
;; square root into one procedure using a named let. 
;; Note the use of free-variable x.
(define (my-sqrt x)
  (define tolerance 0.000001)
  (define (average a b) (/ (+ a b) 2))
  (define (improve y) (average y (/ x y)))
  (define (good-enough? y) (< (abs (- (* y y) x)) tolerance))
  (let loop ((y 1.0))
    (if (good-enough? y) 
        y 
        (loop (improve y)))))



;; Exercise 1.6 ========================================


