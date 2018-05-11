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
  (define tolerance 0.00001)
  (define (average a b) (/ (+ a b) 2))
  (define (improve y) (average y (/ x y)))
  (define (good-enough? y) (< (abs (- (* y y) x)) tolerance))
  (let loop ((y 1.0))
    (if (good-enough? y) 
        y 
        (loop (improve y)))))



;; Exercise 1.6 ========================================

;; Alyssa writes 'if' as a function rather than a special form.
(define (alyssa-if predicate true-branch false-branch)
  (cond (predicate 
         true-branch)
        (else 
         false-branch)))

;; The problem with this is that 'alyssa-if' is a
;; function so it is evaluated with applicative order.
;; The arguments are evaluated then the function is
;; applied.

(alyssa-if (= 2 3) 0 5) ;; ==> 5
(alyssa-if (= 1 1) 0 5) ;; ==> 0

;; So far it works ok. But let's use it to rewrite
;; the square root function.
(define (alyssa-sqrt x)
  (define tolerance 0.000001)
  (define (average a b) (/ (+ a b) 2))
  (define (improve y) (average y (/ x y)))
  (define (good-enough? y) (< (abs (- (* y y) x)) tolerance))
  (let loop ((y 1.0))
    (alyssa-if (good-enough? y) 
               y 
               (loop (improve y)))))

;; This never terminates because (!) both
;; arguments of the alyssa-if are evaluated.
;; That means the second argument is always 
;; evaluated. And so the loop continues no 
;; matter what happens with the good-enough?
;; test in the first argument. 

;; Indeed we can see this even more clearly
;; by contriving an example based on integer
;; products. Multiply out all numbers from 
;; 1 to n using recursion. Do the test with 
;; normal 'if'.

(define (product n)
  (let loop ((i 1) (result 1))
    (if (= i n)
        result
        (loop (add1 i)
              (* i result)))))

(product 10) ;; => 362880

;; But now use alyssa-if and the function never
;; returns anything, since the loop branch is
;; always called. Note: according to the applicative
;; model, 'result' is evaluated. But 'evaluated' and
;; 'returned' are two different notions. A function
;; returns a value only after it's arguments have
;; been evaluated and it has been applied to the 
;; results of those evaluations. 

(define (alyssa-product n)
  (let loop ((i 1) (result 1))
    (alyssa-if (= i n)
               result
               (loop (add1 i)
                     (* i result)))))

;; (alyssa-product 10) ==> never ends



;; Exercise 1.7 ========================================

;; my-sqrt as written above should fail for very small
;; and very large x. First let's illustrate this.

(define small-number 0.00001)
(my-sqrt small-number)
(sqrt small-number)

(define large-number 20000000000)
(my-sqrt large-number)
(sqrt large-number)

;; It gives the wrong answer for very small numbers,
;; and it takes too long for very large numbers.
;; Rather than use an absolute tolerance, we will
;; try an adaptive one: stop iterating when the
;; change in y is a small fraction of y.

