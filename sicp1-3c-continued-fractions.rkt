;;; sicp1-3c-continued-fractions.rkt
;;;
;;; Exercises 1.37 -- 1.43.
;;;
;;; Continued fractions, phi, euler fraction for e,
;;; Lambert's continued fraction, procedures returning
;;; procedures, Newton's method, generalized Newton's
;;; method, composition, 
;;; 
;;; It's very easy to write an incorrect continued
;;; fraction recursion scheme when you are testing
;;; it on N_k = D_k = 1, 1, 1, ... 
;;;
;;; "First class" objects in a programming language
;;; have the fewest restrictions.

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

;; Lambert's continued fraction for tan(x).
;;
;; N(k) = x, -x^2, -x^2, -x^2...
;; D(k) = 1, 3, 5, ...

(define (lambert-tan x)
  (continued-fraction (lambda (k)
                        (if (= k 1)
                            x
                            (- (* x x))))
                      (lambda (k)
                        (- (* 2 k) 1))
                      10))

(let ((tol 0.000001))
  (check-= (lambert-tan 1.1) (tan 1.1) tol)
  (check-= (lambert-tan 2.1) (tan 2.1) tol)
  (check-= (lambert-tan -0.6) (tan -0.6) tol))

;; Works!

;; Procedures that return procedures.
;;
;; make-average-damped takes a function F
;; and returns a function that computes
;; the average of x and F(x).

(define (average x y) (/ (+ x y) 2))

(define (make-average-damped F)
  (lambda (x) (average x (F x))))

(define (square x) (* x x))

(check-equal? ((make-average-damped square) 10) 55)

;; Recall one of the fixed-point procedures from before:

(define (fixed-point1 F initial-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (let loop ((guess initial-guess))
    (let ((next-guess (F guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (loop next-guess)))))

;; We can now define fixed-point-sqrt slightly differently:

(define (fixed-point-sqrt1 x)
  (fixed-point1 (make-average-damped (lambda (y) (/ x y))) 1.0))

(check-= (fixed-point-sqrt1 2) (sqrt 2) 0.0001)

;; The idea here is that the mathematical reasoning behind
;; the procedure is now more explicit.

(define (fixed-point-cube-root x)
  (fixed-point1
   (make-average-damped (lambda (y) (/ x (* y y)))) 1.0))

(check-= (fixed-point-cube-root 5) (expt 5 1/3) 0.00001)

;; Newton's method
;;
;; Newton's method is a generalization of what we have
;; been doing with fixed-points. To find the root of g(x),
;; find the fixed point of
;;
;; f(x) = x - g(x)/g'(x).
;;
;; Let's try it to find square root of y. If x is the
;; square root of y then x^2 - y = 0. So it is sufficient
;; to find the root of g(x) = x^2 - y.
;;
;; f(x) = x - (1/2)*(x - y/x)
;;
;; If x is the fixed point of f(x) then
;;
;; x = x - (1/2)*(x-y/x) => x = y/x and x is sqrt(y).
;;
;; We need a procedure that makes a derivative function.

(define dx 0.000001)

(define (make-derivative g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(check-= ((make-derivative (lambda (x) (* x x x))) 5)
         75.0 0.0001)

;; Using make-derive and fixed-point, we can express
;; Newton's root method. The higher-order procedure
;; newton-transform creates f(x) out of the given g(x).

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((make-derivative g) x)))))

(define (newton-root g guess)
  (fixed-point1 (newton-transform g) guess))

;; For example, to find square roots...

(define (newton-sqrt y)
  (newton-root (lambda (x) (- (* x x) y)) 1.0))

(define (newton-fifth-root y)
  (newton-root (lambda (x) (- (* x x x x x) y)) 1.0))

(check-= (newton-sqrt 2) (sqrt 2) 0.00001)
(check-= (newton-fifth-root 2) (expt 2 1/5) 0.00001)

;; This root finding with transform can be generalized
;; to other kinds of transforms, not just the Newton one.

(define (fixed-point-of-transform g transform guess)
  (fixed-point1 (transform g) guess))

;; Square root by "Babylonian" method.
(define (sqrt1 y)
  (fixed-point-of-transform
   (lambda (x) (/ y x))
   make-average-damped
   1.0))

;; Square root by Newton's root method.
(define (sqrt2 y)
  (fixed-point-of-transform
   (lambda (x) (- (* x x) y))
   newton-transform
   1.0))

(check-= (sqrt1 2) (sqrt 2) 0.00001)
(check-= (sqrt2 3) (sqrt 3) 0.00001)


;; Exercise 1.40 ========================================

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(let ((x (newton-root (cubic 1 2 3) 1.0)))
  (check-= (+ (* x x x) (* 1 x x) (* 2 x) 3) 0 0.0001))


;; Exercise 1.41 ========================================

(define (double-apply f) (lambda (x) (f (f x))))

;; ((make-double-apply add1) 1) => 3

(define (ex-41 x)
  (((double-apply (double-apply double-apply)) add1) x))

;; This applies add1 16 times to x. So if x=5 the result is 21.


;; Exercise 1.42 ========================================

(define (my-compose f g) (lambda (x) (f (g x))))

;; ((my-compose square add1) 6) => 49
;;
;; add1 applied to 6 is 7, squaring that is 49.


;; Exercise 1.43 ========================================
w
;; Create an nth-repeatedly composed function from f.
;; We use pure recursion with named-let. You need named-let
;; here otherwise you're going to be passing a lambda
;; to f(x).

(define (repeated-composition f n)
  (lambda (x)
    (let loop ((n n))
      (if (= n 0)
          x
          (f (loop (- n 1)))))))

(check-equal? ((repeated-composition add1 10) 1) 11)
(check-equal? ((repeated-composition square 2) 5) 625)


;; Exercise 1.41 ========================================

