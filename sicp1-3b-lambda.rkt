;;; sicp1-3a-accumulate.rkt
;;;
;;; Exercises 1.34 -- 1.xx
;;;
;;; Lambdas, local variables, let,


#lang racket

(require rackunit
         "format-table.rkt")

;; Sum can be passed lambda instead named
;; functions like pi-next, pi-term.

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Using lambdas, pi-sum can be defined without
;; having to define other helper functions.
;; In fact one can just as well set a = 1 here.

(define (sum-pi/8 b)
  (sum (lambda (k) (/ 1 (* k (+ k 2))))
       1.0
       (lambda (k) (+ k 4))
       b))

(check-= (sum-pi/8 10000) (/ pi 8) 0.0001)

;; On the order of 10**4 terms are needed to converge
;; within 0.0001 of pi/8.
;;
;; Lambdas can simplify integrate, so that we don't
;; need to define the helper function 'next'.

(define (integral F a b dx)
  (* dx (sum F
             (+ a (/ dx 2.0))
             (lambda (x)
               (+ x dx))
             b)))

(let ((tol 0.000001)
      (dx 0.001))
  (check-= (integral identity 0 1 dx) 1/2 tol)
  (check-= (integral (lambda (x) (* x x)) 0 1 dx) 1/3 tol)
  (check-= (integral exp 0 1 dx) (- (exp 1) 1) tol))

;; SICP recommends an interesting way of "saying"
;; a lambda expression:
;;
;;   (lambda (x) ...
;;
;; "The procedure of argument x that does ..."
;; This is a good way of explaining it to students.

;; Various ways to do the substitutions
;;
;; a = 1 + x*y
;; b = 1 - y
;;
;; into the function my-f(x,y) = x*a^2 + y*b + ab:

;; Using nested define...

(define (my-f x y)
  (define (my-f-helper a b)
    (+ (* x a a) (* y b) (* a b)))
  (my-f-helper (+ 1 (* x y))
               (- 1 y)))

;; Using lambda instead of a named function...

(define (my-f1 x y)
  ((lambda (a b) (+ (* x a a) (* y b) (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;; Using let...

(define (my-f2 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x a a) (* y b) (* a b))))

(check-equal? (my-f 1 2) (my-f1 1 2))
(check-equal? (my-f 7 3) (my-f1 7 3))
(check-equal? (my-f 1 2) (my-f2 1 2))
(check-equal? (my-f 7 3) (my-f2 7 3))


;; Exercise 1.34 ========================================

(define (f g) (g 2))
(define (square x) (* x x))

;; It's easy to understand
;;
;; (f square) => (square 2) => 4
;;
;; (f (lambda (z) (* z (+ z 1))) => (lambda (z) (* z (+ z 1)) 2)
;;                               => 6
;;
;; But what about (f f)? This should give an error,
;; because 2 is not a function. Can be explained using lambda.
;;
;; (f (lambda (g) (g 2))) => ((lambda (g) (g 2)) 2)
;;                        => (2 2) => error


;; Half-interval root finding.
;; 
;; If f(a) is negative, and f(b) is positive,
;; then there must be a root x, f(x) = 0, somewhere in (a, b).
;;
;; (1) Find midpoint x = (a+b)/2.
;;
;; (2) If f(x) > 0 then the root must be in (a, x).
;;     If f(x) < 0 then the root must be in (x, b).
;;
;; (3) Step (2) defines new left and right bounds.
;;     Continue this until b-a < tolerance.
;;
;; (4) Once tolerance is reached, return x = (b-a)/2.
;;
;; We add some robustness to the half-interval-method
;; by making it check which bound is positive and which
;; is negative.

(define (average x y) (/ (+ x y) 2))
(define (close-enough? a b) (< (abs (- a b)) 0.001))

(define (half-interval-search F negative-point positive-point)
  (let ((midpoint (average negative-point positive-point)))
    (if (close-enough? negative-point positive-point)
        midpoint
        (let ((trial (F midpoint)))
          (cond ((positive? trial)
                 (half-interval-search F negative-point midpoint))
                ((negative? trial)
                 (half-interval-search F midpoint positive-point))
                (else
                 midpoint))))))

(define (half-interval-method F a b)
  (let ((a-val (F a))
        (b-val (F b)))
    (cond ((and (negative? a-val) (positive? b-val))
           (half-interval-search F a b))
          ((and (positive? a-val) (negative? b-val))
           (half-interval-search F b a))
          (else
           (error "f(interval bounds) must be opposite signs: "
                  a b (F a) (F b))))))

(check-= (half-interval-method sin 2.0 4.0) pi 0.001)

;; Find root of polynomial x^3 - 2*x -3, between 1 and 2,
;; using lambda.
;;
;; (half-interval-method (lambda (x) (+ (* x x x) (* -2 x) -3))
;;                       1.0
;;                       2.0)
;;
;; => 1.89306640625

