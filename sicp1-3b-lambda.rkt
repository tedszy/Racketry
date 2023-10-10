;;; sicp1-3b-lambda.rkt
;;;
;;; Exercises 1.34 -- 1.36
;;;
;;; Lambdas, local variables, let, half-interval roots,
;;; fixed points, average damping.
;;;
;;; SICP says that defined-functions (using define)
;;; within a function are OK, but defined values
;;; should be avoided, for reasons to be explained later.

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

;; Fixed points of functions.
;;
;; To find x such that f(x) = x, iterate f(x), f(f(x)) for
;; some guess x, and stop when the difference between two
;; iterations is less than some tolerance.

(define (fixed-point F initial-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (define (trial guess)
    (let ((next-guess (F guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (trial next-guess))))
  (trial initial-guess))

;; Version using named-let...

(define (fixed-point1 F initial-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  (let loop ((guess initial-guess))
    (let ((next-guess (F guess)))
      (if (close-enough? guess next-guess)
          next-guess
          (loop next-guess)))))

(check-= (fixed-point cos 1.0)
         (fixed-point1 cos 1.0) 0.0001)

;; Solve x = sin(x) + cos(x):
;;
;; (fixed-point1 (lambda (x) (+ (sin x) (cos x))) 1.0)
;; => 1.2587315962971173

;; Square root as fixed-point search.
;; Let y be sqrt(x), then y^2 = x and y = x/y.
;; so iterating F(y) = x/y until fixed point
;; will find the root y = F(y). But since this
;; does not converge, we try F(y) = average
;; of y and x/y:

(define (fixed-point-sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2))
               1.0))

;; We can also justify this by noting that y = y
;; is fixed, so y+y = y+y/x leads the the same
;; fixed point, and also (y+y)/2 = y = (y+y/x)/2.
;;
;; This idea is also called average-damping.

(check-= (fixed-point-sqrt 3) (sqrt 3) 0.0001)
(check-= (fixed-point-sqrt 2) (sqrt 2) 0.0001)


;; Exercise 1.35 ========================================

;; x -> 1 + 1/x or x = 1 + 1/x when x is the fixed-point.
;; then x^2 - x - 1 = 0 and the solutions are the golden
;; ratios
;;
;; phi1 = (1+sqrt(5))/2
;; phi2 = (1-sqrt(5))/2
;;
;; The first one can be found by fixed-point:

(check-= (fixed-point1 (lambda (x) (+ 1 (/ x))) 1.0)
         (/ (+ 1 (sqrt 5)) 2)
         0.0001)


;; Exercise 1.36 ========================================

;; A new fixed-point that displays steps.
;; Actually we can build lists of lists and then pass that
;; to format-table. We use named-let.

(define (fixed-point-with-steps F initial-guess tol)
  (define (delta v1 v2) (abs (- v1 v2)))
  (define (close-enough? v1 v2) (< (delta v1 v2) tol))
  (display
   (format-table/default #:flonum-precision 10
    (let loop ((guess initial-guess)
               (result empty)
               (n 0))
      (let ((next-guess (F guess)))
        (if (close-enough? guess next-guess)
            (reverse (cons (list n
                                 next-guess
                                 (delta guess next-guess))
                           result))
            (loop next-guess
                  (cons (list n
                              guess
                              (delta guess next-guess))
                        result)
                  (+ n 1))))))))

;; (fixed-point-with-steps (lambda (x) (+ 1 (/ x))) 1.0 0.0001)
;;
;;  0 | 1.0000000000 | 1.0000000000
;;  1 | 2.0000000000 | 0.5000000000
;;  2 | 1.5000000000 | 0.1666666667
;;  3 | 1.6666666667 | 0.0666666667
;;  4 | 1.6000000000 | 0.0250000000
;;  5 | 1.6250000000 | 0.0096153846
;;  6 | 1.6153846154 | 0.0036630037
;;  7 | 1.6190476190 | 0.0014005602
;;  8 | 1.6176470588 | 0.0005347594
;;  9 | 1.6181818182 | 0.0002042901
;; 10 | 1.6180555556 | 0.0000780275
;;
;; Now solve x^x = 1000 => x*log(x) = log(1000)
;;                      => x = log(1000)/log(x)
;; by finding fixed-point of F(x) = log(1000)/log(x).
;;
;; (fixed-point-with-steps
;;  (lambda (x) (/ (log 1000) (log x))) 1.1 0.000001)
;;
;; The table shows that after 42 steps we get to
;; x = 4.5555359709.
;;
;; (expt 4.555555555 4.555555555)
;; => 1000.0499502108756
;;
;; Let's try it with average-damping:
;; Now it converges in 12 steps:
;;
;; (fixed-point-with-steps
;;  (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
;;  1.1 0.000001)
;;
;;  0 |  1.1000000000 | 35.6882868921
;;  1 | 36.7882868921 | 17.4361113603
;;  2 | 19.3521755319 |  8.5103418523
;;  3 | 10.8418336796 |  3.9717853274
;;  4 |  6.8700483521 |  1.6428233902
;;  5 |  5.2272249620 |  0.5252647668
;;  6 |  4.7019601952 |  0.1197634220
;;  7 |  4.5821967732 |  0.0220625435
;;  8 |  4.5601342297 |  0.0038138103
;;  9 |  4.5563204194 |  0.0006510576
;; 10 |  4.5556693618 |  0.0001108988
;; 11 |  4.5555584630 |  0.0000188830
;; 12 |  4.5555395800 |  0.0000032151
;; 13 |  4.5555358175 |  0.0000005474

