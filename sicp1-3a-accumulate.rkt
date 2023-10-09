;;; sicp1-3a-accumulate.rkt
;;;
;;; Exercises 1.29 -- 1.33
;;;
;;; Sum abstraction, tail-recursive sum abstraction,
;;; numerical integration, Simpson's method, product
;;; abstaction, factorial, Wallis product for pi,
;;; generalized sum/product: accumulate, filtered accumulate.
;;;
;;; Interesting point from SICP:
;;;
;;; We don't want to be limited to expressing 5x5x5 for
;;; a particular number, 5. We want to express the concept
;;; of 'cubing' something. And then, the higher-order
;;; concept of summing the cubes of things.


;; to do
;;
;; Get rid of check.rkt
;; get rid of format-test/simple
;; put table formatting and function timer in sicp lib.


#lang racket

(require rackunit
         "format-table.rkt")

;; Some beautiful examples of direct recursion.

;; Summing integers from a to b.
;; Recursion 'equation of motion' and initial condition.
;; 
;;   S(a, b) = a + S(a+1, b)
;;   S(b,b) = 0.

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

(define (sum-cubes-1 a b) (sum cube a add1 b))
(define (sum-integers-1 a b) (sum identity a add1 b))

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
;; the way you draw them under the curve. Area of one
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
;;              =  [  f(a + 0*dx + dx/2)
;;                  + f(a + 1*dx + dx/2)
;;                  + f(a + 2*dx + dx/2)
;;                  ... ]*dx

(define (integral F a b dx)
  (define (next x)
    (+ x dx))
  (* dx (sum F (+ a (/ dx 2.0)) next b)))

(check-= (integral identity 0 1 .001) 1/2 0.001)
(check-= (integral (lambda (x) (* x x)) 0 1 0.001) 1/3 0.001)
(check-= (integral cube 0 1 0.001) 1/4 0.001)


;; Exercise 1.29 ========================================

;; Simpson's integration. Definite integral of F between a and b.
;; Let n be even and...
;;
;;    h = (b - a)/n
;;
;;    y_k = F(a + k*h).
;;
;;    integral_a^b f = (h/3)*[               1*y_0 +
;;                             + 4*y_1     + 2*y_2 +
;;                             + 4*y_3     + 2*y_4 + 
;;                             ...
;;                             + 4*y_(n-1)
;;                                         + 1*y_n   ]
;; 
;; Write this using our 'sum' higher-order concept.

;; Put float versions of a, b to avoid large rationals.
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((odd? k) (* 4 (y k)))
          (else (* 2 (y k)))))
  (define (next k)
    (+ k 1))
  (* (/ h 3)
     (sum term 0.0 next n)))

(check-= (simpson cube 0.0 1.0 1000) 1/4 0.000001)

(define (relative-error x y)
  (* 100.0 (/ (abs (- x y)) y) ))

;; > (relative-error (integral cube 0.0 1.0 0.01) 1/4)
;; 0.004999999999832916
;;
;; > (relative-error (simpson cube 0.0 1.0 100) 1/4)
;; 3.3306690738754696e-14
;;
;; We see that Simpson's method with n=100 is much
;; better than the direct method with rectangle width 1/100.


;; Exercise 1.30 ========================================

;; Tail-recursive version of sum. I use the term
;; 'state-variable recursion' because it uses arguments
;; exactly as if they were state. This is, I think, a better
;; descriptive term than tail-recursion or the 'iterative'
;; terminology of SICP.

(define (sum1 term start next finish)
  (define (sum-helper a result)
    (if (> a finish)
        result
        (sum-helper (next a) (+ (term a) result))))
  (sum-helper start 0))

;; Using named-let:
(define (sum2 term start next finish)
  (let loop ((a start) (result 0))
    (if (> a finish)
        result
        (loop (next a) (+ (term a) result)))))

(check-equal? (sum1 cube 1 add1 100) (sum-cubes 1 100))
(check-equal? (sum2 cube 1 add1 100) (sum-cubes 1 100))


;; Exercise 1.31 ========================================

;; Let's do the same for products, as we did for sum:
;; a higher-order function that evaluates products.
;; Rather than terms we have factors.

;; Direct recursion.
(define (product factor a next finish)
  (if (> a finish)
      1
      (* (factor a)
         (product factor (add1 a) next finish))))

;; State variable recursion.
(define (product1 factor start next finish)
  (let loop ((a start) (result 1))
    (if (> a finish)
        result
        (loop (next a) (* (factor a) result)))))

(define (factorial n) (product identity 1 add1 n))
(define (factorial1 n) (product identity 1 add1 n))

(check-equal? (factorial 10) 3628800)
(check-equal? (factorial1 10) 3628800)

;; Wallis pi/4 infinite product:
;;
;;    pi/4 = (2/3)*(4/3)*(4/5)*(6/5)*(6/7)*(8/7)*...
;;

;; '(3 3 5 5 7 7 9 9 11 ...)
(define (pi/4-factor-denominator k)
  (- (* 2 (- k (floor (/ (- k 2) 2)))) 1))

;; (2 4 4 6 6 8 8 10 10 ...)
(define (pi/4-factor-numerator k)
  (* 2 (- k (floor (/ (- k 1) 2)))))

(define (pi/4-factor k)
  (* 1.0
     (/ (pi/4-factor-numerator k)
        (pi/4-factor-denominator k))))

;; Or a better way...
(define (pi/4-factor1 k)
  (if (odd? k)
      (/ (+ k 1)
         (+ k 2))
      (/ (+ k 2)
         (+ k 1))))

;; Need a lot of factors before we get close to pi.
(check-= (* 4 (product pi/4-factor1 1 add1 1000))
         pi
         0.01)


;; Exercise 1.32 ========================================

;; A higher abstraction comes from the desire to
;; join sum and product into one wider concept
;; called 'accumulate'. Terms combined with + are sums
;; and terms combined with * are products but in accumulate,
;; the terms are combined with a general 'combiner' function.
;;
;; Sum results begin with 0 and product results begin with 1,
;; so we also need a general beginning result value 'base-value'.
;;
;; Write 'accumulate' and use it to implement sum and product.
;; Let 'F' mean the generalized equivalent of term and factor.

(define (accumulate combiner base-value F a next finish)
  (if (> a finish)
      base-value
      (combiner (F a)
                (accumulate combiner
                            base-value
                            F
                            (next a)
                            next
                            finish))))

(define (sum3 F a next finish) (accumulate + 0 F a next finish))
(define (product3 F a next finish) (accumulate * 1 F a next finish))

(check-equal? (sum3 identity 1 add1 10) 55)
(check-equal? (product3 identity 1 add1 10) 3628800)

;; State-variable recursive accumulate.
;; It's pretty elegant.
(define (accumulate1 combiner base-value F start next finish)
  (let loop ((a start) (result base-value))
    (if (> a finish)
        result
        (loop (next a)
              (combiner (F a) result)))))

(define (sum4 F a next finish) (accumulate1 + 0 F a next finish))
(define (product4 F a next finish) (accumulate1 * 1 F a next finish))

(check-equal? (sum4 identity 1 add1 10) 55)
(check-equal? (product4 identity 1 add1 10) 3628800)


;; Exercise 1.33 ========================================

;; Accumuate F(a) only if a passes some predicate test.

(define (filtered-accumulate
         predicate? combiner base-value F start next finish)
  (let loop ((a start) (result base-value))
    (cond ((> a finish)
           result)
          ((predicate? a)
           (loop (next a)
                 (combiner (F a) result)))
          (else
           (loop (next a) result)))))

;; For the rest we need prime? and gcd. We'll use the Racket ones.

(require math/number-theory)

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 (lambda (x) (* x x)) a add1 b))

(define (coprime-product n)
  (filtered-accumulate (lambda (x) (= (gcd x n) 1))
                       *
                       1
                       identity
                       1
                       add1
                       n))

(check-equal? (sum-prime-squares 1 10) 87)
(check-equal? (coprime-product 10) 189)
