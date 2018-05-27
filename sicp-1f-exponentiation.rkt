;;; sicp-1f-exponentiation.rkt

#lang racket

(require "check.rkt"
         "simple-table.rkt")


;; Recursive exponentiation.
;; Theta(n) steps.
;; Theta(n) space.
(define (expo1 b n)
  (if (= n 0) 
      1
      (* b (expo1 b (- n 1)))))

;; As tail-recursive procedure.
;; Steps ==> Theta(n).
;; Space ==> Theta(1)
(define (expo2 b n)
  (let loop ((n n) (result 1))
      (if (zero? n)
          result
          (loop (sub1 n)
                (* b result)))))

;; Fast exponentiation: b^8...
;; b^8 = b^4 * b^4
;; b^4 = b^2 * b^2
;; b^2 = b^1 * b^1
;;
;; Fast exponentiation: b^12...
;; b^12 = b^6 * b^6
;; b^6  = b^3 * b^3
;; b^3  = b^1 * b^2 (odd case)
;; b^2  = b^1 * b^1
;;
;; Use these recursive steps to reduce the
;; problem to a smaller one.
;;
;; (even n) b^n = (b^(n/2))^2
;; (odd  n) b^n = b^1 * b^(n-1)
;;
;; Steps grow as Theta(log(n)).
(define (expo3 b n)
  (define (square x) (* x x))
  (cond ((= n 0)
         1)
        ((odd? n) 
         (* b (expo3 b (sub1 n))))
        (else
         (square (expo3 b (/ n 2))))))



;; Exercise 1.16 ========================================

;; Iterative (tail recursive) version of fast exponentiation.
;; Use an extra state variable that accumulates the answer,
;; such that a*b^n 9s always the same. Since a starts at 1,
;; the combination a*b^n = b0^n0 at every step. When
;; the computation terminates, n=0 and b^n is 1, meaning
;; that a=b0^n0 now.
;;
;; Use [b^(n/2)]^2 = [b^2]^(n/2)
;;
;; To reduce the size of n when n is even.
(define (expo4 b n)
  (define (square x) (* x x))
  (let loop ((b b) (n n) (a 1))
    (cond ((= n 0)
           a)
          ((odd? n)
           (loop b 
                 (sub1 n) 
                 (* b a)))
          (else
           ;; Note that this only makes n smaller.
           ;; There's no actual "computation"! We
           ;; are instead increasing the size of b
           ;; and decreasing n!
           (loop (square b) 
                 (/ n 2) 
                 a)))))
 
;; Beautiful idea!


;; Exercise 1.17 and 1.18 ========================================

;; If we replace multiplication by addition in
;; the above "expo" functions, we end up with
;; multiplication rather than exponentiation.
;;
;;    *        ==> +
;;    squaring ==> doubling
;;    expo     ==> mul
;;
;; The basic idea:
(define (mul1 a b)
  (if (= b 1) ;; Seems to be a typo in SICP here.
      a
      (+ a (mul1 a (- b 1)))))

;; Analogous to fast-exponentiation
;; with tree recursive growth and 
;; Theta(log(n)) steps.
(define (mul2 a b)
  (define (double x) (+ x x))
  (cond ((= b 1)
         a)
        ((odd? b)
         (+ a (mul2 a (sub1 b))))
        (else
         (double (mul2 a (/ b 2))))))

;; Now the analog of the iterative (tail recursive)
;; fast-exponentiation. Iterative fast-multiplication,
;; using only additions.
;;
;; Here the invariant is u + ab where u starts at 0.
;; And ends with ab=0 and u being the answer.
;; Change b=>a n=>b a=>u in expo4, etc.
;; Theta(log(n)) steps and Theta(1) in space.
(define (mul3 a b)
  (define (double x) (+ x x))
  (let loop ((a a) (b b) (u 0))
    (cond ((= b 0)
           u)
          ((odd? b)
           (loop a (sub1 b) (+ a u)))
          (else
           (loop (double a) (/ b 2) u)))))

 
;; Exercise 1.19 ========================================

;; Computing Fibonacci numbers in Theta(log(n)) steps.
;;
;; Fibonacci transformation F and generalized Tpq transformation.
;;
;; Fibonacci transform F.(a,b) = (a + b, a). This can be obtained
;; from a generalized transform
;;
;;    Tpq.(a,b) = (bq + aq + ap, bp + aq)
;;
;; Find composition of Tpq.Tpq (dot means "applied to".)
;;
;;    Tpq.Tpq.(a, b) = (bq' + aq' + ap', bp' + aq')
;;    where p' = p^2 + q^2 and q' = q^2 + 2pq.
;;
;; With the above formula we can do successive squaring
;; like fast-exponentiation. That's the point of T and T^2.
;;
;; You get Fibonacci transform F = T01 when p=0, q=1.
(define (fibT a b count)
  (if (= count 0)
      b
      (fibT b (+ a b) (- count 1))))

; this works. (fibTpq 0 1 1 0 10) ==> 55.
(define (fibTpq p q a b count)
  (if (= count 0)
      b
      (fibTpq p 
              q 
              (+ (* b q) (* a q) (* a p))  
              (+ (* b p) (* a q))
              (- count 1))))

;; -- refactored --------------------------------------------

;; Let's implement the generalized transform
;;
;;    Tpq.(a,b) = (bq + aq + ap, bp + aq)
;;
;; as a closure over p, q that is applied to
;; initial values a, b. It has to return two
;; values, so let's get some practice using 
;; multiple-values in Racket. We may want to 
;; use this in the "inside-out" article, which 
;; is about the relationships between multiple
;; values and multiple or higher-order recursion.
(define (T p q)
  (lambda (a b)
    (values (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q)))))

;; Racket's compose works on multiple-valued functions,
;; so try this, it's beautiful!
;; > (define F (T 0 1))    ;; Define the Fibonacci transform.
;; > ((compose F F F) 1 1) ;; Apply it repeatedly!
;; 5
;; 3
;; Gives Fibonacci(5). Because we start with Fibonacci(1) and Fibonacci(2), 
;; then three calls of F transformation gives Fibonacci(5).

;; We may need this...
;; > (call-with-values (lambda () (values 1 1) F))
;; 2
;; 1

;; -----------------------------------------------------


; it works!
(define (TFibonacci n)
  (fibTpq-iter 0 1 1 0 n))
(define (fibTpq-iter p q a b count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fibTpq-iter (+ (* p p) (* q q))
                      (+ (* q q) (* 2 p q))
                      a
                      b 
                      (/ count 2)))
        (else 
         (fibTpq-iter p
                      q
                      (+ (* b q) (* a q) (* a p))  
                      (+ (* b p) (* a q))
                      (- count 1)))))





