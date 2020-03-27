;;; sicp1-2d-exponentiation.rkt
;;;
;;; Exercises 1.16 -- 1.19.
;;;
;;; Fast exponentiation, growth, tail recursion,
;;; elegant and fast methods for computing
;;; fibonacci numbers, Fibonacci numbers by
;;; continuation passing.

#lang racket

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
;; You get Fibonacci transform F = T01 when p=0, q=1.
;;
;; Find composition of Tpq.Tpq (dot means "applied to".)
;;
;;    Tpq.Tpq.(a, b) = (bq' + aq' + ap', bp' + aq')
;;    where p' = p^2 + q^2 and q' = q^2 + 2pq.
;;
;; With the above formula we can do successive squaring
;; like fast-exponentiation. How does this work exactly?
;; The transrformation p->p`, q->q' changes n applications
;; of T to 2n applications of T. 
;;
;;   p    = 0,             q    = 1                ==> F(1) ==> F
;;   p'   = p^2 + q^2 = 1  q'   = q^2 + 2*p*q = 1  ==> F(2) ==> F^2
;;   p''  = 2              q''  = 3                ==> F(4) ==> F^4
;;   p''' = 13             q''' = 21               ==> F(8) ==> F^8
;;
;; These p,q are themselves Fibonacci numbers. Let's see them
;; in action by using a closure to create these operators 
;; corresponding to p, q.
;;
;;    Tpq.(a,b) = (bq + aq + ap, bp + aq)
;;
;; Use Racket's multiple-values.
(define (T p q)
  (lambda (a b)
    (values (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q)))))

(define F (T 0 1)) 
(define F^2 (T 1 1))
(define F^4 (T 2 3))
(define F^8 (T 13 21))
;; Some tests.
;;
;; (F^4 1 0) ==> 5, 3
;; ((compose F F F F) 1 0) ==> 5, 3
;;
;; (F^8 1 0) => 34, 21
;; ((compose F^4 F^4) 1 0) ==> 34, 21 (!)
;;
;; ﻿((compose F^8 F^8) 1 0) ==> 1597, 987
;; You can see how large F(n) can be computed this way.
;;
;; Solve the SICP problem before going off into any more variations.
;; Tpq applied n times to (a, b) pair.
;; (fibTpq 0 1 1 0 10) ==> 55 = F(10).
(define (Tpq-power-n p q a b n)
  (if (= n 0)
      b
      (Tpq-power-n  p 
                    q 
                    (+ (* b q) (* a q) (* a p))  
                    (+ (* b p) (* a q))
                    (- n 1))))

;; Now we want to do same, but with successive squaring
;; to get Theta(log(n)) performance.
;; (TFibonacci 10) ==> 55
;; (TFibonacci 500) ==> 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
(define (TFibonacci n)
  (let loop ((p 0) (q 1) (a 1) (b 0) (n n))
    (cond ((= n 0)
           b)
          ((even? n)
           (loop (+ (* p p) (* q q))
                 (+ (* q q) (* 2 p q))
                 a
                 b 
                 (/ n 2)))
          (else 
           (loop p
                 q
                 (+ (* b q) (* a q) (* a p))  
                 (+ (* b p) (* a q))
                 (- n 1))))))

;; The magic is more clear if we use matrices. 
;; This transformation:
;;
;;    Tpq.(a,b) = (bq + aq + ap, bp + aq)
;;              = (b*q + a(p+q), bp + aq)
;;
;; can be written as:
;;
;;   Tpq.(a,b) =  [ p+q  q ] [a]
;;                [  q   p ] [b]
;;
;; Let's square the Tpq matrix:
;;
;;   [ p+q q ] [ p+q q ] = [ (p+q)^2 + q^2   q*(p+q) + p*q ]
;;   [  q  p ] [  q  p ]   [ q*(p+q) + p*q   q^2 + p^2     ]
;;
;; This can be written in the form 
;;
;;   [ p'+q' q' ]  = T'
;;   [   q'  p' ]
;;
;; if we choose (look at second row) the new q' and p' to be...
;;
;;   p' = p^2 + q^2,   q' = q^2 + 2*p*q
;;
;; With these we can see how the new elements work out perfectly:
;;
;; T'(2,1) = q' = q*p+q^2+p*q = q^2 + 2*p*q
;; T'(2,2) = p' = p^2 + q^2
;; T'(1,1) = p'+q' = p^2 + q^2 + q^2 + 2*p*q = (p+q)^2 + q^2 (!)
;;
;; Beautiful!!
;;
;; So let's write a matrix version of the Theta(n) and 
;; Theta(log(n)) Fibonacci computations. Represent a 
;; 2x2 matrix as a list of 4 elements and use Racket's 
;; match-let to access them.
;;
;; Matrix multiplication.
(define (mat* A B)
  (match-let (((list a b c d) A)
              ((list e f g h) B))
    (list (+ (* a e) (* b g)) (+ (* a f) (* b h))
          (+ (* c e) (* d g)) (+ (* c f) (* d h)))))

;; Starting conditions.
;;
;;    0 1 1 2 3 5 8 13 21 35 55 ...
;;    b a
;;    p q
;; 
;;    Therefore, T = [ 1 1 ] 
;;                   [ 1 0 ]
;;
;;    Beginning with [ 1 ]  F(1)
;;                   [ 0 ]  F(0)
;;
;;   [ 1 1 ] [ 1 ] = [ 1 ]  F(2)
;;   [ 1 0 ] [ 0 ]   [ 1 ]  F(1)
;;
;;   [ 1 1 ] [ 1 1 ] [ 1 ] = [ 1 1 ] [ 1 ] = [ 2 ]  F(3)
;;   [ 1 0 ] [ 1 0 ] [ 0 ]   [ 1 0 ] [ 1 ]   [ 1 ]  F(2)
;;
;;   T.T.T. [ 1 ] = [ 1 1 ] [ 2 ] = [ 3 ]  F(4)
;;          [ 0 ]   [ 1 0 ] [ 1 ]   [ 2 ]  F(3)
;;
;; See how it works? 
;;
;;   (T^n).[ 1 ] = [ F(n+1) ]
;;         [ 0 ]   [ F(n)   ]
;;
;; Whereas we infer...
;;
;;   T^n = [ F(n+1) F(n)   ]
;;         [ F(n)   F(n-1) ]
;;
;; And so, the simple, Theta(n) steps algorithm.
;; Done with matrices.
(define (Tn n)
  (let loop ((result (list 1 1 1 0)) (n n))
    (if (= n 1)
        result
        (loop  (mat* (list 1 1 1 0) result)
               (sub1 n)))))

;; (Tn 10) ==> (89 55 55 34) ==> [ 89 55 ] = [ F(11) F(10) ]
;;                               [ 55 34 ]   [ F(10) F(9)  ]
;;
;; As expected. Note we don't actually use the initial vector (1,0).

;; Finally if we hard-code the matrix elements as state variables 
;; to a function, and put in the initial vector (1,0), we get code 
;; that's like the SICP version.
;;
;; a, b, c, d are the elements of the T matrices: [ a b ]
;;                                                [ c d ]
;; starting off as [ 1 1 ]
;;                 [ 1 0 ]
;;
;; u,v are the elements of the vector that starts off as [ 1 ].
;;                                                       [ 0 ]
(define (Tn-fast n)
  (let loop ((a 1) (b 1) (c 1) (d 0) (u 1) (v 0) (n n))
    (cond ((= n 0)
           v)
          ((even? n)
           (loop (+ (* a a) (* b c)) ;; simple matrix squaring.
                 (+ (* a b) (* b d))
                 (+ (* a c) (* c d))
                 (+ (* b c) (* d d))
                 u
                 v
                 (/ n 2)))
          (else
           (loop a
                 b
                 c
                 d
                 (+ (* a u) (* b v)) ;; matrix times vector.
                 (+ (* c u) (* d v))
                 (sub1 n))))))

;; (Tn-fast 10)
;; 55
;; (Tn-fast 500)
;; 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
;;
;; And since it is not necessary to carry a,b,c,d when only p and q
;; will do (since a=p+q, b=q, c=q, d=p) that's why the SICP version
;; has two less state variables!.
;;
;; One more Fibonacci variation! Let's define the T 
;; transformation as function, and then use composition 
;; of functions  as the "squaring" part. In the process 
;; we'll use multiple values, call-with-values and a 
;; peculiar kind of recursion -- where one of the state 
;; variables is a function!
;;
;;    0 1 1 2 3 5 8 ....
;;    a b
;;    T(a,b) = (b, a+b)
;;
(define (Tn-functional n)
  (let loop ((T (lambda (a b) 
                  (values b (+ a b))))
             (a 0)
             (b 1)
             (n n))
    (cond ((= n 0)
           (values a b))
          ((even? n)
           (loop (compose T T)
                 a
                 b
                 (/ n 2)))
          (else
           (call-with-values (lambda () 
                               (T a b))
                             (lambda (u v)
                               (loop T u v (sub1 n))))))))

