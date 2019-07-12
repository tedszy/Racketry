;;; sicp-35c.rkt

#lang racket

(require "sicp-35-streams-api.rkt")


(define (integers-from n)
  (sicp-stream-cons n (integers-from (+ n 1))))

(define integers (integers-from 1))

(define (divisible-by? x d) (= (remainder x d) 0))

(define no-sevens
  (sicp-stream-filter (lambda (x)
                        (not (divisible-by? x 7)))
                      integers))
         
;; (sicp-stream-ref no-sevens 100) ==> 117
;; (sicp-stream-ref no-sevens 1000) ==> 1167
;; (sicp-stream-ref no-sevens 10000) ==> 11667


(define (fibonacci-generator a b) 
  (sicp-stream-cons a (fibonacci-generator b (+ a b))))

(define fibonacci (fibonacci-generator 0 1))

;; Sieve of Eratosthenes.

(define (prime-sieve s)
  (sicp-stream-cons 
   (sicp-stream-car s)
   (prime-sieve (sicp-stream-filter 
                 (lambda (x) 
                   (not (divisible-by? x (sicp-stream-car s))))
                 (sicp-stream-cdr s)))))

(define primes (prime-sieve (integers-from 2)))

;; We can also define streams without using a generating 
;; function or helper function, but directly.

(define ones (sicp-stream-cons 1 ones))

(define (stream-add r s) (sicp-stream-mapn + r s))

(define integers2 
  (sicp-stream-cons 1 (stream-add ones integers2)))

(define fibonacci2 
  (sicp-stream-cons 
   0 
   (sicp-stream-cons 1 
                     (stream-add (sicp-stream-cdr fibonacci2) 
                                 fibonacci2))))

;; Interpreting this.



;;       0  1  1  2  3   5   8   13   21  ...  F
;;       1  1  2  3  5   8  13   21  ...       cdr(F)
;;       1  2  3  5  8  13  21  ...            F + cdr(F)
;; 0  1  1  2  3  5  8  13  21  ...   cons 0, 1 on F + cdr(F)








;; Exercise 3.53 ========================================
