;;; sicp-35a-intro.rkt

#lang racket

(require "simple-table.rkt")

;; We need a simple prime test to do the efficiency example.
;; Find the smallest divisor > 1 of n. If it is n, then n
;; must be prime.
(define (prime? n)
  (define (square x) (* x x))
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (add1 test-divisor)))))
  (if (= n 1) #f (= n (smallest-divisor n))))

;; Sum all primes in the interval [a, b] by iterating from a to b
;; and accumulating the primes into result.
(define (sum-primes-in-interval-1 a b)
  (let loop ((n a) (result 0))
    (cond ((> n b) result)
          ((prime? n) (loop (add1 n) (+ result n)))
          (else (loop (add1 n) result)))))

;; Now we do this a more inefficient way, by costructing
;; lists and operating on them by higher-order functions.
;; Large lists are made, then mapped, filtered, reduced.

(define (make-interval a b)
  (if (> a b) '() (cons a (make-interval (add1 a) b))))
(define (sum-primes-in-interval-2 a b)
  (foldl + 0 (filter prime? (make-interval a b))))

(define (performance intervals)
  (print-table #:bars true #:head true 
   (cons (list "lower" "upper" "sum1 time" "sum2 time") 
         (map (lambda (interval)
                (let ((a (car interval))
                      (b (cadr interval)))
                  (map number->string 
                       (list a 
                             b
                             (let ((tt (current-milliseconds)))
                               (begin (sum-primes-in-interval-1 a b)
                                      (- (current-milliseconds) tt)))
                             (let ((tt (current-milliseconds)))
                               (begin (sum-primes-in-interval-2 a b)
                                      (- (current-milliseconds) tt)))))))
              intervals))))

(define (performance-table)
  (performance '((10000 100000)
                 (10000 200000)
                 (10000 300000)
                 (10000 400000))))

;; Let's compare the times to sum all primes in the iterval,
;; as done by both the efficient iterative method and the
;; inefficient list-map-reduce-HOF method. The difference
;; is not as big as I expected!

;; sicp-35a-intro.rkt﻿> (performance-table)
;;
;; lower    upper   sum1 time   sum2 time
;; --------------------------------------
;; 10000 | 100000 |       135 |       171
;; 10000 | 200000 |       359 |       395
;; 10000 | 300000 |       633 |       687
;; 10000 | 400000 |       948 |      1016


;; Another example of inefficiency: finding the 2nd 
;; prime in an interval. If we do this by constructing
;; lists, we have to construct the entire interval and filter
;; all of it before we extract the 2nd element of what is
;; left (the primes). So this is extremely inefficient.

(define (get-second-prime a b)
  (second (filter prime? (make-interval a b))))

(define (second-prime-performance-table get-second-prime)
  (let ((intervals '((10000 100000)
                     (10000 200000)
                     (10000 400000))))
    (print-table #:bars true #:head true
                 (cons (list "lower" "upper" "2nd prime" "time" )
                       (map (lambda (interval)
                              (let ((a (car interval))
                                    (b (cadr interval)))
                                (map number->string
                                     (let ((tt (current-milliseconds))
                                           (second-prime (get-second-prime a b)))
                                       (list a
                                             b
                                             second-prime
                                             (- (current-milliseconds) tt))))))
                            intervals)))))

;; sicp-35a-intro.rkt﻿> (second-prime-performance-table get-second-prime)
;;
;; lower    upper   2nd prime   time
;; ---------------------------------
;; 10000 | 100000 |     10009 |   98
;; 10000 | 200000 |     10009 |  171
;; 10000 | 400000 |     10009 |  445

;; We will see soon that this is very slow when compared to streams.
;; It's also bad that the time to find the same prime increases.

;; What we want is a method of programming that allows us to use the
;; same kind of map-reduce-list-HOF programming idioms, but with
;; efficiency more similar to iteration. The answer is streams.
