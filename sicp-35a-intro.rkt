;;; sicp-35a-intro.rkt

#lang racket

(require "format-table.rkt")

;; Use create a version of a function that also returns its execution time.
(define (make-profiled fn)
  (lambda args
    (let ((tt (current-milliseconds)))
      (values (apply fn args)
              (- (current-milliseconds) tt)))))

;; Now the function will format a string with
;; return value of fn and the time it took.
(define (make-format-profile timed-fn)
  (lambda args
    (let-values (((v t) (apply timed-fn args)))
      (format "~a, ~ams" v t))))
  
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

(define sum-primes-1
  (make-format-profile
   (make-profiled
    sum-primes-in-interval-1)))

(define sum-primes-2
  (make-format-profile
   (make-profiled
    sum-primes-in-interval-2)))

(define (sum-prime-performance-table)
  (let ((intervals '((10000 100000)
                     (10000 200000)
                     (10000 300000)
                     (10000 400000))))
    (displayln (format-table
                #:header-char #\- #:separator " | "
                (cons (list "upper" "lower" "sum-primes-1" "sum-primes-2")
                      (map (lambda (interval)
                             (let ((a (car interval))
                                   (b (cadr interval)))
                               (list (number->string a)
                                     (number->string b)
                                     (sum-primes-1 a b)
                                     (sum-primes-2 a b))))
                           intervals))))))

;; Let's compare the times to sum all primes in the iterval,
;; as done by both the efficient iterative method and the
;; inefficient list-map-reduce-HOF method. The difference
;; is not as big as I expected!
;;
;; sicp-35a-intro.rkt﻿> (sum-prime-performance-table)
;;
;; upper    lower        sum-primes-1        sum-primes-2
;; ------------------------------------------------------
;; 10000 | 100000 |   448660141, 63ms |   448660141, 66ms
;; 10000 | 200000 | 1703864417, 158ms | 1703864417, 171ms
;; 10000 | 300000 | 3703770718, 275ms | 3703770718, 306ms
;; 10000 | 400000 | 6453165135, 411ms | 6453165135, 449ms

;; Another example of inefficiency: finding the 2nd 
;; prime in an interval. If we do this by constructing
;; lists, we have to construct the entire interval and filter
;; all of it before we extract the 2nd element of what is
;; left (the primes). So this is extremely inefficient.

(define (get-second-prime a b)
  (second (filter prime? (make-interval a b))))

(define second-prime-1
  (make-format-profile
   (make-profiled
    get-second-prime)))

(define (second-prime-performance-table)
  (let ((intervals '((10000 100000)
                     (10000 200000)
                     (10000 400000))))
    (displayln
     (format-table #:separator " | "
                   #:header-char #\-
                   (cons (list "lower" "upper" "2nd prime" )
                         (map (lambda (interval)
                                (let ((a (car interval))
                                      (b (cadr interval)))
                                  (list (number->string a)
                                        (number->string b)
                                        (second-prime-1 a b))))
                              intervals))))))

;; sicp-35a-intro.rkt﻿> (second-prime-performance-table)
;;
;; lower    upper      2nd prime
;; -----------------------------
;; 10000 | 100000 |  10009, 65ms
;; 10000 | 200000 | 10009, 182ms
;; 10000 | 400000 | 10009, 443ms

;; We will see soon that this is very slow when compared to streams.
;; It's also bad that the time to find the same prime increases.

;; What we want is a method of programming that allows us to use the
;; same kind of map-reduce-list-HOF programming idioms, but with
;; efficiency more similar to iteration. The answer is streams.

(require "sicp-35-streams-api.rkt")

;; This version uses streams.
(define (get-second-prime-by-streams a b)
  (sicp-stream-ref 
   (sicp-stream-filter prime? (sicp-stream-make-interval a b)) 1))

(define second-prime-2
  (make-format-profile
   (make-profiled
    get-second-prime-by-streams)))

(define (compare-lists-streams-table)
  (let ((intervals '((10000 100000) 
                     (10000 200000) 
                     (10000 400000))))
    (displayln
     (format-table
      #:separator " | "
      #:header-char #\-
      (cons (list "lower" "upper" "2nd prime: lists" "2nd prime: streams")
            (map (lambda (interval)
                   (let ((a (car interval))
                         (b (cadr interval)))
                     (list (number->string a)
                           (number->string b)
                           (second-prime-1 a b)
                           (second-prime-2 a b))))
                 intervals))))))

;; Now the advantage of streams is revealed.
;;
;; sicp-35a-intro.rkt﻿> (compare-lists-streams-table)
;;
;; lower    upper   2nd prime: lists   2nd prime: streams
;; ------------------------------------------------------
;; 10000 | 100000 |      10009, 84ms |         10009, 0ms
;; 10000 | 200000 |     10009, 169ms |         10009, 0ms
;; 10000 | 400000 |     10009, 451ms |         10009, 0ms



