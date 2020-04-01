;;; sicp1-2f-primality.rkt
;;;
;;; Exercises 1.21 -- 1.27.
;;;
;;; Prime testing, divisors, mod exponential, Fermat test,
;;; performance comparisons, log(n) growth of Fermat test,
;;; Charmichael numbers.

#lang racket

(require rackunit
         math/base
         "format-table.rkt")

;; A brute-force Theta(sqrt(n)) order of growth
;; primality test. 
;;
;; Find the smallest divisor >= 2 of n.
;; We try divisors d up to d^2 <= n.
;;
;; If remainder of n/d is 0, then answer is d.
;; If no divisor such that d^2 <= n is found then
;; n itself is the only such divisor. 
;; And n is therefore prime. 
;;
;; Why? Because if d|n then n/d | n also. It's not
;; possible that both are larger than sqrt(n) because
;; if so then d * n/d would be greater than n.
;; One of these divisors must be smaller than sqrt(n).
;; If there isn't one below sqrt(n) then there isn't
;; one bigger than sqrt(n) either.
(define (get-smallest-divisor n)
  (let loop ((d 2))
    (cond ((> (* d d) n)
           n)
          ((= (remainder n d) 0)
           d)
          (else 
           (loop (add1 d))))))

(define (prime? n)
  (= n (get-smallest-divisor n)))

;; Fermat's theorem gives a Theta(log(n) primality test.
;;
;; Fermat's theorem says that 
;;
;;    if q is prime then a^q = a mod q, a < q.
;;
;; So if q is prime, all the residues 'a' satisfy a^q = a mod q.
;;
;; Logical converse of this is: 
;;
;;    If a^q is not a mod q for some a<q, then q is not prime.
;;
;; If we keep testing these residues, and we find one
;; for which the condition a^q = a mod q is not true,
;; then for sure q is NOT prime.
;;
;; But what happens if we test all the residues 'a'
;; from 1 to q-1 and the condition is true for all of them?
;; The Fermat theorem does NOT tell us that q must be prime.
;; q may be prime or it may be composite. In fact there
;; are rare composite numbers (Charmichael numbers) for
;; which all residues 'a' satisfy the condition.
;;
;; The fact that the Charmichael numbers are rare
;; leads us to a probabilistic prime test. If many a's
;; pass the Fermat condition then q is probably a prime.
;;
;; We need a fast mod-exponential to evaluate a^q mod q.
;; Let's write a tail-recursive one.

;; This does a^q mod q (just what we need for Fermat test.)
(define (mod-expt a q)
  (define (mod-square x) (remainder (* x x) q))
  (let loop ((a a) (qq q) (result 1))
    (cond ((= qq 0) result)
          ((odd? qq) (loop a
                           (- qq 1)
                           (remainder (* a result) q)))
          (else (loop (mod-square a)
                      (/ qq 2)
                      result)))))

;; Compare with SICP version which is tree-recursive.
;; This does base^exp mod m.
(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
         (remainder (* base (expmod base (sub1 exp) m)) m))))
 
;; Some tests.
(check-equal? (mod-expt 119 19) (expmod 119 19 19))
(check-equal? (mod-expt 310 18) (expmod 310 18 18))
(check-equal? (mod-expt 640 29) (expmod 640 29 29))
(check-equal? (mod-expt 110 100) (expmod 110 100 100))

;; Choose a random residue 'a' and do the test a^q = a mod q.
;; For big integers, use random-integer from math/base module:
;;
;;   (random-integer a k)
;;
;; this returns a random integer n: 1 <= n < k.
(define (fermat-test q)
  (let ((a (random-integer 1 q)))
    (= (mod-expt a q) a)))

;; Do the Fermat test a certain number of times. If q passes
;; all these tests it is probably prime, but if q fails it is
;; for sure not prime.
(define (ft-prime? q trials)
  (cond ((= trials 0) true)
        ((fermat-test q) (ft-prime? q (sub1 trials)))
        (else false)))

(check-true (ft-prime? 641 500) "should be true for prime")
(check-true (ft-prime? 64111111111111 500) "should be true for big prime")
(check-false (ft-prime? 1000071 500) "should be false for composite")
(check-true (ft-prime? 6601 500) "should be fooled by Charmichael")
(check-false (prime? 6601) "should be false for composite Charmichael")

;; But wait a sec! Do we know how false and true
;; residues are distributed? What if for some composites,
;; 99.9999% of the residues will yield true and only a
;; few yield false? The chance of a false positive is
;; then pretty high. Doing many trials might not even
;; help much! We have to convince ourselves that with
;; each trial, the probability of making a correct
;; determination goes up.

;; Check the condition a^q = a mod q for all residues a<q
;; and make a table of results:

(define (fermat-statistics . qs)
  (displayln
   (format-table
    (cons
     (list "q" "# true" "# false" "% true" "% false")
     (map (lambda (q)
            (let loop ((num-a-true 0) (num-a-false 0) (a 1))
              (if (= a q)
                  (list (number->string q)
                        (number->string num-a-true)
                        (number->string num-a-false)
                        (format-real (* 100.0 num-a-true (/ (- q 1))) 5)
                        (format-real (* 100.0 num-a-false (/ (- q 1))) 5))
                  (if (= (mod-expt a q) a)
                      (loop (+ num-a-true 1)
                            num-a-false
                            (+ a 1))
                      (loop num-a-true
                            (+ num-a-false 1)
                            (+ a 1))))))
          qs)))))

;; Try it on a prime, a big prime, a Charmichael,
;; products of 2 and 3 primes, and a highly composite number.
;;
;; (fermat-statistics 641
;;                    5211881
;;                    6601
;;                    (* 641 19)
;;                    (* 41 31 23)
;;                    (* 2 3 4 5 6 7 8))
;;
;;       q |  # true | # false |    % true |  % false
;;     641 |     640 |       0 | 100.00000 |  0.00000
;; 5211881 | 5211880 |       0 | 100.00000 |  0.00000
;;    6601 |    6600 |       0 | 100.00000 |  0.00000
;;   12179 |       8 |   12170 |   0.06569 | 99.93431
;;   29233 |     188 |   29044 |   0.64313 | 99.35687
;;   40320 |      15 |   40304 |   0.03720 | 99.96280
;;
;; Well this is good news. Most of the residues are false
;; when q is composite. If this pattern holds up for all
;; normal (non-Charmichael) composites then the chance of
;; getting a false positive after 100 trials is vanishingly
;; small.


;; Exercise 1.21 ========================================

;; (get-smallest-divisor 199)   ==> 199
;; (get-smallest-divisor 1999)  ==> 1999
;; (get-smallest-divisor 19999) ==> 7


;; Exercise 1.22 ========================================

;; We dont have 'runtime' in Racket but we do have 'time'
;; and 'current-milliseconds'.

(define (timed-prime? q)
  (let ((t1 (current-milliseconds)))
    (if (prime? q)
        (begin
          (displayln
           (string-join
            (list "*** prime:"
                  (number->string (- (current-milliseconds)
                                     t1))
                  "milliseconds.")))
          true)
        false)))

;; (timed-prime? 64111111111111)
;; *** prime: 113 milliseconds.
;; #t

;; Find first 3 primes above N = 1000, 10,000, 100,000, 1,000,000.
;; Test the hypothesis that this procedure is Theta(sqrt(n)).
;; Higher N times should be about sqrt(10) times longer than 
;; lower N times.
;;
;; Computers are a lot faster these days so we must use much
;; larger N to get measurable milliseconds:
;;
;;   N = 10^11, 10^12, 10^13.
;;
;; SICP wants me to skip over even numbers since they 
;; cannot be prime. Also, we want to refactor this problem 
;; in such a way as to work with simple-table:
;;
;; (first-three-primes-larger-than (expt 10 12))
;; ==> '((1000000000039 33) (1000000000061 29) (1000000000063 29))
;; Gives a list of the found primes and their computation times.
(define (first-three-primes-larger-than N)
  (let loop ((N (if (even? N) (add1 N) (+ N 2))) 
             (count 0) 
             (start-time (current-milliseconds))
             (result '()))
    (cond ((= count 3)
           (reverse result))
          ((prime? N)
           (loop (+ N 2)
                 (add1 count)
                 (current-milliseconds)
                 (cons (list N (- (current-milliseconds) start-time))
                       result)))
          (else
           (loop (+ N 2)
                 count
                 (current-milliseconds)
                 result)))))

;; Aggregate the results into a table.
;; 
;;   limit       primes       milliseconds
;;
(define (prime-time-table limit-list)
  (format-table #:header-char #\-
                #:separator " | "
               (cons (list "lower limit" "primes" "msecs")
                     (map (lambda (N)
                            (let ((row (first-three-primes-larger-than N)))
                              (list
                               (number->string N)
                               (string-join (map (lambda (u)
                                                   (number->string (car u)))
                                                 row)
                                            ", ")
                               (number->string (apply + (map cadr row))))))
                          limit-list))))

(define *limit-list* 
  (map (lambda (e) 
         (expt 10 e))
       (list 10 11 12 13 14)))

;; (displayln (prime-time-table *limit-list*))
;;
;;     lower limit                                              primes   msecs
;; ---------------------------------------------------------------------------
;;     10000000000 |             10000000019, 10000000033, 10000000061 |     9
;;    100000000000 |          100000000003, 100000000019, 100000000057 |    28
;;   1000000000000 |       1000000000039, 1000000000061, 1000000000063 |    88
;;  10000000000000 |    10000000000037, 10000000000051, 10000000000099 |   282
;; 100000000000000 | 100000000000031, 100000000000067, 100000000000097 |   891
;;
;; Now, sqrt(10) ~ 3.16 and we see...
;;    
;;      9 * 3.16 =  28.44
;;     28 * 3.16 =  88.48
;;     88 * 3.16 = 278.08
;;    282 * 3.16 = 891.12
;;
;; which are very close to the observed times!


;; Exercise 1.23 ========================================

;; The simple method of testing primality, prime? with
;; get-smallest-divisor, iterates through candidate divisors
;; consecutively. But we don't need to test any even
;; divisors if 2 does not divide n. So we will create a new
;; version of get-smallest-divisor having this refinement.
;;
;; (next d) => 3
;; (next d>2) => k+2.
;;
;; Since d starts at 2, next will produce 3, 5, 7, etc
;;
;; prime2? iterates over half the divisors that prime? does.
;; Does it run twice as fast? 

(define (get-smallest-divisor2 n)
  (define (next d) (if (= d 2) 3 (+ d 2)))
  (let loop ((d 2))
    (cond ((> (* d d) n) n)
          ((= (remainder n d) 0) d)
          (else (loop (next d))))))

(define (prime2? n)
  (= n (get-smallest-divisor2 n)))

;; We will construct a table to compare two prime predicate
;; functions and calculate the ratio of their performance
;; times. To do this we need some helper code.

;; This takes a predicate function and returns a new
;; multiple-value function returning the time elapsed
;; along with the result of predicate.
(define (make-prime-test-timer prime-predicate)
  (lambda (candidate)
    (let ((t1 (current-milliseconds)))
      (let ((is-prime? (prime-predicate candidate)))
        (values (- (current-milliseconds) t1)
                is-prime?)))))

(define prime-time-1 (make-prime-test-timer prime?))
(define prime-time-2 (make-prime-test-timer prime2?))

;; Construct performance table for exercise 1.23.
(define (time-table-123 timed-prime-predicate-A
                        timed-prime-predicate-B . my-primes)
  (displayln
   (format-table
    (cons (list "p" "prime test A" "prime test B" "ratio A/B")
          (map (lambda (p)
                 (let-values (((timeA resultA) (timed-prime-predicate-A p))
                              ((timeB resultB) (timed-prime-predicate-B p)))
                   (list (format "~a" p)
                         (format "~a ~ams" resultA timeA)
                         (format "~a ~ams" resultB timeB)
                         (format-real (/ timeA timeB 1.0) 2))))
               my-primes)))))

;; Applying this to the 12 big primes we discovered earlier...
;;
;; > (time-table-123 prime-time-1 prime-time-2
;;                  10000000019 10000000033 10000000061
;;                  100000000003 100000000019 100000000057
;;                  1000000000039 1000000000061 1000000000063
;;                  10000000000037 10000000000051 10000000000099
;;                  100000000000031 100000000000067 100000000000097)
;;
;;               p | prime test A | prime test B | ratio A/B
;;     10000000019 |       #t 1ms |       #t 1ms |  1.00
;;     10000000033 |       #t 2ms |       #t 1ms |  2.00
;;     10000000061 |       #t 2ms |       #t 1ms |  2.00
;;    100000000003 |       #t 5ms |       #t 3ms |  1.67
;;    100000000019 |       #t 5ms |       #t 3ms |  1.67
;;    100000000057 |       #t 5ms |       #t 3ms |  1.67
;;   1000000000039 |      #t 14ms |      #t 10ms |  1.40
;;   1000000000061 |      #t 14ms |      #t 10ms |  1.40
;;   1000000000063 |      #t 14ms |       #t 9ms |  1.56
;;  10000000000037 |      #t 45ms |      #t 31ms |  1.45
;;  10000000000051 |      #t 43ms |      #t 31ms |  1.39
;;  10000000000099 |      #t 44ms |      #t 31ms |  1.42
;; 100000000000031 |     #t 141ms |      #t 97ms |  1.45
;; 100000000000067 |     #t 140ms |      #t 98ms |  1.43
;; 100000000000097 |     #t 140ms |      #t 97ms |  1.44
;;
;; Can we explain why the ratio is 1.44 and not 2?
;;
;; It's true that the improved version iterates over half
;; of the possible divisors. But for each iteration it
;; introduces a function call (next), and a comparison
;; with the divisor 2. We guess that this adds extra time
;; for each divisor test, making the ratio of A/B less than 2.
;; Let's test this guess by (1) moving the test for d=2 out
;; of the loop entirely, and (2) inlining the function:

(define (get-smallest-divisor3 n)
  (if (= (remainder n 2) 0)
      2
      (let loop ((d 3))
        (cond ((> (* d d) n) n)
              ((= (remainder n d) 0) d)
              (else (loop (+ d 2)))))))

(define (prime3? n)
  (= n (get-smallest-divisor3 n)))

(define prime-time-3 (make-prime-test-timer prime3?))

;; Check it on a bigger prime.
;;
;; > (time-table-123 prime-time-1 prime-time-3 1000000000000091)
;;
;;                p | prime test A | prime test B | ratio A/B
;; 1000000000000091 |     #t 449ms |     #t 220ms |      2.04
;;
;; Yes! Now the ratio is close enough to 2!


;; Exercise 1.24 ========================================

;; A study of the run times for the fast Fermat test.
;; The Fermat test has Theta(log n) growth.
;; Check this for the 12 primes we found above. Is it so?
;; If not, can we explain why?

;; It's convenient it is to have this higher order function!
(define ft-prime-time (make-prime-test-timer
                       (lambda (p)
                         (ft-prime? p 10000))))

(define (another-time-table timed-prime-predicate . my-primes)
  (displayln
   (format-table
    (cons (list "prime" "ft results" "time/log(prime)")
          (map (lambda (p)
                 (let-values (((runtime result)
                               (timed-prime-predicate p)))
                   (list (number->string p)
                         (format "~a ms, ~a" runtime result)
                         (format-real (/ runtime (log p 10)) 7))))
               my-primes)))))

;; (another-time-table ft-prime-time
;;                     10000000019 10000000033 10000000061
;;                     100000000003 100000000019 100000000057
;;                     1000000000039 1000000000061 1000000000063
;;                     10000000000037 10000000000051 10000000000099
;;                     100000000000031 100000000000067 100000000000097)
;;
;; Running this a few times and dropping results which
;; have anomalously long times (I assume it's garbage
;; collection), we get...
;;
;;           prime | ft results | time/log(prime)
;;     10000000019 | 144 ms, #t | 14.4000000
;;     10000000033 | 111 ms, #t | 11.1000000
;;     10000000061 | 115 ms, #t | 11.5000000
;;    100000000003 | 149 ms, #t | 13.5454545
;;    100000000019 | 150 ms, #t | 13.6363636
;;    100000000057 | 155 ms, #t | 14.0909091
;;   1000000000039 | 156 ms, #t | 13.0000000
;;   1000000000061 | 160 ms, #t | 13.3333333
;;   1000000000063 | 163 ms, #t | 13.5833333
;;  10000000000037 | 167 ms, #t | 12.8461538
;;  10000000000051 | 170 ms, #t | 13.0769231
;;  10000000000099 | 170 ms, #t | 13.0769231
;; 100000000000031 | 187 ms, #t | 13.3571429
;; 100000000000067 | 182 ms, #t | 13.0000000
;; 100000000000097 | 182 ms, #t | 13.0000000
;;
;; The fit is pretty good for large primes. time/log(prime)
;; is roughly constant. So we can conclude that the computation
;; growth is Theta(log(n)).
;;
;; Why log(n)? Because most of the work is done
;; by mod-expt which is a Theta(log(n)) algorithm. 


;; Exercise 1.25 ========================================


;; Exercise 1.26 ========================================


;; Exercise 1.27 ========================================

;; Charmicheal numbers fool the FT test.
;; If q is a Charmichael number then
;;
;;    a^q = a mod q for all a < q.
;;
;; but q is not prime. The first few
;; Charmichael numbers are:
;;
;;    561, 1105, 1729, 2465, 2821, 6601...
;;
;; Let's verify that they fool the Fermat test.

(define (fermat-test-all q)
  (for/and ((a (range 1 q)))
    (= (mod-expt a q) a)))

;; Make a table: we check that fermat test passes for
;; all a < q. Then we verify the slow way, by finding
;; divisors, that q is indeed NOT prime.

(define (charmichael-table)
  (let ((charmichaels (list 561 1105 1729 2465 2821 6601)))
    (displayln
     (format-table
      (cons (list "charmichael q" "FT all a<q" "prime? q")
            (map (lambda (q)
                   (map (lambda (x)
                          (format "~a" x))
                        (list q
                              (fermat-test-all q)
                              (prime? q))))
                 charmichaels))))))

;; > (charmichael-table)
;;
;; charmichael q | FT all a<q | prime? q
;;           561 |         #t |       #f
;;          1105 |         #t |       #f
;;          1729 |         #t |       #f
;;          2465 |         #t |       #f
;;          2821 |         #t |       #f
;;          6601 |         #t |       #f
;;
;; They indeed fool the Fermat test, no matter
;; how many trials!

;; We might as well try to find the next few
;; Charmichael numbers (by brute force.)

(define (charmichael-search start stop)
  (let loop ((q start) (result empty))
    (cond ((= q stop) (reverse result))
          ((and (fermat-test-all q)
                (not (prime? q)))
           (loop (+ q 1)
                 (cons q result)))
          (else (loop (+ q 1)
                      result)))))

;; > (charmichael-search 5000 50000)
;;
;; '(6601 8911 10585 15841 29341 41041 46657)
;;
;; The ratio of Charmichaels to normal numbers
;; in this range is 0.0156%. But (I read elsewhere)
;; that the percentage of Charmichaels becomes
;; smaller and smaller as numbers become larger.


