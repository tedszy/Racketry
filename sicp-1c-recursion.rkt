;;; chapter-1c-recursion.rkt
;;;
;;;

#lang racket

(require "check.rkt"
         "simple-table.rkt")

;; This is tree-recursive. According to the applicative
;; evaluation model it works like this (call the function F).
;;
;; (F 4)
;; (* 4 (F 3))
;; (* 4 (* 3 (F 2)))
;; (* 4 (* 3 (* 2 (F 1))))
;; 
;; We hit the base case, (F 1) = 1. Now the function
;; calls (F 1), (F 2)... can be evaluated.
;;
;; (* 4 (* 3 (* 2 (F 1)))) ==> (* 4 (* 3 (* 2 1)))
;; (* 4 (* 3 (F 2)))       ==> (* 4 (* 3 2))
;; (* 4 (F 3))             ==> (* 4 6)
;; (F 4)                   ==> 24
(define (factorial n)
  (if (= n 1) 
      1 
      (* n (factorial (- n 1)))))

;; This one is linear-recursive (or tail-recursive).

;; Note use of free varialble n in factorial-iterator.
;; This way we don't have to keep passing it in the
;; function calls.
(define (factorial1 n)
  (define (factorial-iterator result k)
    (if (> k n)
        result
        (factorial-iterator (* result k)
                            (+ k 1))))
  (factorial-iterator 1 1))

;; Better yet, using named-let. It's much cleaner.
;; Let's trace the calls to loop when we do (factorial2 5).
;;
;; (loop 1 1)
;; (loop 1 2)
;; (loop 2 3)
;; (loop 6 4)
;; (loop 24 5)
;; (loop 120 6)
;; 6 > 5 so return result 120.
;;
;; Notice there's no growth of function call stack.
(define (factorial2 n)
  (let loop ((result 1) (k 1))
    (if (> k n)
        result
        (loop (* k result) (+ k 1)))))

;; Exercise 1.9 ========================================

;; Trace how my+ and my-other+ work on (+ 4 5).
;; Which one is linear and which one is tree recursive?
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (my+ a b)
  (if (= a 0) 
      b
      (inc (my+ (dec a) b))))
;; (my+ 4 5)
;; (+ 1 (my+ 3 5))
;; (+ 1 (+ 1 (my+ 2 5)))
;; (+ 1 (+ 1 (+ 1 (my+ 1 5))))
;; (+ 1 (+ 1 (+ 1 (+ 1 (my+ 0 5)))))
;; (my+ 0 5) is base case ==> returns 5
;; now the function calls can be evaluated.
;; (my+ 0 5) = 5
;; (my+ 1 5) = (+ 1 (my+ 0 5)) = 6
;; (my+ 2 5) = (+ 1 (my+ 1 5)) = 7
;; (my+ 3 5) = (+ 1 (my+ 2 5)) = 8
;; (my+ 4 5) = (+ 1 (my+ 3 5)) = 9
;; my+ is tree-recursive.

(define (my-other+ a b)
  (if (= a 0)
      b
      (my-other+ (dec a) (inc b))))
;; (my-other+ 4 5)
;; (my-other+ 3 6)
;; (my-other+ 2 7)
;; (my-other+ 1 8)
;; (my-other+ 0 9) ==> base case ==> returns 9
;; my-other+ is linear-recursive.



;; Exercise 1.10 ========================================

;; The Ackerman function.
(define (ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else
         (ackermann (- x 1)
                    (ackermann x (- y 1))))))

(check-me ackermann 
          '((1 10) . 1024)
          '((2 4)  . 65536)
          '((3 3)  . 65536))

;; Give mathematical descriptions of what these functions are.
(define (f n) (ackermann 0 n))
(define (g n) (ackermann 1 n))
(define (h n) (ackermann 2 n))
(define (k n) (* 5 n n))

;; Don't do ackerman table on n>4.
;; (ackermann-table 4)
;; n | f |  g |     h |  k
;; 1 | 2 |  2 |     2 |  5
;; 2 | 4 |  4 |     4 | 20
;; 3 | 6 |  8 |    16 | 45
;; 4 | 8 | 16 | 65536 | 80
;; 
;; From the table we conjecture 
;; f(n) = 2*n
;; g(n) = 2^n
;; h(n) = 2^[(2^(n-1)]^2
;; k(n) = 5*n^2.
(define (ackermann-table n)
  (print-table 
   (let loop ((i 1) (table (list (list "n" "f" "g" "h" "k"))))
     (if (> i n )
         (reverse table)
         (loop (+ i 1)
               (cons (map number->string 
                          (list i (f i) (g i) (h i) (k i)))
                     table))))))
