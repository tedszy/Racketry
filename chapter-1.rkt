;;; chapter-1.rkt
;;;
;;; Exercises from chapter 1.

#lang racket

(require rackunit)


;; Exercise 1.1 ========================================

10                       ;; ==> 10
(+ 5 3 4)                ;; ==> 12
(- 9 1)                  ;; ==> 8
(/ 6 2)                  ;; ==> 3
(+ (* 2 4)
   (- 4 6))              ;; ==> 6
(define a 3)             ;; ==> none, a is 3.
(define b (+ a 1))       ;; ==> none, b is 4. 
(+ a b (* a b))          ;; ==> 19
(= a b)                  ;; ==> #f
(if (and (> b a)
         (< b (* a b)))
    b
    a)                   ;; ==> 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))         ;; ==> 16

(+ 2 (if (> b a) b a))   ;; ==> 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))              ;; ==> 16



;; Exercise 1.2 ========================================

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))         ;; ==> -37/150



;; Exercise 1.2 ========================================

;; Procedure f(x,y,z) returns sum of the squares
;; of the two larger arguments.

(define (square a) (* a a))

;; Sum of squares.
(define (sos a b) 
  (+ (square a) (square b)))

(define (sos-larger x y z) 
  (cond ((and (<= x y) (<= x z)) (sos y z))
        ((and (<= y x) (<= y z)) (sos x z))
        (else (sos x y))))

;; Make this into many-arg function testing.
(for-each (lambda (args expected) 
            (check-equal? 
             (sos-larger (car args) (cadr args) (caddr args))
             expected))
          (list '(1 1 1) '(1 1 3) '(1 2 3)
                '(3 2 1) '(2 3 1) '(3 3 1) 
                '(3 1 3) '(1 3 1))
          (list 2 10 13 
                13 13 18 
                18 10))



;; Exercise 1.4 ========================================

;; Describe how this works.

(define (a-plus-abs-b a b)
  ((if (> b a) + -) a b))

;; If b > a then you add a and b ==> a + (positive b) ==> a + |b|
;; If b < a then subtract, a - b ==> a - (negative b) ==> a + |b|
;; Clever!



;; Exercise 1.5 ========================================
