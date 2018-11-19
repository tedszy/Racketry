;;; lambda-calculus.rkt

#lang racket

;; True is Lxy.x. False is Lxy.y. 
;; AND is Lxy.xyx or Lxy.xyF
;; OR is Lxy.xxy
;; NOT is Lx.xFT
;; XOR => if x then not y else y

(define T (lambda (x y) x))
(define F (lambda (x y) y))

(define NOT (lambda (x) (x F T)))

(define AND (lambda (x y) (x y x))) 
(define OR (lambda (x y) (x x y)))
(define XOR (lambda (x y) (x (NOT y) y)))


