;;; lambda-calculus.rkt

#lang racket

(require "simple-table.rkt")

;; Valid formulas.
;;
;; p v ~p
;; (p -> q) <=> (~p -> ~q)
;; ((~p -> q) & (~p -> ~q)) -> p
;; ~(p & q) <=> (~p v ~q)
;; ((p -> q) & (p -> r)) -> r
;; ((p & q) -> r) <=> (p -> (q -> r))
;; ((p & q) & (p -> r)) v ~(p v q) v ~(p -> r)

;; Lambda expressions.
;;
;; True  = Lxy.x. 
;; False = Lxy.y. 
;; NOT   = Lx.xFT
;; AND   = Lxy.xyx = Lxy.xyF
;; OR    = Lxy.xxy = Lxy.xTy
;; XOR   = Lxy.x(NOT y)y 
;; ->    = Lxy.(OR (NOT x) y)
;; <=>   = Lxy.(NOT (XOR x y))
;;
;; Its possible to define everything in terms of "if x then y else z."

(define T (lambda (x y) x))
(define F (lambda (x y) y))

;; Convenient boolean symbols. We can use anything.
;; The lambda expressions are applied to these to
;; get an easy-to-understand answer.
(define booleans (list 'True 'False))
(define boolean->lambda (lambda (x) (if 'True T F)))

(define NOT (lambda (x) (x F T)))
(define AND (lambda (x y) (x y x))) ;; (x y F)
(define OR (lambda (x y) (x x y)))  ;; (x T y) makes perfect sense.
(define XOR (lambda (x y) (x (NOT y) y)))
(define IMPLIES (lambda (x y) (OR (NOT x) y)))
(define IFF (lambda (x y) (NOT (XOR x y))))

(define cartesian-2 (for*/list ((x booleans)
                                (y booleans))
                      (list x y)))

(define cartesian-3 (for*/list ((x booleans)
                                (y booleans)
                                (z booleans))
                      (list x y z)))

(define (truth-table-2 f)
  (map (lambda (u)
         (match-let (((list x y) u))
           (list x y ((f (boolean->lambda x)
                         (boolean->lambda y)) 'True 'False))))
       cartesian-2))

(define (truth-table-3 f)
  (map (lambda (u)
         (match-let (((list x y z) u))
           (list x y z ((f (boolean->lambda x)
                           (boolean->lambda y)
                           (boolean->lambda z)) 'True 'False))))
       cartesian-3))

(define (display-truth-table tt formula . args)
  (let ((tt (cons (append args (list formula)) 
                  (map (lambda (row)
                         (map symbol->string row))
                       tt))))
    (print-table tt 
                 #:head #t 
                 #:bars #t 
                 #:align (append (make-list (length args) 'left) 
                                 (list 'center)))))

;; Valid formula p->q <=> ~q->~p.
(define f (lambda (p q)
            (IFF (IMPLIES p q) 
                 (IMPLIES (NOT q) (NOT p)))))

;; Valid formula ~(p v q) <=> ~p & ~q.
(define g (lambda (p q)
            (IFF (NOT (OR p q))
                 (AND (NOT p)
                      (NOT q)))))

;; Valid formula ((p & q) -> r) <=> (p -> (q -> r))
(define h (lambda (p q r)
            (IFF (IMPLIES (AND p q) r)
                 (IMPLIES p (IMPLIES q r)))))

(newline)
(display-truth-table (truth-table-2 f) "p->q <=> ~q->~p" "p" "q")
(newline)
(display-truth-table (truth-table-2 g) "~(p v q) <=> ~p & ~q" "p" "q")
(newline)
(display-truth-table (truth-table-3 h) "((p & q) -> r) <=> (p -> (q -> r))" "p" "q" "r")
