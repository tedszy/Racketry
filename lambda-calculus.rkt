;;; lambda-calculus.rkt

#lang racket

;; Valid formulas.
;;
;; p v ~p
;; (p -> q) <=> (~p -> ~q)
;; ((~p -> q) & (~p -> ~q)) -> p
;; ~(p & q) <=> (~p v ~q)
;; ((p -> q) & (p -> r)) -> r
;; ((p & q) -> r) <=> (p -> (q -> r))
;; ((p & q) & (p -> r)) v ~(p v q) v ~(p -> r)


;; True is Lxy.x. False is Lxy.y. 
;; AND is Lxy.xyx or Lxy.xyF
;; OR is Lxy.xxy
;; NOT is Lx.xFT
;; XOR => if x then not y else y

(define T (lambda (x y) x))
(define F (lambda (x y) y))

(define booleans (list (list T 'T) (list F 'F)))

(define NOT (lambda (x) (x F T)))

(define AND (lambda (x y) (x y x))) 
(define OR (lambda (x y) (x x y)))
(define XOR (lambda (x y) (x (NOT y) y)))
(define IMPLIES (lambda (x y) (OR (NOT x) y)))
(define IFF (lambda (x y) (NOT (XOR x y))))

;; Valid formula p->q <=> ~q->~p.
(define f (lambda (p q)
            (IFF (IMPLIES p q) 
                 (IMPLIES (NOT q) (NOT p)))))

;; Valid formula ~(p v q) <=> ~p & ~q.
(define g (lambda (p q)
            (IFF (NOT (OR p q))
                 (AND (NOT p)
                      (NOT q)))))

(define logic-table (lambda (f)
                      (for*/list ((x booleans)
                                  (y booleans))
                        (list (cadr x) 
                              (cadr y) 
                              ((f (car x) (car y)) 'T 'F)))))

(logic-table f)
(logic-table g)

