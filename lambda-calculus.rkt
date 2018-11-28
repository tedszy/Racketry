;;; lambda-calculus.rkt

#lang racket

(require "simple-table.rkt")

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

(define *T (lambda (x y) x))
(define *F (lambda (x y) y))

;; Convenient boolean symbols. We can use anything.
;; The lambda expressions are applied to these to
;; get an easy-to-understand answer.
(define booleans (list 'True 'False))
(define boolean->lambda (lambda (x) (if 'True *T *F)))

(define *not (lambda (x) (x *F *T)))
(define *and (lambda (x y) (x y x))) ;; (x y F)
(define *or (lambda (x y) (x x y)))  ;; (x T y) makes perfect sense.
(define *xor (lambda (x y) (x (*not y) y)))
(define *implies (lambda (x y) (*or (*not x) y)))
(define *iff (lambda (x y) (*not (*xor x y))))

(define cartesian/1 booleans)

(define cartesian/2 (for*/list ((x booleans)
                                (y booleans))
                      (list x y)))

(define cartesian/3 (for*/list ((x booleans)
                                (y booleans)
                                (z booleans))
                      (list x y z)))

(define (truth-table/1 f)
  (map (lambda (u)
         (list u ((f (boolean->lambda u)) 'True 'False)))
       cartesian/1))

(define (truth-table/2 f)
  (map (lambda (u)
         (match-let (((list x y) u))
           (list x y ((f (boolean->lambda x)
                         (boolean->lambda y)) 'True 'False))))
       cartesian/2))

(define (truth-table/3 f)
  (map (lambda (u)
         (match-let (((list x y z) u))
           (list x y z ((f (boolean->lambda x)
                           (boolean->lambda y)
                           (boolean->lambda z)) 'True 'False))))
       cartesian/3))

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

;; Define some valid formulas (tautologies): f.../arity
;;
;; ~(p v q) <=> ~p & ~q" 
;; ((~p -> q) & (~p -> ~q)) -> p
;; ((p -> q) & (p -> r)) -> r

;; p v ~p
(define fa/1
  (lambda (p)
    (*or p (*not p))))

;; p->q <=> ~q->~p.
(define fb/2
  (lambda (p q)
    (*iff (*implies p q) (*implies (*not q) (*not p)))))

;; ~(p v q) <=> ~p & ~q.
(define fc/2
  (lambda (p q)
    (*iff (*not (*or p q)) (*and (*not p) (*not q)))))

;; ~(p & q) <=> (~p v ~q)
(define fd/2
  (lambda (p q)
    (*iff (*not (*and p q)) (*or (*not p) (*not q)))))

;; ((p & q) -> r) <=> (p -> (q -> r))
(define fe/3
  (lambda (p q r)
    (*iff (*implies (*and p q) r) (*implies p (*implies q r)))))

;;  ((p & q) & (p -> r)) v (~(p v q) v ~(p -> r))
(define ff/3
  (lambda (p q r)
    (*or (*and (*and p q)
               (*implies p r))
         (*or (*not (*or p q))
              (*not (*implies p r))))))

;; (~p xor ~q) <=> ~(p <=> q)
(define fg/2
  (lambda (p q)
    (*iff (*xor (*not p) (*not q))
          (*not (*iff p q)))))

(newline)
(display-truth-table (truth-table/1 fa/1)
                     "p v ~p"
                     "p")

(newline)
(display-truth-table (truth-table/2 fb/2)
                     "p->q <=> ~q->~p"
                     "p"
                     "q")

(newline)
(display-truth-table (truth-table/3 fe/3)
                     "((p & q) -> r) <=> (p -> (q -> r))"
                     "p"
                     "q"
                     "r")

(newline)
(display-truth-table (truth-table/3 ff/3)
                     "((p & q) & (p -> r)) v (~(p v q) v ~(p -> r))"
                     "p"
                     "q"
                     "r")

(newline)
(display-truth-table (truth-table/2 fg/2)
                     "(~p xor ~q) <=> ~(p <=> q)"
                     "p"
                     "q")


