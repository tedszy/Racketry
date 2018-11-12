;; sicp-4a.rkt

#lang racket

;; Exercise 4.1 =======================================

;; Rewrite map-to-values so that you are certain
;; that it evaluates left-to-right.

;; (define (map-to-values exprs scope)
;;   (if (no-operands? exprs)
;;      '()
;;      (cons (evaluate (first-operand exprs) scope)
;;            (reduce-to-values (rest-operands exprs)))))

(define actions (list (lambda () (displayln "foo evaluated") 'foo)
                      (lambda () (displayln "goo evaluated") 'goo)
                      (lambda () (displayln "soo evaluated") 'soo)
                      (lambda () (displayln "zarf evaluated") 'zarf)))

;; Redefine cons using macros to get a right and left version.
;; consr evaluates left to right. consl evaluates right to left.
;; Then test this idea with consr and consl. We must use a macro
;; because if we did (define (consl x y) ...) then the function
;; call would evaluate arguments left to right no matter what.
;; So this calls for a macro.

(define-syntax-rule (consl x y)
  (let ((tmp y))
    (cons x tmp)))

(define-syntax-rule (consr x y)
  (let ((tmp x))
    (cons tmp y)))

(define (map-to-values-right actions)
  (if (empty? actions)
      '()
      (consr ((car actions)) 
             (map-to-values-right (cdr actions)))))

(define (map-to-values-left actions)
  (if (empty? actions)
      '()
      (consl ((car actions)) 
             (map-to-values-left (cdr actions)))))

(define (map-to-values-consr actions)
  (if (empty? actions)
      '()
      (let ((tmp ((car actions))))
        (consr tmp 
               (map-to-values-right (cdr actions))))))

(define (map-to-values-consl actions)
  (if (empty? actions)
      '()
      (let ((tmp ((car actions))))
        (consl tmp 
               (map-to-values-consl (cdr actions))))))

(newline)

(displayln "Evaluate using consr...")
(map-to-values-right actions)
(newline)

(displayln "Evaluate using consl...")
(map-to-values-left actions)
(newline)

(displayln "Force left-to-right evaluation with consr")
(map-to-values-consr actions)
(newline)
(displayln "Force left-to-right evaluation with consl")
(map-to-values-consl actions)


;; Exercise 4.2 =======================================
