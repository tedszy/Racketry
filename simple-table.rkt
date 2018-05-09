;;; simple-table.rkt
;;;
;;; Simple, no nonsense ways of displaying 
;;; list-of-lists in aligned columns.

#lang racket

(require racket/format)

(define (format-integer n)
  (number->string n))

;; (format-bits 1024 32) ==> "00000000000000000000010000000000"
(define (format-bits n width)
  (~r n #:base 2 #:min-width 32 #:pad-string "0"))

;; (format-real pi 5) ==> "3.14159"
(define (format-real x precision)
  (~r x #:precision (list '= precision)))

;; (format-string "zarf" 10 'left)              ==> "zarf      "
;; (format-string "zarf" 10 'right)             ==> "      zarf"
;; (format-string "zarf" 10 'center)            ==> "   zarf   "
;; (format-string (format-real pi 5) 15 'right) ==> "        3.14159" 
(define (format-string s width align)
  (~a #:width width #:align align s))

(define (transpose table)
  (apply map list table))
