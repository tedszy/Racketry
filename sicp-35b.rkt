;; sicp-35b.rkt

#lang racket

(require "sicp-35-streams-api.rkt")


;; Exercise 3.51 ========================================

(define (show u) (newline) (display u) u)
(define y (smake-interval 0 10))
;;(define x (smap show y))


;; Compare with native Racket streams.
;; (define (stream-interval a b)
;;   (if (> a b)
;;       empty-stream
;;       (stream-cons a (stream-interval (+ a 1) b))))
;; (define yy (stream-interval 0 10))
;; (define xx (stream-map show yy))

;; (sref x 5) ==> displays newline, the elements from 1 to 5 
;;                on new lines and then returns 5. For some 
;;                reason the 0th element is not displayed.
;;
;; So, a good question is why 0 is not displayed? Even doing
;; (sref x 0) returns 0 but does not display it.
;;
;; And I noticed that the definition 
;;
;;  (define x (smap show y))
;;
;; Causes a newline and 0 to be printed out when this file 
;; is loaded into Racket. 



;; ==> I GET IT!! This will take some explaining though!!
;;
;; sicp-35b.rkt﻿> (lambda () (displayln 'zarf))
;; #<procedure>
;; sicp-35b.rkt﻿> ((lambda () (displayln 'zarf)))
;; zarf
;; sicp-35b.rkt﻿> 


;; If we built these with Racket's native streams we'd
;; get diffetent behavior: newline, 5 is displayed, and
;; 5 is returned. 




;; Exercise 3.52 ========================================

(define sum 0)

(define (accumulate x)
  (set! sum (+ x sum)) 
  sum)

;; Triangular numbers.
(define seq (smap accumulate (smake-interval 1 20)))

;; Even triangular numbers.
(define seq2 (sfilter even? seq))

;; Triangular numbers divisible by 5.
(define seq3  (sfilter (lambda (x) (= (remainder x 5) 0)) seq))

;; sicp-35b.rkt﻿> (ls seq)
;;
;; [1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...]
;;
;; sicp-35b.rkt﻿> (ls seq2)
;;
;; [6, 224, 230, 254, 264, 300, 314, 362, 380, ()]
;;
;; sicp-35b.rkt﻿> (ls seq3)
;;
;; [15, 430, 445, 475, 500, 545, 580, ()]

