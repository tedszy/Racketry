#lang racket

(require rackunit)
(provide check-me)

;; args-and-expected is a list of pairs (args . expected).
;; args are the arguments that the function is applied to,
;; expected is the value that you're supposed to get.
(define (check-me function . args-and-expected)
  (for-each (lambda (u)
              (check-equal? 
               (apply function (car u))
               (cdr u)))
            args-and-expected))

