;; Sierpinski triangle using pict graphics.

#lang racket

(require math/number-theory ;; binomial
         pict)              ;; colorize, filled-rectangle,
                            ;; hc-append, vc-append

;; Color the even and odd elements of Pascal's 
;; triangle differently and you get the 
;; Sierpinski fractal.
;;
;; A triangle like this is too tight, it won't 
;; look good.
;;
;;             1
;;            1 1
;;           1 2 1
;;          1 3 3 1
;;         1 4 6 4 1
;;        1 5 X X 5 1
;;       1 5 X X X 6 1
;;
;; Instead we want to render the even and odd 
;; squares spread out against a background of 
;; squares.
;;
;; Each number ==> a graphic square.
;;                 odd  -> green
;;                 even -> blue
;;                 zero -> black (background)
;;
;;
;;         n = 6
;; k=0     0 0 0 0 0 0 1 0 0 0 0 0 0
;;         0 0 0 0 0 1 0 1 0 0 0 0 0       n - k 
;;     0 0 0 0     1 0 2 0 1     0 0 0 0   6 - 2 = 4
;;         0 0 0 1 0 3 0 3 0 1 0 0 0       padding
;;         0 0 1 0 4 0 6 0 4 0 1 0 0
;;         0 1 0 5 0 X 0 X 0 5 0 1 0
;; k=6     1 0 6 0 X 0 X 0 X 0 6 0 1
;;
;; Notice how we calculate the left and right 
;; padding. Also notice that a row has 2*n+1 
;; elements.
;;
;; Create lists like this:
;;
;;   (1 0 5 0 15 0 20 0 15 0 6 0 1)
;; 
;; Index goes from 0 to 2n. So stop at  i = 2n+1.
;; Even index ==> cons binomial(n,k/2) on result.
;; Odd index ==> cons 0 on result.
(define (pascal-row n)
  (let loop ((row empty) (k 0))
    (cond ((= k (+ (* 2 n) 1)) 
           row)
          ((even? k)
           (loop (cons (binomial n (/ k 2)) 
                       row)
                 (+ k 1)))
          (else
           (loop (cons 0 row)
                 (+ k 1))))))

;; A table is a list of rows. All rows must be 
;; the same length, padded by zeros on both sides.

(define (zeros n k)
  (make-list (- n k) 0))

(define (make-table n)
  (for/list ((k (+ n 1)))
    (append (zeros n k)
            (pascal-row k)
            (zeros n k))))

;; Pict graphics.

(define (square size color)
  (colorize (filled-rectangle size size) 
            color))

(define square-size 5)
(define even-square (square square-size "green"))
(define odd-square (square square-size "blue"))
(define background-square (square square-size "black"))

;; Test these squares with hc-append and vc-append.

(define foo (hc-append odd-square 
                       background-square 
                       even-square)) 
(define goo (vc-append odd-square 
                       background-square 
                       even-square))

;; You can use these definitions as any other 
;; defined symbol. Rather than evaluating to 
;; a number displayed in Emacs, they evaluate
;; to a graphic displayed in Emacs.

(define (number->square x)
  (cond ((zero? x) background-square)
        ((odd? x) odd-square)
        (else 
         even-square)))

(define (render-row row)
  (apply hc-append (map number->square row)))

;; Try it: (render-row test-row)
(define test-row
  (list 1 0 6 0 15 0 20 0 15 0 5 0 1))

;; n = powers of 2 gives perfect results.
(define (render n)
  (apply vc-append 
         (map render-row (make-table n))))



