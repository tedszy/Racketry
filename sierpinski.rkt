;; Sierpinski triangle using pict graphics module.

#lang racket

(require pict
         math/number-theory)

(define square-size 50)

;; > odd-square ==> emacs displays square.

(define (square size color)
  (colorize (filled-rectangle size size) 
            color))

(define even-square (square square-size "green"))
(define odd-square (square square-size "red"))
(define empty-square (square square-size "black"))

;; Test these squares with hc-append and vc-append.
;;
(define foo 
  (hc-append odd-square empty-square even-square)) 
(define goo 
  (vc-append odd-square empty-square even-square))

;; You can use these definitions as any other
;; defined symbol. Rather than evaluating to 
;; a number displayed in emacs, they evaluate
;; to a graphic displayed in Emacs.

(define test-row
  (list 1 0 6 0 15 0 20 0 15 0 5 0 1))

(define (render-row row)
  (apply hc-append 
         (map (lambda (u)
                (if (zero? u)
                    empty-square
                    (if (odd? u)
                        odd-square
                        even-square)))
              row)))

;; Try it:
;; 
;; (render-row test-row)


;; This won't look good. We need filler
;; in between the squares that represent t
;; the elements of Pascal's triangle.
;;
;;               1
;;              1 1
;;             1 2 1
;;            1 3 3 1
;;           1 4 6 4 1
;;         1 5 10 10 5 1
;;        1 6 15 20 15 6 1

;;               1 
;;             1 0 1
;;           1 0 2 0 1
;;         1 0 3 0 3 0 1
;;       1 0 4 0 6 0 4 0 1
;;     1 0 5 0 X 0 X 0 5 0 1
;;   1 0 6 0 X 0 X 0 X 0 6 0 1

;; This is what we want to create:
;;
;;   0 0 0 0 0 0 1 0 0 0 0 0 0
;;   0 0 0 0 0 1 0 1 0 0 0 0 0
;;   0 0 0 0 1 0 2 0 1 0 0 0 0
;;   0 0 0 1 0 3 0 3 0 1 0 0 0
;;   0 0 1 0 4 0 6 0 4 0 1 0 0
;;   0 1 0 5 0 X 0 X 0 5 0 1 0
;;   1 0 6 0 X 0 X 0 X 0 6 0 1


;; Total length of a zero-padded row is 2n+1.
;; Counting i starts from 0 to 2n+1

;; maybe we can eliminate k because it
;; can be computed from i ==> k = i/2

;; Even index ==> put in binomial(n, i/2)
;; Odd index ==> put in 0.
;; No need to reverse on return since
;; Pascal triangle rows are symmetrical
(define (pascal-row-zero-padded n)
  (let loop ((row '()) (i 0))
    (if (= i (+ (* 2 n) 1))
        row
        (if (even? i)
            (loop (cons (binomial n (/ i 2)) row)
                  (+ i 1))
            (loop (cons 0 row)
                  (+ i 1))))))

