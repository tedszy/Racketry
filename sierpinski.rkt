;; Sierpinski triangle using pict graphics.

#lang racket

(require pict
         math/number-theory)

(define square-size 4)

;; > odd-square ==> emacs displays square.

(define (square size color)
  (colorize (filled-rectangle size size) 
            color))

(define even-square (square square-size "green"))
(define odd-square (square square-size "blue"))
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

;; The following won't look good when we render it
;; to little squares. 
;;
;;               1
;;              1 1
;;             1 2 1
;;            1 3 3 1
;;           1 4 6 4 1
;;         1 5 10 10 5 1
;;        1 6 15 20 15 6 1
;;
;; We need filler in between the 
;; elements of Pascal's triangle.
;; Let the filler be zero.
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
;;   0 0 0 0 1 0 2 0 1 0 0 0 0  <== 6 - 2 padding
;;   0 0 0 1 0 3 0 3 0 1 0 0 0
;;   0 0 1 0 4 0 6 0 4 0 1 0 0  <== 6 - 4 padding
;;   0 1 0 5 0 X 0 X 0 5 0 1 0
;;   1 0 6 0 X 0 X 0 X 0 6 0 1

;; Total length of a zero-padded row is 2n+1.
;;
;; No need to reverse list on return since
;; rows are symmetrical.
;;
;;   (1 0 6 0 15 0 20 0 15 0 6 0 1)
;; 
;; 2*6+1 elements.
;; even ==> cons binomial(n,k/2) on result.
;; odd  ==> cons 0 on result.
(define (pascal-row n)
  (let loop ((row '()) (k 0))
    (if (= k (+ (* 2 n) 1))
        row
        (if (even? k)
            (loop (cons (binomial n (/ k 2)) row)
                  (+ k 1))
            (loop (cons 0 row)
                  (+ k 1))))))

;; A table is a list of lists (rows). All rows
;; are the same length -- padded by zeros on both sides.

(define (make-table n)
  (let ((table (for/list ((k (+ n 1)))
                 (let ((zero-padding (make-list (- n k) 0)))
                   (append zero-padding
                           (pascal-row k)
                           zero-padding)))))
    table))

;; Render the Sierpinski triangle.

;; n = powers of 2 gives perfect results.
(define (render n)
  (apply vc-append 
         (map render-row (make-table n))))
