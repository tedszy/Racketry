;;; simple-table.rkt
;;;
;;; Simple, no nonsense tools for displaying 
;;; list-of-lists in aligned columns.

#lang racket

(require racket/format)

(provide format-integer
         format-bits
         format-real
         format-string
         transpose
         print-table)

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

;; A "table" is a list of lists of strings.

(define (transpose table)
  (apply map list table))

(define (get-max-length-of-string-list string-list)
  (apply max (map string-length string-list)))

(define (get-max-column-widths table)
  (map get-max-length-of-string-list (transpose table)))

(define standard-column-separator " | ")
(define standard-row-separator "\n")

;; The most common alignment that I use is left-aligned
;; for the first column, then right-aligned for all the rest.
(define (print-table table)
  (let ((column-widths (get-max-column-widths table))
        (alignments (cons 'left 
                          (make-list (sub1 (length (car table))) 'right)))
        (out (open-output-string)))
    (for-each (lambda (row)
                (let loop ((row row)
                           (column-widths column-widths)
                           (alignments alignments))
                  (let ((cell-text (format-string (car row)
                                                  (car column-widths)
                                                  (car alignments))))
                    (cond ((null? (cdr row))
                           ;; We are at the last cell of the row,
                           ;; so handle it differently. Put row
                           ;; separator instead of column separator.
                           ;; And don't recurse. We're done with the row.
                           (fprintf out "~a~a"
                                    cell-text
                                    standard-row-separator))
                          (else
                           (fprintf out "~a~a"
                                    cell-text
                                    standard-column-separator)
                           (loop (cdr row)
                                 (cdr column-widths)
                                 (cdr alignments)))))))
              table)
    (display (get-output-string out))))

;; test
(define tt '(("zarf" "greeblies" "terwilligbert")
             ("1" "2" "3")
             ("10" "20" "30")
             ("100" "200" "300")))
