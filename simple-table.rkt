;;; simple-table.rkt
;;;
;;; Simple, no nonsense tools for displaying 
;;; list-of-lists in aligned columns.

#lang racket

(require racket/format)

(provide format-bits
         format-real
         format-string
         transpose
         print-table)

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

;; Takes a table of strings and returns a table where
;; all columns have equal string widths, and have been
;; aligned in a given way. Alignments default to right
;; if not specified by keyword parameter.
(define (normalize-columns table #:align [alignments null])
  (map  (lambda (row) 
          (map format-string row 
               (get-max-column-widths table) 
               (if (null? alignments)
                   (make-list (length (car table)) 'right)
                   alignments)))
        table))

;; Put item in between elements of mylist.
;; > (intersperse '(1 2 3 4) 'zarf)
;; '(1 zarf 2 zarf 3 zarf 4)
;; We use this for putting column separators.
(define (intersperse mylist item)
  (let loop ((mylist mylist) (result '()))
    (if (null? (cdr  mylist))
        (reverse (cons (car mylist) result))
        (loop (cdr mylist) 
              (cons item
                    (cons (car mylist) 
                          result))))))

(define (print-table table #:align [alignments null] #:head [head #f] #:bars [bars #f])
  (let ((column-separator (if bars " | " "  "))
        (head-column-separator (if bars "   " "  "))
        (normalized-table (normalize-columns table #:align alignments)))
    (for-each displayln 
             (map (lambda (u) 
                    (string-join u ""))
                  (if head
                      ;; If head=true, then the first row has to be handled
                      ;; differently from the body of the table. No bars and 
                      ;; with underline.
                      (let ((header-row (intersperse (car normalized-table) 
                                                     head-column-separator)))
                        (cons header-row 
                              ;; Add the underline.
                              (cons (map (lambda (u) 
                                           (make-string (string-length u) #\-))
                                         header-row)
                                    ;; Add the body of the table.
                                    (map (lambda (row)
                                           (intersperse row column-separator)) 
                                         (cdr normalized-table)))))
                      ;; No special header.
                      (map (lambda (row)
                             (intersperse row column-separator))
                           normalized-table))))))

(define tt '(("zarf" "greeblies" "terwilligbert")
             ("1" "2" "3")
             ("10" "20" "30")
             ("100" "200" "300")))

;; Usage.
;;
;; > (print-table tt)
;; zarf  greeblies  terwilligbert
;;   1          2              3
;;  10         20             30
;; 100        200            300
;;
;; > (print-table tt #:bars #t)
;; zarf | greeblies | terwilligbert
;;    1 |         2 |             3
;;   10 |        20 |            30
;;  100 |       200 |           300
;;
;; > (print-table tt #:head #t)
;; zarf  greeblies  terwilligbert
;; ------------------------------
;;    1          2              3
;;   10         20             30
;;  100        200            300
;;
;; > (print-table tt #:head #t #:bars #t)
;; zarf   greeblies   terwilligbert
;; --------------------------------
;;    1 |         2 |             3
;;   10 |        20 |            30
;;  100 |       200 |           300
;;
;; > (print-table tt #:head #t #:bars #t #:align '(left center right))
;; zarf   greeblies   terwilligbert
;; --------------------------------
;; 1    |     2     |             3
;; 10   |    20     |            30
;; 100  |    200    |           300

