;;; sicp-35b-streams-api.rkt

#lang racket

(provide sicp-stream-cons
         sicp-stream-car
         sicp-stream-cdr
         sicp-stream-ref
         sicp-stream-map
         sicp-stream-mapn
         sicp-stream-for-each
         sicp-stream-make-interval
         sicp-stream-filter
         sicp-stream-null?
         sicp-empty-stream
         ls
         display-stream)

(define (memoize proc)
  (let ((already-run #f)
        (result #f))
    (lambda ()
      (if (not already-run)
          (begin (set! result (proc))
                 (set! already-run #t)
                 result)
          result))))

;; We wish to avoid naming things that exist in Racket:
;; delay, force, stream, empty-stream etc. Racket's stream
;; api closely follows the SICP naming conventions. So instead
;; of stream-car, stream-cdr etc, we will add a tedious 
;; (but clear) prefix "sicp-".

(define-syntax delay-plain
  (syntax-rules ()
    ((_ expr) (lambda () expr))))

(define-syntax delay-memoized
  (syntax-rules ()
    ((_ expr) (memoize (lambda () expr)))))

(define (my-force delayed-object) (delayed-object))

(define-syntax sicp-stream-cons
  (syntax-rules ()
    ((_ a b) (cons a (delay-plain b)))))

(define (sicp-stream-car s) (car s))
(define (sicp-stream-cdr s) (my-force (cdr s)))

;; Of course having the empty stream to be the empty list
;; will cause problems if we are making streams of lists,
;; some possibly empty. We won't have this problem in the future
;; because we switch over to Racket's built-in streams.
;; Racket's empty-stream object is not equivalent to the empty list.
(define sicp-empty-stream '())
(define (sicp-stream-null? s) (null? s))

(define (sicp-stream-ref s n)
  (if (= n 0)
      (sicp-stream-car s)
      (sicp-stream-ref (sicp-stream-cdr s) (sub1 n))))

(define (sicp-stream-map proc s)
  (if (sicp-stream-null? s)
      sicp-empty-stream
      (sicp-stream-cons (proc (sicp-stream-car s))
             (sicp-stream-map proc (sicp-stream-cdr s)))))

(define (sicp-stream-for-each proc s)
  (if (sicp-stream-null? s)
      'done
      (begin (proc (sicp-stream-car s))
             (sicp-stream-for-each proc (sicp-stream-cdr s)))))

(define (sicp-stream-make-interval a b)
  (if (> a b)
      sicp-empty-stream
      (sicp-stream-cons a (sicp-stream-make-interval (add1 a) b))))

(define (sicp-stream-filter pred s)
  (cond ((sicp-stream-null? s) sicp-empty-stream)
        ((pred (sicp-stream-car s))
         (sicp-stream-cons (sicp-stream-car s)
                (sicp-stream-filter pred (sicp-stream-cdr s))))
        (else (sicp-stream-filter pred (sicp-stream-cdr s)))))

;; Displays both finite (empty-terminated) streams
;; and infinite streams.
(define (ls s (terms 10))
  (display "[")
  (let loop ((s s) (n terms))
    (cond ((< n 1) (display "...]"))
          ((sicp-stream-null? s) (begin (display s)
                                        (display "]")))
          (else (begin (display (sicp-stream-car s))
                       (display ", ")
                       (loop (sicp-stream-cdr s) (sub1 n)))))))

;; We prefer ls, but have to use this for some exercises.
(define (display-stream s) (sicp-stream-for-each displayln s))

;; Exercise 3.50 ========================================

;; Generalize smap so it maps over many streams (in args).

(define (sicp-stream-mapn fn . args)
  (if (sicp-stream-null? (car args))
      sicp-empty-stream
      (sicp-stream-cons
       (apply fn (map sicp-stream-car args))
       ;;(apply sicp-stream-mapn (cons fn (map sicp-stream-cdr args)))
       (apply sicp-stream-mapn fn (map sicp-stream-cdr args)))))

;; The last two lines merit discussion. In 
;; 
;;   (apply sicp-stream-mapn (cons fn (map sicp-stream-cdr args)))
;;
;; motice how fn is passed to smapn by a clever trick. 
;; The function fn is consed onto a list of updated 
;; (cdr'd) streams, and then smapn is applied to this list. 
;; Thus fn ends up being the first argument to sicp-stream-mapn. 
;; This allowed us to pass the sicp-stream-cdr's of an unspecified 
;; number of streams into the recursive call to sicp-stream-mapn.
;;
;; So 
;;
;;   (apply sicp-stream-mapn (cons fn (map sicp-stream-cdr args))) 
;;
;; is equivalent to
;;
;;   (apply sicp-stream 
;;          mapn 
;;          (list fn 
;;                (sicp-stream-cdr s1) 
;;                (scdr s2) 
;;                (scdr s3) ...))
;;
;; and that's the same as
;;
;;   (sicp-stream-mapn fn 
;;                     (sicp-stream-cdr s1) 
;;                     (sicp-stream-cdr s2) 
;;                     (sicp-stream-cdr s3) ...)
;;
;; where s1, s2, s3 are the streams in the arg list. 
;; But since doing a recursive call to a function with
;; a variable argument list is a common need, there exists
;; an extended 'apply' syntax that handles it! 
;;
;;   (apply sicp-stream-mapn fn (map sicp-stream-cdr args)))))
;;
;; fn is consed onto the list returned by map. 
;; You can even do things like:
;;
;;   (apply + 1 2 3 '(4 5 6)) ==> 21.
;; 
;; Perhaps this ability of apply didn't exist in SICP scheme.
;;
;; Test:
;;
(define foo (sicp-stream-make-interval 1 100))
;;   ==> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...]

(define goo (sicp-stream-cdr foo))
;;   ==> [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]

(define soo (sicp-stream-cdr goo))
;;   ==> [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, ...]

;; (ls (sicp-stream-mapn + foo goo soo))
;;  ==> [6, 9, 12, 15, 18, 21, 24, 27, 30, 33, ...]




