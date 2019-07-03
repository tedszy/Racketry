;;; sicp-35b-streams-api.rkt

#lang racket

(provide scons
         scar
         scdr
         sref
         smap
         sfor-each
         smake-interval
         sfilter
         snull?
         the-empty-stream
         ls)

(define (memoize proc)
  (let ((already-run #f)
        (result #f))
    (lambda ()
      (if (not already-run)
          (begin (set! result (proc))
                 (set! already-run #t)
                 result)
          result))))

;; We wish to avoid using functions that exist in Racket:
;; delay, force, stream, empty-stream etc. Racket's stream
;; api closely follows the SICP naming conventions. Instead
;; of stream-car, stream-cdr etc, we will use prefix "s"
;; to mean stream: scar, scdr etc.

(define-syntax delay-plain
  (syntax-rules ()
    ((_ expr) (lambda () expr))))

(define-syntax delay-memoized
  (syntax-rules ()
    ((_ expr) (memoize (lambda () expr)))))

(define (my-force delayed-object) (delayed-object))

(define-syntax scons
  (syntax-rules ()
    ((_ a b) (cons a (delay-plain b)))))

(define (scar s) (car s))
(define (scdr s) (my-force (cdr s)))

;; Of course having the empty stream to be the empty list
;; will cause problems if we are making streams of lists,
;; some possibly empty. We won't have this problem in the future
;; because we switch over to Racket's built-in streams.
;; Racket's empty-stream object is not equivalent to the empty list.
(define the-empty-stream '())
(define (snull? s) (null? s))

(define (sref s n)
  (if (= n 0)
      (scar s)
      (sref (scdr s) (sub1 n))))

(define (smap proc s)
  (if (snull? s)
      the-empty-stream
      (scons (proc (scar s))
             (smap proc (scdr s)))))

(define (sfor-each proc s)
  (if (snull? s)
      'done
      (begin (proc (scar s))
             (sfor-each proc (scdr s)))))

(define (smake-interval a b)
  (if (> a b)
      the-empty-stream
      (scons a (smake-interval (add1 a) b))))

(define (sfilter pred s)
  (cond ((snull? s) the-empty-stream)
        ((pred (scar s))
         (scons (scar s)
                (sfilter pred (scdr s))))
        (else (sfilter pred (scdr s)))))

;; Displays both finite (empty-terminated) streams
;; and infinite streams.
(define (ls s (terms 10))
  (display "[")
  (let loop ((s s) (n terms))
    (cond ((< n 1) (display "...]"))
          ((snull? s) (begin (display s)
                             (display "]")))
          (else (begin (display (scar s))
                       (display ", ")
                       (loop (scdr s) (sub1 n)))))))


;; Test.
(define ones (scons 1 ones))
(define foo (smake-interval 10 15))

