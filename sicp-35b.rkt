;; sicp-35b.rkt

#lang racket

(require "sicp-35-streams-api.rkt")


;; Exercise 3.51 ========================================

;; To understand what is going on in this exercise we should 
;; meditate on the following passage from SICP (pg.434):
;;
;;   "...we will arrange for the cdr of a stream to be 
;;   evaluated when it is accessed by the [sicp-]stream-cdr
;;   procedure rather than when the stream is constructed
;;   by [sicp-]cons-stream."
;;
;; Look at the way sicp-streams are constructed in the stream
;; api file: 
;;
;;   (define-syntax sicp-stream-cons
;;     (syntax-rules ()
;;       ((_ a b) (cons a (delay-plain b)))))
;;
;;   (define (sicp-stream-car s) (car s))
;;   (define (sicp-stream-cdr s) (my-force (cdr s)))
;;
;;   (define (sicp-stream-map proc s)
;;     (if (sicp-stream-null? s)
;;         sicp-empty-stream
;;         (sicp-stream-cons (proc (sicp-stream-car s))
;;                (sicp-stream-map proc (sicp-stream-cdr s)))))
;;
;; The car part of the stream cons-cell is evaluated on construction, 
;; while the cdr part is wrapped in a lambda for later evaluation. 
;;
;;   (define foo (sicp-stream-cons (+ 1 1) (+ 2 2)))
;;   
;; When you type foo into the repl you see (+ 1 1) has been
;; evaluated to 2 and stuffed in the car:
;;
;;   > foo
;;   '(2 . #<procedure:...-streams-api.rkt:46:21>)
;;
;; In this exercise we are to use a function called show
;; which displays a newline, displays the argument, and then
;; returns the argument. To make things clearer, we will modify
;; what it displays, to make it more distinctive, because it 
;; can be confusing to have to distinguish between what is 
;; printed (displayed) and what is returned as a return value, 
;; when working in the repl.

(define (show u) (printf "[~a]\n" u) u)

;; Now (show 2) will print [2] and a newline. After that comes
;; the return value 2.
;;
;; sicp-35b.rkt﻿> (show 2)
;; [2]
;; 2

;; With this display setup, it will be easier to see what is
;; going on in exercise 3.51.
;;
;; Now, mote what happens when we run this definition:
;;
;; (define foo (sicp-stream-cons (show 1) (show 2)))
;; [1]
;;
;; [1] was displayed because at construction time, the
;; car part is evaluated before being consed. We can check:
;; 
;; > foo
;; '(1 . #<procedure:...-streams-api.rkt:46:21>)
;;
;; We can see that the value 1 returned by evaluating (show 1)
;; sits in the car. Accessing the car again does not again display
;; [1]\n to standard output: the function had been run during 
;; the definintion of foo:
;;
;; sicp-35b.rkt﻿> (sicp-stream-car foo)
;; 1
;;
;; However, every time we access the cdr part of foo, [2]\n is
;; dispayed and 2 returned. That's because the show function is
;; delayed in the cdr and there is NO MEMOIZATION.
;;
;; sicp-35b.rkt﻿> (sicp-stream-cdr foo)
;; [2]
;; 2
;;
;; sicp-35b.rkt﻿> (sicp-stream-cdr foo)
;; [2]
;; 2
;;
;; If we change sicp-stream-cons in the api module to use delay-memoized, 
;; we get different behavior. When the cdr is accessed the first time, 
;; [2]\n is printed, followed by the return value. Subsequent accesses 
;; only give us the return value. There is no more side effect:
;;
;; sicp-35b.rkt﻿> foo
;; '(1 . #<procedure:...-streams-api.rkt:21:4>)
;;
;; sicp-35b.rkt﻿> (sicp-stream-cdr foo)
;; [2]
;; 2
;;
;; sicp-35b.rkt﻿> (sicp-stream-cdr foo)
;; 2

;; Summary of behavior (memoized and unmemoized)
;; 
;; Unmemoized: car is evaluated on construction. 
;;             cdr invokes show function and the value.
;;
;; Memoized: car is evaluated on construction.
;;           cdr invokes show function the fist time cdr is accessed.
;;           subsequent accesses returns the value.

;; So finally we can not only answer the 3.51 question, 
;; we can explain why the result is what it is.

(define x (sicp-stream-map show (sicp-stream-make-interval 0 10)))

;; With no memoization:
;;
;; When this define is run, [0]\n is displayed in the repl.
;; We now understand why: it's because show is run during the
;; construction of the car part of the stream cons cell.

(displayln "---")
(sicp-stream-ref x 5)

;; When this is run, ref walks down the stream to get to element 5.
;; The 0th element is just the number 0. The elements in the cdr
;; are delayed functions that were not invoked at construction time.
;; So as we walk down the stream to get 5th element, the delayed
;; functions are forced: we see what they display. Finally the 
;; 5th element is forced, [5]\n is displayed, and element 5 is 
;; returned, because that's what we are looking to return.
;; Running (sicp-stream-ref x 5) again gives the same thing.
;; Thing to note: [0]\n IS NOT DISPLAYED.
;;
;; It's the same story for (sicp-stream-ref x 7)

;; Now, WITH MEMOIZATION:
;; 
;; Behavior is the same, except when stream ref is run 
;; a second time:
;;
;; sicp-35b.rkt﻿> (sicp-stream-ref x 5)
;; 5
;;
;; And now something really interesting happens when we run
;; stream-ref to get the 7th element. the elements 6 and 7
;; have not been memoized yet, and so the show function will
;; be evaluated and we should see [6]\n and [7]\n printed out!
;;
;; sicp-35b.rkt﻿> (sicp-stream-ref x 7)
;; [6]
;; [7]
;; 7
;;
;; I think this detail is the point of this exercise. However 
;; it is also necessary to explain why [0]\n is printed out
;; at definition time but not at stream-ref time. Which we 
;; have done.

;; --------------------------------------------------------

;; Racket streams.
;;
;; Racket has its own native streams based on SRFI-41. They 
;; behave like memoized SICP streams but with an important 
;; difference:

(define y (stream-map show (for/stream ((u (range 0 11))) u)))

;; There is no [0]\n displayed when this is run. That's because
;; SRFI-41 streams delay their car as well as their cdr!


(displayln "---")
(stream-ref y 5)
(stream-ref y 5)

;; Notice that stream-ref skips over the elements of the stream that
;; if does not need to evaluate. It evaluates the 5th. But the second
;; time it returns the memoized value. Same with 7th element.

(displayln "---")
(stream-ref y 7)
(stream-ref y 7)

;; Now if we dump the whole stream to list, we predict that
;; [5]\n and [7]\n will not be printed!

(stream->list y)

;; [0]
;; [1]
;; [2]
;; [3]
;; [4]
;; [6]
;; [8]
;; [9]
;; [10]
;; '(0 1 2 3 4 5 6 7 8 9 10)
;;
;; And we are right! So not only do we understand SICP streams
;; a whole lot better, we understand the construction of Racket 
;; streams too.








;; Exercise 3.52 ========================================

(define sum 0)
(define (accumulate x)
  (set! sum (+ x sum)) 
  sum)

;; Triangular numbers.
(define seq
  (sicp-stream-map accumulate
                   (sicp-stream-make-interval 1 20)))

;; Even triangular numbers.
(define seq2 (sicp-stream-filter even? seq))

;; Triangular numbers divisible by 5.
(define seq3
  (sicp-stream-filter (lambda (x) (= (remainder x 5) 0))
                      seq))

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

