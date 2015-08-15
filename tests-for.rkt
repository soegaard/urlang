#lang racket
(require "urlang.rkt" "for.rkt" rackunit)

;;;
;;; TEST
;;;

(current-urlang-run?                           #t) ; run using Node?
(current-urlang-echo?                          #t) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t) ; print top-level expression?

(define (rs s) (read (open-input-string s)))

; test for with single clause and in-range
(check-equal? (rs (urlang
                   (urmodule test-for
                     (define sum 0)
                     (for ([x in-range 1 101])
                       (+= sum x))
                     sum)))
              5050)

; test in-array
(check-equal? (rs (urlang
                   (urmodule test-for
                     (define sum 0)
                     (for ([x in-array (array 1 2 3 4 5)])
                       (+= sum x))
                     sum)))
              15)

; test for with two parallel clauses
(check-equal? (rs (urlang
                   (urmodule test-for
                     (define sum 0)
                     (for ([x in-array (array 1 2 3 4 5)]
                           [y in-range 100 200])
                       (+= sum (+ x y)))
                     sum)))
              525)

; test in-naturals and #:break <guard>
(check-equal? (rs (urlang
                   (urmodule test-for
                     (define sum 0)
                     (define stop? #f)
                     (for ([x in-naturals 1])
                       (+= sum x)
                       #:break (= x 5))
                     sum)))
              15)

; test in-naturals works from varying starting points
(check-equal? (rs (urlang
                   (urmodule test-for
                     (define sum 0)
                     (define stop? #f)
                     (for ([x in-naturals 3])
                       (+= sum x)
                       #:break (= x 5))
                     sum)))
              12)

              
