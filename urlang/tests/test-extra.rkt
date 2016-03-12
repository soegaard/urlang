#lang racket
(require urlang urlang/extra)

;;;
;;; TEST
;;;

;;; Note: No output means all tests ran successfully.

;;; Note: You need to install  node  in order to run these tests.
;;;            https://nodejs.org/en/

;;; Running these tests will for each module produce
;;; both a JavaScript file and a file containing names of exports.

;;; Then node (a command line version of the JavaScript engine
;;; used in Chrome) is invoked on the JavaScript file, and the
;;; output is compared with the expected output.

(current-urlang-run?                           #t) ; run using Node?
(current-urlang-echo?                          #f) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t)

(require rackunit)
(define (rs s) (read (open-input-string s))) ; note: only reads FIRST value

(check-equal? (rs (urlang (urmodule test-cond
                            (define x 3)
                            (cond
                              [(= x 1) "one"]
                              [(= x 2) "two"]
                              [(= x 3) "three"]
                              [#t      "huh"]))))
              'three)

(check-equal? (rs (urlang (urmodule test-sconf
                            (var r [n 3])
                            (scond
                              [(= n 2) (:= r "fail")]
                              [(= n 3) (:= r "success")])
                            r)))
              'success)

(check-equal? (rs (urlang (urmodule test-begin0
                            (begin0 1 2 3 4))))
              1)

(check-equal? (rs (urlang (urmodule test-when1
                            (let ([x 3] [y 4])
                              (when (= x 3) (:= y 5))
                              y))))
              5)
(check-equal? (rs (urlang (urmodule test-when2
                            (let ([x 3] [y 4])
                              (when (= x 2) (:= y 5))
                              y))))
              4)

(check-equal? (rs (urlang (urmodule test-unless1
                            (let ([x 3] [y 4])
                              (unless (= x 3) (:= y 5))
                              y))))
              4)
(check-equal? (rs (urlang (urmodule test-unless2
                            (let ([x 3] [y 4])
                              (unless (= x 2) (:= y 5))
                              y))))
              5)


(check-equal? (rs (urlang (urmodule test-unless3
                            (define msg "")
                            (sunless (> 5 0)
                                     (:= msg "hi"))
                            (sunless (< 5 0)
                                     (:= msg "hi-there"))
                            msg)))
                  'hi-there)


(check-equal? (rs (urlang (urmodule test-letrec
                            (define (zero? x) (= x 0))
                            (define (sub1 x) (- x 1))
                            (letrec ([is-even? (lambda (n)
                                                 (or (zero? n)
                                                     (is-odd? (sub1 n))))]
                                     [is-odd? (lambda (n)
                                                (and (not (zero? n))
                                                     (is-even? (sub1 n))))])
                              (is-odd? 11)))))
              'true)

(check-equal? (rs (urlang (urmodule test-let*
                            (let* ([x 1]
                                   [y (+ x 1)])
                              (+ (* 100 x) y)))))
              102)

(check-equal? (rs (urlang (urmodule test-case1
                            (case (+ 7 5)
                              [(1 2 3)    "small"]
                              [(10 11 12) "big"]))))
              'big)

(check-equal? (rs (urlang (urmodule test-case1
                            (case (- 7 5)
                              [(1 2 3)    "small"]
                              [(10 11 12) "big"]))))
              'small)
