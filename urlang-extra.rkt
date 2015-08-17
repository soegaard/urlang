#lang racket
(require "urlang.rkt" syntax/parse)

;;;
;;; URLANG EXTRA
;;;

;; This module provide a few construct which are simple to 
;; translate to JavaScript.

;; SYNTAX  (cond [test statement ... expr] ...)
;;   Like cond in Racket. The right hand sides are in a new scope.

(define-urlang-macro cond
  (λ (stx)
    (syntax-parse stx
      [(_cond)
       (syntax/loc stx
         undefined)]
      [(_cond [test statement ... expr] clause ...)
       (syntax/loc stx
         (if test
             (let () statement ... expr)
             (cond clause ...)))])))

;;; 3.15 Sequencing

;; SYNTAX (begin0 expr statement ...)
;;   Expression evalues expr, statement, ... in order.
;;   Returns the result of evaluating expr as result.
(define-urlang-macro begin0
  (λ (stx)
    (syntax-parse stx
      [(_begin0 expr statement ...)
       (syntax/loc stx
         (let ([t expr]) statement ... t))])))


;;; 3.16 Guarded Evaluation

;; SYNTAX (when test-expr statement ... expr)
;;   This form expands to an expression.
(define-urlang-macro when
  (λ (stx)
    (syntax-parse stx
      [(_when test statement ... expr)
       (syntax/loc stx
         (if test
             (let () statement ... expr)
             undefined))])))

;; SYNTAX (swhen test-expr statement ...)
;;   This form expands to a statment (Statement when).
(define-urlang-macro swhen
  (λ (stx)
    (syntax-parse stx
      [(_swhen test statement ...)
       (syntax/loc stx
         (sif test
              (block statement ...)
              (sempty)))])))

;; SYNTAX (unless test-expr statement ... expr)
;;   This form expands to an expression.
(define-urlang-macro unless
  (λ (stx)
    (syntax-parse stx
      [(_unless test statement ... expr)
       (syntax/loc stx
         (when (not test) statement ... expr))])))

;; SYNTAX (sunless test-expr statement ...)
;;   This form expands to a statment (Statement unless).
(define-urlang-macro sunless
  (λ (stx)
    (syntax-parse stx
      [(_sunless test statement ...)
       (syntax/loc stx
         (swhen (not test) statement ...))])))


;;;
;;; TEST
;;;

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

