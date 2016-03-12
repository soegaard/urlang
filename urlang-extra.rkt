#lang racket
(require "urlang.rkt" syntax/parse)

;;;
;;; URLANG EXTRA
;;;

;; This module provide a few construct which are simple to 
;; translate to JavaScript.

;; Available here:
;     cond, case
;     begin0, when, swhen, unless, sunless
;    letrec, let*,

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

;; SYNTAX  (scond [test statement ...] ...)
;;   Like cond in Racket. Return values of rhs not used.

(define-urlang-macro scond
  (λ (stx)
    (syntax-parse stx
      [(_scond)
       (syntax/loc stx
         undefined)]
      [(_scond [test statement ...] clause ...)
       (syntax/loc stx
         (sif test
              (let () statement ...)
              (scond clause ...)))])))

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

;;; 3.9 Local Binding

;; SYNTAX (letrec ([id val-expr] ...) statement ... expr)

(define-urlang-macro letrec
  (λ (stx)
    (syntax-parse stx
      [(_letrec ([id val-expr] ...) statement ... expr)
       (with-syntax ([(t ...) (generate-temporaries (syntax (id ...)))])
          (syntax (let ([id #f] ...)
                    (let ([t val-expr] ...)
                      (:= id t) ...
                      (let () statement ... expr)))))])))


(define-urlang-macro let*
  (λ (stx)
    (syntax-parse stx
      [(_let* () statement ... expr)
       (syntax/loc stx
         (let () statement ... expr))]
      [(_let* ([id val-expr] clause ...) statement ... expr)
       (syntax/loc stx
         (let ([id val-expr])
           (let* (clause ...) statement ... expr)))])))

(define-urlang-macro case
  (λ (stx)
    (syntax-parse stx
      #:literals (else)
      [(_case val-expr clause ...)
       (syntax/loc stx
         (let ([t val-expr])
           (case-helper t clause ...)))])))

(define-urlang-macro case-helper
  (λ (stx)
    (syntax-parse stx
      #:literals (else)
      [(_case-helper t [else statement ... expr] clause ...)
       (syntax/loc stx
         (let () statement ... expr))]
      [(_case-helper t [(datum ...) statement ... expr] clause ...)
       (syntax/loc stx
         (if (or #f (= t datum) ...)
             (let () statement ... expr)
             (case-helper t clause ...)))]
      [(_case-helper t)
       (syntax/loc stx
         undefined)])))



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


