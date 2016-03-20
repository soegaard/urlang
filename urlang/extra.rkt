#lang racket
(require urlang syntax/parse)
(provide else)
;;;
;;; URLANG EXTRA
;;;

;; This module provide a few constructs which are simple to 
;; translate to JavaScript.

;; These constructs can be used as expressions:

;     (begin0 expr statement ...)
;     (when   test statement ... expr)
;     (unless test statement ... expr)

;     (cond [test statement ... expr] ...)
;     (case val-expr clause ...)
;          where clause is  [(datum ...) statement ... expr]
;                       or  [else        statement ... expr]
;     (letrec ([id val-expr] ...) statement ... expr)
;     (let*   ([id val-expr] ...) statement ... expr)

;; These constructs can be used as statements:
;;     (swhen   test statement ...)
;;     (sunless test statement ...)
;;     (scond  [test statement ...] ...)


;; SYNTAX  (cond [test statement ... expr] ...)
;;   Like cond in Racket. The right hand sides are in a new scope.

(define-urlang-macro cond
  (λ (stx)
    (syntax-parse stx
      #:datum-literals (else)
      [(_cond)
       (syntax/loc stx
         undefined)]
      [(_cond [else then-body ...])
       (syntax/loc stx
         (let () then-body ...))]
      [(_cond [test] clause ...)
       (syntax/loc stx
         (let ([t test]) (if t t (let () clause ...))))]
      [(_cond [test statement ... expr] clause ...)
       (syntax/loc stx
         (if test
             (let () statement ... expr)
             (cond clause ...)))])))

;; SYNTAX  (scond [test statement ...] ...)
;;   Like cond in Racket. Return values of rhs not used.

(define-urlang-macro else (λ (stx) (raise-syntax-error 'else "used out of context")))

(define-urlang-macro scond
  (λ (stx)
    (syntax-parse stx
      #:datum-literals (else)
      [(_scond)
       (syntax/loc stx
         undefined)]
      [(_scond [else then-body ...])
       (syntax/loc stx
         (let () then-body ...))]
      [(_scond [test] clause ...)
       (syntax/loc stx
         (sif t undefined (scond clause ...)))]
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
