#lang racket
(require (prefix-in compiler: (only-in compiler-rjs/compiler eval)))

(define (read+eval-expression line)
  (with-input-from-string
      (with-output-to-string
          (λ () (compiler:eval
                 (datum->syntax #'here
                                `(displayln ,(syntax->datum line))))))
    read))


;;;
;;; "tests-compiler/numbers.rkt"
;;;

;; Each line contains one expression.
;; Check that the compiler and racket get the same result.

(define (test-numbers)
  (define src "tests-compiler/numbers.tests")
  (with-input-from-file src
    (λ ()
      (parameterize ([current-namespace (make-base-namespace)])
        (for/list ([line (in-port (λ (port) (read-syntax src port)))] [i (in-naturals)])
          (display (~a i " "))
          (displayln line)
          (define actual   (read+eval-expression line))
          (define expected (eval line))
          (list actual expected (equal? actual expected)))))))

;;;
;;; "tests-compiler/letrec.rkt"
;;;

;; Each expression is of the form (test expected test-name expression)


(define (test-letrec)
  (define src "tests-compiler/letrec.tests")
  (define (test? stx) (and (syntax? stx) (eq? 'test (syntax-e stx))))
  (define all
    (with-input-from-file src
      (λ ()
        (parameterize ([current-namespace (make-base-namespace)])
          (for/list ([line (in-port (λ (port) (read-syntax src port)))] [i (in-naturals)])
            (match (syntax->list line)
              [(list (? test?) expected name expression)
               (display (~a i " "))
               (displayln (syntax->datum expression))
               (define actual   (read+eval-expression expression))
               (define expected (eval expression))
               (define results (list actual expected (equal? actual expected)))
               (unless (equal? actual expected)
                 (displayln "Problem:")
                 (displayln results))
               results]))))))
  (define problems (filter (compose not third) all))
  (unless (empty? problems) (displayln "Problems:") (displayln problems))    
  all)


;(test-numbers)
(test-letrec)
