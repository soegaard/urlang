#lang racket
(require "urlang.rkt" rackunit)

(current-urlang-run?                           #t)
(current-urlang-echo?                          #f)
(current-urlang-console.log-module-level-expr? #t) ; print top-level expression

(define (rs s) (read (open-input-string s)))

;;;
;;; DATUMS
;;;

(check-equal? (rs (urlang (urmodule test1 5)))     5)
(check-equal? (rs (urlang (urmodule test1 "foo"))) 'foo)

;;;
;;; EXPRESSIONS
;;;

;; Application of infix operator
(check-equal? (rs (urlang (urmodule t1 (+ 5 1))))  6)
;; Application of unary operator
(check-equal? (rs (urlang (urmodule t2 (- 5))))   -5)
;; Application of defined function
(check-equal? (rs (urlang (urmodule t3 (define (add x y) (+ x y)) (add 1 2)))) 3)
;; Application of array
(check-equal? (urlang (urmodule t4 (array 1 2 3))) "[ 1, 2, 3 ]\n")
;; Application of ref (array reference)
(check-equal? (rs (urlang (urmodule t5 (ref (array 1 2 3) 0)))) 1)
;; Application of abstraction
(check-equal? (rs (urlang (urmodule t6 ((lambda (a b) (+ a b)) 1 2)))) 3)
;; Assignments
(check-equal? (rs (urlang (urmodule t7 (var v) (:= v 3) v))) 3)
(check-equal? (rs (urlang (urmodule t8 (var (v 4)) (+= v 3) v))) 7)
;; If (ternary)
(check-equal? (rs (urlang (urmodule t9 (if (= 3 3) 1 2)))) 1)
(check-equal? (rs (urlang (urmodule t10 (if (= 3 4) 1 2)))) 2)
;; Begin
(check-equal? (rs (urlang (urmodule t11 (begin 1 2)))) 2)
(check-equal? (rs (urlang (urmodule t12 (+ 3 (begin 1 2))))) 5)
;; Let
(check-equal? (rs (urlang (urmodule t13 (let ((x 5)) (+ x 1))))) 6)
(check-equal? (rs (urlang (urmodule t14 (let ((x 5)) (let ((x 2)) (+ x 1)))))) 3)
;; Lambda
(check-equal? (rs (urlang (urmodule t15 ((lambda (x) (+ x 1)) 2)))) 3)
(check-equal? (rs (urlang (urmodule t16 ((lambda () 4)))))          4)
; Lambda with optional arguments
(check-equal? (rs (urlang (urmodule t17 ((lambda (x [y 2]) (+ x y)) 3)))) 5)
(check-equal? (rs (urlang (urmodule t18 ((lambda (x [y 2]) (+ x y)) 3 4)))) 7)

;;;
;;; EXPORT AND IMPORT
;;;

(check-equal? (urlang       ; test export
               (urmodule factorial
                 (export fact)
                 (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))))
              "")

(check-equal? (rs
               (urlang ; test import and require generated module
                (urmodule test-factorial
                  (import require)
                  (define F (require "./factorial.js"))
                  (F.fact 5))))
              120)

;;;
;;; STATEMENTS
;;;

;; While
(check-equal? (rs ; test while
               (urlang (urmodule test-while
                         (define a (array 1 2 3 4 5))
                         (var (sum 0) (i 0) (n a.length))
                         (while (< i n)
                                (+= sum (ref a i))
                                (+= i 1))
                         sum)))
              15)
;; DoWhile
(check-equal? (rs ; test do-while
               (urlang (urmodule test-do-while
                         (define a (array 1 2 3 4 5))
                         (var (sum 0) (i 0) (n a.length))
                         (do-while (< i n)
                                   (+= sum (ref a i))
                                   (+= i 1))
                         sum)))
              15)
;; Block
(check-equal? (rs (urlang (urmodule test-block
                            (var (a 1) (b 2))
                            (block (:= a 11) (:= b 12))
                            (+ a b))))
              23)

;;;
;;; MACROS
;;;

(require syntax/parse)

;; SYNTAX
;;  As expression: (when <expr> <expr> ...)
;;  As statement:  (when <expr> <statement> ...)
(define-urlang-macro when
  (λ (stx)
    (syntax-parse stx
      [(_when e0 e ...)
       ; (displayln (list 'when 'context: (macro-expansion-context)))
       (match (macro-expansion-context) ; one of 'module-level, 'statement, 'expression
         ['expression (syntax/loc stx (if  e0 (begin e ...) undefined))]
         [_           (syntax/loc stx (sif e0 (block e ...) (block)))])])))

(check-equal? (rs (urlang (urmodule test-when-macro
                            (var (a 1))
                            (when #t (:= a 2) (:= a 3))  ; module-level context
                            (block (when #f (:= a 4)))   ; statement context
                            (:= a (+ a (when #t 4)))     ; expression context
                            a)))
              7)

;;;
;;; FOR AS A MACRO
;;;

(define-syntax in-array (λ (stx) (raise-syntax-error 'in-array "used out of context" stx)))
(define-literal-set for-keywords (in-array))
(define for-keyword? (literal-set->predicate for-keywords))

(define-urlang-macro for
  (λ (stx)
    (syntax-parse stx
      #:literal-sets (for-keywords)
      [(_for (x:Id in-array e:Expr) σ:Statement ...)
       (syntax/loc stx
         (for ((x ignored) in-array e) σ ...))]
      [(_for ((x:Id i:Id) in-array e:Expr) σ:Statement ...)
       ; evaluate e, check it is an array a,
       ; for each element v of a :
       ;   assign v to x, assign the index if v to i and evaluate the statements σ ...
       (match (macro-expansion-context)
         ['expression
          (syntax/loc stx
            (let ()
              (var x (i 0) (a e))
              ; (unless (array? a) (console.log (error "URLANG: expected array")))
              (var (n a.length))
              (while (< i n)
                     (:= x (ref a i))
                     σ ...
                     (+= i 1))
              undefined))]
         [_ ; statement or module-level context
          (syntax/loc stx
            (block
             (var x (i 0) (a e))
             ; (unless (array? a) (console.log (error "URLANG: expected array")))
             (var (n a.length))
             (while (< i n)
                    (:= x (ref a i))
                    σ ...
                    (+= i 1))))])])))

(check-equal? (rs ; test for
               (urlang (urmodule test-while
                         (define a (array 1 2 3 4 5))
                         (define b (array 2 3 4 5 6))
                         (var (asum 0) (bsum 0))
                         (for (x in-array a) (+= asum x))                   ; statement context
                         (block (begin (for (x in-array b) (+= bsum x)) 1)) ; expression context
                         (+ (* asum 10000) bsum))))
              150020) ; asum=15 and bsum = 20

;;;
;;; MACRO EXPANDING TO ANOTHER MACRO
;;;

;; SYNTAX
;;  (for/sum (x     in-array e) σ ... r)   ; σ is a statement and r is an expression
;;  (for/sum ((x i) in-array e) σ ... r)
;;    0. Set the running sum to 0.
;;    1. Evaluate the expression e  (expected to be an array)
;;    2. For each element in the array:
;;         - bind x to the element (and possible i to its index)
;;         - evaluate the statemtens σ
;;         - evaluate the expression r
;;         - add r to the running sum
;;    3. Return the running sum

(define-urlang-macro for/sum
  (λ (stx)
    (syntax-parse stx
      #:literal-sets (for-keywords)
      [(_for/sum (x:Id in-array e:Expr) σ:Statement ... r:Expr)
       (syntax/loc stx (for/sum ((x ignored) in-array e) σ ... r))]
      [(_for/sum ((x:Id i:Id) in-array e:Expr) σ:Statement ... r:Expr)
       (syntax/loc stx
         (let ([sum 0] [x 0])
           (for ((x i) in-array e) σ ... (+= sum r))
           sum))])))

(check-equal? (rs (urlang (urmodule test-for-sum
                            (define a (array 1 2 3 4 5))
                            (for/sum (x in-array a)
                              (* x x)))))  ; sum the squares
              55) ; = 1^2 + 2^2 + 3^2 + 4^2 + 5^2
             
(check-equal? (rs (urlang (urmodule test-for-sum
                            (define a (array 1 2 3 4 5))
                            (for/sum ((x index) in-array a)
                              index))))  ; sum the indices
              10) ; 0 + 1 + 2 + 3 + 4 

;;;
;;; for/first
;;;

(define-urlang-macro for/first
  (λ (stx)
    (syntax-parse stx
      #:literal-sets (for-keywords)
      [(_for/first (x:Id in-array e:Expr) σ:Statement ... r:Expr)
       (syntax/loc stx (for/first ((x ignored) in-array e) σ ... r))]
      [(_for/first ((x:Id i:Id) in-array e:Expr) σ:Statement ... r:Expr)
       (syntax/loc stx
         (let ([result #f])
           (for ((x i) in-array e)
             (when (not result)      ; todo: use break instead
               σ ...
               (:= result r)))
           result))])))

(check-equal? (rs (urlang (urmodule test-for-sum
                            (define a (array 1 2 3 4 5))
                            (for/first ((x index) in-array a)
                              (and (or (= x 3) (= x 4)) (* x x))))))
              9)

