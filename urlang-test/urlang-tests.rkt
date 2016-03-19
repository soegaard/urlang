#lang racket
;;;
;;; URLANG BASE TESTS
;;;

;; Run this file to test urlang.rkt.
;; No output means no errors.

(require rackunit urlang)

(current-urlang-run?                           #t) ; run using Node?
(current-urlang-echo?                          #t) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t) ; print top-level expression?

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
; Object literals
;(check-equal? (rs (urlang (urmodule t19 (ref (object [foo 10] [11 12] ["bar" 13]) "foo")))) 10)
;(check-equal? (rs (urlang (urmodule t20 (ref (object [foo 10] [11 12] ["bar" 13]) 11))))    12)
;(check-equal? (rs (urlang (urmodule t21 (ref (object [foo 10] [11 12] ["bar" 13]) "bar")))) 13)
;; Defined functions with optional arguments
(check-equal? (rs (urlang (urmodule t22 (define (add x [y 1]) (+ x y)) (add 3)))) 4)
;; Dot notation
(check-equal? (rs (urlang (urmodule t23
                            (define t (object [n 1]
                                              [o (object [m 10]
                                                         [f (λ (y) (+ y 97))])]
                                              [g (λ (x)
                                                   (object [m (+ x 996)]
                                                           [o (object [h (λ (y) (+ x y 9991))])]))]))
                            (+ (dot t n)              ;     1
                               (dot t o m)            ;    10
                               (dot t o (f 3))        ;   100
                               (dot t (g 4) m)        ;  1000
                               (dot t (g 4) o (h 5))) ; 10000
                            )))
              11111)
(check-equal? (rs (urlang (urmodule t24 (var [o (object [foo-bar 3])]) o.foo-bar))) 3)
                                              

;;;
;;; EXPORT AND IMPORT
;;;

(check-equal? (urlang       ; test export
               (urmodule factorial
                 (export fact)
                 (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))))
              "")

; TODO this example doesn't run due to (require ...) used as an expression.
; Introduce a require specification such that (require (node F "./factorial.js")) is possible.
#;(check-equal? (rs
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
         [_           (syntax/loc stx (sif e0 (block e ...) (sempty)))])])))

(check-equal? (rs (urlang (urmodule test-when-macro
                            (var (a 1))
                            (when #t (:= a 2) (:= a 3))  ; module-level context
                            (block (when #f (:= a 4)))   ; statement context
                            (:= a (+ a (when #t 4)))     ; expression context
                            a)))
              7)

#;(urlang
   (urmodule label-break
     (var i j)
     (:= i 0)
     (label loop1
            (while (< i 3)
                   (label loop2
                          (block
                           (:= j 0)
                           (while (< j 3)
                                  (sif (and (= i 1) (= j 1))
                                       (break loop1)
                                       (block))
                                  (console.log (+ "i = " i ", j = " j))
                                  (+= j 1))))
                   (+= i 1)))))

#;(urlang
 (urmodule label-continue
   (var (i 0) (n 0))
   (while (< i 5)
          (+= i 1)
          (sif (= i 3) (continue) (block))
          (+= n i))
   n))
