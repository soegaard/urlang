#lang racket
(require "urlang.rkt" syntax/parse)

;;;
;;; MACROS
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
         (for ((x ignore) in-array e) σ ...))]
      [(_for ((x:Id i:Id) in-array e:Expr) σ:Statement ...)
       ; evaluate e, check it is an array a,
       ; for each element v of a :
       ;   assign v to x, assign the index if v to i and evaluate the statements σ ...
       #'(block
          (var x (i 0) (a e))
          ; (unless (array? a) (console.log (error "URLANG: expected array")))
          (var (n a.length))
          (while (< i n)
                 (:= x (ref a i))
                 σ ...
                 (:= i (+ i 1))))])))

(define-urlang-macro for/sum
  (λ (stx)
    (syntax-parse stx
      [(_for (s:Id x:Id e:Expr) σ:Statement ... r:Expr)
       #'(block
          (var x (i 0) (a e) (n a.length))
          (while (< i n)
                 (:= x (ref a i))
                 σ ...
                 (:= s (+ s r))
                 (:= i (+ i 1))))])))


#;(module+ test (require rackunit)
    (check-equal? (unparse-L (parse #'(urmodule 1))) '(urmodule '1)))

#;(emit
   (generate-code
    (parse
     #'(urmodule
        (define (sum xs)
          (var (n xs.length) (s 0) (i 0))
          (while (< i n)
                 (:= s (+ s (ref xs i)))
                 (:= i (+ i 1)))
          s)
        (sum (Array 1 2 3))))))

#;(emit
   (generate-code
    (parse
     #'(urmodule
        (define (sum xs)
          (var (s 0))
          (for-array (x i 3) (:= s (+ s x)))
          s)
        (sum (array 1 2 3))))))

#;(emit
   (generate-code
    (parse
     #'(urmodule
        (define (fact n)
          4
          x)
        (:= x 3)
        x))))

(unparse-L1
 (annotate-bodies
  (collect
  (parse
   #'(urmodule
      fact-example
      (export fact)
      (import +)
      (define (fact n)
        (var a (y 87))
        (block
         (:= a 9)
         (var j (k 0)))
        3)
      (define (fact2 n)
        (var a (z 87))
        (block
         (:= a 9)
         (var j (k 0))
         (var j (k 0)))
        3))))))

(compile
 #'(urmodule
    "list.js"
    (export NULL PAIR cons car cdr null?)
    (import array displayln ref + - >= Array or not and === < arguments in-array)
    ;; TAGS
    (define PAIR (array "PAIR"))
    (define NULL (array "NULL"))
    (define (array? v) (Array.isArray v))
    (define (tag v)    (and (array? v) (ref v 0)))
    (define (true? v)    (=== v #t))
    (define (false? v)   (=== v #f))
    (define (boolean? v) (or (true? v) (false? v)))
    (define (null? v)  (=== v NULL))
    (define (pair? v)  (=== (tag v) PAIR))
    (define (cons a d) (if (or (=== d NULL) (list? d))
                           (array "PAIR" #t a d)
                           (array "PAIR" #f a d)))
    (define (list? v) (and (pair? v) (ref v 1)))
    (define (car p)   (ref p 2))
    (define (cdr p)   (ref p 3))
    (define (list)
      (var (a arguments) (n a.length) (i (- n 1)) (xs NULL))
      (do-while (>= i 0)
                (:= xs (cons (ref a i) xs))
                (:= i (- i 1)))
      xs)
    (define (length xs)
      (var (n 0))
      (while (not (null? xs))
             (:= n (+ n 1))
             (:= xs (cdr xs)))
      n)
    
    (define l (list 1 2 3 4 5))
    (length l)
    (define s 0)
    (block
     (for (x in-array (array 1 2 3 4 5))
       (:= s (+ s x))))
    s))

; SYNTAX: (mec)
;   expands to a string indicating the expansion context
;   in which mec was expanded.
(define-urlang-macro mec
  (λ (stx)
    (syntax-parse stx
      #:literal-sets (for-keywords)
      [(_double)
       (with-syntax ([mec (~a (macro-expansion-context))])
         (syntax/loc stx
           mec))])))

; EXAMPLE
;> (compile
;   #'(urmodule
;      (export)
;      (import)
;      (mec)
;      (define (foo x) (mec) 5)
;      (define (bar x) (mec))))
;
;"use strict;"
;"module-level";
;function foo(x){"statement";return 5;};
;function bar(x){return "expression";};

;; SYNTAX (cond [e0 e1 e2 ...] ... [else en]), 
;;   like Racket cond except there is no new scope 
(define-urlang-macro cond
  (λ (stx)   
  (syntax-parse stx
    [(_cond [else e0:Expr e:Expr ...])
     #'(begin e0 e ...)]
    [(_cond [e0 e1 e2 ...] clause ...)
     (syntax/loc stx
       (if e0 (begin e1 e2 ...) (cond clause ...)))]
    [(_cond)
     (raise-syntax-error 'cond "expected an else clause" stx)])))

(define-urlang-macro (case stx)
  (syntax-parse stx
    [(_case e:Expr clauses ...)
     (syntax/loc stx
       (let ((v e))
         (vcase v clauses ...)))]))

(define-urlang-macro (vcase stx)
  (syntax-parse stx
    #:literals (else)
    [(_vcase v:Id [else e0:Expr e:Expr ...])
     (syntax/loc stx
       (begin e0 e ...))]
    [(_vcase v:Id [(d:Datum ...) e0:Expr e:Expr ...] clause ...)
     (syntax/loc stx
       (if (array-memq v (array d ...))
           (begin e0 e ...)
           (vcase v clause ...)))]))

(define-urlang-macro (when stx)
  (with-syntax
      ([(<if> <seq>)
        (case (macro-expansion-context)
          [(expression) #'(if begin)]
          [else         #'(sif block)])])
    (syntax-parse stx
      [(_when e/s0 e/s ...)
       (syntax/loc stx
         (<if> e/s0 (<seq> e/s ...) undefined))])))
;;;
;;; DISPLAY
;;;

(eval
    #'(urmodule display
        (export display)
        (import array ref < * + === undefined typeof console process true false)
        ;;; DISPLAY
        (define (out v)       (process.stdout.write v))
        (define (newline)     (console.log ""))
        (define (displayln v) (display v) (newline))
        (define (display v)
          (var (t (typeof v)))
          (console.log t)
          (if (=== t "number") (display-number v)
              (if (=== t "string") (display-string v)
                  (if (=== t true) (out "#t")
                      (if (=== t false) (out "#f")
                          (begin (displayln "error: display unknown value")
                                 (console.log v)))))))
        (define (display-number v) (out (v.toString)))
        (define (display-string v) (out "'") (out v) (out "'"))
        (display 43)
        (display "foo")))
