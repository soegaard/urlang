#lang racket

;;;
;;; Racket for-loops for Urlang.
;;;

; The standard JavaScript for-loops aren't as enjoyable to use
; as the Racket counterpart. This module implement Racket style
; for-loops for Urlang (which compiles to JavaScript).

; The main idea is best illustrated with an example.
; When I write this:

#;(for ([x in-range 3 7]    ; the x and y loop run in parallel
        [y in-array ys])
    (console.log (+ x y)))

; I mean:

#;(block/let
   ;; initial states and helper variables
   (var [i 3]                                 ; index i counts from 3
        [t 7]                                 ; to 7 (exclusive)
        [a ys]                                ; store the array in a
        [n a.length]                          ; used by array condition test         
        [t 0])                                ; index into the array (counts from 0 to n (exclusive))
   (while
    ;; if any of the clauses are exhausted the loop ends
    (and (< i 7)
         (< t n))
    ;; state to value
    (let ([x i]               
          [y (ref a t)])
      ;; statements from for body
      (console.log (+ x y))
      ;; step states forward
      (+= i 1)                
      (+= t 1))))

; The JavaScript becomes:
;    {var i=3,t=7,a=ys,n=a.length,i_4=0;
;     while( (true && (i<t) && (i_4<n) )){
;       ((function(x,y){                    // let 
;           (console.log((x+y)));
;           (i+=1);
;           return (i_4+=1);
;           })(i,a[i_4]));
;       };
;     undefined;                           // due to expression context
;     }


(require syntax/parse
         syntax/stx
         "urlang.rkt")

(define-literal-set for-keywords (in-array in-range))
(define for-keyword? (literal-set->predicate for-keywords))

(define-syntax-class ForKeyword
  #:opaque (pattern x #:fail-unless (for-keyword? #'x) #f))


(define-urlang-macro block/let
  ; expand to (let () ... undefined) in an expresion context
  ; and to    (block  ...)           otherwise
  (λ (stx)
    (syntax-parse stx
      [(_block/let more ...)
       (match (macro-expansion-context)
         ['expression (syntax/loc stx (let () more ... undefined))]
         [_           (syntax/loc stx (block  more ...))])])))


(define (handle-in-range-clause clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-range from to]
     #'(([i from]       ; list of var clauses to create initial state
         [t to])
        (< i t)         ; termination condition
        ([x i])         ; let bindings (needs to bind x)
        ((+= i 1)))]))  ; statements to step state forward

(define (handle-in-array-clause clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-array array-expr]
     #'(([a array-expr]       ; list of var clauses to create initial state
         [n a.length]
         [i 0])
        (< i n)              ; termination condition
        ([x (ref a i)]) ; let bindings (needs to bind x)
        ((+= i 1)))]))  ; statements to step state forward

(define (handle-clause clause-stx)
  (syntax-parse clause-stx
    #:literal-sets (for-keywords)
    [ [x in-range from to]    (handle-in-range-clause clause-stx) ]
    [ [x in-array array-expr] (handle-in-array-clause clause-stx) ]
    [_ (raise-syntax-error 'for "unknown clause" clause-stx)]))

(define (expand-for stx)   ; parallel for
  (syntax-parse stx
    [(_for () statement ...)
     (syntax/loc stx (block statement ...))]
    [(_for (clause ...) statement ...)
     ;; Note: The idea is to call (map handle-clause clauses) and the
     ;;       combine the pieces to one large while-loop.
     ;;       Since the same handler can be called multiple times.
     ;;       any variables introduced by a handler needs α-renaming.
     ;;       This is handled by marking the clause before and after
     ;;       calling handle-clause.
     (define clauses                (syntax->list #'(clause ...)))
     (define marks                  (map make-syntax-introducer clauses))
     (define marked-clauses         (for/list ([clause clauses] [mark marks])
                                      (mark clause)))
     (define handled-clauses        (map handle-clause marked-clauses))
     (define marked-handled-clauses (for/list ([handled-clause handled-clauses] [mark marks])
                                      (mark handled-clause)))
     (match (map syntax->list marked-handled-clauses)
       [(list (list var-bindings termination-expr let-bindings step-statements) ...)
        (let ([var-bindings    (append* (map syntax->list var-bindings))]
              [let-bindings    (append* (map syntax->list let-bindings))]
              [step-statements (append* (map syntax->list step-statements))])
          (with-syntax ([(vb ...)   var-bindings]
                        [(te ...)   termination-expr]
                        [(lb ...)   let-bindings]
                        [(step ...) step-statements])
            (syntax/loc stx
              (block/let
               (var vb ...)
               (while (and #t te ...)
                      (let (lb ...)
                        statement ...
                        step ...))
               undefined))))])]))

(define-urlang-macro for expand-for)


;;;
;;; TEST
;;;

#;(
   
   (current-urlang-run?                           #t) ; run using Node?
   (current-urlang-echo?                          #t) ; print generated JavaScript?
   (current-urlang-console.log-module-level-expr? #t) ; print top-level expression?
   
   (urlang
    (urmodule test-for
      (define sum 0)
      (for ([x in-range 1 101])
        (+= sum x))
      sum))
   
   (urlang
    (urmodule test-for
      (define sum 0)
      (for ([x in-array (array 1 2 3 4 5)])
        (+= sum x))
      sum))
   
   (urlang
    (urmodule test-for
      (define sum 0)
      (for ([x in-array (array 1 2 3 4 5)]
            [y in-range 100 200])
        (console.log (+ "" x " " y))
        (+= sum x)
        (+= sum y))
      sum))
   )
