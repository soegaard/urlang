#lang racket
(require syntax/parse syntax/stx "urlang.rkt")
(define-literal-set for-keywords (in-array in-range in-naturals in-string in-value))
(define for-keyword? (literal-set->predicate for-keywords))
(define-syntax-class ForKeyword #:opaque (pattern x #:fail-unless (for-keyword? #'x) #f))

;;;
;;; Racket for-loops for Urlang.
;;;

; The standard JavaScript for-loops aren't as enjoyable to use
; as the Racket counterpart. This module implement Racket style
; for-loops for Urlang (which compiles to JavaScript).

; SYNTAX
; Parallel loops:
;   (for         (clause ...) <statement-or-break> ...)
;   (for/array   (clause ...) <statement-or-break> ... <expression>)
;   (for/or      (clause ...) <statement-or-break> ... <expression>)
;   (for/and     (clause ...) <statement-or-break> ... <expression>)
;   (for/sum     (clause ...) <statement-or-break> ... <expression>)
;   (for/product (clause ...) <statement-or-break> ... <expression>)
; Nested loops:
;   (for* (clause ...) <statement-or-break> ...)
;   ...

; <statement-or-break> is a either
;    a statement
;    #:break <guard-expression>
; If the guard expression evaluates to true, the loop is done.
; Any statements after the #:break <guard> are not evaluated.

; CLAUSES

;  [x in-range from to]
;                       evaluates the expressions from and to
;                       binds x to from, from+1, ..., to-1
;                       Note: from and to are only evaluated once.
;  [x in-naturals from]
;                       binds x to from, from+1, ...
;  [x in-value expr]
;                       binds x to the result of expr
;  [x in-array expr]
;                       evaluates expr
;                       if the result is not an array, an error is thrown
;                       binds x to the elements of the array one at a time
;  [x in-string expr]
;                       evaluates expr
;                       if the result is not a string, an error is thrown
;                       binds x to one character at a time



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
;       ((function(x,y){                    // let x=i, y=a[i_4]
;           (console.log((x+y)));
;           (i+=1);
;           return (i_4+=1);
;           })(i,a[i_4]));
;       };
;     undefined;                           // due to expression context
;     }


(define-urlang-macro block/let
  ; expand to (let () ... undefined) in an expresion context
  ; and to    (block  ...)           otherwise
  (λ (stx)
    (syntax-parse stx
      [(_block/let more ...)
       (match (macro-expansion-context)
         ['expression (syntax/loc stx (let () more ... undefined))]
         [_           (syntax/loc stx (block  more ...))])])))

(define (handle-in-value clause) ; generates a single value
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-value expr]
     #'(([t #t])                 ; list of var clauses to create initial state
        t                 ; termination condition (#t = never terminate)
        ([x expr])         ; let bindings (needs to bind x)
        ((:= t #f)))]))

(define (handle-in-naturals clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-naturals from]
     #'(([i from])      ; list of var clauses to create initial state
        #t              ; termination condition (#f means terminate)
        ([x i])         ; let bindings (needs to bind x)
        ((+= i 1)))]))

(define (handle-in-range clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-range from to]
     #'(([i from]       ; list of var clauses to create initial state
         [t to])
        (< i t)         ; termination condition
        ([x i])         ; let bindings (needs to bind x)
        ((+= i 1)))]))  ; statements to step state forward

(define (handle-in-array clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-array array-expr]
     #'(([a array-expr]       ; list of var clauses to create initial state
         [n a.length]
         [i 0])
        (< i n)              ; termination condition
        ([x (ref a i)]) ; let bindings (needs to bind x)
        ((+= i 1)))]))  ; statements to step state forward

(define (handle-in-string clause)
  (syntax-parse clause
    #:literal-sets (for-keywords)
    [[x in-string string-expr]
     #'(([s string-expr]       ; list of var clauses to create initial state
         [n s.length]
         [i 0])
        (< i n)              ; termination condition
        ([x (ref s i)]) ; let bindings (needs to bind x)
        ((+= i 1)))]))

(define (get-clause-handler clause-stx)
  (syntax-parse clause-stx
    #:literal-sets (for-keywords)
    [ [x in-range    from to]    handle-in-range    ]
    [ [x in-array    array-expr] handle-in-array    ]
    [ [x in-naturals from]       handle-in-naturals ]
    [ [x in-value    expr]       handle-in-value    ]
    [ [x in-string   expr]       handle-in-string   ]
    [_ (raise-syntax-error 'for "unknown clause" clause-stx)]))

(define (expand-for stx)   ; parallel for
  (define (is-break? x)          (or (eq? x '#:break) (and (syntax? x) (is-break? (syntax-e x)))))
  (define (rewrite-breaks statement-or-breaks)
    (match statement-or-breaks
      ['()                                   '()]
      [(list  (? is-break? b))               (raise-syntax-error
                                              'foo "missing guard expression after #:break" b)]
      [(list* (? is-break? b) guard more)    (cons #`(sif #,guard
                                                          (block (:= cont #f) (break skip))
                                                          (sempty))
                                                   (rewrite-breaks more))]
      [(list* statement more)                (cons statement
                                                   (rewrite-breaks more))]))
  (syntax-parse stx
    [(_for () statement-or-break ...)
     (define break-used? (ormap is-break? (syntax->list #'(statement-or-break ...))))
     (with-syntax ([(stat ...) (rewrite-breaks (syntax->list #'(statement-or-break ...)))])
       (if break-used?
           (syntax/loc stx (let ([cont #t]) (label skip (while cont stat ...)) undefined))
           (syntax/loc stx (block stat ...))))]
    [(_for (clause ...) statement-or-break ...)
     ;; Note: The idea is to call (map handle-clause clauses) and the
     ;;       combine the pieces to one large while-loop.
     ;;       Since the same handler can be called multiple times.
     ;;       any variables introduced by a handler needs α-renaming.
     ;;       This is handled by marking the clause before and after
     ;;       calling handle-clause.
     (define clauses                (syntax->list #'(clause ...)))
     (define handlers               (map get-clause-handler clauses))     
     (define marks                  (map make-syntax-introducer clauses))
     (define marked-clauses         (for/list ([clause clauses] [mark marks])
                                      (mark clause)))
     (define handled-clauses        (for/list ([handle handlers] [marked-clause marked-clauses])
                                      (handle marked-clause)))
     (define marked-handled-clauses (for/list ([handled-clause handled-clauses] [mark marks])
                                      (mark handled-clause)))
     (define (is-break? x)          (or (eq? x '#:break) (and (syntax? x) (is-break? (syntax-e x)))))
     (define break-used?            (ormap is-break? (syntax->list #'(statement-or-break ...))))
     (define (maybe-wrap-in-loop-label loop-label statement)
       (if break-used? #`(label #,loop-label #,statement) statement))
     
     (match (map syntax->list marked-handled-clauses)
       [(list (list var-bindings termination-expr let-bindings step-statements) ...)
        (let* ([var-bindings    (append* (map syntax->list var-bindings))]
               [let-bindings    (append* (map syntax->list let-bindings))]
               [step-statements (append* (map syntax->list step-statements))]          
               [statements      (rewrite-breaks (syntax->list #'(statement-or-break ...)))])
          (with-syntax ([(vb ...)   var-bindings]
                        [(te ...)   termination-expr]
                        [(lb ...)   let-bindings]
                        [(step ...) step-statements]
                        [(stat ...) statements])
            (cond
              [(not break-used?)  (syntax/loc stx
                                    (block/let
                                     (var vb ...)
                                     (while (and #t #t te ...)
                                            (let (lb ...)
                                              stat ...
                                              step ...))
                                     undefined))]
              [else               (syntax/loc stx
                                    (block/let
                                     (var vb ... (cont #t))
                                     (while (and #t cont te ...)
                                            (let (lb ...)
                                              ; break does not work across function boundaries
                                              ; and the let introduces one, so we need
                                              ; a fake while here.
                                              (label skip (while #t
                                                                 stat ...
                                                                 step ...
                                                                 (break skip)))
                                              undefined))
                                     undefined))])))])]))

(define (expand-for* stx)
  ; The basic idea is to rewrite for* into nested fors"
  ;      (for* (clause0 clause1 ...) body)
  ;   => (for (clause 0) (for (clause1) ... body))       
  ; Only problem is that #:break <guard> needs to break out to the outer loop,
  ; and since the JavaScript break statement doesn't work across function boundaries,
  ; we need to introduce a try-catch. We only do this if #:break is used though.
  (define (is-break? x)
    (or (eq? x '#:break) (and (syntax? x) (is-break? (syntax-e x)))))
  (define (rewrite-breaks statement-or-breaks)
    (match statement-or-breaks
      ['()                                   '()]
      [(list  (? is-break? b))               (raise-syntax-error
                                              'for* "missing guard expression after #:break" b)]
      [(list* (? is-break? b) guard more)    (cons #`(sif #,guard
                                                          (throw "break: for*")
                                                          (sempty))
                                                   (rewrite-breaks more))]
      [(list* statement more)                (cons statement
                                                   (rewrite-breaks more))]))
  (syntax-parse stx
    ;[(_for* () statement ...)                   (syntax/loc stx (for () statement ...))]
    ;[(_for* (clause0) statement ...)            (syntax/loc stx (for (clause0) statement ...))]
    [(_for* (clause ...) statement-or-break ...)
     (define (for*->for reverse-clauses body)
       ; rewrite for* to nested fors. 
       ;    (for* (clause0 clause1 ...) body)
       ; => (for (clause 0) (for (clause1) ... body))       
       (match reverse-clauses
         ['()                           body]
         [(list clause0)                (with-syntax ([body body] [clause0 clause0])
                                        #'(for (clause0) body))]
         [(list last-clause clause ...) (for*->for
                                         clause
                                         (with-syntax ([body body] [last-clause last-clause])
                                           (syntax/loc stx (for (last-clause) body))))]))
     ; rewrite breaks to break out of the (other) loop
     (define statements (rewrite-breaks (syntax->list #'(statement-or-break ...))))
     ; rewrite for* to nested fors
     (with-syntax ([(statement ...) statements])
       (define body #'(block statement ...))
       (define fors (for*->for (reverse (syntax->list #'(clause ...))) body))
       ; determine if #:break <guard> is used
       (define break-used? (ormap is-break? (syntax->list #'(statement-or-break ...))))
       ; if #:break is used setup a try-catch
       (with-syntax ([fors fors])
         (cond
           [break-used?    (syntax/loc stx
                             (try {fors}
                                  (catch msg
                                    (sif (= msg "break: for*")
                                         (sempty)
                                         (throw msg)))))]
           [else           #'fors])))]))

(define (expand-for/array stx)
  (syntax-parse stx
    [(_for/array #:length length-expr (clause ...) statement-or-break ... expr)
     ; Allocate array of length length-expr and fill in the values afterwards.
     ; This is faster than pushing repeatedly.
     (syntax/loc stx
       (let ()
         (var [n length-expr] [a (new Array n)] [i 0])
         (for (clause ...)
           statement-or-break ...
           (array! a i expr)
           (+= i 1))
         ; fill in remaining elements with 0
         (sif (< i n)
              (for ([j in-range i n]) (array! a j 0))
              (sempty))
         a))]
    [(_for/array (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([a (array)])
         (for (clause ...)
           statement-or-break ...
           (a.push expr))
         a))]))

(define (expand-for*/array stx)
  (syntax-parse stx
    [(_for*/array (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([a (array)])
         (for* (clause ...)
           statement-or-break ...
           (a.push expr))
         a))]))

(define (expand-for/and stx)
  (syntax-parse stx
    [(_for/and (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([b #t])
         (for (clause ...)
           statement-or-break ...
           (:= b (and b expr))
           #:break (not b))
         b))]))

(define (expand-for*/and stx)
  (syntax-parse stx
    [(_for*/and (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([b #t])
         (for* (clause ...)
           statement-or-break ...
           (:= b (and b expr))
           #:break (not b))
         b))]))

(define (expand-for/or stx)
  (syntax-parse stx
    [(_for/or (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([b #f])
         (for (clause ...)
           statement-or-break ...
           (:= b (or b expr))
           #:break b)
         b))]))

(define (expand-for*/or stx)
  (syntax-parse stx
    [(_for*/or (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([b #f])
         (for* (clause ...)
           statement-or-break ...
           (:= b (or b expr))
           #:break b)
         b))]))

(define (expand-for/sum stx)
  (syntax-parse stx
    [(_for/sum (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([s 0])
         (for (clause ...)
           statement-or-break ...
           (+= s expr))
         s))]))

(define (expand-for*/sum stx)
  (syntax-parse stx
    [(_for*/sum (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([s 0])
         (for* (clause ...)
           statement-or-break ...
           (+= s expr))
         s))]))

(define (expand-for/product stx)
  (syntax-parse stx
    [(_for/product (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([p 1])
         (for (clause ...)
           statement-or-break ...
           (*= p expr))
         p))]))

(define (expand-for*/product stx)
  (syntax-parse stx
    [(_for*/product (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([p 1])
         (for* (clause ...)
           statement-or-break ...
           (*= p expr))
         p))]))

(define-urlang-macro for          expand-for)
(define-urlang-macro for*         expand-for*)
(define-urlang-macro for/array    expand-for/array)
(define-urlang-macro for*/array   expand-for*/array)
(define-urlang-macro for/and      expand-for/and)
(define-urlang-macro for*/and     expand-for*/and)
(define-urlang-macro for/or       expand-for/or)
(define-urlang-macro for*/or      expand-for*/or)
(define-urlang-macro for/sum      expand-for/sum)
(define-urlang-macro for*/sum     expand-for*/sum)
(define-urlang-macro for/product  expand-for/product)
(define-urlang-macro for*/product expand-for*/product)

;;;
;;; TEST
;;;

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

   #;(urlang
    (urmodule test-for
      (define i 0)
      (define stop? #f)
      (for ([x in-naturals 5])
        (:= i x))
      i))