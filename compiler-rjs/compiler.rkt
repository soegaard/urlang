#lang racket
(provide eval)
;;;
;;; TODO
;;;

; FIX tail recursion in code generator.
;     the problem is code like   (begin (:= result call)  result)

; - fill the initial namespace
; - or make a kernel-namespace

; - improve code generation for top in generate-ur
;   (the (string->symbol x) is inefficient)

; DONE   1. Handle references to primitives.
; DONE   2. Handle references to operator primitives +,-,...
; DONE   3. non-atomic datums in quote form
; DONE   4. variadic functions
; DONE   5. case-lambda
; DONE   6. higher order functions
; DONE        map, andmap, ormap, build-list, apply
; CLOSE  7. structures
;            works: construction, references, assignments, super structures
;            todo:  non-transparent structs, inspectors, guards, properties, prefab
; CLOSE  8. source locations
; DONE       srcloc, srcloc->string
; TODO       store source locations for each application, etc ...
;        9. parameters
;       10. exceptions
;       11. error checking
;             - applications of non-procedures
;             - arities
;             - wrong number of return values
;       12. top-level-namespaces
;       13. modules
;       14. submodules
;       15. top-level requires
;       16. tco (trampoline)
;       17. with-continuation-mark
;       18. #%variable-reference
;       19 test suite

;;;
;;; NOTES
;;;

;; APPLY
;   The kernel  apply  does not handle keywords, so a racket and racket/base
;   provides new-apply-proc renamed to apply instead.
;   In case apply is used in a situation with no keywords, the application
;   of apply can expand into an application of the kernel apply.
;   This is a bit tricky to handle (before implementing modules), so
;   as a hack the kernel apply is added to the globals in α-rename
;   urlang.rkt.

;; SRCLOC
;   srcloc structures are defined in the kernel, so as a hack,
;   kernel:srcloc is added in α-rename in urlang.rkt.


(module+ test (require rackunit))
(require nanopass/base)
(require
  (prefix-in ur- urlang)
  (for-syntax nanopass/base syntax/parse racket/syntax)
  syntax/kerncase ; for kernel-form-identifier-list
  syntax/stx
  racket/syntax
  (except-in syntax/parse str) ; the identifier str is used in the runtime 
  (rename-in racket/match [match Match])
  (only-in srfi/1 list-index))

;;;
;;; EXPANSION
;;;

;;; Note: To avoid top-level related problems with structure definitions
;;;       the entire program is wrapped in a let.
;;;       Compare (expand #'(begin  (struct foo (bar)) (foo 42))
;;;       and     (expand #'(let () (struct foo (bar)) (foo 42))
;;        Note that the (foo 42) doesn't match the constructor name in the begin snippet.

(define (topexpand top-level-form-stx)
  (parameterize ([current-namespace (make-base-namespace)]
                 [eval-jit-enabled  #f])  ; affects expansion
    ;                                     ; (reverse becomes alt-reverse unless disabled)
    (expand-syntax top-level-form-stx)
    #;(expand-syntax #`(let () #,top-level-form-stx))))

;;;
;;; HELPERS
;;;

(define-syntax (letv stx)
  ; syntax: (letv ((x ...) e) b ...)
  ;   bind the result of e to the variables x ... in the body b ...
  ;   That is: letv is let-values with only one clause
  (syntax-parse stx
    [(_letv ((x:id ...) e:expr) b:expr ...)
     (syntax/loc stx
       (let-values ([(x ...) e]) b ...))]))

(define (map* f αs ρ)
  ; only first value is returned
  (for/fold ([ρ ρ]) ([α αs])
    (f α ρ)))

(define (map2* f xs ρ)
  ; f : α β -> (values α β)
  ; map f over xs while threading the seconding value  
  (define (f* xs ρ)
    (match xs
      ['()         (values '() ρ)]
      [(cons x xs) (letv ((x ρ) (f x ρ))
                     (letv ((xs ρ) (f* xs ρ))
                       (values (cons x xs) ρ)))]))
  (f* xs ρ))

;;;
;;; VARIABLES
;;;

; Representation of variables during compilation.

(struct variable (id) #:transparent)

(define (unparse-variable x)
  (syntax->datum (variable-id x)))

;;; Quick and dirty sets of variables represented as lists
(define (variable=? x y)         (free-identifier=? (variable-id x) (variable-id y)))
(define (ids->id-set xs)         (for/fold ([s '()]) ([x xs]) (set-add s x)))
(define (make-id-set . xs)       (ids->id-set xs))
(define empty-set                (make-id-set))
(define (set-in? x s)            (member x s variable=?))  
(define (set-add s x)            (if (set-in? x s) s (cons x s)))
(define (set-union s1 s2)        (for/fold ([s s1]) ([x s2]) (set-add s x)))
(define (set-union* ss)          (for/fold ([u empty-set]) ([s ss]) (set-union u s)))
(define (set-remove s x)         (remove x s variable=?))
(define (set-difference s1 s2)   (for/fold ([s s1]) ([x s2]) (set-remove s x)))
(define (set-intersection s1 s2) (for/list ([x s1] #:when (set-in? x s2)) x))
(define (set-empty? s)           (equal? s '()))
(define (set-disjoint? s1 s2)    (set-empty? (set-intersection s1 s2)))


;;;
;;; DATUMS AND CONSTANTS
;;;

; Representation of datums during compilation.
; Quotation using quote creates a datum.
; Constants are literals ("selfquoting").
; Parse will introduce explicit quotes for constants.

(struct datum (stx value) #:transparent)
(define (unparse-datum d) (datum-value d))

(define (js-number? v)
  (and (number? v)
       (or (fixnum? v) (inexact? v))))

(define (constant? v)
  (or (js-number? v)
      (boolean? v)
      (symbol? v)))

(define datum:undefined (datum #f #f)) ; TODO: make the undefined value a datum

;;;
;;; PRIMITIVES
;;;

;; For some primitives we have need an easy way to construct a
;; variable reference to the primitive.

(define primitives '())

(define-syntax (define-primitive stx)
  (syntax-parse stx
    [(_define-primitive name)
     ; for each primitive  name  we define a function var:name
     ; that creates a new variable reference to the primitive.
     (define var:name (format-id #'name "var:~a" #'name))
     (with-syntax ([var:name var:name])
       (syntax/loc stx
         (begin
           ; add the primitive (as a symbol) to the primitives list
           (set! primitives (cons 'name primitives))
           (define (var:name) (variable #'name)))))]))

(define-syntax (define-primitives stx)
  (syntax-parse stx
    [(_define-primtives name ...)
     (syntax/loc stx
       (begin
         (define-primitive name)
         ...))]))

(require (for-syntax (only-in urlang urmodule-name->exports)))

(define-syntax (define-runtime-primitives stx)
  (syntax-parse stx
    [(_define-runtime-primitives)
     (with-syntax ([(pr ...)
                    (for/list ([p (urmodule-name->exports 'runtime)])
                      (datum->syntax stx p))])
       (syntax/loc stx
         (define-primitives pr ...)))]))

;; declare names as primitives - the compiler now treats these names as primitives
(define-runtime-primitives)

;; Primitives with names special names.
;; The following primitives require special attention.
;; The identifiers +, -, ... can't be used as names for functions in JavaScript.
;; The compiler knows how to open code the operators.
;; References to a primitive + will be compiled into PRIM+ which
;; is exported from the runtime.

(define-primitives
  + - * /
  = < > <= >=
  null   ; special (called Null in JavaScript)
  void)  ; special (called Vull in JavaScript)

(define-primitives
  ; these are not primitives in Racket, but are reserved names in Urlang
  not)


(define (primitive? v)
  (and (or (and (syntax? v)   (primitive? (syntax-e v)))
           (and (variable? v) (primitive? (variable-id v)))
           (member v primitives))
       #t))

(define (unparse-primitive pr)
  (unparse-variable pr))


;;;
;;; SOURCE LANGUAGE
;;;

;; The following language is a large subset of the Fully Expanded Syntax
;; which is the output of the Racket expander. Since the output of
;; expand-syntax has been through the macro expander, the output
;; has already been verified (i.e. no need to check for duplicate
;; identifiers etc.).

(define-language LFE    ; FE = Fully Expanded
  (entry TopLevelForm)
  (terminals
   ((variable    (x xd)) . => . unparse-variable)
   ((datum       (d))    . => . unparse-datum)
   (syntax       (s)))
  (Formals (f)
    (formals (x ...))            => (x ...)
    (formals (x0 x1 ... . xd))   => (x0 x1 ... . xd)
    (formals x)                  => x)
  (TopLevelForm (t)
    ; turns out it is best keep unique tags (Expr also has a begin)
    (topbegin s t ...)                            => (begin t ...)
    (#%expression s e)                            => (#%expression e)
    g)  
  (GeneralTopLevelForm (g)
    e
    (define-values   s (x ...) e)                 => (define-values   (x ...) e)
    (define-syntaxes s (x ...) e)                 => (define-syntaxes (x ...) e)
    (#%require s rrs ...)                         => (#%require s rrs ...))
  (RawRequireSpec     (rrs) rrmp) ; todo
  ;(RawRequireSpec    (rrs) ps)   ; todo  <- the correct one
  ;(PhaselessSpec     (ps)  rmp)  ; todo
  ;(RawModulePath     (rmp) rrmp) ; todo
  (RawRootModulePath (rrmp) (quote x))
  (Expr (e)
    x
    (λ s f e ...)                                 => (λ f e ...)                      ; #%plain-lambda
    (case-lambda s (f e0 e ...) ...)              => (case-lambda (f e0 e ...) ...)
    (if s e0 e1 e2)                               => (if e0 e1 e2)
    (begin  s e0 e1 ...)                          => (begin  e0 e1 ...)
    (begin0 s e0 e1 ...)                          => (begin0 e0 e1 ...)
    (let-values    s ([(x ...) e] ...) e0 e1 ...) => (let-values    ([(x ...) e] ...) e0 e1 ...)
    (letrec-values s ([(x ...) e] ...) e0 e1 ...) => (letrec-values ([(x ...) e] ...) e0 e1 ...)
    (set! s x e)                                  => (set! x e)
    (quote s d)                                   => (quote d)
    (quote-syntax s d)                            => (quote-syntax d) 
    (wcm s e0 e1 e2)                              => (with-continuation-mark e0 e1 e2)
    (app s e0 e1 ...)                             => (e0 e1 ...)             ; (#%plain-app e0 e1 ...)
    (top s x)                                     => (#%top . x)))

;;; 
;;; Parse
;;;

;; The standard tools in Racket represents programs as syntax objects.
;; In particular the output of expand-syntax is a syntax object representing
;; a program in fully expanded form. Here parse takes such a syntax object
;; and return a nanopass representation of the fully expanded program.

(define-pass parse : * (stx) -> LFE ()
  (definitions
    (define (Datum E d)                (datum E (syntax->datum d)))
    (define (Expr* es)                 (map Expr              (stx->list es)))
    (define (Expr** ess)               (map Expr*             (stx->list ess)))
    (define (Formals* fs)              (map Formals           (stx->list fs)))
    (define (TopLevelForm* ts)         (map TopLevelForm      (stx->list ts)))
    (define (variable* xs)             (map variable          (stx->list xs)))
    (define (RawRequireSpec* rsss)     (map RawRequireSpec    (stx->list rsss)))
    (define (RawRootModulePath* rrmps) (map RawRootModulePath (stx->list rrmps))))
  
  (Formals : * (F) -> Formals ()
    (with-output-language (LFE Formals)
      (syntax-parse F
        [(x:id ...)               `(formals (,(variable* #'(x ...)) ...))]
        [(x0:id x:id ... . xd:id) `(formals (,(variable #'x0) ,(variable* #'(x ...)) ...
                                                              . ,(variable #'xd)))]
        [x:id                     `(formals ,(variable #'x))]
        [_ (raise-syntax-error 'parse "expected formals" F)])))
  
  (TopLevelForm : * (T) -> TopLevelForm ()
    (with-output-language (LFE TopLevelForm)
      (syntax-parse T #:literal-sets (kernel-literals) ; keywords in fully expanded programs
        [(begin t ...)                `(topbegin ,T ,(TopLevelForm* #'(t ...)) ...)]
        [(#%expression e)             `(#%expression ,T ,(Expr #'e))]
        [g                            `,(GeneralTopLevelForm #'g)])))
  
  (GeneralTopLevelForm : * (G) -> GeneralTopLevelForm ()
    (with-output-language (LFE GeneralTopLevelForm)
      (syntax-parse G #:literal-sets (kernel-literals) ; keywords in fully expanded programs
        [(define-values   (x ...) e)  `(define-values   ,G (,(variable* #'(x ...)) ...) ,(Expr #'e))]
        [(#%require rrs ...)          `(#%require       ,G ,(RawRequireSpec* #'(rrs ...)) ...)]
        [(define-syntaxes (x ...) e)  (Expr #''"define-syntaxes is ignored")]
        ; Note: The (Expr #'e) will fail. The syntax patterns will fail: the literals are
        ;       in phase 0 and the expression e is in phase 1.
        ; `(define-syntaxes ,G (,(variable* #'(x ...)) ...) ,(Expr #'e))]
        
        [e                            `,(Expr #'e)])))
  
  (RawRequireSpec : * (RRS) -> RawRequireSpec ()
    (with-output-language (LFE RawRequireSpec)
      (displayln (list 'in RRS))
      (RawRootModulePath RRS))) ; TODO - temporary solution

  (RawRootModulePath : * (RRMP) -> RawRootModulePath ()
    (with-output-language (LFE RawRootModulePath)
      (syntax-parse RRMP #:literal-sets (kernel-literals)
        [(quote x)      `(quote ,(variable #'x))]
        [x              `,(variable #'x)])))
  ; (PhaselessSpec     (ps)  rmp)
  ; (RawModulePath     (rmp) rrmp)
  
  (Expr : * (E) -> Expr ()
    (with-output-language (LFE Expr)
      (syntax-parse E #:literal-sets (kernel-literals)
        [x:id                                      `,(variable #'x)]
        [c #:when (constant? (syntax->datum #'c))  `(quote ,E ,(Datum E #'c))]
        [(if e0 e1 e2)                             `(if ,E ,(Expr #'e0) ,(Expr #'e1) ,(Expr #'e2))]
        [(begin  e0 e1 ...)                        `(begin  ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(begin0 e0 e1 ...)                        `(begin0 ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(#%plain-lambda f e ...+)                 `(λ ,E ,(Formals #'f)
                                                      ,(Expr*  #'(e ...)) ...)]
        [(case-lambda (f e ...+) ...)              (let* ([f (Formals* #'(f ...))]
                                                          [e (Expr**   #'((e ...) ...))])
                                                     (match e
                                                       [(list (list e0 e ...) ...)
                                                        `(case-lambda ,E (,f ,e0 ,e ...) ...)]))]
        [(quote d)                                 `(quote ,E ,(Datum E #'d))]
        [(quote-syntax d)                          `(quote-syntax ,E ,(Datum E #'d))]
        [(let-values ([(x ...) e] ...) e0 e1 ...)  (let ()
                                                     (define xss (map variable*
                                                                      (syntax->list #'((x ...) ...))))
                                                     (define es  (Expr* #'(e ...)))
                                                     `(let-values ,E ([(,xss ...) ,es] ...)
                                                        ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...))]
        ;; Temporarily expand letrec such that letrec only binds lambdas
        [(letrec-values ([(x ...) e] ...) e0 e1 ...)
         ;    (letrec-values ([(x ...) ce] ...) e)
         ; => (let ([x undefined] ... ...])
         ;      (letrec ([xl le] ...)
         ;        (let-values ([(t ...) ce])
         ;           (set! x t) ...)
         ;        ...
         ;        e)
         ; where x is divided into xl
         (define (lambda-clause? stx)
           (syntax-parse stx #:literal-sets (kernel-literals) [[(x) (#%plain-lambda . _)] #t] [_ #f]))
         (define clauses (syntax->list #'([(x ...) e] ...)))
         ; partition clauses into lambda clauses and complex clauses
         (define-values (lambda-clauses complex-clauses) (partition lambda-clause? clauses))
         (define/with-syntax ([(xl)     le] ...) lambda-clauses)
         (define/with-syntax ([(xc ...) ce] ...) complex-clauses)
         (let* ([xc* (variable* (syntax->list #'(xc ... ...)))]
                [0s  (map (λ(_) `(quote ,E ,(datum E 0))) xc*)]
                [xl  (variable* (syntax->list #'(xl ...)))]
                [le  (Expr* #'(le ...))])
           (define (build-begin s . Es)
             (match (append* Es)
               [(list E0)        E0]
               [(list E0 E1 ...) `(begin ,s ,E0 ,E1 ...)]))
           `(let-values ,E ([(,xc*) ,0s] ...)      ; declare as undefined
              (letrec-values ,E ([(,xl) ,le] ...)  ; fix lambda expressions
                ,(build-begin
                  E (for/list ([complex complex-clauses])
                      (syntax-parse complex
                        [[(xc)     ce]  (Expr #'(set! xc ce))]
                        [[(xc ...) ce]  (with-syntax ([(tc ...) (generate-temporaries #'(xc ...))])
                                          (Expr #'(let-values ([(tc ...) ce])
                                                    (set! xc tc) ...)))]))
                  (Expr* #'(e0 e1 ...))))))]
        [(set! x:id e)                              `(set! ,E ,(variable #'x) ,(Expr #'e))]
        [(with-continuation-mark e0 e1 e2)          `(wcm ,E ,(Expr #'e0) ,(Expr #'e1) ,(Expr #'e2))]
        [(#%plain-app e0 e1 ...)                    `(app ,E ,(Expr #'e0) ,(Expr* #'(e1 ...)) ...)]
        [(#%top . x)                                `(top ,E ,(variable #'x))]
        [_ (displayln E)
           (error 'parse "expected <expression> got: ~a" E)])))
  ; start parsing
  (TopLevelForm stx))


(module+ test
  (let ([test (λ (stx) (unparse-LFE (parse (expand-syntax stx))))])
    (check-equal? (test #'y) '(#%top . y))
    (check-equal? (test #''(1 x)) ''(1 x))
    (check-equal? (test #'(#%plain-lambda (x) '1)) '(λ (x) '1))
    (check-equal? (test #'(let-values ([(x y) 1]) 3)) '(let-values ([(x y) '1]) '3))
    (check-equal? (test #'(let-values ([(x y) 1] [(z) 2]) 3 4))
                  '(let-values ([(x y) '1] [(z) '2]) '3 '4))
    ; todo: insert test of letrec
    (check-equal? (test #'(set! x 3)) '(set! x '3))
    (check-equal? (test #'(if 1 2 3)) '(if '1 '2 '3))
    (check-equal? (test #'(begin  1 2 3)) '(begin  '1 '2 '3))
    (check-equal? (test #'(begin0 1 2 3)) '(begin0 '1 '2 '3))
    (check-equal? (test #'(with-continuation-mark 1 2 3)) '(with-continuation-mark '1 '2 '3))
    (check-equal? (test #'(#%plain-app + 1 2)) '(+ '1 '2))
    (check-equal? (test #'(#%top . foo)) '(#%top . foo))
    (check-equal? (test #'(λ (x . y) (+ x y))) '(#%expression (λ (x . y) (+ x y))))
    (check-equal? (test #'(λ x (+ x 1))) '(#%expression (λ x (+ x '1))))))

;;;
;;; QUOTATIONS
;;;

;;; Atomic values that are allowed as quotations in Urlang are unchanged.
;;; Quoted non-Urlang-atomic and non-atomic values are collected.
;;; A quotation  '(foo bar) becomes   _quote44
;;; and      (define _quote42 (string->symbol "foo"))
;;;          (define _quote43 (string->symbol "bar"))
;;;          (define _quote44 (list _quote42 _quote42))
;;; are lifted to the beginning of the top-level.
;;; Optimization:
;;;    Since symbols are interned, multiple quotations of the same symbol
;;;    will reference the same _quote variable.

(define-pass convert-quotations : LFE (T) -> LFE ()
  (definitions
    (define h #'cq)
    (define quotation-defines '())  ; in reverse order    
    (define symbols-ht (make-hasheq))
    (define (add-quotation! define-form)
      (set! quotation-defines (cons define-form quotation-defines)))
    (define (quoted-symbol! sym)
      (match (hash-ref symbols-ht sym #f)
        [#f (let ([id (new-var #'_quote)] [str (symbol->string sym)])
              (with-output-language (LFE GeneralTopLevelForm)
                (hash-set! symbols-ht sym id)
                (add-quotation!
                 `(define-values ,h (,id)
                    (app ,h ,(var:string->symbol) (quote ,h ,(datum h (~a sym)))))))
              id)]
        [id id]))
    (define (datum->construction-expr s v)
      (define (loop v) (datum->construction-expr s v))
      (with-output-language (LFE Expr)
        (match v
          ; this should match the definition of ur-datum?
          [(and (or (? ur-fixnum?) (? flonum?) (? string?) (? boolean?)) v)
           `(quote ,s ,(datum s v))]
          [(? symbol? sym)  (quoted-symbol! sym)]
          [(? pair? p)      `(app ,h ,(var:cons) ,(loop (car p)) ,(loop (cdr p)))]
          [(? vector? v)    (let ([vs (map loop (vector->list v))])
                              `(app ,h ,(var:vector-immutable) ,vs ...))]
          [(? box? b)        `(app ,h ,(var:box-immutable) ,(loop (unbox b)))]
          ; handle NULL and strings in code generations
          [else             `(quote ,h ,(datum s v))]))))
  (Expr : Expr (e) -> Expr ()
    [(quote ,s ,d)
     (datum->construction-expr s (datum-value d))])
  (TopLevelForm : TopLevelForm (t) -> TopLevelForm ())

  (let ([T (TopLevelForm T)]
        [quotations (reverse quotation-defines)])
    `(topbegin ,h ,quotations ... ,T)))

;;;
;;; explicit-begin
;;;

;;  Rewrite the bodies of λ, let-values and letrec-values to a single expression.
;;  This simplifies the treatment of let-values and letrec-values later on.
;;  (Read: Fewer rules contains the pattern e ... )

(define-language LFE1 (extends LFE)
  (Expr (e)
    (- (λ s f e ...)
       (let-values    s ([(x ...) e] ...) e0 e1 ...)
       (letrec-values s ([(x ...) e] ...) e0 e1 ...)
       (case-lambda s (f e0 e ...) ...))
    (+ (λ s f e)                               => (λ f e)
       (let-values     s ([(x ...) e] ...) e0) => (let-values    ([(x ...) e] ...) e0)
       (letrec-values  s ([(x ...) e] ...) e0) => (letrec-values ([(x ...) e] ...) e0)
       (case-lambda    s (f e) ...)            => (case-lambda   (f e) ...))))

(define-language LFE2 (extends LFE1)
  (Abstraction (ab)
    (+ (λ s f e) => (λ f e)))
  (Expr (e)
    (- (λ s f e)
       (case-lambda s (f e) ...))
    (+ ab
       (case-lambda s ab ...) => (case-lambda ab ...))))


(define-pass explicit-begin : LFE (T) -> LFE1 ()
  (definitions
    (define h #'eb) ; h = here = source location info
    (define (Begin s e0 es)
      ;; don't wrap single expressions in (begin ...)
      (with-output-language (LFE1 Expr)
        (match (length es)
          [0 e0]
          [_ `(begin ,s ,e0 ,es ...)]))))
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Formals             : Formals             (F) -> Formals             ())
  (Expr : Expr (E) -> Expr ()
    [(λ ,s ,[f] ,[e0] ,[e] ...)
     `(λ ,s ,f ,(Begin h e0 e))]
    [(let-values     ,s ([(,x ...) ,[e]] ...) ,[e0] ,[e1] ...)
     `(let-values    ,s ([(,x ...) ,e] ...) ,(Begin h e0 e1))]
    [(letrec-values  ,s ([(,x ...) ,[e]] ...) ,[e0] ,[e1] ...)
     `(letrec-values ,s ([(,x ...) ,e] ...) ,(Begin h e0 e1))]
    [(case-lambda ,s (,[f] ,[e0] ,[e] ...) ...)
     (let* ([hs (map (λ(_) h) f)]
            [e  (map Begin hs e0 e)])
       `(case-lambda ,s (,f ,e) ...))]))

#;(module+ test
    (let ([test (λ (stx)
                  (unparse-LFE1
                   (explicit-begin
                    (parse                  
                     (expand-syntax stx)))))])
      (check-equal? (test #'(let-values ([(x) 1]) 2 3))
                    '(let-values ([(x) '1]) (begin '2 '3)))))


;;;
;;; explicit-case-lambda
;;;

;;  Rewrite case-lambda to have explicit lambda.
;;             (case-lambda [f e] ...)
;;  becomes    (case-lambda (λ f e) ...)


(define-pass explicit-case-lambda : LFE1 (T) -> LFE2 ()
  (definitions
    (define h #'ecl)) ; h = here = source location info
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Formals             : Formals             (F) -> Formals             ())
  (Expr : Expr (E) -> Expr ()
    [(λ ,s ,[f] ,[e])
     (with-output-language (LFE2 Abstraction)
       `(λ ,s ,f ,e))]
    [(case-lambda ,s (,[f] ,[e]) ...)
     (let ([s* (map (λ(_) s) f)])
       `(case-lambda ,s (λ ,s* ,f ,e) ...))]))

;;;
;;; Remove begin0
;;;

; This pass illustrates how to write a simple pass that rewrites
; a single construct. Notice that a rule for each nonterminal is needed.
; The [_] in (begin0 ,s ,[e0] ,[e1] ...) ensures that e0 e1 .. are
; already rewritten into LFE3 Expression when the right hand side
; is evaluated. Note also that the variable t0 is not represented
; as a symbol, and thus  (let ([t0 (new-var 't0)]) ... ,t0) is
; needed to bind t0 to a variable (represented as a struct).

;(define-language LFE3 (extends LFE1)
;    (Expr (e)
;      (- (begin0 s e0 e1 ...))))

;(define-pass remove-begin0 : LFE1 (T) -> LFE3 ()
;    (definitions (define h #'rb0)) ; h = here = source location info    
;    (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
;    (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
;    (Formals             : Formals             (F) -> Formals             ())
;    (Expr : Expr (E) -> Expr ()
;      [(begin0 ,s ,[e0] ,[e1] ...)
;       ; TODO: This rewrite rule breaks if e0 returns multiple values
;       (let ([t0 (new-var 't0)])
;         `(let-values ,s ([(,t0) ,e0]) ,`(begin ,h ,t0 ,e1 ... ,t0)))]))



;;;
;;; α-RENAMING
;;;

; To make the next pass easier to implement we do α-renaming.

; The α-renaming pass makes names unique.
;
; Example:
;                 (let ([x 1]) (let ([x   2]) x))
; == α-rename ==> (let ([x 1]) (let ([x.2 2]) x.2))
;
; The code below uses an environment ρ that maps original identifiers
; into ones used in the output. Identifiers refering to primitives are
; mapped to themselves in order not to rename primitives in the output.

; FACT: α-renaming will not rename identifiers bound to primitives.
;       After renaming an indentifier that "looks like a primitive is a
;       primitive".
;       Example:  (begin (define + 42) (+ 1 2))))))
;         becomes (begin (define-values (|+.8|) '42) (|+.8| '1 '2))
;         The + that didn't refer to a primitive was renamed.

; An unbound identifier is mapped to #f by the environment ρ.

; The expression (fresh x ρ) returns a new identifier not already mapped in ρ.
; The new name is based in the identifier x.

;; New identifiers are produced by new-var.

(define counter 0)
(define joiner "_")
(define (new-var [prefix "t"])
  (set! counter (+ counter 1))
  (define pre (cond
                [(syntax?   prefix) (syntax-e prefix)]
                [(variable? prefix) (syntax-e (variable-id prefix))]
                [else prefix]))
  (variable (datum->syntax #'here (string->symbol (~a pre joiner counter)))))
(define (reset-counter! [joiner ""])
  (set! counter 0))
(define-syntax (with-fresh* stx)
  (syntax-parse stx
    [(_with-fresh* (ts:id xs:expr) b ...)
     (syntax/loc stx
       (let ([ts (map new-var xs)])
         b ...))]))




(define (id=? v1 v2)
  ; TODO: The issue is that expand-syntax puts marks on identifiers.
  ;       The environment knows the difference between two identifiers
  ;       with the same symbolic names (the environment uses free-identifier=?).
  ;       Here we want to rename identifiers if the symbolic names clash.
  ;       Should the environment allow lookup with two different predicates?
  ; HACK HACK - a quick hack for now
  ; Actually: After α-renaming all names are unique so we are in the clear.
  (eq? (syntax-e (variable-id v1)) (syntax-e (variable-id v2)))
  #;(define r (free-identifier=? (variable-id v1) (variable-id v2)))
  #;(displayln (list 'id=? v1 v2 '=> r))
  #;r)


(define-pass α-rename : LFE2 (T) -> LFE2 ()
  (definitions
    (define (reserved-urlang-keyword? id)
      ; names that are reserved keywords in Urlang needs to be renamed
      (syntax-parse (variable-id id) #:literals (not)
        [not (variable #'PRIM_not)]
        [_  #f]))
    (define (initial-ρ x)
      (cond [(reserved-urlang-keyword? x) => values]
            [(primitive? (variable-id x))    x]
            [else                            #f]))
    (define (extend ρ original renamed)
      (λ (x) (if (id=? x original) renamed (ρ x))))
    (define (fresh x ρ [orig-x x])
      (if (ρ x) (fresh (new-var x) ρ x) x))
    (define (rename x ρ)
      (define x* (fresh x ρ))
      (values x* (extend ρ x x*)))
    (define (rename*       xs ρ) (map2* rename   xs ρ))
    (define (rename**     xss ρ) (map2* rename* xss ρ))
    (define (TopLevelForm* xs ρ) (map2* TopLevelForm xs ρ))
    (define (Formals*      fs ρ) (map2* Formals fs ρ))
    (define (Abstraction* abs ρ) (map2* Abstraction abs ρ))
    ; Expr* : no threading! all e use same env
    (define (Expr*         es ρ) (map (λ (e) (letv ((e _) (Expr e ρ)) e)) es))    
    (define (Binding* xss es ρ)
      (letv ((es) (Expr* es ρ))  ; xss are not bound in es
        (letv ((xss ρ) (rename** xss ρ))
          (values xss es ρ))))
    (define (RecBinding* xss es ρ)
      (letv ((xss ρ) (rename** xss ρ))
        (letv ((es) (Expr* es ρ)) ; xss are bound in es
          (values xss es ρ)))))
  
  (TopLevelForm : TopLevelForm (T ρ) -> TopLevelForm (ρ)
    [(topbegin ,s ,t ...) (letv ((t ρ) (TopLevelForm* t ρ))
                            (values `(topbegin ,s ,t ...) ρ))]
    [(#%expression ,s ,e) (letv ((e ρ) (Expr e ρ))
                            (values `(#%expression ,s ,e) ρ))]
    [,g                   (letv ((g ρ) (GeneralTopLevelForm g ρ))
                            (values `,g ρ))])
  
  (GeneralTopLevelForm : GeneralTopLevelForm (G ρ) -> GeneralTopLevelForm (ρ)
    [,e                                 (letv ((e ρ) (Expr e ρ))
                                          (values `,e ρ))]
    [(define-values   ,s (,x ...) ,e)   (letv ((x ρ) (rename* x ρ))
                                          (letv ((e _) (Expr e ρ))
                                            (values `(define-values ,s (,x ...) ,e) ρ)))]
    [(define-syntaxes ,s (,x ...) ,e)   (error)])
  
  (Formals : Formals (F ρ) -> Formals (ρ)
    [(formals (,x ...))               (letv ((x ρ) (rename* x ρ))
                                        (values `(formals (,x ...)) ρ))]
    [(formals (,x0 ,x1 ... . ,xd))    (letv ((x0 ρ) (rename x0 ρ))
                                        (letv ((x1 ρ) (rename* x1 ρ))
                                          (letv ((xd ρ) (rename xd ρ))
                                            (values `(formals (,x0 ,x1 ... . ,xd)) ρ))))]
    [(formals ,x)                     (letv ((x ρ) (rename x ρ))
                                        (values `(formals ,x) ρ))])
  
  (Abstraction : Abstraction (AB ρ) -> Abstraction (ρ)
    [(λ ,s ,f ,e)                               (let ([ρ-orig ρ])
                                                  (letv ((f ρ) (Formals f ρ))
                                                    (letv ((e ρ) (Expr e ρ))
                                                      (values `(λ ,s ,f ,e) ρ-orig))))])
  (Expr : Expr (E ρ) -> Expr (ρ)
    [,x                                         (let ([ρx (ρ x)])
                                                  (unless ρx
                                                    (raise-syntax-error
                                                     'α-rename "compiler.rkt: unbound variable"
                                                     (variable-id x)))
                                                  (values `,ρx ρ))]
    [,ab                                        (Abstraction ab ρ)]
    [(case-lambda ,s ,ab ...)                   (let ([ρ-orig ρ])
                                                  (let-values ([(ab ρ) (Abstraction* ab ρ)])
                                                    (values `(case-lambda ,s ,ab ...) ρ-orig)))]
    [(if ,s ,e0 ,e1 ,e2)                        (letv ((e0 ρ) (Expr e0 ρ))
                                                  (letv ((e1 ρ) (Expr e1 ρ))
                                                    (letv ((e2 ρ) (Expr e2 ρ))
                                                      (values `(if ,s ,e0 ,e1 ,e2) ρ))))]
    [(begin  ,s ,e0 ,e1 ...)                    (letv ((e0 _) (Expr e0 ρ))
                                                  (letv ((e1) (Expr* e1 ρ))
                                                    (values `(begin ,s ,e0 ,e1 ...) ρ)))]
    [(begin0 ,s ,e0 ,e1 ...)                    (letv ((e0 _) (Expr e0 ρ))
                                                  (letv ((e1) (Expr* e1 ρ))
                                                    (values `(begin0 ,s ,e0 ,e1 ...) ρ)))]
    [(let-values ,s ([(,x ...) ,e] ...) ,e0)    (let ([ρ-orig ρ]) 
                                                  (letv ((x e ρ) (Binding* x e ρ))
                                                    (letv ((e0 ρ) (Expr e0 ρ))
                                                      (values `(let-values ,s ([(,x ...) ,e] ...)
                                                                 ,e0) ρ-orig))))]
    [(letrec-values ,s ([(,x ...) ,e] ...) ,e0) (let ([ρ-orig ρ])
                                                  (letv ((x e ρ) (RecBinding* x e ρ))
                                                    (letv ((e0 ρ) (Expr e0 ρ))
                                                      (values `(letrec-values
                                                                   ,s ([(,x ...) ,e] ...)
                                                                 ,e0) ρ-orig))))]
    [(set! ,s ,x ,e)                            (let ([x (or (ρ x) x)]) ; ignores unbound x
                                                  ; note: If x is unbound, then a module-level
                                                  ; assignment should give an error.
                                                  ; A top-level assignment is ok.
                                                  ; Compare: Racket compile
                                                  (letv ((e ρ) (Expr e ρ))
                                                    (values `(set! ,s ,x ,e) ρ)))]
    [(wcm ,s ,e0 ,e1 ,e2)                       (letv ((e0 ρ) (Expr e0  ρ))
                                                  (letv ((e1 ρ) (Expr e1  ρ))
                                                    (letv ((e2 ρ) (Expr e2  ρ))
                                                      (values
                                                       `(wcm ,s ,e0 ,e1 ,e2) ρ))))]
    [(app ,s ,e0 ,e1 ...)                     (letv ((e0 _) (Expr e0 ρ))
                                                (letv ((e1) (Expr* e1 ρ))
                                                  (values `(app ,s ,e0 ,e1 ...) ρ)))]
    ; Note: top-level-variables are looked up by name in the namespace,
    ;       so they can't be renamed.
    [(top ,s ,x)                              (values `(top ,s ,x) ρ)])
  
  (letv ((T ρ) (TopLevelForm T initial-ρ))
    T))


(module+ test
  #;(let ([test (λ (stx) (reset-counter!)
                  (unparse-LFE2 (α-rename
                                 (explicit-case-lambda
                                  (explicit-begin
                                   (parse (expand-syntax stx)))))))])
      (check-equal? (test #'(let ([x 10])
                              (let ([x 1]
                                    [y x])
                                y)))
                    '(let-values (((x) '10))
                       (let-values (((x.6) '1)
                                    ((y)   x))
                         y)))
      
      (check-equal? (test #'(let-values ([(x y z) 1] [(a b c) 2])
                              (let-values ([(x y) 1] [(a b) 2])
                                (begin x y z a b c))))
                    '(let-values (((x y z) '1) ((a b c) '2))
                       (let-values (((x.1 y.2) '1) ((a.3 b.4) '2))
                         (begin x.1 y.2 z a.3 b.4 c))))))

;;;
;;; ASSIGNMENT CONVERSION
;;;

;; Assignment conversion consists of two sub-passes:
;;     - collect-assignable-variables
;;     - box-mutables

;; The first pass returns an id-set of all variables that are (potentially) assigned to.
;; The second pass converts assignable variables into boxes and assignments into box mutations.

(define-pass collect-assignable-variables : LFE2 (T) -> * ()
  ;; Assumption: α-conversion has been done
  (definitions
    (define (TopLevelForm* Ts  xs)          (map* TopLevelForm Ts  xs))
    (define (Expr*         Es  xs)          (map* Expr         Es  xs))
    (define (Abstraction*  ABs xs) (append* (map* Abstraction  ABs xs))))
  (TopLevelForm : TopLevelForm (T xs) -> * (xs)
    [(topbegin ,s ,t ...)  (TopLevelForm* t xs)]
    [(#%expression ,s ,e)  (Expr e xs)]
    [,g                    (GeneralTopLevelForm g xs)])  
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> * (xs)
    [,e                                 (Expr e xs)]
    [(define-values   ,s (,x ...) ,e)   (Expr e xs)]
    [(define-syntaxes ,s (,x ...) ,e)   (Expr e xs)]
    [(#%require       ,s ,rrs ...)      empty-set])
  (Abstraction : Abstraction (AB xs) -> * (xs)
    [(λ ,s ,f ,e)                               (Expr e xs)])
  (Expr : Expr (E xs) -> * (xs)
    [,x                                         xs]
    [,ab                                        (Abstraction ab xs)]
    [(case-lambda ,s ,ab ...)                   (Abstraction* ab xs)]
    [(if ,s ,e0 ,e1 ,e2)                        (Expr* (list e0 e1 e2) xs)]
    [(begin  ,s ,e0 ,e1 ...)                    (Expr* (cons e0 e1) xs)]
    [(begin0 ,s ,e0 ,e1 ...)                    (Expr* (cons e0 e1) xs)]
    [(let-values    ,s ([(,x ...) ,e] ...) ,e0) (Expr* e (Expr e0 xs))]
    [(letrec-values ,s ([(,x ...) ,e] ...) ,e0) (Expr* e (Expr e0 xs))]
    [(set! ,s ,x ,e)                            (cons x (Expr e xs))]    
    [(top ,s ,x)                                xs]
    [(quote ,s ,d)                              xs]
    [(quote-syntax ,s ,d)                       xs]
    [(wcm ,s ,e0 ,e1 ,e2)                       (Expr* (list e0 e1 e2) xs)]
    [(app ,s ,e0 ,e1 ...)                       (Expr* (cons e0 e1) xs)])  
  (apply make-id-set (TopLevelForm T '())))

(define-pass box-mutables : LFE2 (T ms) -> LFE2 ()
  ;; Assumption: α-conversion has been done
  ;;  ms = assignable variables collected by collect-assignable-variables
  
  ;; The basic idea is to heap allocate the location for a mutable variable.
  ;; If (set! x e) is present in the program, then x is a mutable variable.
  ;; When a mutable variable is bound, it is bound to box keeping the value,
  ;; all assignments are then transformed into set-box.
  ;;    (let ([x 1]) (set! x 2))  ==>  (let ([x (box 1)]) (set-box! x 2))
  ;; In a (let-values ([(x ...) e] ...) b) expression the e expressions
  ;; can return multiple values, and only those corresponding to mutable
  ;; variables need to be boxed. Example:
  ;;     (let-values ([(x y) (values 1 2)]) (set! x 3))
  ;; ==> (let-values ([(x y) (let-values ([(tx ty) (values 1 2)])
  ;;                           (values (box tx) ty))
  ;;       (set-box! x 3))
  ;; Note that only clauses with mutable variables need this extra let-values expression.
  (definitions
    (define h #'ac)
    (define (mutable? x)   (set-in? x ms))
    (define (immutable? x) (not (mutable? x)))    
    (define (formal-variables F)
      ; list of variables occuring as formal arguments
      (nanopass-case (LFE2 Formals) F
        [(formals (,x ...))            x]
        [(formals (,x0 ,x1 ... . ,xd)) (cons x0 (append x1 (list xd)))]
        [(formals ,x)                  (list x)]))
    (define (Box e)     (with-output-language (LFE2 Expr) `(app ,h ,(var:box) ,e)))
    (define (Unbox e)   (with-output-language (LFE2 Expr) `(app ,h ,(var:unbox) ,e)))
    (define (Undefined) (with-output-language (LFE2 Expr) `(quote ,h ,datum:undefined)))
    (define (LambdaBody s f fs e)
      ; fs are the variables to be bound in the body e
      (with-output-language (LFE2 Expr)
        (define (Begin es)  (match es [(list e0 e1 ...) `(begin ,h ,e0 ,e1 ...)]))
        (match (set-disjoint? fs ms) 
          [#t `,e]
          [_ (Begin (append (for/list ([a (set-intersection fs ms)])
                              `(set! ,h ,a ,(Box a)))
                            (list e)))])))
    (define (RHS xs e) ; rewrite right hand side of binding clause
      (define n (length xs))
      (if (andmap immutable? xs)
          e
          (match n
            [0 e]
            [1 (if (mutable? (first xs)) (Box e) e)]
            [_ (with-fresh* (ts xs)
                 (with-output-language (LFE2 Expr)
                   (let ([bt (for/list ([x xs] [t ts])
                               (if (mutable? x) (Box t) t))])
                     `(let-values ,h ([(,ts ...) ,e])
                        (app ,h ,(var:values) ,bt ...)))))]))))
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (Formals             : Formals             (F) -> Formals             ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ()
    [(define-values   ,s (,x ...) ,[e]) `(define-values   ,s (,x ...) ,(RHS x e))]
    [(define-syntaxes ,s (,x ...) ,[e]) `(define-syntaxes ,s (,x ...) ,(RHS x e))])
  (Abstraction : Abstraction (AB) -> Abstraction ()
    [(λ ,s ,[f] ,[e])   (let ([lb (LambdaBody s f (formal-variables f) e)])
                          `(λ ,s ,f ,lb))])
  (Expr : Expr (E) -> Expr ()
    ; variable reference, set! and all binding forms must be rewritten
    [,x                       (if (set-in? x ms) (Unbox x) x)]
    [(set! ,s ,x ,[e])        `(app ,h ,(var:set-box!) ,x ,e)]
    [,ab                      (Abstraction ab)]
    [(case-lambda ,s ,ab ...) (let ([ab (map Abstraction ab)])
                                `(case-lambda ,s ,ab ...))]
    [(let-values    ,s ([(,x ...) ,[e*]] ...) ,[e0])
     (let ([e* (for/list ([e e*] [xs x]) (RHS xs e))])
       `(let-values ,s ([(,x ...) ,e*] ...) ,e0))]
    [(letrec-values ,s ([(,x ...) ,[e*]] ...) ,[e0])
     (let ([e* (for/list ([e e*] [xs x]) (RHS xs e))])
       `(letrec-values ,s ([(,x ...) ,e*] ...) ,e0))])
  (TopLevelForm T))

(define (assignment-conversion T) ; LFE2 -> LFE2
  ; more convenient to use assignment-conversion than
  ; calling collect-assignable-variables and box-mutables in order
  ; (since  assignment-conversion has type T -> T)
  (box-mutables T (collect-assignable-variables T)))

#;(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-LFE2
       (assignment-conversion
        (α-rename
         (explicit-case-lambda
          (explicit-begin
           (parse
            (expand-syntax stx))))))))
    (check-equal? (test #'(set! x 1)) '(set-box! x '1))
    (check-equal? (test #'(λ (x) (set! x 1) x))
                  '(#%expression (λ (x)
                                   (let-values (((x) (box '#f)))
                                     (begin (set-box! x '1) (unbox x))))))
    (check-equal? (test #'(let-values ([(x) 1]) (set! x 2) x))
                  '(let-values (((x) (box '1))) (begin (set-box! x '2) (unbox x))))
    (check-equal? (test #'(letrec-values ([(x y) (begin (set! x 1) (+ x y))]) (- x y)))
                  '(letrec-values (((x y)
                                    (let-values (((x_1 y_2)
                                                  (begin (set-box! x '1) (+ (unbox x) y))))
                                      (values (box x_1) y_2))))
                     (- (unbox x) y)))
    (check-equal? (test #'(define-values (x y) (begin (set! x 2) (+ x y))))
                  '(define-values (x y)
                     (let-values (((x_1 y_2)
                                   (begin (set-box! x '2) (+ (#%top . x) (#%top . y)))))
                       (values (box x_1) y_2))))
    (check-equal? (test #'(λ (x) x)) '(#%expression (λ (x) x)))))

;;;
;;; CATEGORIZE APPLICATIONS
;;;

;; Application (app s e0 e1 ...) is rewritten into one of the following:
;;    (primapp     s pr  e1 ...)    application of a  primitive 
;;    (closedapp   s ab  e1 ...)    application of an abstraction
;;    (app         s e0  e1 ...)    application of a  general procedure

;; Closed applications of the type ((λ (x ...) e) e) is rewritten to let-values.
;; Closed application where the abstraction have formals of the form (x ... . y) and x
;; are kept (for now).

;; The terminal pr is a variable bound to a primitive.

(define-language LFE3 (extends LFE2)
  (terminals
   (+ ((primitive (pr)) . => . unparse-primitive)))
  (CaseAbstraction (cab)
    (+ (case-lambda s ab ...) => (case-lambda ab ...)))
  (Expr (e)
    (- (case-lambda s ab ...)
       (app s e0 e1 ...))
    (+ cab
       (primapp   s pr e1 ...)      => (primapp   pr e1 ...)
       (closedapp s ab e1 ...)      => (closedapp ab e1 ...)
       (app       s e0 e1 ...)      => (app       e0 e1 ...))))

(define-pass categorize-applications : LFE2 (T) -> LFE3 ()
  (definitions
    (define (Expr* Es) (map Expr Es))
    (define (Values s es)
      ; avoid wrapping a single expression in a values form
      (with-output-language (LFE3 Expr)
        (match (length es)
          [1 (first es)]
          [_ `(primapp ,s ,(var:values) ,es ...)]))))
  (Formals             : Formals             (F) -> Formals             ())
  (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (Expr : Expr (E) -> Expr ()
    ; the rule order is important here, since this rule applies in the other cases too    
    [(app ,s ,x    ,[e1] ...) (guard (primitive? x))  `(primapp   ,s ,x              ,e1 ...)]
    [(app ,s (λ ,s1 ,[f] ,[e]) ,[e1] ...)             `(closedapp ,s ,`(λ ,s1 ,f ,e) ,e1 ...)]
    [(app ,s ,[e0] ,[e1] ...)                         `(app       ,s ,e0             ,e1 ...)])
  (TopLevelForm T))

(module+ helpers
  (provide primapp closedapp app)
  ;;;
  ;;; HELPERS
  ;;;
  
  ; At times it can be convenient to run an unparsed program
  ; in DrRacket. In order to do that we need to define the
  ; new language constructs used.
  
  ; The following pass categorize-applications categorizes
  ; applications in three different categories.
  ; The are all equivalent to standard application.
  
  (define-syntax (primapp stx)
    (syntax-parse stx
      [(_primapp pr e1 ...)
       (syntax/loc stx (#%app pr e1 ...))]))
  
  (define-syntax (closedapp stx)
    (syntax-parse stx
      [(_closedapp ab e1 ...)
       (syntax/loc stx (#%app ab e1 ...))]))
  
  (define-syntax (app stx)
    (syntax-parse stx
      [(_closedapp e0 e1 ...)
       (syntax/loc stx (#%app e0 e1 ...))])))


(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-LFE3
       (categorize-applications
        (assignment-conversion
         (α-rename
          (explicit-case-lambda
           (explicit-begin
            (parse
             (expand-syntax stx)))))))))
    (check-equal? (test #'(+ 1 2)) '(primapp + '1 '2))
    (check-equal? (test #'(begin (define foo 3)    (foo 1 2)))
                  ; at the top-level the expression is evaluted before the binding is created
                  ; also (#%top . foo) looks up foo in the current namespace by name,
                  ; so it should not be renamed.
                  '(begin (define-values (foo) '3) (app (#%top . foo) '1 '2)))
    (check-equal? (test #'((λ (x) 1) 2))
                  '(closedapp (λ (x) '1) '2))
    (check-equal? (test #'((λ (x y) 1) 2 3))
                  '(closedapp (λ (x y) '1) '2 '3))
    (check-equal? (test #'((λ (x . y) 1) 2 3))
                  '(closedapp (λ (x . y) '1) '2 '3))))

;;;
;;; ANORMALIZATION
;;;

; All subexpression are given a name unless
;  1) it is a RHS in a let-assigment  (it already has a name)
;  2) it is a tail expression         (avoid building context)
;  3) it is an atomic expression
;  4) the value will be ignored       (non-last expressions in a begin)

; Introduce administrative normal form
(define-language LANF (extends LFE3)
  ;; Atomic Expressions
  ;;  - always terminates
  ;;  - cause no errors
  ;;  - have no side effects
  ;; Note: Application of pure primitives could be added here
  (AExpr (ae)
    (+ x
       ab
       cab
       (quote s d)         => (quote d)
       (quote-syntax s d)  => (quote-syntax d)
       (top s x)           => (#%top . x)))
  ;; Complex Expressions
  ;;   - defer execution to at most one subexpression
  ;;   - may not terminate
  ;;   - may not error
  ;;   - may have side effects  
  (CExpr (ce)
    (+ ae 
       (if s ae0 e1 e2)                     => (if ae0 e1 e2)
       (set! s x ae)                        => (set! x ae)
       (wcm s ae0 ae1 e)                    => (with-continuation-mark ae0 ae1 e)
       (app       s ae ae1 ...)             => (app       ae ae1 ...)
       (primapp   s pr ae1 ...)             => (primapp   pr ae1 ...)
       (closedapp s ab ae1 ...)             => (closedapp ab ae1 ...)
       (begin  s e0 e1 ...)                 => (begin  e0 e1 ...)
       (begin0 s e0 e1 ...)                 => (begin0 e0 e1 ...)))
  (Expr (e)
    (-
     ; atomic
     x
     ab
     cab
     (quote s d)
     (quote-syntax s d)
     (top s x)
     ; complex
     (if s e0 e1 e2)
     (set! s x e) 
     (wcm s e0 e1 e2)
     (primapp   s pr e1 ...)
     (closedapp s ab e1 ...)
     (app       s e0 e1 ...)
     (begin  s e0 e1 ...)
     (begin0 s e0 e1 ...)
     ; expr
     ; 
     (let-values    s ([(x ...) e] ...) e0)
     (letrec-values s ([(x ...) e] ...) e0))
    (+ ; ae ; not needed, an ae is also an ce
     ce
     (let-values    s ([(x ...) ce] ...) e) => (let-values    ([(x ...) ce] ...) e)
     (letrec-values s ([(x ...) ce] ...) e) => (letrec-values ([(x ...) ce] ...) e))))

; anormalize : LFE3 -> LANF
(define (anormalize T)
  ; (Formals             : Formals             (F) -> Formals             ())
  ; (TopLevelForm        : TopLevelForm        (T) -> TopLevelForm        ())
  ; (GeneralTopLevelForm : GeneralTopLevelForm (G) -> GeneralTopLevelForm ())
  (define (id x) x)
  (define h #'an)
  
  (define (TopLevelForm* ts)
    (map TopLevelForm ts))
  
  (define (TopLevelForm T)
    (with-output-language (LANF TopLevelForm)
      (nanopass-case (LFE3 TopLevelForm) T
        [(topbegin ,s ,t ...)  `(topbegin ,s ,(TopLevelForm* t) ...)]
        [(#%expression ,s ,e)  `(#%expression ,s ,(Expr e id))]
        [,g                    (GeneralTopLevelForm g)])))
  
  (define (GeneralTopLevelForm G)
    (with-output-language (LANF GeneralTopLevelForm)
      (nanopass-case (LFE3 GeneralTopLevelForm) G
        [,e                                (Expr e id)]
        [(define-values   ,s (,x ...) ,e)  `(define-values   ,s (,x ...) ,(RHS e id))]
        [(define-syntaxes ,s (,x ...) ,e)  `(define-syntaxes ,s (,x ...) ,(RHS e id))]
        [(#%require       ,s ,rrs ...)   `(#%require ,s ,(map RawRequireSpec rrs) ...)]
        [else (error 'anormalize-GeneralTopLevelForm
                     "expected general top-level form, got ~a"
                     G)])))

  (define (RawRequireSpec RRS)
    (with-output-language (LANF RawRequireSpec)
      (nanopass-case (LFE3 RawRequireSpec) RRS
        [,rrmp (RawRootModulePath rrmp)])))

  (define (RawRootModulePath RRMP)
    (with-output-language (LANF RawRootModulePath)
      (nanopass-case (LFE3 RawRootModulePath) RRMP
        [(quote ,x)  `(quote ,x)])))
  
  (define (Abstraction ab k)
    (with-output-language (LANF AExpr) 
      (nanopass-case (LFE3 Abstraction) ab
        [(λ ,s ,f ,e) (k `(λ ,s ,(Formals f) ,(Expr e id)))]
        [else (error 'anormalize-Abstraction "expected abstraction")])))

  (define (CaseAbstraction cab k)
    (with-output-language (LANF AExpr) 
      (nanopass-case (LFE3 CaseAbstraction) cab
        [(case-lambda ,s ,ab ...)
         (k `(case-lambda ,s ,(map (λ (ab) (Abstraction ab id)) ab) ...))]
        [else (error 'anormalize-CaseAbstraction "expected case-abstraction")])))
  
  (define (Formals f)
    (with-output-language (LANF Formals) 
      (nanopass-case (LFE3 Formals) f
        [(formals (,x ...))            `(formals (,x ...))]
        [(formals (,x0 ,x1 ... . ,xd)) `(formals (,x0 ,x1 ... . ,xd))]
        [(formals ,x)                  `(formals ,x)])))
  
  (define (LFE3-Formals f)
    (with-output-language (LFE3 Formals) 
      (match f
        [(list x ...)         `(formals (,x ...))]
        [(list* x0 x1 ... xd) `(formals (,x0 ,x1 ... . ,xd))]
        [x                    `(formals ,x)])))
  
  (define (RHS E k)
    (nanopass-case (LFE3 Expr) E
      [,ab  (Abstraction ab k)]
      [else (Expr (with-output-language (LFE3 Expr)
                    `(closedapp ,h (λ ,h ,(LFE3-Formals '()) ,E))) k)]))
  
  (define (RHS* es k)
    (cond
      [(empty? es) (k '())]
      [else        (RHS (first es)
                     (λ (e0) (RHS* (rest es)
                               (λ (es) (k (cons e0 es))))))]))    
  
  (define (Expr E k)
    ;(displayln (list 'anormalize-Expr E)) (newline)
    (with-output-language (LANF CExpr)
      (nanopass-case (LFE3 Expr) E
        [(if ,s ,e0 ,e1 ,e2)
         (Expr/name e0 (λ (ae0) (k `(if ,s ,ae0 ,(Expr e1 id) ,(Expr e2 id)))))]
        [(set! ,s ,x ,e)
         (Expr/name e (λ (ae) (k `(set! ,s ,x ,ae))))]
        [(let-values ,s ([(,x ...) ,e] ...) ,e0)
         ; ad 1) don't name e ... 
         (RHS* e (λ (ce) (with-output-language (LANF Expr)
                           `(let-values    ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(letrec-values ,s ([(,x ...) ,e] ...) ,e0)
         (RHS* e (λ (ce) (with-output-language (LANF Expr)
                           `(letrec-values ,s ([(,x ...) ,ce] ...) ,(Expr e0 k)))))]
        [(primapp   ,s ,pr ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(primapp   ,s ,pr ,ae1 ...))))]
        [(closedapp ,s ,ab ,e1 ...)
         (Expr*/names e1 (λ (ae1) (k `(closedapp ,s ,(Abstraction ab id) ,ae1 ...))))]
        [(app       ,s ,e0 ,e1 ...)
         (Expr/name e0 (λ (ae0) (Expr*/names e1 (λ (ae1) (k `(app ,s ,ae0 ,ae1 ...))))))]
        ; Atomic expressions
        [,x                      (k x)]
        [,ab                     (Abstraction ab k)]
        [,cab                    (CaseAbstraction cab k)]
        [(quote ,s ,d)           (with-output-language (LANF AExpr) (k `(quote ,s ,d)))]
        [(quote-syntax ,s ,d)    (with-output-language (LANF AExpr) (k `(quote-syntax ,s ,d)))]
        [(top ,s ,x)             (with-output-language (LANF AExpr) (k `(top ,s ,x)))]
        [(begin ,s ,e0 ,e1 ...)
         (define (Expr/id e) (Expr e identity))
         (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])
           (k (with-output-language (LANF Expr)
                `(begin ,s ,e0 ,e1 ...))))]
        [(begin0 ,s ,e0 ,e1 ...)
         (define (Expr/id e) (Expr e identity))
         (let ([e0 (Expr/id e0)] [e1 (map Expr/id e1)])
           (k (with-output-language (LANF Expr)
                `(begin0 ,s ,e0 ,e1 ...))))]
        [else
         (displayln (list 'anormalize-Expr "got" E))
         (error 'anormalize-Expr "internal error")])))

  ; Expr/name : Expr (AExpr -> Expr) -> Expr
  (define (Expr/name e k)
    ;(displayln (list 'anormalize-Expr/name e)) (newline)
    ; Transform e, then name it (unless it is an atomic expression),
    ; then call k with the name or the atomic expression
    (Expr e (λ (e)
              (nanopass-case (LANF Expr) e
                [,ae   (k ae)]               ; ad 3) don't name atomic expressions
                [else  (let ([t (new-var)])
                         (with-output-language (LANF Expr)
                           `(let-values ,h ([(,t) ,e])
                              ,(k t))))]))))

  (define (Expr*/names es k)
    (cond
      [(empty? es) (k '())]
      [else        (Expr/name (first es)
                     (λ (t) (Expr*/names (rest es) 
                              (λ (ts) (k (cons t ts))))))]))
  (TopLevelForm T))

#;(module+ test
  (let ()
    (define (test stx)
      (reset-counter!)
      (unparse-LANF
       (anormalize
        (categorize-applications
         (assignment-conversion
          (α-rename
           (explicit-begin
            (parse
             (expand-syntax stx)))))))))
    (check-equal? (test #'1) ''1)
    (check-equal? (test #'(+ 2 3)) '(primapp + '2 '3))
    (check-equal? (test #'(+ 2 (* 4 5)))
                  '(let-values (((t_1) (primapp * '4 '5))) (primapp + '2 t_1)))
    (check-equal? (test #'(begin (+ 2 (* 4 5)) (+ (* 6 7) (/ 8 9))))
                  '(begin
                     (let-values (((t_1) (primapp * '4 '5))) (primapp + '2 t_1))
                     (let-values (((t_2) (primapp * '6 '7)))
                       (let-values (((t_3) (primapp / '8 '9))) (primapp + t_2 t_3)))))
    (check-equal? (test #'(let-values ([(x) (+ 1 (* 2 3))])
                            (let-values ([(y) (+ (* 4 x) 1)])
                              ((λ (z) (+ x y)) 5))))
                  '(let-values (((x) (closedapp (λ () (let-values (((t_1) (primapp * '2 '3)))
                                                        (primapp + '1 t_1))))))
                     (let-values (((y) (closedapp (λ () (let-values (((t_2) (primapp * '4 x)))
                                                          (primapp + t_2 '1))))))
                       (closedapp (λ (z) (primapp + x y)) '5))))
    (check-equal? (test #'(letrec ([fact (λ (n) (if (= n 0) 1 (* n (fact (- n 1)))))]) (fact 5)))
                  '(letrec-values (((fact)
                                    (λ (n)
                                      (let-values (((t_1) (primapp = n '0)))
                                        (if t_1 '1 (let-values (((t_2) (primapp - n '1)))
                                                     (let-values (((t_3) (app fact t_2)))
                                                       (primapp * n t_3))))))))
                     (app fact '5)))))


;;;
;;; FREE VARIABLE ANALYSIS
;;;

;; The goal of this pass is to determine the set of free variables
;; in each abstraction. The next pass, closure conversion, will use
;; this information to turn lambda abstractions into closures.

;; Rather than defining a new language with a variation of the
;; nonterminal λ with a field to store the free variables,
;; we use a hashtable to associate abstractions with sets of free variables.

;; Each nonterminal transformation produces two values: the form and the set of free variables
;; When a catamorphism is used as in (λ ,s ,f ,[e xs]) then
;; the Expr transformations is called and its output values are bound to e and xs.

;; There ought to be no free variables in top-level-forms, but we compute them
;; anyway to catch any internal errors.

(define-pass determine-free-variables : LANF (T) -> LANF (free-ht abstractions)
  (definitions
    ; list of all abstractions
    (define abs '())
    (define (seen-abstraction! ab) (set! abs (cons ab abs)))
    ; hash table from abstraction to its free variables
    (define ht (make-hasheq))
    (define (add! ab xs) (hash-set! ht ab xs))
    (define (get ab)     (hash-ref  ht ab))
    (define (formal-variables F)
      (nanopass-case (LANF Formals) F
        [(formals (,x ...))            (ids->id-set x)]
        [(formals (,x0 ,x1 ... . ,xd)) (set-union (make-id-set x0 xd) (ids->id-set x1))]
        [(formals ,x)                  (make-id-set x)]))
    (define (bound-at-top-level ts)
      ; return set of all xs occuring in a 
      ;     (define-values   ,s (,x ...) ,[e xs])
      (for/fold ([bound empty-set]) ([t ts])
        (define newly-bound
          (nanopass-case (LANF TopLevelForm) t
            [,g (nanopass-case (LANF GeneralTopLevelForm) g
                  [(define-values ,s (,x ...) ,e) (ids->id-set x)]
                  [else empty-set])]
            [else empty-set]))
        (set-union bound newly-bound)))
    (define (primitives->id-set)
      (ids->id-set (map (λ (sym) (variable (datum->syntax #'dfv sym))) primitives))))
  (Formals : Formals (F) -> Formals ())
  (TopLevelForm : TopLevelForm (T xs) -> TopLevelForm (xs)
    [(topbegin ,s ,[t xs] ...) (values T (set-difference
                                          (set-union* xs)
                                          (set-union (bound-at-top-level t)
                                                     ; todo: only import primitives that are declared
                                                     (primitives->id-set))))]
    [(#%expression ,s ,[e xs]) (values T xs)]
    [,g                        (GeneralTopLevelForm g xs)])
  (GeneralTopLevelForm : GeneralTopLevelForm (G xs) -> GeneralTopLevelForm (xs)
    [(define-values   ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))]
    [(define-syntaxes ,s (,x ...) ,[e xs]) (values G (set-difference xs (ids->id-set x)))] ; todo
    [,e (Expr e xs)])
  (Abstraction : Abstraction (AB xs) -> Abstraction (xs)
    [(λ ,s ,f ,[e xs])        (let ([xs (set-difference xs (formal-variables f))])
                                (add! AB xs)
                                (seen-abstraction! AB)
                                (values AB xs))])
  (CaseAbstraction : CaseAbstraction (CAB xs) -> CaseAbstraction (xs)
    ; the free variables of a case-lambda is the union of the free variables of the clauses
    [(case-lambda ,s ,ab* ...) (define xs* (set-union*
                                            (for/list ([ab ab*])
                                              (letv ((ab xs) (Abstraction ab xs))
                                                xs))))
                               (add! CAB xs*)
                               (seen-abstraction! CAB)
                               (values CAB xs*)])
  (AExpr : AExpr (AE xs) -> AExpr (xs)
    [,x                   (values AE (make-id-set x))]
    [,ab                  (Abstraction AE xs)]
    [,cab                 (CaseAbstraction AE xs)]
    [(quote ,s ,d)        (values AE empty-set)]
    [(quote-syntax ,s ,d) (values AE empty-set)]
    ; Note: Top-level variables are stored in a namespace.
    ;       In this pass we are only looking for variables that need to be stored in closures.
    [(top ,s ,x)          (values AE empty-set)])
  (CExpr : CExpr (CE xs) -> CExpr (xs)
    [,ae                                      (AExpr ae xs)]
    [(if ,s ,[ae0 xs0] ,[e1 xs1] ,[e2 xs2])   (values CE (set-union* (list xs0 xs1 xs2)))]
    [(wcm ,s ,[ae0 xs0] ,[ae1 xs1] ,[e xs2])  (values CE (set-union* (list xs0 xs1 xs2)))]
    [(app       ,s ,[ae xs] ,[ae1 xs1] ...)   (values CE (set-union* (cons xs xs1)))]
    [(primapp   ,s ,pr      ,[ae1 xs1] ...)   (values CE (set-union* xs1))]
    [(closedapp ,s ,[ab xs] ,[ae1 xs1] ...)   (values CE (set-union* (cons xs xs1)))]
    [(begin     ,s ,[e0 xs0] ,[e1 xs] ...)    (values CE (set-union* (cons xs0 xs)))]
    [(begin0    ,s ,[e0 xs0] ,[e1 xs] ...)    (values CE (set-union* (cons xs0 xs)))])
  (Expr : Expr (E xs) -> Expr (xs)
    [,ce (CExpr E xs)]
    [(let-values    ,s (((,x ...) ,[ce rhs-xs]) ...) ,[e xs])
     (values E (set-union (set-union* rhs-xs) ; the ((x ...) ...) are not bound in ce ...
                          (set-difference xs  ; the free variables of e except those bound here
                                          (set-union* (map ids->id-set x)))))]
    [(letrec-values ,s (((,x ...) ,[ce rhs-xs]) ...) ,[e xs])
     (values E (set-difference (set-union* (cons xs rhs-xs)) (set-union* (map ids->id-set x))))])
  (letv ((T xs) (TopLevelForm T (make-id-set)))
    
    (unless (set-empty? xs)
      (displayln "\n---\n")
      (displayln (unparse-LANF T))
      (displayln "\n---\n") (displayln xs) (newline)
      (error 'determine-free-variables "detected free variables (shouldn't be possible)"))
    (values T ht abs)))

;;;
;;; FINISH CLOSURE CONVERSION
;;;

;;; Step 1: Generate labels for each abstraction
;;;   label1:  (λ (x ...) e)
;;; Step 2:
;;;   Add a top-level-form for each abstraction:
;;;       (define-label label1  (λ (cl x ...) e))
;;;       where e has been closure converted
;;;   Rewrite each abstraction:
;;;       (λ (x ...) e)    =>  (make-closure label1 f ...)
;;;                            where f ... are the free variables of the abstraction
;;;   Rewrite references to free variables:
;;;       f                =>  (free-ref cl i)
;;;                            where i is the index of f in the list of free variables
;;; Runtime:
;;;   The top-level-form becomes
;;;      var label1 = function (cl, x, ...) { [[e]] }
;;;   An application of a closure cl to arguments a ... becomes:
;;;      cl.label(a,...)
;;;   Make closure becomes:
;;;      [the_unique_closure_tag, label1, v0, ...]
;;;      where v0, ... are the values stored in the closure
;;;   Local references (free-ref cl i) becomes:
;;;      cl[i+2]

(define (closure-conversion T)
  (define labels-ht (make-hasheq))
  (define (add-label! ab) (hash-set! labels-ht ab (new-var "label")))
  (letv ((T free-ht abs) (determine-free-variables T))
    (for ([ab abs])
      (add-label! ab))
    (finish-closure-conversion T free-ht labels-ht)))

(define (closure+primitive? v)
  (or (eq? v 'closure)
      (primitive? v)))

(define (natural? v) (and (integer? v) (not (negative? v))))

(define arity? integer?)

(define-language LANF+closure (extends LANF)
  (terminals
   (- (primitive (pr)))
   (+ (closure+primitive (pr)))
   (+ (arity (ar)))
   (- (variable (x xd)))
   (+ (variable (x xd l)) => unparse-variable)  ; l for label
   (+ (natural (i))))                           ; i for index 
  (Abstraction (ab)
    (- (λ s f e)))
  (CaseAbstraction (cab)
    (- (case-lambda s ab ...)))
  (ClosureAllocation (ca)
    (+ (closure s l ae1 ...)   => (closure l ae1 ...)))
  (CaseClosureAllocation (cca)
    (+ (case-closure s l [ar ca] ...) => (case-closure [ar ca] ...)))
  (AExpr (ae)
    (- ab
       cab)
    (+ ca
       cca
       (free-ref x i)))
  (CExpr (ce)
    (- (closedapp s ab ae1 ...))
    (+ (closedapp s ca ae1 ...) => (closedapp ca ae1 ...)))
  (ConvertedAbstraction (cab)
    (+ (λ s f e) => (λ f e)))
  #;(ConvertedCaseAbstraction (ccab)
      (+ (case-lambda s cab ...) => (case-lambda cab ...)))
  (TopLevelForm (t)
    (+ (define-label l cab))))

(define-pass finish-closure-conversion : LANF (T free-ht labels-ht) -> LANF+closure ()
  (definitions
    (define h #'fcc)
    (define (label-of ab) (hash-ref labels-ht ab))
    (define (free-of ab)  (hash-ref   free-ht ab))
    (define (index-of x free)
      (define (x? y) (id=? x y))
      (list-index x? free))
    ; the converted abstractions are lifted to the top-level
    (define lifted-abstractions (make-hasheq))
    (define (lift! label formals body)
      (define cab
        (with-output-language (LANF+closure ConvertedAbstraction)
          `(λ ,h ,formals ,body)))
      (hash-set! lifted-abstractions label cab))    
    (define (case-lift! label formals* body)
      (define ccab
        (with-output-language (LANF+closure ConvertedAbstraction)
          (let ([f (with-output-language  (LANF+closure Formals) `(formals ()))])
            `(λ ,h ,f ,body))))   ; TODO BUG TODO BUG
      (hash-set! lifted-abstractions label ccab))
    (define (abstraction-formals ab)
      (nanopass-case (LANF Abstraction) ab
        [(λ ,s ,f ,e) f]))
    (define current-free (make-parameter empty-set)))
  (TopLevelForm : TopLevelForm (T) -> TopLevelForm ())
  (Expr         : Expr         (E) -> Expr ())
  (Abstraction : Abstraction (AB) -> ClosureAllocation ()
    [(λ ,s ,[f] ,e)
     (define l  (label-of AB))
     (let ([xs (map AExpr (free-of AB))]
           [e  (parameterize ([current-free (free-of AB)])
                 (Expr e))])
       (lift! l f e)
       `(closure ,h ,l ,xs ...))])
  (CaseAbstraction : CaseAbstraction (CAB) -> CaseClosureAllocation ()
    [(case-lambda ,s ,ab ...)
     (define (formals->arity f)
       ; +n means precisely n
       ;  0 means preciely  0
       ; -1 means at least 0
       ; -2 means at least 1
       ; -n means at least n-1
       (nanopass-case (LANF Formals) f
         [(formals (,x ...))            (length x)]
         [(formals (,x0 ,x1 ... . ,xd)) (- (+ 2 (length x1)))]
         [(formals ,x)                  -1]))     
     (define (abstraction->formals ab)
       (nanopass-case (LANF Abstraction) ab [(λ ,s ,f ,e) f]))
     (define l  (label-of CAB))
     (let* ([fs*  (map abstraction-formals ab)]
            [ar   (map formals->arity fs*)]
            [ab   (map Abstraction ab)]           
            [E    (with-output-language (LANF+closure AExpr)
                    `(quote ,#'h ,(datum #'42 42)))])
       (case-lift! l fs* E)
       `(case-closure ,h ,l [,ar ,ab] ...))])
  (AExpr : AExpr (AE) -> AExpr ()
    [,x ; note: x is kept for debugging purposes
     (define xs (current-free))  ; set by the enclosing abstraction
     (match (index-of x xs)
       [#f x] ; refers to argument
       [i `(free-ref ,x ,(index-of x xs))])])
  (begin
    (let ([T (TopLevelForm T)])
    (with-output-language (LANF+closure TopLevelForm)
      ; the lifted (converted) abstractions are added to the top-level
      (define dl (for/list ([(l cab) lifted-abstractions])
                   `(define-label ,l ,cab)))
      `(topbegin ,h ,dl ... ,T)))))

;;;
;;; GENERATING URLANG (JAVASCRIPT)
;;;

;; Rather than generating JavaScript directly we generate
;; an Urlang program represented as nanopass structures.
;; An Urlang program is a convenient representation of a JavaScript program.
;; The Urlang to JavaScript compiler is trivial.

;; The benefits of having a layer before generating JavaScript is
;;   - more flexibility in producing JavaScript
;;   - allows macros to write JavaScript code
;;   - early detection of unbound identifiers
;;     (with support from DrRacket)



; > (language->s-expression LANF+closure)
#;(define-language LANF+closure
    (entry TopLevelForm)
    (terminals
     (natural (i))
     (variable (l))
     (closure+primitive (pr))
     #'(s)
     (datum (d))
     (variable (x xd)))
    (Formals (f)
      (x ...)
      (x0 x1 ... . xd)
      x)
    (TopLevelForm (t)
      (define-label l cab)
      (topbegin s t ...)
      (#%expression s e)
      g)
    (GeneralTopLevelForm (g)
      e
      (define-values s (x ...) e)
      (define-syntaxes s (x ...) e))
    (Expr (e)
      ce
      (letrec-values s (((x ...) ce) ...) e)
      (let-values s (((x ...) ce) ...) e))
    (AExpr (ae)
      x
      ca
      (free-ref x i)
      (quote s d)
      (quote-syntax s d)
      (top s x))
    (CExpr (ce)
      ae
      (closedapp s ca ae1 ...)
      (if s ae0 e1 e2)
      (set! s x ae)
      (wcm s ae0 ae1 e)
      (app s ae ae1 ...)
      (primapp s pr ae1 ...)
      (begin s e0 e1 ...)
      (begin0 s e0 e1 ...))
    (ConvertedAbstraction (cab)
      (λ s f e))
    (ClosureAllocation (ca)
      (closure s l ae1 ...)))

(define-pass generate-ur : LANF+closure (T) -> ur-Lur ()
  (definitions
    (define (~clos-label f)          (with-output-language (ur-Lur Expr)
                                       `(ref ,f '1)))
    (define (Label v)                (variable-id v))
    (define (Var v)                  (variable-id v))
    (define (Ref v)                  (variable-id v))
    (define (Prim pr)                (variable-id pr))
    (define (PrimRef pr)
      ;; A few primitives have names that aren't legal JavaScript identifiers.
      ;; References to these are rewritten here.
      (define encoding
        #hasheq((+ . PRIM+) (-  . PRIM-)  (*  . PRIM*)  (/ . PRIM/)
                            (=  . PRIM=)  (<  . PRIM+)  (> . PRIM+)
                            (<= . PRIM+)  (>= . PRIM>=) 
                            (null . Null) (void . Void)))
      (Var (cond [(hash-ref encoding (syntax-e (variable-id pr)) #f)
                  => (λ (PRIMpr) (variable (datum->syntax #'here PRIMpr)))]
                 [else pr])))
    (define (AExpr ae)               (AExpr2 ae #f))
    (define (AExpr* aes)             (map (λ (ae) (AExpr2 ae #f)) aes))
    (define (Expr* es dest)          (map (λ (e)  (Expr e dest)) es))
    (define (TopLevelForm* ts dest)  (map (λ (t)  (TopLevelForm t dest)) ts)))
  (ClosureAllocation : ClosureAllocation (ca dest) -> Expr ()
    ; dest = #f, means the ca is in value position
    [(closure ,s ,l ,ae1 ...)
     ; Note: We need to allocate the closure and then fill the free slots
     ;       in order to handle recursive references to the closure.
     ; If one of the ae1 ... are equal to dest, then closure has a recursive reference.
     ; If there is no self-reference then allocation is simple.
     ; If there is a self-reference we first need to allocate the closure,
     ; and then mutate fields with self-references.
     (define (maybe-store-in-dest e) (if dest `(:= ,dest ,e) e))
     (define self-reference-indices
       (for/list ([ae ae1] [i (in-naturals)]
                           #:when (and (variable? ae) (syntax? dest)
                                       (eq? (syntax-e (Var ae)) (syntax-e dest))))
         i))
     (maybe-store-in-dest
     (match self-reference-indices
       ['() (let ([ae1 (AExpr* ae1)] [l (Label l)])
              `(array '"CLOS" ,l ,ae1 ...))]
       [is  (define t (or dest (Var (new-var)))) ; reuse dest if possible
            (define inits (for/list ([ae ae1] [j (in-naturals)])
                            (if (member j is) #'undefined (AExpr ae))))            
            (let ([l (Label l)])
              `(app (lambda ()
                      (body
                       ; allocate closure
                       (var [binding ,t (array '"CLOS" ,l ,inits ...)])
                       ; plug any slots that are self-referencing
                       ,(for/list ([i self-reference-indices])
                          `(app ,#'array! ,t ',(+ i 2) ,t))
                       ...
                       ,t))))]))])
  (CaseClosureAllocation : CaseClosureAllocation (cca dest) -> Expr ()
    ; dest = #f, means the cca is in value position
    [(case-closure ,s ,l [,ar ,ca] ...)
     (define t (or dest (Var (new-var)))) ; reuse dest if possible
     (let ([l  (Label l)]
           [ca (for/list ([c ca]) (ClosureAllocation c #f))])
       `(app (lambda ()
               (body
                (var [binding ,t (array '"CLOS" ,#'dispatch-case-lambda
                                        (array (quote ,ar) ...)
                                        (array ,ca ...))])
                ,t))))])
  (AExpr2 : AExpr (ae [dest #f]) -> Expr ()  ; only ca needs dest
    [,x                    (if (memq (syntax-e (variable-id x)) primitives)
                               ; Note: α-conversion means rebinding primitives work
                               (PrimRef x)  ; reference to primitive
                               (Var x))]    ; reference to non-free variable
    [,ca                   (ClosureAllocation ca dest)]
    [(quote ,s ,d)         (let ([v (datum-value d)])
                             (cond
                               [(null? v)    `,#'NULL]
                               [(symbol? v)  `(app ,#'string->symbol ',(~a v))]
                               [(char? v)    `(app ,#'string-ref ',(~a v) '0)]
                               [(keyword? v) `(app ,#'string->keyword ',(~a (keyword->string v)))]
                               [else         `',v]))]
    [(free-ref ,x ,i)      `(ref ,#'_free (app ,#'+ ',i '2))] ; tag and label occupy first two slots
    ;                                                         ; (ignore x which is for debug only)
    [(top ,s ,x)
     ; Note: This is a quick hack until namespaces are implemented.
     ;       Note that if x is present in a top-level define-values
     ;       then (top x) will become x anyway.
     ; Note: What is missing here: is error handling for an undefined top-level-variable.
     `(app ,#'namespace-variable-value (app ,#'string->symbol ',(symbol->string (syntax-e (Var x))))
           ',#f ; use-mapping? TODO: this should be #t but that isn't implemented yet in runtime
           )
     ;(Var x)
     #;(error 'generate-ur "TODO top level reference - implement namespaces")]
    [(quote-syntax ,s ,d)  (error 'generate-ur "TODO quote-syntax not supported" s)])
  (CExpr : CExpr (ce dest) -> Statement ()
    ;; All Complex Expressions are translated to statements
    [,ae                       `(:= ,dest ,(AExpr2 ae dest))] ; ClosureAllocation needs the dest
    [(if ,s ,ae0 ,e1 ,e2)      `(sif ,(AExpr ae0) ,(Expr e1 dest) ,(Expr e2 dest))]
    [(set! ,s ,x ,ae)          `(:= ,(Var x) ,(AExpr ae))]
    [(begin  ,s ,e0 ,e1 ...)   `(block ,(Expr e0 dest) ,(Expr* e1 dest) ...)]
    [(begin0 ,s ,e0 ,e1 ...)   (let ([t (Var (new-var))]) ; redirect output
                                 `(let ([,t ,#'undefined])
                                    (body
                                     ,(Expr e0 dest)
                                     ,(Expr* e1 t) ...)))] ; TODO use #f
    [(primapp ,s ,pr ,ae1 ...) (define sym (syntax->datum (variable-id pr)))
                               (match (length ae1)
                                 [0 (case sym
                                      [(+)  `(:= ,dest '0)]
                                      [(-)  `(app '"ERROR - arity mismatch")]
                                      [(*)  `(:= ,dest '1)]
                                      [(/)  `(app '"ERROR / arity mismatch")]
                                      [else `(:= ,dest (app ,(Prim pr)))])]
                                 [1 (case sym
                                      [(+ *)  `(:= ,dest ,(AExpr (first ae1)))]
                                      [(-)    `(:= ,dest (app ,(Prim pr) ,(AExpr (first ae1))))]
                                      [(/)    `(:= ,dest (app ,(Prim pr) '1 ,(AExpr (first ae1))))]
                                      [else `(:= ,dest (app ,(Prim pr) ,(AExpr (first ae1))))])]
                                 [_
                                  `(:= ,dest (app ,(Prim pr) ,(AExpr* ae1) ...))])]
    [(app ,s ,ae ,ae1 ...)     (let ([f (Var (new-var 'f))])
                                 `(block
                                   (var [binding ,f ,(AExpr ae)])
                                   (:= ,dest
                                       (if (app ,#'closure? ,f)
                                           (app ,(~clos-label f) ,(cons f (AExpr* ae1)) ...)
                                           (app ,f ,(AExpr* ae1) ...)))))]    
    [(closedapp ,s ,ca ,ae2 ...)
     (let ([ae2 (AExpr* ae2)])
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ae1 ...)
          (match ae1
            [(list) ; no free variable => no need to actually allocate a closure, just jump to label
             `(:= ,dest (app ,(Label l) ,#'undefined ,ae2 ...))] ; undefined due to no closure
            [_ `(block
                 ,(ClosureAllocation ca dest) ; use dest as temporary
                 (:= ,dest (app ,(Label l) ,dest ,ae2 ...)))])]))]
    [(wcm ,s ,ae0 ,ae1 ,e)       (error 'generate-ur "TODO with-continuation-mark")])
  (Expr : Expr  (e  dest) -> Statement ()
    [,ce (CExpr ce dest)]
    #;[(letrec-values ,s (((,x** ...) ,ce*) ...) ,e)
     (define init
       (for/list ([x* x**] [ce ce*])
         (match (map Var x*)
           ; single value return is the common case, so this needs to be fast
           [(list x)                  (CExpr ce x)]  ; single value
           ; multiple values are returned in a tagged array [VALUES,v0,v1,...]
           ; to avoid allocating an extra variable, we receive the array in x0,
           ; then assign the individual variables (in reverse order so x0 is last)
           [(list x0 x1 ...) `(block ,(CExpr ce x0)
                                     ,(for/list ([i (in-range (length x*) 0 -1)]
                                                 [x (reverse (cons x0 x1))])
                                       `(:= ,x (ref ,x0 ',i)))
                                     ...)]
           ; no values are expected
           ['() (CExpr ce #f)])))
     (let* ([x*         (map Var (append* x**))]
            [undefined* (map (λ(_) #'undefined) x*)]
            [e          (Expr e dest)])
       `(block (var ,x* ...)   ; declare
               ,init ...       ; initialize
               ,e))]
    [(letrec-values ,s (((,x* ...) ,ce) ...) ,e)
     (define (AllocateClosure ca dest) ; called by letrec-values
      (nanopass-case (LANF+closure ClosureAllocation) ca
        [(closure ,s ,l ,ae1 ...)
         ; TODO: No need to fill in quotations later.
         ;       Do them there. And skip in FillClosure.
         (let ([0s (map (λ(_) `'0) ae1)]
               [l (Label l)])
           `(:= ,dest (array '"CLOS" ,l ,0s ...)))]))
    (define (FillClosure ca dest)
      (nanopass-case (LANF+closure ClosureAllocation) ca
        [(closure ,s ,l ,ae1 ...)
         
           `(block ,(for/list ([i (in-naturals 2)] [ae (AExpr* ae1)])
                      `(app ,#'array! ,dest ',i ,ae))
                   ...)]))
     ;;      (letrec-values (((even) (closure label_7 odd))
     ;;                      ((odd) (closure label_6 even)))
     ;;         expression)
     (unless (apply = (list* 1 1 (map length x*))) ; all clauses expect one value?
       (error 'generate-ur "TODO support multiple values in letrec-values"))
     (let* ([x  (map Var (map first x*))])
       `(block (var ,x ...)  ; declaration
               ,(map AllocateClosure ce x) ...
               ,(map FillClosure ce x) ...
               ,(Expr e dest)))]
    #;[(letrec-values ,s (((,x** ...) ,ce*) ...) ,e)
     ;;      (letrec-values (((even) (closure label_7 odd))
     ;;                      ((odd) (closure label_6 even)))
     ;;         expression)

     ;; TODO: At this time we can assume that all ce are lambda expressions
     ;;       - change code to allocate closures, then fill in free variables
     ;;       - this will allow mutual recursive references.
     ;; this is the version that does not support multiple values
     (unless (apply = (list* 1 1 (map length x**))) ; all clauses expect one value?
       (error 'generate-ur "TODO support multiple values in letrec-values"))
     (let* ([x* (map Var (map first x**))]
            [ce (map CExpr ce* x*)])
       `(block (var ,x* ...)        ; declare variables
               ,ce ...              ; initialize them
               ,(Expr e dest)))]    ; body 
    [(let-values ,s (((,x** ...) ,ce*) ...) ,e)
     (define initialize
       (for/list ([x* x**] [ce ce*])
         (match (map Var x*)
           ; single value return is the common case, so this needs to be fast
           [(list x)                  (CExpr ce x)]  ; single value
           ; multiple values are returned in a tagged array [VALUES,v0,v1,...]
           ; to avoid allocating an extra variable, we receive the array in x0,
           ; then assign the individual variables (in reverse order so x0 is last)
           [(list x0 x1 ...) `(block ,(CExpr ce x0)
                                     ,(for/list ([i (in-range (length x*) 0 -1)]
                                                 [x (reverse (cons x0 x1))])
                                       `(:= ,x (ref ,x0 ',i)))
                                     ...)]
           ; no values are expected
           ['() (CExpr ce #f)]))) ; todo: signal error if values are produced
     (let* ([xs         (map Var (append* x**))]
            [undefined* (map (λ(_) #'undefined) xs)]
            [e          (Expr e dest)])
       `(let ([,xs ,undefined*] ...) ; declare variable
          (body ,initialize ...      ; initialize
                ,e                   ; evaluate
                ,dest)))]            ; return
    ) ; TODO use #f instead of t
  (ConvertedAbstraction : ConvertedAbstraction (cab) -> Expr ()
    [(λ ,s (formals (,x ...)) ,e)
     (let* ([t (Var (new-var 'r))] [x (map Var x)] [e (Expr e t)])
       `(lambda (,#'_free ,x ...)
          (body (var ,t) ,e ,t)))]
    [(λ ,s (formals ,x) ,e)
     (let* ([t (Var (new-var 'r))] [x (Var x)] [e (Expr e t)])
       `(lambda (,#'_free ,x)
          (body (var ,t)
                (:= ,x (app ,#'cdr (app ,#'array->list ,#'arguments)))
                ,e ,t)))]
    [(λ ,s (formals (,x0 ,x1 ... . ,xd)) ,e)
     (let* ([t (Var (new-var 'r))] [x0 (Var x0)] [x1 (map Var x1)] [xd (Var xd)] [e (Expr e t)])
       `(lambda (,#'_free ,x0 ,x1 ... ,xd)
          (body (var ,t)
                (:= ,xd (app ,#'array-end->list ,#'arguments ',(length (cons x0 x1))))
                ,e ,t)))])
  (TopLevelForm : TopLevelForm (t dest) -> ModuleLevelForm ()
    [,g                       (GeneralTopLevelForm g dest)]
    [(#%expression ,s ,e)     (Expr e dest)]
    [(define-label ,l ,cab)  `(var [binding ,(Label l) ,(ConvertedAbstraction cab)])]
    [(topbegin ,s ,t ...)     (let ([t (TopLevelForm* t dest)])
                                `(block ,t ...))])
  (GeneralTopLevelForm : GeneralTopLevelForm (g dest) -> ModuleLevelForm ()
    [,e                           (Expr e dest)]
    [(#%require     ,s ,rrs ...)  `'"ignored #%require"]
    [(define-values ,s (,x ...) ,e)
     (match (map Var x)
       [(list x0) ; single value is the common case
        `(block (var ,x0) ,(Expr e x0))]
       [(list x0 x1 ...)
        (define x* (cons x0 x1))
        `(block (var ,x0 ,x1 ...)
                ,(Expr e x0)    ; reuse x0 to store values
                ,(for/list ([i (in-range (length x*) 0 -1)]
                            [x (reverse x*)])
                   `(:= ,x (ref ,x0 ',i)))
                ...)]
       ['() (error 'generate-ur "TODO define-values of no values")])]
    [(define-syntaxes ,s (,x ...) ,e) (error 'generate-ur "TODO define-syntaxes")])
  (let* ([result #'result]
         [t        (TopLevelForm T result)] ; todo generate unique id
         [prims    (ur-urmodule-name->exports 'runtime)]
         [pr       (map (λ (prim) (datum->syntax #'runtime prim)) prims)]
         [pr-str   (map (λ (prim) (~a (ur-mangle prim))) pr)]
         [RUNTIMES (map (λ (_) #'RUNTIME) prims)])
    (with-output-language (ur-Lur Module)
      `(urmodule amodulename
         (import ,#'console.log ,#'require ,#'Array.isArray
                 ,#'null  ; Note: null is called Null in runtime.js (null is a keyword in JavaScript)
                 ,#'void  ; Note: void is called Void in runtime.js (void is a keyword in JavaScript)
                 )
         ;; Import the Node module "runtime.js"
         (define ,#'RUNTIME
           (app ,#'require '"/Users/soegaard/Dropbox/GitHub/urlang/compiler-rjs/runtime.js"))
         ;; Bind all imported identifiers
         (var [binding ,pr (ref ,RUNTIMES ',pr-str)] ...)
         ;; The result of evaluating this module:
         (define ,result '0) ; todo: make it undefined
         ,t))))


(define (unparse-test stx)
  (unparse-LANF+closure
   (test stx)))

(define (test stx)
  (reset-counter!)
  (closure-conversion
   (anormalize
    (categorize-applications
     (assignment-conversion
      (α-rename
       (explicit-case-lambda
        (explicit-begin
         (convert-quotations
          (parse
           (topexpand stx)))))))))))

(define (compile stx)
  (ur-compile
   (generate-ur
    (test stx))))

(define (compile-ur stx)
  (define t (test stx))
  ;(displayln (unparse-LANF+closure t))
  ;(displayln "... generating ur...")
  (define ur (generate-ur t))
  (ur-unparse-Lur ur)
  ur
  #;(ur-compile
     (generate-ur
      (test stx))
     #t))

;;;
;;; EVAL
;;;

(define (eval stx)
  (ur-eval
   (generate-ur
    (test stx))))

(ur-current-urlang-run?                           #t) ; run using Node?
(ur-current-urlang-echo?                          #t) ; print generated JavaScript?
(ur-current-urlang-console.log-module-level-expr? #t) ; print top-level expression?
(ur-current-urlang-delete-tmp-file?               #f)
