#lang racket
(provide eval)      
(provide (all-defined-out)) 

;;; 
;;; TODO
;;;

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
; CLOSE  9. tco (trampoline)
; DONE  10. with-continuation-mark
; DONE  11. parameters
; CLOSE 12. exceptions
; WIP   13. error checking
;             - applications of non-procedures
;             - arities  
;             - wrong number of return values
;       14. top-level-namespaces
;       15. modules
;       17. submodules
;       18. top-level requires
;       19. #%variable-reference
;       20. test suite
;       21. keywords
;       22. promises

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
  (only-in srfi/1 list-index)
  '#%paramz) ; contains the identifier psarameterization-key
   
;;;
;;; EXPANSION
;;;

;;; Note: To avoid top-level related problems with structure definitions
;;;       the entire program is wrapped in a let.
;;;       Compare (expand #'(begin  (struct foo (bar)) (foo 42))
;;;       and     (expand #'(let () (struct foo (bar)) (foo 42))
;;        Note that the (foo 42) doesn't match the constructor name in the begin snippet.

;; Require these before the expander.
(require (only-in racket/unsafe/ops unsafe-fx< unsafe-fx> unsafe-fx+ unsafe-fx- unsafe-fx*)
         (only-in racket/base in-range))

(define (topexpand top-level-form-stx)
  (parameterize ([current-namespace (make-base-namespace)]
                 [eval-jit-enabled  #f])  ; affects expansion
    ;                                     ; (reverse becomes alt-reverse unless disabled)
    (namespace-require 'errortrace)
    ; (namespace-require 'racket/match)
    ; (namespace-require 'racket/unsafe/ops)
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

(define macro-introduced '(call-handled-body))

(define (primitive? v)
  (and (or (and (syntax? v)   (primitive? (syntax-e v)))
           (and (variable? v) (primitive? (variable-id v)))
           (member v primitives))
       #t))

; A few primitives need access to _tc and are therefore wrapped in a closure.
; This means that applications of these are (app ...) and not (primapp ...).
(define (special-primitive? x)
  (cond [(syntax? x)   (special-primitive? (syntax-e x))]
        [(variable? x) (special-primitive? (variable-id x))]
        [else          (or (memq x '(apply
                                     call-handled-body
                                     uncaught-exception-handler)))]))

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
     (datum->construction-expr s (datum-value d))]
    [(quote-syntax ,s ,d)
     (let* ([src (syntax-source s)]
            [src (if (path? src)   (path->string src)   src)] ; no paths yet ...
            [src (if (symbol? src) (symbol->string src) src)] ; keep it simple for now ...
            [src   (Expr `(quote ,s ,(datum s src)))]
            [l     (Expr `(quote ,s ,(datum s (syntax-line s))))]
            [c     (Expr `(quote ,s ,(datum s (syntax-column s))))]
            [p     (Expr `(quote ,s ,(datum s (syntax-position s))))]
            [sp    (Expr `(quote ,s ,(datum s (syntax-span s))))]
            [false (Expr `(quote ,s ,(datum s #f)))]
            [v     (Expr `(quote ,s ,d))])
       (displayln (list 'generate-ur: 'quote-syntax s d (list src l c p sp)))
       ; srcloc(source,line,column,position,span)
       ; make_syntax_object(source_location,lexical_info,datum)       
       `(app ,s ,(var:make-syntax-object)
             (app ,s ,(var:srcloc) ,src ,l ,c ,p ,sp) ; source location
             ,false                                   ; lexical info
             ,v))])
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
