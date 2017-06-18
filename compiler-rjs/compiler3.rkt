#lang racket

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

(require "compiler2.rkt")

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
