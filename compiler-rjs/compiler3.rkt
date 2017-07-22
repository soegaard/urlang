#lang racket
;; TODO Fix debug output of unbound variables...

;;; TODO
; Handle an '<effect> destination in wcm
;    (begin (with-continuation-mark 1 2 (with-continuation-mark 1 3 (current-continuation-marks))) 42)
; Fix display of symbols
; unsafe-cdr
; make-sequence

    #;[(letrec-values ,s (((,x** ...) ,ce*) ...) ,e)
     (define init
       (for/list ([x* x**] [ce ce*])
         (match (map Var x*)
           ; single value return is the common case, so this needs to be fast
           [(list x)                  (CExpr ce x cd)]  ; single value
           ; multiple values are returned in a tagged array [VALUES,v0,v1,...]
           ; to avoid allocating an extra variable, we receive the array in x0,
           ; then assign the individual variables (in reverse order so x0 is last)
           [(list x0 x1 ...) `(block ,(CExpr ce x0 cd)
                                     ,(for/list ([i (in-range (length x*) 0 -1)]
                                                 [x (reverse (cons x0 x1))])
                                       `(:= ,x (ref ,x0 ',i)))
                                     ...)]
           ; no values are expected
           ['() (CExpr ce #f cd)])))
     (let* ([x*         (map Var (append* x**))]
            [undefined* (map (λ(_) #'undefined) x*)]
            [e          (Expr e dd)])
       `(block (var ,x* ...)   ; declare
               ,init ...       ; initialize
               ,e))]

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

(define datum:true  (datum #f #t))
(define datum:false (datum #f #f))

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
      (let-values    s (((x ...) ce) ...) e))
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
    (AExpr (ae)
      x
      ca
      (free-ref x i)
      (quote s d)
      (quote-syntax s d)
      (top s x))    
    (ConvertedAbstraction (cab)
      (λ s f e))
    (ClosureAllocation (ca)
      (closure s l ae1 ...)))

;;; This code generator is inspired by "Destination-driven Code Generation"
;;; by Dybvig, Hieb and Butler. There are som differences however. The code
;;; generator in the paper generates "flat" code (assembler) where as we
;;; generate nested JavaScript constructs.

;;; The code generator for an expression takes a data destination (dd) and a control destionation (cd)
;;; as arguments. The data destination determines where the value of the expression is to be placed.
;;;
;;;    dd in Data Destination  is  #f or a location (identifier),
;;;
;;; here #f means the expression is evaluated for its effect.

;;; If the data destination is #f (an effect) the value of the expression does not need to be stored.
;;; The control destination
;;;
;;;    cd in ControlDestination  is  label  or  label x label
;;;
;;; If the label is #f (return) it means that the value is to be returned from the function.
;;; If two labels are given, then there are two branches according to the expression value.

; Data destinations (also an identifier)
(define <effect> '<effect>)
(define <value>  '<value>)
; Control destinations
(define <return> '<return>)
(define <expr>   '<expr>)
(define <stat>   '<stat>)
; identifier used to hold tail call flag
(define _tc      #'_tc)

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
                            (=  . PRIM=)  (<  . PRIM+)  (> . PRIM>)
                            (<= . PRIM<=) (>= . PRIM>=) 
                            (null . Null) (void . Void)))
      (Var (cond [(hash-ref encoding (syntax-e (variable-id pr)) #f)
                  => (λ (PRIMpr) (variable (datum->syntax #'here PRIMpr)))]
                 [else pr])))
    (define (Expr e dd cd)           #;(displayln (list 'Expr e dd cd)) (Expr2 e dd cd))
    (define (AExpr2 ae [dd #f])      #;(displayln (list 'AExpr ae dd))  (AExpr3 ae dd))
    (define (AExpr ae)               (AExpr2 ae #f))
    (define (AExpr* aes)             (map (λ (ae) (AExpr2 ae #f)) aes))
    (define (Expr* es dd cd)         (map (λ (e)  (Expr e dd cd)) es))
    (define (CExpr ce dd cd)         #;(displayln (list 'CExpr ce dd cd)) (CExpr2 ce dd cd))      
    (define (TopLevelForm* ts dd)    (map (λ (t)  (TopLevelForm t dd)) ts))
    (define f-tmp                    (Var (new-var 'f))) ; used by app
    )
  (ClosureAllocation : ClosureAllocation (ca dd) -> Expr ()
    [(closure ,s ,l ,ae1 ...)
     ; Note: We need to allocate the closure and then fill the free slots
     ;       in order to handle recursive references to the closure.
     ; If one of the ae1 ... are equal to dd, then closure has a recursive reference.
     ; If there is no self-reference then allocation is simple.
     ; If there is a self-reference we first need to allocate the closure,
     ; and then mutate fields with self-references.
     (define (maybe-store-in-dest e)
       (match dd
         [(or '<effect> '<value> #f)                     e]
         [dest                               `(:= ,dest ,e)]))
     (define self-reference-indices
       (for/list ([ae ae1] [i (in-naturals)]
                           #:when (and (variable? ae) (syntax? dd)
                                       (eq? (syntax-e (Var ae)) (syntax-e dd))))
         i))
     (maybe-store-in-dest
      (match self-reference-indices
        ['() (let ([ae1 (AExpr* ae1)] [l (Label l)])
               `(array '"CLOS" ,l ,ae1 ...))]
        [is  (define t (or dd (Var (new-var)))) ; reuse dest if possible
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
  (CaseClosureAllocation : CaseClosureAllocation (cca dd) -> Expr ()
    ; dest = #f, means the cca is in value position
    [(case-closure ,s ,l [,ar ,ca] ...)
     (define t (or dd (Var (new-var)))) ; reuse dest if possible
     (let ([l  (Label l)]
           [ca (for/list ([c ca]) (ClosureAllocation c #f))])
       `(app (lambda ()
               (body
                (var [binding ,t (array '"CLOS" ,#'dispatch-case-lambda
                                        (array (quote ,ar) ...)
                                        (array ,ca ...))])
                ,t))))])
  (AExpr3 : AExpr (ae [dd #f]) -> Expr ()  ; only ca needs dest
    [,x               (if (memq (syntax-e (variable-id x)) primitives)
                          ; Note: α-conversion means rebinding primitives work
                          (PrimRef x)  ; reference to primitive
                          (Var x))]    ; reference to non-free variable
    [,ca              (ClosureAllocation ca dd)]
    [(quote ,s ,d)    (let ([v (datum-value d)])
                        (cond
                          [(null? v)    `,#'NULL]
                          [(symbol? v)  `(app ,#'string->symbol ',(~a v))]
                          [(char? v)    `(app ,#'string-ref ',(~a v) '0)]
                          [(keyword? v) `(app ,#'string->keyword ',(~a (keyword->string v)))]
                          [else         `',v]))]
    [(free-ref ,x ,i) `(ref ,#'_free (app ,#'+ ',i '2))] ; tag and label occupy first two slots
    ;                                                    ; (ignore x which is for debug only)
    [(top ,s ,x)
     ; Note: This is a quick hack until namespaces are implemented.
     ;       Note that if x is present in a top-level define-values
     ;       then (top x) will become x anyway.
     ; Note: What is missing here: is error handling for an undefined top-level-variable.
     `(app ,#'namespace-variable-value (app ,#'string->symbol ',(symbol->string (syntax-e (Var x))))
           ',#f ; use-mapping? TODO: this should be #t but that isn't implemented yet in runtime
           )]
    [(quote-syntax ,s ,d)
     (raise-syntax-error 'generate-ur "quote-syntax gone at this point")])

  (CExpr2 : CExpr (ce dd cd) -> Statement ()
    ;; All Complex Expressions are translated to statements    
    [,ae                     (match dd
                               ['<effect>  `(block)]
                               ['<value>   (match cd ; ClosureAllocation needs the dest
                                             ['<return> `(return ,(AExpr2 ae dd))]
                                             [_                 `,(AExpr2 ae dd)])]
                               [x          `(:= ,x ,(AExpr2 ae x))])]

    [(if ,s ,ae0 ,e1 ,e2)    `(sif ,(AExpr2 ae0 <expr>) ,(Expr e1 dd cd) ,(Expr e2 dd cd))]

    [(set! ,s ,x ,ae)        (match dd
                               ['<effect>  `(:= ,(Var x) ,(AExpr2 ae dd))]
                               ['<value>   `(begin (:= ,(Var x) ,(AExpr2 ae <effect>)) ,(void))]
                               [y         (CExpr `(begin (:= ,(Var x) ,(AExpr ae))
                                                         (:= ,y ,(Var x))) dd cd)])]
    
    [(begin  ,s ,e0 ,e1 ... ) (match (cons e0 e1)
                                [(list e0 ... en)
                                 (let ([e0 (Expr* e0 <effect> <stat>)]
                                       [en (Expr  en dd cd)])
                                   `(block ,e0 ... ,en))])]
    [(begin0 ,s ,e0 ,e1 ...)  (match dd
                                ['<effect> `(block
                                             ,(Expr  e0 '<effect> '<stat>)
                                             ,(Expr* e1 '<effect> '<stat>) ...)]
                                ['<value>  (let ([t (Var (new-var))])
                                             `(let ([,t ,#'undefined])
                                                (body
                                                 ,(Expr  e0 t         '<stat>)
                                                 ,(Expr* e1 '<effect> '<stat>) ...
                                                 ,t)))]
                                [y           `(block
                                               ,(Expr  e0 y         '<stat>)
                                               ,(Expr* e1 '<effect> '<stat>) ...)])]
    [(primapp ,s ,pr ,ae1 ...) (define sym (syntax->datum (variable-id pr)))
                               (define work                                 
                                 (match (length ae1)
                                   [0 (case sym
                                        [(+)  `'0]
                                        [(-)  `(app ,(PrimRef pr))] ; signals error
                                        [(*)  `'1]
                                        [(/)  `(app ,(PrimRef pr))]
                                        [else `(app ,(PrimRef pr))])]
                                   [1 (case sym
                                        [(+ *)  (AExpr (first ae1))]
                                        [(-)    `(app ,(Prim pr)    ,(AExpr (first ae1)))]
                                        [(/)    `(app ,(Prim pr) '1 ,(AExpr (first ae1)))]
                                        [else   `(app ,(PrimRef pr)  ,(AExpr (first ae1)))])]
                                   [2 (case sym
                                        [(+ - *) `(app ,(Prim pr)
                                                       ,(AExpr (first ae1)) ,(AExpr (second ae1)))]
                                        ; / needs to signal an Racket error ...
                                        [else   `(app ,(PrimRef pr)
                                                      ,(AExpr (first ae1)) ,(AExpr (second ae1)))])]
                                   [_ `(app ,(PrimRef pr) ,(AExpr* ae1) ...)]))
                               (match dd
                                 [(or '<value> '<effect>)
                                  (match cd
                                    ['<return>             `(return ,work)]
                                    [(or '<expr> '<stat>)            work]
                                    [_ (error)])]
                                 [x  `(:= ,x ,work)])]
    [(app ,s ,ae ,ae1 ...)
     ; NOTE: Only one function application is active at a time, so we can
     ;       reuse the variable holding the function f-tmp.
     (let* ([f    f-tmp]
            [tc   (eq? cd '<return>)]
            [work `(if (app ,#'closure? ,f)
                       (app ,(~clos-label f) ,f ',tc ,(AExpr* ae1) ...)
                       (if (app ,#'= (app ,#'typeof ,f) '"function")
                           (app ,f ,(AExpr* ae1) ...)
                           (app ,#'raise-application-error ,f (array ,(AExpr* ae1) ...))))])
       (match dd
         ['<effect> (match cd
                      ['<expr>   `(begin (:= ,f ,(AExpr ae)) ,work)]
                      ['<stat>   `(begin (:= ,f ,(AExpr ae)) ,work)]
                      [_         (error "INTERNAL ERROR - combination impossible")])]
         ['<value>  (match cd
                      ['<return> `(block (:= ,f ,(AExpr ae)) (return ,work))]
                      ['<expr>   `(begin (:= ,f ,(AExpr ae))         ,work)]
                      [_         (display (list 'app ce dd cd))
                                 (error)])]
         [x         `(begin (:= ,f ,(AExpr ae))  (:= ,x ,work))]))]
    [(closedapp ,s ,ca ,ae2 ...)
     (let ([ae2 (AExpr* ae2)])
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ae1 ...)
          (define tc (eq? cd '<return>))
          (define work
            (match ae1
              [(list) ; no free variable => no need to actually allocate a closure, just jump to label
               `(app ,(Label l) ,#'undefined ',tc ,ae2 ...)] ; undefined due to no closure
              [_ (match dd
                   [(or '<effect> '<value>)
                    (let ([f (Var (new-var 'f))])
                      `(let ([,f ,#'undefined])
                         (body ,(ClosureAllocation ca f)
                               (app ,(Label l) ,f ',tc ,ae2 ...))))]
                   [y
                    `(begin
                       ,(ClosureAllocation ca dd) ; use dest as temporary
                       (app ,(Label l) ,dd ',tc ,ae2 ...))])]))
          (match dd
            [(or '<effect> '<value>)  (match cd
                                        ['<return> `(return ,work)]
                                        ['<expr>             work]
                                        ['<stat>             work])]
            [y                        `(:= ,y ,work)])]))]
    [(wcm ,s ,ae0 ,ae1 ,e)
     ;(displayln (list 'wcm 'dd dd 'cd cd s))
     (define-values (dest dest-declaration)
       (match dd
         [(or '<value> '<effect>) (let ([r (Var (new-var 'res))])
                                    (values r `(var [binding ,r '#f])))]
         [y                         (values y `(block))]))
     (let* ([key       (AExpr ae0)]
            [val       (AExpr ae1)]
            [result    (Expr e dest '<return>)] ; todo : is this correct?
            [enter     (match cd
                         ['<return>  `(sif ,_tc
                                           (app ,#'set-continuation-mark       ,key ,val)
                                           (app ,#'new-continuation-mark-frame ,key ,val))]
                         [_          `(app ,#'new-continuation-mark-frame ,key ,val)])]
            [leave     (match cd
                         ['<return> `(sif ,_tc
                                          (block) ; no frame to remove in tail position
                                          (app ,#'remove-continuation-mark-frame))]
                         [_         `(app ,#'remove-continuation-mark-frame)])]
            [return-it (match cd
                         ['<return> `(return ,dest)]
                         ['<effect> `(block)]
                         [_                   dest])])
       `(block
         (var [binding ,#'old_tc ,_tc])
         ; (app ,#'console.log ,_tc)
         ,dest-declaration                            ; maybe declare temporary variable
         (try {,enter                                 ; add new frame (in non-tail position)
               (:= ,_tc '#t)                          ; an inner wcm is in tail pos (relatively)
               ; wrap in lambda - since result might have a return
               (app (lambda () (body ,result '#f)))} ; calculate result
              (finally
               (:= ,_tc ,#'old_tc)
               ,leave))                      ; remove frame  (in non-tail position)
         ,return-it))])                               ; return the result                
  (Expr2 : Expr  (e  dd [cd #f]) -> Statement ()  ; todo the #f default is a temporary fix
    [,ce (CExpr ce dd cd)]

    [(let-values ,s (((,x** ...) ,ce*) ...) ,e)
     (define initialize
       (for/list ([x* x**] [ce ce*])
         (match (map Var x*)
           ; single value return is the common case, so this needs to be fast
           [(list x)                  (CExpr ce x <stat>)]  ; single value
           ; multiple values are returned in a tagged array [VALUES,v0,v1,...]
           ; to avoid allocating an extra variable, we receive the array in x0,
           ; then assign the individual variables (in reverse order so x0 is last)
           [(list x0 x1 ...) `(block ,(CExpr ce x0 <stat>)
                                     ,(for/list ([i (in-range (length x*) 0 -1)]
                                                 [x (reverse (cons x0 x1))])
                                       `(:= ,x (ref ,x0 ',i)))
                                     ...)]
           ; no values are expected
           ['() (CExpr ce <effect> '<stat>)]))) ; todo: signal error if values are produced
     (let* ([xs         (map Var (append* x**))]
            [undefined* (map (λ(_) #'undefined) xs)]
            [e          (Expr e dd cd)])
       `(block
         (var ,xs ...)     ; declare variables
         ,initialize ...   ; initialize
         ,e))]

    [(letrec-values ,s (((,x* ...) ,ce) ...) ,e)
     (define (AllocateClosure ca dest) ; called by letrec-values
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ae1 ...)
          ; TODO: No need to fill in quotations later.
          ;       Do them here. And skip in FillClosure.
          (let ([0s (map (λ(_) `'0) ae1)]
                [l (Label l)])
            `(:= ,dest (array '"CLOS" ,l ,0s ...)))]))
     (define (FillClosure ca dd)
       (nanopass-case (LANF+closure ClosureAllocation) ca
         [(closure ,s ,l ,ae1 ...)          
          `(block ,(for/list ([i (in-naturals 2)] [ae (AExpr* ae1)])
                     `(app ,#'array! ,dd ',i ,ae))
                  ...)]))
     ;;      (letrec-values (((even) (closure label_7 odd))
     ;;                      ((odd) (closure label_6 even)))
     ;;         expression)
     (unless (apply = (list* 1 1 (map length x*))) ; all clauses expect one value?
       (error 'generate-ur "TODO support multiple values in letrec-values"))
     (let* ([x  (map Var (map first x*))]
            [u  (for/list ([x x*]) `,#'undefined)]
            [e  (Expr e dd cd)])  ; todo: change #f to undefined
       `(block (var ,x ...)                    ; declare 
               (:= ,x ,u) ...                  ; undefined
               ,(map AllocateClosure ce x) ... 
               ,(map FillClosure ce x) ...
               ,e))])
  (ConvertedAbstraction : ConvertedAbstraction (cab) -> Expr ()
    [(λ ,s (formals (,x ...)) ,e)
     (let* ([x (map Var x)] [σ (Expr e <value> <return>)] [ar (length x)])
       (define-values (src line col pos span)
         (values (~a (syntax-source s)) (syntax-line s) (syntax-column s)
                 (syntax-position s) (syntax-span s)))
       `(lambda (,#'_free ,_tc ,x ...)
          (body (sif (app ,#'= ',(+ 2 ar) (ref ,#'arguments '"length")) ,#'VOID
                     (app ,#'raise-clos-arity-error* (app ,#'list ',src ',line ',col ',pos ',span)
                          ,#'_free ',ar ,#'arguments))
                ,σ ,#'VOID)))]
    [(λ ,s (formals ,x) ,e)
     (let* ([x (Var x)] [σ (Expr e <value> <return>)])
       `(lambda (,#'_free ,_tc ,x)
          (body (:= ,x (app ,#'rest-args->list ,#'arguments '0))
                ,σ ,#'VOID)))]
    [(λ ,s (formals (,x0 ,x1 ... . ,xd)) ,e)
     (let* ([x0 (Var x0)] [x1 (map Var x1)] [xd (Var xd)] [σ (Expr e <value> <return>)]
                          [ar (+ 1 (length x1))])
       (define-values (src line col pos span)
         (values (~a (syntax-source s)) (syntax-line s) (syntax-column s)
                 (syntax-position s) (syntax-span s)))
       `(lambda (,#'_free ,_tc ,x0 ,x1 ... ,xd)
          (body (sif (app ,#'<= ',(+ 2 ar) (ref ,#'arguments '"length")) ,#'VOID
                     (app ,#'raise-clos-arity-error* (app ,#'list ',src ',line ',col ',pos ',span)
                          ,#'_free ',(- (+ ar 1)) ,#'arguments))
                (:= ,xd (app ,#'rest-args->list ,#'arguments ',(length (cons x0 x1))))
                ,σ ,#'VOID)))])
  (GeneralTopLevelForm : GeneralTopLevelForm (g dd) -> ModuleLevelForm ()
    [,e                           (Expr e dd <stat>)]
    [(#%require     ,s ,rrs ...)  `'"ignored #%require"]
    [(define-values ,s (,x ...)   ,e)
     (match (map Var x)
       [(list x0) ; single value is the common case
        `(block (var ,x0) ,(Expr e x0 <stat>))]
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
  (TopLevelForm : TopLevelForm (t dd) -> ModuleLevelForm ()
    [,g                       (GeneralTopLevelForm g dd)]
    [(#%expression ,s ,e)     (Expr e dd)]
    [(define-label ,l ,cab)  `(var [binding ,(Label l) ,(ConvertedAbstraction cab)])]
    [(topbegin ,s ,t ...)     (let ([t (TopLevelForm* t dd)])
                                `(block ,t ...))])
  (let* ([result #'result] ; this is an urlang identifier
         [t        (TopLevelForm T result)] ; todo generate unique id
         [prims    (ur-urmodule-name->exports 'runtime)]
         [pr       (map (λ (prim) (datum->syntax #'runtime prim)) prims)]
         [pr-str   (map (λ (prim) (~a (ur-mangle prim))) pr)]
         [RUNTIMES (map (λ (_) #'RUNTIME) prims)]
         [e        #'e])
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
         ;; Global variables
         (var ,f-tmp)              ; used by applications to store the function temporarily
         (var [binding ,_tc '#f]) ; used by wcm (also top-level expressions aren't in tc)
         ;; The result of evaluating this module:
         (define ,result '0) ; todo: make it undefined
         
         (try {,t}        ; run the program              
              (catch ,e   ; catch any uncaught exceptions
                (var [binding ,#'ueh-label     (ref ,#'uncaught-exception-handler '1)])
                (var [binding ,#'handler       (app ,#'ueh-label '#f ,#'uncaught-exception-handler)])
                (var [binding ,#'handler-label (ref ,#'handler '1)])
                (return (app ,#'handler-label '#f ,#'handler ,e))))))))

;;;
;;; Global Variables 
;;;

(require "../urlang/globals.rkt")

(require (only-in racket/unsafe/ops unsafe-fx< unsafe-fx> unsafe-fx+ unsafe-fx- unsafe-fx*)
         (only-in racket/base in-range))
(require racket/list syntax/parse racket/match)

; The alpha renaming phase signals an error when an unbound
; identifier is found - unless - the identifier is present
; in the list of identifiers held by the parameter global-variables.

; The following variables are (seen from Urlang) unbound.
; Most of them stem from macro expansions.
; These global variables are only needed until support for modules is in place.

(define (find-identifier-in-tree sym t)  
  (define (loop t)
    (syntax-parse t
      [x:id #:when (eq? (syntax-e #'x) sym) #'x]
      [(a . d)                              (or (loop #'a) (loop #'d))]
      [other                                #f]))
  (loop t))

(macro-introduced-identifiers
 '(select-handler/no-breaks ; from racket/private/more-scheme produced by with-handlers
   call-handled-body
   check-struct-type        ; from racket/racket/collects/racket/private/define-struct.rkt   
   syntax-srclocs           ; from racket/match/runtime.rkt
   unsafe-vector-length
   unsafe-vector-ref
   unsafe-fx<
   unsafe-fx+   
   in-list))

(let ([find find-identifier-in-tree])
  (global-variables
   ; These identifiers are not define in runtime.rkt.
   ; If an identifier is in this list and in runtime.rkt, then the identifier
   ; exported from runtime.rkt will be alpha-renamed.
   (list (expand-syntax #'srcloc)                          ; expands to kernel:srcloc   
         (expand-syntax #'apply)                           ; expands to new-apply-proc
         (second (syntax->list (expand-syntax #'(apply)))) ; expands to apply
         (find 'match:error                 (expand-syntax #'(match 1 [1 1])))
         (find 'syntax-srclocs              (expand-syntax #'(match 1 [a a])))
         ; (find 'unsafe-vector-ref           (expand-syntax #'(match 1 [(vector a) a])))
         (find 'select-handler/no-breaks    (expand-syntax #'(with-handlers  ([f g]) b)))
         (find 'select-handler/breaks-as-is (expand-syntax #'(with-handlers* ([f g]) b)))
         (find 'call-handled-body           (expand-syntax #'(with-handlers  ([f g]) b)))
         ; in-range
         (find 'in-range                    (expand-syntax #'(in-range  10)))
         (find 'in-list                     (expand-syntax #'(in-list   10)))
         (find 'in-vector                   (expand-syntax #'(in-vector 10)))
         ; Exceptions
         (expand-syntax #'exn)                                 ; kernel:exn
         (expand-syntax #'exn:fail)                            ; kernel:exn:fail
         (expand-syntax #'exn:fail:contract)                   ; kernel:exn:contract
         (expand-syntax #'exn:fail:contract:arity)             ; kernel:exn:contract:arity
         (expand-syntax #'exn:fail:contract:divide-by-zero)    ; kernel:exn:contract:divide-by-zero
       #'exit)))

;;;
;;;
;;;

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

;;;
;;;
;;;

(eval #'(displayln ((λ() 42))))

#;(begin
    (require racket/unsafe/ops)
    (eval #'(begin
                    (let () (define (fn lst)
                              (for ([v (in-list lst)])
                                (displayln v)))            
                      (fn (list 1 2 3 4))))))