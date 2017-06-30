#lang racket
(require "compiler.rkt")
(provide (all-defined-out)
         (all-from-out "compiler.rkt"))

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
  '#%paramz)  

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

;; Closed applications of the type ((λ (x ...) e) e) are rewritten to let-values.
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
    [(app ,s ,x    ,[e1] ...) (guard (primitive? x))  (if (special-primitive? x)
                                                          `(app       ,s ,x              ,e1 ...)
                                                          `(primapp   ,s ,x              ,e1 ...))]
    [(app ,s (λ ,s1 ,[f] ,[e]) ,[e1] ...)                 `(closedapp ,s ,`(λ ,s1 ,f ,e) ,e1 ...)]
    [(app ,s ,[e0] ,[e1] ...)                             `(app       ,s ,e0             ,e1 ...)])
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
;  1) it is a RHS in a let-assignment (it already has a name)
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
        [(wcm ,s ,e0 ,e1 ,e2)
         (Expr/name e0
           (λ (ae0) (Expr/name e1
                      (λ (ae1)
                        (k `(wcm ,s ,ae0 ,ae1 ,(Expr e2 id)))))))]
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
    [(if  ,s ,[ae0 xs0] ,[e1  xs1] ,[e2 xs2]) (values CE (set-union* (list xs0 xs1 xs2)))]
    [(wcm ,s ,[ae0 xs0] ,[ae1 xs1] ,[e  xs2]) (values CE (set-union* (list xs0 xs1 xs2)))]
    [(app ,s ,[ae  xs]  ,[ae1 xs1] ...)       (if (special-primitive? ae)
                                                  (values CE (set-union* xs1))
                                                  (values CE (set-union* (cons xs xs1))))]
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

