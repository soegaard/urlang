#lang racket

;;;
;;; TODO
;;;

; * Automatic "import" of predefined functions and operators
; * Provide list of reserved identifiers and operators in JavaScript
; * Source map

; * Improve error when infix operators are invoked with too few arguments (* 3)

;;;
;;; URLANG
;;;

;; Urlang is a language designed to allow straightforward translation to JavaScript.
;; Think of Urlang as JavaScript with sane syntax and JavaScript semantics.
;; JavaScript in this context is short for ECMAScript 5 in strict mode.

;; Although the constructs of Urlang and JavaScript map almost one-to-one,
;; a little sugar was added:
;;   * function definitions allow default arguments
;;   * let expression

;; Even though the syntax of Urlang is Racket-like, remember that the
;; semantics is standard JavaScript. This means in particular that tail calls
;; build context.

;; Example (factorial):

; > (define fact-program
;     #'(urmodule
;        (export fact)
;        (import + - * = displayln ref console)
;        (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
;        (console.log (fact 5))))
;
; > (compile fact-program)
; "use strict;"
; function fact(n){return (((n===0)===false)?(n*(fact((n-1)))):1);};
; ((console["log"])((fact(5))));
; exports.fact=fact;
;
; > (eval fact-program)
; 120

;; Example (cond-macro and array)

#;(compile
   #'(urmodule
      (export)
      (import + - * % = === < displayln ref console array)
      (define (even? x) (=== (% x 2) 0))
      (var (sum 0) x (a (array 1 2 3 4 5)) (i 0) (n a.length))
      (while (< i n)
             (:= x (ref a i))
             (cond
               [(even? x)  (:= sum (+ sum (ref a i)))]
               [else       "skip"])
             (:= i (+ i 1)))
      (console.log sum)))


;; SYNTAX (cond [e0 e1 e2 ...] ... [else en]), 
;;   like Racket cond except there is no new scope 

;  (define-urlang-macro cond
;    (λ (stx)   
;    (syntax-parse stx
;      [(_cond [else e0:Expr e:Expr ...])
;       #'(begin e0 e ...)]
;      [(_cond [e0 e1 e2 ...] clause ...)
;       (syntax/loc stx
;         (if e0 (begin e1 e2 ...) (cond clause ...)))]
;      [(_cond)
;       (raise-syntax-error 'cond "expected an else clause" stx)])))


;; The heart of the system is a compiler written using the Nanopass
;; compiler Framework. The compiler is exported as a function
;;     compile : urlang-module -> JavaScript
;; that compiles an urlang module and produces JavaScript,
;; that can be evaluated by the Node.js platform (or be embedded in a web page).

;; The Urlang module to be compiled can be represented 
;;    1) as a syntax object
;;    2) as a Nanopass structure (representing an Lurlang program)

;; Use 1) to program in Urlang directly.
;; Use 2) if you intend to use Urlang as a compiler backend.
;; [Note: Nanopass is a framework for implementing compilers.]

;; The intended use of Urlang is to use 1) to write (generate) a Racket runtime in JavaScript.
;; The middle-end of the Racket-to-JavaScript compiler will produce output as Nanopass
;; structures, so 2) will be used as the backend for the Racket-to-JavaScript compiler.

;; Internally the function expand
;;     expand : syntax -> LUrlang
;; will parse and expand its input and produce an LUrlang representation.

;; Note that `expand` allows the user to extend the input language
;; using define-urlang-macro. An Urlang macro is a syntax to syntax
;; transformation implemented as a normal Racket function.
;; This allow you to use all of the standard Racket macro machinery.

;; Main functions:

;;   expand : syntax -> Lurlang
;;     expand the input and produce a fully expanded Urlang program
;;     represented as a Lurlang structure

;;   compile : syntax ->
;;     Expand and compile. The output is written to standard out.

;;   eval : syntax -> value
;;     expand, compile and run the input (an Urlang module represented as a syntax object)
;;     Running means that `node` is used to run the generated JavaScript.

;; Having Urlang as a #lang language allows

;;  * macros (using full Racket at compile time)
;;  * export of defined names
;;  * easier testing

;; In the grammar below:
;;   x stands for a non-keyword identifier
;;   f stands for an identifier defined as a function

; <module>            ::= (urmodule <module-name> <module-path> <module-level-form> ...)

; <module-level-form> ::= <export> | <import> | <definition> | <statement> 
; <export>            ::= (export x ...)
; <import>            ::= (import x ...)
; <definition>        ::= (define (f <formal> ...) <body>)
;                      |  (define x <expr>)
; <formal>           ::= x | [x <expr>]

; <statement>         ::= <var-decl> | <block> | <while> | <do-while> | <if> | <expr>
; <var-decl>          ::= (var <var-binding> ...)
; <block>             ::= (block <statement> ...)
; <var-binding>       ::= x | (x e)
; <while>             ::= (while <expr> <statement> ...)
; <do-while>          ::= (do-while <expr> <statement> ...)
; <if>                ::= (sif <expr> <statement> <statement>)

; <body>              ::= <statement> ... <expr>

; <expr>              ::= <datum>   | <reference> | <application> | <sequence>
;                      |  <ternary> | <assignment> | <let> | <lambda> | <dot>
; <ternary>           ::= (if <expr> <expr> <expr>)
; <reference>         ::= x
; <application>       ::= (<expr> <expr> ...)
; <sequence>          ::= (begin <expr> ...)
; <assignment>        ::= (:= x <expr>)
; <let>               ::= (let ((x <expr>) ...) <statement> ... <expr>)
; <lambda>            ::= (lambda (<formal> ...) <body>)

; <keyword>           ::= define | begin | urmodule | if | := | ...se code...

; <datum>             ::= <fixnum> | <string> | #t | #f

; <identifier>     an identifier that is not a keyword
; <fixnum>         an integer between -2^53 and 2^53
; <module-name>    a symbol or string

;;;
;;; NOTES
;;;

; Some application are special cases:
;    (ref e0 e1)     becomes  e0[e1]
;    (array e ...)   becomes  [e,...]

; Property access with dot notation is rewritten to use bracket syntax in the parser.
; Example:  object.property becomes object["property"]

;;;
;;; SEMANTICS
;;;

; (if e0 e1 e2)
;   If e0 evaluates to value strictly equal to false, then e2 otherwise e1.
;   Note: The JavaScript becomes  ((e0===false) ? e2 : e1)

; (var x (y 3))
;   Binds x to undefined and y to 3.

(require syntax/parse syntax/stx nanopass/base
         (for-syntax syntax/parse))

(define-syntax (letv stx)
  ; syntax: (letv ((x ...) e) b ...)
  ;   bind the result of e to the variables x ... in the body b ...
  ;   That is: letv is let-values with only one clause
  (syntax-parse stx
    [(_letv ((x:id ...) e:expr) b:expr ...)
     (syntax/loc stx
       (let-values ([(x ...) e]) b ...))]))

(define-syntax (receive1 stx)
  (syntax-parse stx
    [(_receive1 e:expr ...)
     (syntax/loc stx
       (let-values ([(x _) (begin e ...)]) x))]))

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

(require (for-syntax racket/syntax))

; tables of free identifiers
(define-syntax (define-free-table stx)
  (syntax-parse stx
    [(_define-free-table id)
     (with-syntax ([id-ft (format-id #'id "~a-ft" #'id #:source #'id)]
                   [ids   (format-id #'id "~as"   #'id #:source #'id)]
                   [id!   (format-id #'id "~a!"   #'id #:source #'id)]
                   [id?   (format-id #'id "~a?"   #'id #:source #'id)])
       (syntax/loc stx
         (begin
           (define id-ft                (make-free-id-table))
           (define (id! x [v #t])       (free-id-table-set! id-ft x v))
           (define (ids)                (reverse (free-id-table-map id-ft (λ (x _) x))))
           (define (id? x [default #f]) (free-id-table-ref id-ft x default)))))]))

; tables of bound identifiers
(define-syntax (define-bound-table stx)
  (syntax-parse stx
    [(_define-bound-table id)
     (with-syntax ([id-bt (format-id #'id "~a-bt" #'id #:source #'id)]
                   [ids   (format-id #'id "~as"   #'id #:source #'id)]
                   [id!   (format-id #'id "~a!"   #'id #:source #'id)]
                   [id?   (format-id #'id "~a?"   #'id #:source #'id)])
       (syntax/loc stx
         (begin
           (define id-bt                (make-bound-id-table))
           (define (id! x [v #t])       (bound-id-table-set! id-bt x v))
           (define (ids)                (reverse (bound-id-table-map id-bt (λ (x _) x))))
           (define (id? x [default #f]) (bound-id-table-ref id-bt x default)))))]))


;;;
;;; DATUMS
;;;

(define min-fixnum (- (expt 2 53)))
(define max-fixnum    (expt 2 53))

(define (fixnum? r)
  (and (number? r) (integer? r)
       (<= min-fixnum r max-fixnum)))

;;;
;;; KEYWORDS
;;;

(define-syntax block    (λ (stx) (raise-syntax-error 'block    "used out of context" stx)))
(define-syntax do-while (λ (stx) (raise-syntax-error 'do-while "used out of context" stx)))
(define-syntax export   (λ (stx) (raise-syntax-error 'export   "used out of context" stx)))
(define-syntax global   (λ (stx) (raise-syntax-error 'global   "used out of context" stx)))
(define-syntax import   (λ (stx) (raise-syntax-error 'import   "used out of context" stx)))
(define-syntax sif      (λ (stx) (raise-syntax-error 'sif      "used out of context" stx)))
(define-syntax urmodule (λ (stx) (raise-syntax-error 'urmodule "used out of context" stx)))
(define-syntax var      (λ (stx) (raise-syntax-error 'var      "used out of context" stx)))
(define-syntax while    (λ (stx) (raise-syntax-error 'while    "used out of context" stx)))
(define-syntax :=       (λ (stx) (raise-syntax-error ':=       "used out of context" stx)))

(define-literal-set keywords (begin block define do-while export global
                                    if import lambda λ let sif urmodule var while :=))
(define keyword? (literal-set->predicate keywords))

;;; EcmaScript 6 Reserved keywords
; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference
; /Lexical_grammar#Reserved_keywords_as_of_ECMAScript_6
(define ecma6-reservered-keywords
  '(break case class catch const continue debugger default
          delete do else export extends finally for function if import in instanceof let
          new return super switch this throw try typeof var void while with yield
          ; Future Keywords
          enum await
          ; Future Keywords in strict mode
          implements package protected static interface private public
          ; Literals
          null true false))

(define (unparse-id x)     (syntax-e x))
(define (unparse-datum d)  d)
(define (unparse-syntax s)
  ; nanopass picks the wrong unparser for identifiers
  ; (#'x is accepted as the syntax terminal even though the grammar says x)
  (if (identifier? s) (syntax-e s) '_))

(define (id? v)          (identifier? v))
(define (datum? v)       (or (fixnum? v) (string? v) (boolean? v)))
(define (module-name? v) (or (symbol? v) (string? v)))

;;;
;;; URLANG AS NANOPASS LANGUAGE
;;;

(define-language L 
  (entry Module)
  (terminals
   ((id          (f x)) . => . unparse-id)
   ((datum       (d))   . => . unparse-datum)
   (module-name  (mn)))
  (Module (u)
    (urmodule mn m ...))
  (ModuleLevelForm (m)
    (export x ...)
    (import x ...)
    δ σ)
  (Definition (δ)
    (define (f x ...) b)          ; function definition
    (define x e))                 ; variable definition
  #;(Formal (φ) ; When Nanpass Issue xxx is fixed this will be used in Definition and (lambda ...)
    x                             ; parameter name
    (x e))                        ; parameter name and default value
  (Body (b)
    (body σ ... e))
  (VarBinding (vb)
    x
    (binding x e) => (x e))
  (Statement (σ)
    e                             ; expression
    (var vb ...)                  ; variable definition
    (sif e σ1 σ2)                 ; statement if
    (block      σ ...)            ; block (no new scope in JavaScript)
    (while    e σ ...)          
    (do-while e σ ...))  
  (Expr (e)
    x                             ; reference
    (app e0 e ...) => (e0 e ...)  ; application
    (:= x e)                      ; assignment
    (begin e ...)                 ; sequence
    (if e0 e1 e2)                 ; ternary
    (let ((x e) ...) b)           ; local binding
    (lambda (x ...) b)            ; anonymous function
    (quote d)))                   ; quotation (the parser quotes all datums)

;;;
;;; GRAMMAR AS SYNTAX CLASSES
;;;

; The above grammar as syntax classes.
; Parsing from syntax-object to nanopass structures will use syntax-parse.
; Defining syntax-classes makes the translation easy.

(define-syntax-class Fixnum
  #:opaque
  (pattern d
           #:fail-unless (fixnum? (syntax-e #'d)) #f))

(define-syntax-class String
  #:opaque
  (pattern d
           #:fail-unless (string? (syntax-e #'d)) #f))

(define-syntax-class Symbol
  #:opaque
  (pattern d
           #:fail-unless (symbol? (syntax-e #'d)) #f))

(define-syntax-class Datum
  #:description "<datum>"
  (pattern (~or d:Fixnum d:String bool:boolean)))

(define-syntax-class ModuleName
  #:description "<module-name>"
  (pattern (~or mn:Symbol mn:String)))  

(define-syntax-class Keyword
  #:literal-sets (keywords)
  (pattern x:identifier
           #:fail-unless (keyword? #'x) "keyword"))

(define-syntax-class Id
  (pattern (~and x:id (~not y:Keyword))))

(define-syntax-class Reference
  (pattern x:Id))

(define-syntax-class Application
  (pattern (e0:Expr e:Expr ...)))

(define-syntax-class MacroApplication
  #:literal-sets (keywords)
  (pattern (m:Id . _)
           #:when (macro? #'m)
           #:attr transformer (macro-transformer #'m)))

(define-syntax-class Sequence
  #:literal-sets (keywords)
  (pattern (begin e0:Expr e:Expr ...)))

(define-syntax-class Ternary
  #:literal-sets (keywords)
  (pattern (if e0:Expr e1:Expr e2:Expr)))

(define-syntax-class Assignment
  #:literal-sets (keywords)
  (pattern (:= x4:Id e4:Expr)))

(define-syntax-class Definition
  #:literal-sets (keywords)
  (pattern
   (~or (define (f:Id φ:Formal ...) σ:Statement ... body:Body)
        (define x:id e:Expr))))

(define-syntax-class Lambda
  #:literal-sets (keywords)
  (pattern (~or (lambda (x ...) body:Body)
                (λ      (x ...) body:Body))))

(define-syntax-class Statement
  #:literal-sets (keywords)
  (pattern (~or m:Expr w:While v:VarDecl β:Block dw:DoWhile i:If)))

(define-syntax-class Block
  #:literal-sets (keywords)
  (pattern (block σ:Statement ...)))

(define-syntax-class VarDecl
  #:literal-sets (keywords)
  (pattern (var vb:VarBinding ...)))

(define-syntax-class VarBinding
  #:literal-sets (keywords)
  (pattern (~or x:Id (x:Id e:Expr))))

(define-syntax-class Formal
  #:literal-sets (keywords)
  (pattern (~or x:Id (x:Id e:Expr))
           #:attr xe (if (attribute e) #'(x e) #f)))

(define-syntax-class While
  #:literal-sets (keywords)
  (pattern (while e:Expr σ:Statement ...)))

(define-syntax-class If
  #:literal-sets (keywords)
  (pattern (sif e:Expr σ1:Statement σ2:Statement)))

(define-syntax-class Let
  #:literal-sets (keywords)
  (pattern (let ((xl:Id el:Expr) ...) b:Body)))

(define-syntax-class DoWhile
  #:literal-sets (keywords)
  (pattern (do-while e:Expr σ:Statement ...)))

(define-splicing-syntax-class Body
  (pattern (~seq σ:Statement ... b:Expr)))

(define-syntax-class Expr
  (pattern (~or e:Datum
                e:Application
                e:Reference
                e:Sequence
                e:Ternary
                e:Assignment
                e:Let
                el:Lambda)))

(define-syntax-class Export
  #:literal-sets (keywords)
  (pattern (export x:Id ...)))

(define-syntax-class Import
  #:literal-sets (keywords)
  (pattern (import x:Id ...)))

(define-syntax-class ModuleLevelForm
  (pattern (~or ex:Export
                im:Import
                m:Definition
                m:Statement)))

(define-syntax-class Module
  #:literal-sets (keywords)
  (pattern (urmodule mn:ModuleName m:ModuleLevelForm ...)))

;;;
;;; PARSING: FROM SYNTAX OBJECT TO NANOPASS REPRESENTATION
;;;

(define macro-expansion-context
  (make-parameter 'module-level
                  (λ (c) (or (and (member c '(module-level statement expression)) c)
                             (error 'expansion-context
                                    "expected one of: 'module-level, 'statement, 'exression ; got ~a"
                                    c)))))

; There are three expansion contexts:
;   'module-level (during expansion of a module-level-form)
;   'statement    (during expansion of a statement)
;   'expression   (during expansion of a expression)

; If a macro transformer is called with an context of
;    'expression   then the transformer output is parsed with parse-expression
;    'statement    then the transformer output is parsed with parse-statement
;    'module-level then the transformer output is parsed with parse-module-level-form

(define macros-ht (make-hash))

(define-syntax (define-urlang-macro stx)
  (syntax-parse stx
    [(_define-urlang-macro (name:id arg:id) e:expr)
     (syntax/loc stx
       (hash-set! macros-ht 'name (λ (arg) e)))]
    [(_define-urlang-macro name:id transformer)
     (syntax/loc stx
       (hash-set! macros-ht 'name transformer))]))

(define (macro? id)
  (hash-ref macros-ht (syntax-e id) #f))

(define (macro-transformer id)
  (hash-ref macros-ht (syntax-e id) #f))

(define (parse stx)
  (parse-urmodule stx))

(define (parse-urmodule u)
  (with-output-language (L Module)
    (syntax-parse u
      #:literal-sets (keywords)
      [(urmodule mn:ModuleName m:ModuleLevelForm ...)
       (let ((m  (stx-map parse-module-level-form #'(m ...)))
             (mn (syntax-e #'mn)))
         `(urmodule ,mn ,m ...))])))

(define (parse-export ex)
  (with-output-language (L ModuleLevelForm)
    (syntax-parse ex
      #:literal-sets (keywords)
      [(export x:Id ...)
       (let ([x (syntax->list #'(x ...))])
         `(export ,x ...))])))

(define (parse-import im)
  (with-output-language (L ModuleLevelForm)
    (syntax-parse im
      #:literal-sets (keywords)
      [(import x:Id ...)
       (let ([x (syntax->list #'(x ...))])
         `(import ,x ...))])))

(define (parse-module-level-form m)
  (parameterize ([macro-expansion-context 'module-level])
    (with-output-language (L ModuleLevelForm)
      (syntax-parse m
        #:literal-sets (keywords)
        [ex:Export            (parse-export #'ex)]
        [im:Import            (parse-import #'im)]
        [ma:MacroApplication  (parse-module-level-form
                               (parse-macro-application #'ma))]
        [e:Expr               (parse-statement #'e)]
        [d:Definition         (parse-definition #'d)]
        [σ:Statement          (parse-statement  #'σ)]))))

(define (parse-statement σ)
  (parameterize ([macro-expansion-context 'statement])
    (with-output-language (L Statement)
      (syntax-parse σ
        #:literal-sets (keywords)
        [ma:MacroApplication (parse-statement
                              (parse-macro-application #'ma))]
        [e:Expr              (parse-expr     #'e)]
        [w:While             (parse-while    #'w)]
        [dw:DoWhile          (parse-do-while #'dw)]
        [v:VarDecl           (parse-var-decl #'v)]
        [β:Block             (parse-block    #'β)]
        [i:If                (parse-if       #'i)]))))

(define (parse-macro-application ma)
  (syntax-parse ma
    #:literal-sets (keywords)
    [ma:MacroApplication
     (define mark (make-syntax-introducer))
     (let ((transform (attribute ma.transformer)))
       (mark (transform (mark #'ma))))]))

(define (parse-if i)
  (with-output-language (L Statement)
    (syntax-parse i
      #:literal-sets (keywords)
      [(sif e:Expr σ1:Statement σ2:Statement)
       (let ((e  (parse-expr #'e))
             (σ1 (parse-statement #'σ1))
             (σ2 (parse-statement #'σ2)))
         `(sif ,e ,σ1 ,σ2))])))

(define (parse-let l)
  (with-output-language (L Expr)
    (syntax-parse l
      #:literal-sets (keywords)
      [(let ((x:Id e:Expr) ...) body:Body)
       (let ([x  (syntax->list #'(x ...))]
             [e (stx-map parse-expr #'(e ...))]
             (b (parse-body #'body)))
       `(let ((,x ,e) ...) ,b))])))

(define (parse-block β)
  (with-output-language (L Statement)
    (syntax-parse β
      #:literal-sets (keywords)
      [(block σ:Statement ...)
       (let ((σ (stx-map parse-statement #'(σ ...))))
         `(block ,σ ...))])))

(define (parse-var-decl v)
  (with-output-language (L Statement)
    (syntax-parse v
      #:literal-sets (keywords)
      [(var vb:VarBinding ...)
       (let ((vb (stx-map parse-var-binding #'(vb ...))))
         `(var ,vb ...))])))

(define (parse-var-binding vb)
  (with-output-language (L VarBinding)
    (syntax-parse vb
      #:literal-sets (keywords)
      [x:Id          #'x]
      [(x:Id e:Expr) `(binding ,#'x ,(parse-expr #'e))])))

(define (parse-while w)
  (with-output-language (L Statement)
    (syntax-parse w
      #:literal-sets (keywords)
      [(while e:Expr σ:Statement ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(while ,e ,σ ...))])))

(define (parse-do-while dw)
  (with-output-language (L Statement)
    (syntax-parse dw
      #:literal-sets (keywords)
      [(do-while e:Expr σ:Statement ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(do-while ,e ,σ ...))])))

(define (parse-definition d)
  (with-output-language (L Definition)
    (syntax-parse d
      #:literal-sets (keywords)
      [(define x:Id e:Expr)
       (let ((e (parse-expr #'e)))
         `(define ,#'x ,e))]
      [(define (f:Id φ:Formal ...) b:Body)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ0 ...) #'((sif (=== x0 undefined) (:= x0 e0) (block)) ...)])
             (with-syntax ([(σ ... en) #'b])
               (let ((b (parse-body #'(σ0 ... σ ... en))))
                 `(define (,#'f ,x ...) ,b))))))])))

(define (parse-lambda d)
  (with-output-language (L Expr)
    (syntax-parse d
      #:literal-sets (keywords)
      [(_lambda (φ:Formal ...) b:Body)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ0 ...) #'((sif (=== x0 undefined) (:= x0 e0) (block)) ...)])
             (with-syntax ([(σ ... en) #'b])
               (let ((b (parse-body #'(σ0 ... σ ... en))))
                 `(lambda (,x ...) ,b))))))])))
      
(define (parse-body b)
  (with-output-language (L Body)
    (syntax-parse b
      #:literal-sets (keywords)
      [(σ:Statement ... e:Expr)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(body ,σ ... ,e))])))

(define (parse-expr e)
  (parameterize ([macro-expansion-context 'expression])
    (syntax-parse e
      #:literal-sets (keywords)
      [ma:MacroApplication  (parse-expr
                             (parse-macro-application #'ma))]
      [d:Datum              (parse-datum       #'d)]
      [a:Application        (parse-application #'a)]
      [r:Reference          (parse-reference   #'r)]
      [s:Sequence           (parse-sequence    #'s)]
      [t:Ternary            (parse-ternary     #'t)]
      [a:Assignment         (parse-assignment  #'a)]
      [l:Let                (parse-let         #'l)]
      [la:Lambda            (parse-lambda      #'la)]
      [_ (raise-syntax-error 'parse-expr (~a "expected an expression, got " e) e)])))

(define (parse-application a)
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(e0:Expr e:Expr ...)
       (let ([e0 (parse-expr #'e0)]
             [e  (stx-map parse-expr #'(e ...))])
         `(app ,e0 ,e ...))])))

(define (parse-reference r)
  (with-output-language (L Expr)
    (syntax-parse r
      #:literal-sets (keywords)
      [x:Id
       (match (regexp-match #rx"(.*)[.](.*)" (symbol->string (syntax-e #'x)))
         [#f `,#'x]
         [(list y.p y p)
          ; object.property becomes object["property"]
          (let ([y (format-id #'x y #:source #'x)])
            (with-output-language (L Expr)
              (let ([e (parse-reference y)]
                    [p (parse-datum p)])
                `(app ,#'ref ,e ,p))))])])))

(define (parse-sequence a)
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(begin e0:Expr)            (parse-expr #'e0)]
      [(begin e0:Expr e:Expr ...) (let ([e0 (parse-expr #'e0)]
                                        [e  (stx-map parse-expr #'(e ...))])
                                    `(begin ,e0 ,e ...))])))

(define (parse-assignment a)
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(:= x:Id e:Expr)
       (let ((e (parse-expr #'e)))
         `(:= ,#'x ,e))])))

(define (parse-ternary t)
  (with-output-language (L Expr)
    (syntax-parse t
      #:literal-sets (keywords)
      [(if e0:Expr e1:Expr e2:Expr)
       (let ([e0 (parse-expr #'e0)]
             [e1 (parse-expr #'e1)]
             [e2 (parse-expr #'e2)])
         `(if ,e0 ,e1 ,e2))])))

(define (parse-datum d)
  (with-output-language (L Expr)
    (syntax-parse d
      #:literal-sets (keywords)
      [f:Fixnum  `(quote ,(parse-fixnum  #'f))]
      [s:String  `(quote ,(parse-string  #'s))]
      [b:boolean `(quote ,(parse-boolean #'b))])))

(define (parse-fixnum f)  (syntax-e f))
(define (parse-string f)  (syntax-e f))
(define (parse-boolean f) (syntax-e f))

;;;
;;; OPERATORS
;;;

(define infix-operators
  '(+ - * / %                        ; arithmetical
      = == === != !== < > <= >=      ; comparison   (= is an alias for ===)
      and or not                     ; logical
      ~ << >> >>>                    ; bitwise
      bit-and bit-or bit-xor bit-not))

(define (symbols->ids ss)
  (map (λ (s) (format-id #'here "~a" s)) ss))

(define infix-operators-ids (symbols->ids infix-operators))

(define (infix-operator? v)
  (or (and (identifier? v) (infix-operator? (syntax-e v)))
      (memq v infix-operators)))

;;;
;;; PREDEFINED NAMES AND RESERVED WORDS
;;;

(define predefined-names '(ref console arguments array))

(define reserved-words-ids (symbols->ids ecma6-reservered-keywords))
(define predefined-ids     (symbols->ids predefined-names))

         


;;;
;;; DESUGARED URLANG
;;;

#;(define-language L9 (extends L)
  (Definition (δ)
    (- (define x e))    
    (+ (define x e))
    (- (define (f φ ...) b))
    (+ (define (f x ...) b))
    )
  (Formal (φ)
    (- x)
    (- (x e))))

;;;
;;; URLANG ANNOTATED MODULE 
;;;

(define-language L0 (extends L)
  (terminals
   (- ((id (f x)) . => . unparse-id))
   (+ ((id (f x)) . => . unparse-id)))
  (ModuleLevelForm (m)
    (- (export x ...)
       (import x ...)))
  (Annotation (an)
    (+ (export x0 ...) (import x1 ...) (funs x2 ...) (vars x3 ...)))
  (Module (u)
    (- (urmodule mn m ...))
    (+ (urmodule mn (an ...) m ...))))

;;;
;;; ANNOTATED URLANG
;;;

; Each body is annotated with a list of all variables declared in the body.
; Since variable declarations in JavaScript apply to their entire context
; (function body or global scope), we need to "hoist" variable declarations
; before α-renaming can be done.

(define-language L1 (extends L0)
  (terminals
   (- ((id (f x)) . => . unparse-id))
   (+ ((id (f x)) . => . unparse-id)))
  (Body (b)
    (- (body σ ... e)))
  (AnnotatedBody (ab)
    (+ (annotated-body (x ...) σ ... e)))
  (Definition (δ)
    (- (define (f x ...) b))
    (+ (define (f x ...) ab)))
  (Expr (e)
    (- (let ((x e) ...) b))
    (+ (let ((x e) ...) ab))
    (- (lambda (x ...) b))
    (+ (lambda (x ...) ab))))

;;;
;;; COLLECT
;;;

;; This pass:
;;   * collects all exported and imported identifiers
;;   * imports predefined operators (+, -, *, ...)
;;   * collects all module-level defined function names
;;   * collects all module-level defined variable names
;;   * Adds an annotation to the urmodule form
;;       (annotation
;;         (export x ...) (import x ...) (funs ...) (vars ...))
;;   * Remove import and export forms from ModuleLevelForm

;;   * checks that a function f is not declared as global
;;   * checks that there are no duplicate function names
;;   * checks that all exports are defined as functions

(define-pass collect : L (U) -> L0 ()
  (definitions
    (define-free-table  export)
    (define-free-table  import)
    (define-free-table  fun)
    (define-bound-table var)
    (define-free-table  operator)   ; operators 
    (define-free-table  reserved)   ; reserved words
    (define-free-table  predefined) ; predefined names

    (define context      (make-parameter 'module-level))
    (define remove-form  (list 'remove-form))
    (define (keep? v)    (not (eq? v remove-form)))
    (define (complain msg where) (raise-syntax-error 'collect msg where)))
  (Module : Module (u) ->  Module ()
    [(urmodule ,mn ,[m] ...)
     ;; register predefined names, reserved words, and, operators
     (for ([op infix-operators-ids]) (operator!   op))
     (for ([op reserved-words-ids])  (reserved!   op))
     (for ([op predefined-ids])      (predefined! op))     
     ;; check that all exports are defined
     (match (for/first ([x (exports)] #:unless (or (fun? x) (var? x))) x)
       [#f #f] [x  (complain "exported identifier not defined" x)])
     ;; check that imports don't appear as functions or (module-level) variables
     (match (for/first ([x (append (vars) (funs))] #:when (import? x)) x)
       [#f #f] [x  (complain "identifier is declared as imported" x)])
     ;; check that funs and vars names aren't operators
     (match (for/first ([x (append (vars) (funs))] #:when (operator? x)) x)
       [#f #f] [x  (complain "identifier is a predefined operator (choose a different name)" x)])
     ;; check that funs and vars names aren't reserved
     (match (for/first ([x (append (vars) (funs))] #:when (reserved? x)) x)
       [#f #f] [x  (complain "identifier is a reserved word in EcmaScript 6" x)])
     ;; check that funs and vars names aren't predefined
     (match (for/first ([x (append (vars) (funs))] #:when (predefined? x)) x)
       [#f #f] [x  (complain "identifier is a predefined name" x)])
     ;; check that vars are aren't defined as functions
     (match (for/first ([x (vars)] #:when (fun? x)) x)
       [#f #f] [x  (complain "identifier is declared as a function" x)])
     ;; annotate module
     (let ((m (filter keep? m)))
       (let ((an (with-output-language (L0 Annotation)
                   (list `(export ,(exports) ...)
                         `(import ,(append (predefineds) (operators) (imports)) ...)
                         `(funs   ,(funs)    ...)
                         `(vars   ,(vars)    ...)))))
         `(urmodule ,mn (,an ...) ,m ...)))])
  (ModuleLevelForm : ModuleLevelForm (m) -> ModuleLevelForm ()
    [(import ,x ...) (for-each import! x) remove-form]
    [(export ,x ...) (for-each export! x) remove-form])
  (Definition : Definition (δ) ->  Definition ()
    [(define ,x ,[e])         (when (var? x)
                                (raise-syntax-error 'collect "identifier is declared twice as var" x))
                               (var! x)
                              `(define ,x ,e)]
    [(define (,f ,x0 ...) ,b) (when (fun? f)
                                (raise-syntax-error 'collect "identifier is declared twice as fun" f))
                              (fun! f)
                              (parameterize ([context 'body])
                                (let ((b (Body b)))
                                  `(define (,f ,x0 ...) ,b)))])
  (Body : Body (b) -> Body ())
  (VarBinding : VarBinding (vb) -> VarBinding ()
    [,x                  (when (eq? (context) 'module-level)
                           (var! x))
                         vb]
    [(binding ,x ,[e])  (when (eq? (context) 'module-level)
                          (var! x))
                        `(binding ,x ,e)])
  (Module U))


;;;
;;; ANNOTATE BODIES
;;;

; Bodies are annotated with all declared variables.

(define-pass annotate-bodies : L0 (U) -> L1 ()
  (definitions
    (define locals         (make-parameter #f)) ; #f indicate module-level
    (define (local? x)     (bound-id-table-ref (locals) x #f))
    (define (get-locals)   (reverse (bound-id-table-map (locals) (λ (x _) x))))
    (define (add! x)       (when (locals) (bound-id-table-set! (locals) x #t))))
  (Annotation : Annotation (an) -> Annotation ()
    [(export ,x ...)                     `(export ,x ...)]
    [(import ,x ...)   (for-each add! x) `(import ,x ...)]
    [(funs   ,x ...)   (for-each add! x) `(funs   ,x ...)]
    [(vars   ,x ...)   (for-each add! x) `(vars   ,x ...)])
  (Statement : Statement (σ) ->  Statement ())
  (Expr : Expr (e) -> Expr ())
  (Body : Body (b) -> AnnotatedBody ()
    [(body ,σ ... ,e)
     (parameterize ([locals (make-bound-id-table)])
       (let ((σ (map Statement σ)) (e (Expr e)))
         `(annotated-body (,(get-locals) ...) ,σ ... ,e)))])
  (VarBinding : VarBinding (vb) -> VarBinding ()
    [,x                (add! x) vb]
    [(binding ,x ,[e]) (add! x) `(binding ,x ,e)]))

;;;
;;; α-RENAMING (Not update to work with L1)
;;;

; Macro introduced variables are renamed in this pass.

(define counter 0)
(define joiner "_")
(define (new-var [prefix "t"])
  (displayln (list 'newvar prefix counter))
  (set! counter (+ counter 1))
  (define pre (if (syntax?   prefix) (syntax-e prefix) prefix))
  (datum->syntax #'here (string->symbol (~a pre joiner counter))))
(define (reset-counter! [joiner "_"])
  (set! counter 0))

(define-pass α-rename : L1 (U) -> L1 ()
  (definitions
    (define-free-table global)  ; free references refer to module-level-defined variables
    (define-bound-table var)    ; references to variables declared with var can be macro introduced
    (define (initial-ρ x [id= #f])
      (cond
        [(var? x) => identity] ; get (potentially renamed x)
        [(global? x) x]        ; exports, imports, funs are never renamed
        [else        #f]))     ; no other variables are bound
    (define (fid= x y)      (free-identifier=? x y))
    (define (bid= x y)      (bound-identifier=? x y))
    (define (extend ρ original renamed)
      (λ (x id=) (if (bid= x original) renamed (ρ x id=))))
    (define (extend* ρ xs) (for/fold ((ρ ρ)) ((x xs)) (extend ρ x x)))
    (define (Statement* σs ρ) (map (λ (σ) (Statement σ ρ)) σs))
    (define (fresh x ρ [orig-x x]) (if (ρ x fid=) (fresh (new-var x) ρ x) x))
    (define (fresh-var x) (if (global? x) (fresh-var (new-var x)) x))
    (define (rename x ρ) (define x* (fresh x ρ)) (values x* (extend ρ x x*)))
    (define (rename* xs ρ) (map2* rename xs ρ))
    (define (lookup x ρ [on-not-found (λ (_) #f)])
      (match (ρ x bid=) [#f (match (ρ x fid=) [#f (on-not-found x)] [y y])] [y y]))
    (define (unbound-error x) (raise-syntax-error 'α-rename "unbound variable" x)))
  (Annotation : Annotation (an) -> Annotation ()
    [(export ,x  ...)                      an]
    [(import ,x  ...) (for-each global! x) an]
    [(funs   ,x  ...) (for-each global! x) an]
    [(vars   ,x* ...) (for ((x x*))
                        (let ([y (fresh-var x)])
                          (global! y)
                          (var! x y)))
                      an])
  (Module : Module (u) -> Module ()
    [(urmodule ,mn (,[an] ...) ,m ...)
     (let ((ρ initial-ρ))
       ; Note: Macros can introduce global variable bindings
       ;       The statement (block (var x) ...) introduces x as a module-level variable.
       ;       This means that module-level variables may have to be renamed.
       ; Note: Macros can't introduce functions, imports or exports (yet).
       ;       No renaming needed for them (yet).
       ; TODO TODO rename vars from x to y in annotations
       (let ((m (map (λ (m) (ModuleLevelForm m ρ)) m)))
         `(urmodule ,mn (,an ...) ,m ...)))])
  (ModuleLevelForm : ModuleLevelForm (m ρ) -> ModuleLevelForm ()
    [,δ (Definition δ ρ)]
    [,σ (Statement  σ ρ)])
  (Definition : Definition  (δ ρ)  ->  Definition  ()
    [(define ,x ,e)              (let ((ρ (extend ρ x x))) ; map x to x
                                   (let ((e (Expr e ρ)))
                                     `(define ,x ,e)))]
    [(define (,f ,x ...) ,ab)    (let ([ρ (extend* ρ x)])  ; map x to x
                                   (let ([ab (AnnotatedBody ab ρ)])
                                     `(define (,f ,x ...) ,ab)))])
  (VarBinding : VarBinding (vb ρ) -> VarBinding ()
    [,x                             (ρ x bid=)]
    [(binding ,x ,[e])   `(binding ,(ρ x bid=) ,e)])
  (AnnotatedBody : AnnotatedBody (b ρ) -> AnnotatedBody ()
    [(annotated-body (,x ...) ,σ ... ,e)
     (letv ((y ρ) (rename* x ρ)) ; extend and rename
       (let ((σ (Statement* σ ρ)) (e (Expr e ρ)))
         `(annotated-body (,y ...) ,σ ... ,e)))])
  (Statement : Statement (σ ρ) -> Statement ())
  ; Expression never change the environment, so only a single return value
  (Expr : Expr (e ρ) -> Expr ()
    ; all expressions that contain an id (x or f) needs consideration
    [,x                         (lookup x ρ unbound-error)]
    [(:= ,x ,[e])               (let ((y (lookup x ρ unbound-error))) `(:= ,y ,e))]
    [(let ((,x ,[e]) ...) ,ab)  (let ([ρ (extend* ρ x)])  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(let ((,x ,e) ...) ,ab)))]
    [(lambda (,x ...) ,ab)      (let ([ρ (extend* ρ x)])  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(lambda (,x ...) ,ab)))])
  (Module U))

;;;
;;; CODE GENERATION
;;;

(define-pass generate-code : L1 (U) -> * ()
  (definitions
    (define (~parens . t)    (list "(" t ")"))
    (define (~braces . t)    (list "{" t "}"))
    (define (~brackets . t)  (list "[" t "]"))
    (define (~commas ts)     (add-between ts ","))
    (define (~newline t)     (list t "\n"))
    (define (~semi t)        (list t ";"))
    (define (~Statement . t) (list t ";"))
    (define (~Return t)      (~Statement "return " t))
    (define (~displayln t)   (list "console.log" (~parens t)))
    (define (exports.id x)   (format-id x "exports.~a" x)))
  (Module : Module (u) -> * ()
    [(urmodule ,mn (,an ...) ,m ...) (list (~newline (~Statement "\"use strict;\""))
                                           (map ModuleLevelForm m)
                                           (map Annotation an))])
  (Annotation : Annotation (an) -> * ()
    [(import ,x ...)   ""]
    [(funs   ,x ...)   ""]
    [(vars   ,x ...)   ""]
    [(export ,x* ...)  (for/list ([x x*])
                         (~newline (~Statement (exports.id x) "=" x)))])
  (ModuleLevelForm : ModuleLevelForm (m) -> * ()
    [,δ (~newline (Definition δ))]
    [,σ (~newline (Statement σ))]
    [,e (~newline (~Statement (~displayln (Expr e))))])
  (Definition : Definition (δ) -> * ()
    [(define ,x ,e)            (let ([e (Expr e)])
                                 (~Statement `(var ,x "=" ,e)))]
    [(define (,f ,x ...) ,ab) (let ((ab (AnnotatedBody ab)))
                                (~Statement `(function ,f ,(~parens (~commas x))
                                                       ,ab)))])
  (Statement : Statement (σ) -> * ()
    [,e (~Statement (Expr e))]
    [(block ,σ ...)       (let ((σ (map Statement σ)))
                            (~braces σ))]
    [(sif ,e ,σ1 (block)) (let ((e (Expr e)) (σ1 (Statement σ1)))
                            (list "if" (~parens "!" (~parens e "===false")) σ1))]
    [(sif ,e ,σ1 ,σ2)     (let ((e (Expr e)) (σ1 (Statement σ1)) (σ2 (Statement σ2)))
                            (~Statement "if" (~parens "!" (~parens e "===false")) σ1 "else" σ2))]
    [(while ,e ,σ ...)    (let ((e (Expr e)) (σ (map Statement σ)))
                            (~Statement "while" (~parens e) (~braces σ)))]
    [(do-while ,e ,σ ...) (let ((e (Expr e)) (σ (map Statement σ)))
                            (~Statement "do" (~braces σ) "while" (~parens e)))]
    [(var ,vb ...)        (match (map VarBinding vb)
                            [(list (list xs es) ...)
                             (~Statement
                              `(var ,(~commas (for/list ([x xs] [e es])
                                                (if e (list x "=" e) x)))))])])
  (VarBinding : VarBinding (vb) -> * ()
    [,x              (list x #f)]
    [(binding ,x ,e) (list x (Expr e))])
  (AnnotatedBody : AnnotatedBody (ab) -> * ()
    [(annotated-body (,x ...) ,σ ... ,e) (let ((σ (map Statement σ)) (e (Expr e)))
                                           (~braces σ (~Return e)))])
  (Expr : Expr (e) -> * ()
    [,x x]
    [(quote ,d)             (cond
                              [(string? d)  (list "\"" d "\"")]
                              [(boolean? d) (if d "true" "false")]
                              [(number? d)  d]
                              [else (error 'generate-code "expedcted datum, got ~a" d)])]
    [(if ,e0 ,e1 ,e2)       (let ((e0 (Expr e0)) (e1 (Expr e1)) (e2 (Expr e2)))
                              (~parens (~parens e0 "===false") "?" e2 ":" e1))]
    [(begin ,e ...)         (let ((e (map Expr e)))
                              (~parens (~commas e)))]
    [(:= ,x ,e)             (let ((e (Expr e)))
                              (~parens x "=" e))]
    [(let ((,x ,e) ...) ,ab) (let ((e (map Expr e)) (ab (AnnotatedBody ab)))
                               (list (~parens (~parens "function" (~parens (~commas x)) ab)
                                              (~parens (~commas e)))))]
    [(lambda (,x ...) ,ab)   (let ((ab (AnnotatedBody ab)))
                               (~parens "function" (~parens (~commas x)) ab))]
    [(app ,e0 ,e ...)       (let ((f (Expr e0))
                                  (e (map Expr e)))
                              (cond
                                [(identifier? f)
                                 (cond
                                   [(and (eq? (syntax-e f) 'ref))
                                    (match e
                                      [(list e0 e1) (list e0 (~brackets e1))]  ; e0[e1]
                                      [else
                                       (raise-syntax-error 'ref "(ref expr expr) expected, at " f)])]
                                   [(and (eq? (syntax-e f) 'array))
                                    (~brackets (~commas e))]
                                   [(and (infix-operator? f) (= (length e) 1)) ; unary
                                    (~parens f e)]
                                   [(infix-operator? f)
                                    (~parens (add-between e f))]               ; nary
                                   [else
                                    (~parens f (~parens (~commas e)))])]       ; prefix
                                [else
                                 (~parens (~parens f) (~parens (~commas e)))]))])
  (Module U))

;;;
;;; EMITTER
;;;

(define-syntax bit-and (λ (stx) (raise-syntax-error 'bit-and "used out of context" stx)))
(define-syntax bit-or  (λ (stx) (raise-syntax-error 'bit-or  "used out of context" stx)))
(define-syntax bit-xor (λ (stx) (raise-syntax-error 'bit-xor "used out of context" stx)))
(define-syntax bit-not (λ (stx) (raise-syntax-error 'bit-not "used out of context" stx)))

(require syntax/id-table racket/syntax)
(define bound-ids (make-bound-id-table))
(define free-ids  (make-free-id-table))

(define emit-counter 0)

(define (rename-id id)
  (unless (identifier? id)
    (error 'rename-id (~a "expected identifier, got " id)))
  (cond
    [(bound-id-table-ref bound-ids id #f) => identity] ; already renamed
    [(free-id-table-ref free-ids id #f)                ; needs renaming
     (set! emit-counter (+ emit-counter 1))
     (define renamed-id (format-id id "~a_~a" id emit-counter #:source id))
     (bound-id-table-set! bound-ids id renamed-id)
     renamed-id]
    [else                       ; new identifier, no renaming needed
     (free-id-table-set!  free-ids  id #t)
     (bound-id-table-set! bound-ids id id)
     id]))

(define (substitute orig)
  ; TODO: This is a bit ad hoc
  ; TODO: Cache results
  (define substitutions
    (make-hash '(#;("_"."__")
                 ("-"."_")  ("+"."_a") ("*"."_m") ("/"."_q")
                 ("?"."_p") ("!"."_e")
                 (">" . "_g") ("<" . "_l"))))
  (string-join (for/list ([c (~a orig)])
                 (hash-ref substitutions (~a c) (~a c))) ""))

(define (mangle id)
  (unless (identifier? id)
    (error 'mangle (~a "expected identifier, got " id)))
  ; DISABLE (define id* (rename-id id))
  ; (displayln (list "  " 'mangle 'id (syntax-e id) 'id* (syntax-e id*)))
  ; (set! id id*)  
  (syntax-parse id
    #:literals (and or not = bit-and bit-or bit-xor bit-not displayln
                    < > <= >= + - * /)
    ; take care of JavaScript operators first
    [=       "==="]
    [<       "<"]
    [>       ">"]
    [<=      "<="]
    [>=      ">="]
    [+        "+"]
    [-        "-"]
    [*        "*"]
    [/        "/"]
    [and     "&&"]
    [or      "||"]
    [not      "!"]
    [bit-and  "&"]
    [bit-or   "|"]
    [bit-xor  "^"]
    [bit-not  "~"]
    ; handle characters like ? - / etc in identifiers
    [_   (substitute (syntax-e id))]))

;;;
;;;
;;; 

(define (compile u [emit? #t])
  (define t
    (generate-code
     (α-rename
      (annotate-bodies
       (collect
        (parse u))))))
  (if emit? (emit t) t))

(define (expand u)
  (unparse-L (parse u)))

(define (expand+ u)
  (unparse-L1
   (α-rename
    (annotate-bodies
     (collect
      (parse u))))))

;;;
;;; EMIT
;;;

; emit : tree -> void
;   display the elements in the tree in order
(define (emit x)
  (cond
    [(or (number? x)
         (string? x)) (display x)]
    [(symbol? x)      (display x) (display " ")]
    [(identifier? x)  (display (mangle x))]
    [(list? x)        (for-each emit x)]
    [else
     (displayln x)
     (error 'emit "got ~a" x)]))

;;;
;;; RUN
;;;

; In order to run the generated JavaScript program the source
; needs to be saved to a file before a JavaScript engine
; can evaluate the program. Here the JavaScript implementation
; Node is used.

(define (run js-tree [delete-tmp? #f])
  (define tmp (make-temporary-file "tmp~a.js"))
  ; (displayln (path->string tmp))
  (with-output-to-file tmp
    (λ () (emit js-tree))
    #:exists 'replace)
  (for ([line (in-port read-line (open-input-string (node tmp)))])
    (displayln line))
  (when delete-tmp?
    (delete-file tmp)))

(define (node path)
  (with-output-to-string
      (λ ()
        (parameterize ([current-subprocess-custodian-mode 'kill])
          (system (string-append "/usr/local/bin/node " " " (path->string path)))))))

;;;
;;; EVAL
;;;

(define (eval stx)
  (run
   (compile stx #f)))


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
