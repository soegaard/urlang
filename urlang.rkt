#lang racket
;TODO : In the process of introducing Formal (φ) in Definition and Lambda.
;       Done: introducing φ to definition. Handle the x case.
;       TODO  Handle [x e] case.
;       TODO  handle Lambda too.

(provide urlang)
;; Parameters
(provide current-urlang-output-file                      ; overrides module-name as output file
         current-urlang-run?                             ; run after compilation
         current-urlang-echo?                            ; echo JavaScript after compilation
         current-urlang-console.log-module-level-expr?)  ; call console.log on each module-level expr?

;; Keywords
(provide begin block break catch continue define do-while export finally if import
         object label lambda λ let sempty sif throw try urmodule var while :=)
(provide bit-and bit-not bit-or bit-xor ===)
;; Compiler 
(provide compile  ; syntax -> *         prints JavaScript
         eval)    ; syntax -> string    compiles, saves, runs - output returned as string
;; Compiler Phases
(provide parse            ; syntax -> L      parse and expand syntax object into L
         desugar          ; L      -> L-     remove optional arguments
         annotate-module  ; L-     -> L0     annotate module with exports, imports, funs and vars 
         annotate-bodies  ; L0     -> L1     annotate bodies with local variables
         α-rename         ; L1     -> L1     make all variable names unique
         generate-code    ; L1     -> tree   make tree of JavaScript
         emit)            ; tree   -> *      print tree to standard output port
;; Macros 
(provide define-urlang-macro          ; (define-urlang-macro name transformer)
         macro-expansion-context      ; returns of of 'module-level, 'statement, 'expression
         macros-ht) ; internal
;; Syntax Classes
(provide Module ModuleName Export Import
         ModuleLevelForm Statement Expr Definition MacroApplication
         ;; Datums
         Datum Fixnum String Symbol
         ;; Expressions
         Application Assignment Id Let Sequence Ternary
         ;; Statements         
         Block If While DoWhile)
;; Languages
(provide L L- L0 L1
         unparse-L unparse-L- unparse-L0 unparse-L1)

;;;
;;; IDEAS
;;;

; * Source map
; * Improve error when infix operators are invoked with too few arguments (* 3)
; * Static check of identifiers from NodeJS imported modules.
; * Static check: Is the label in (break label) declared as a label?
; * Static check: Number of arguments of assignment operators (+= x 1 2)

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

;; Examples

; The following examples are compiled using the urlang form.
;     (urlang <module> ...)
; The urlang form compiles the modules. The result of compiling
; a module is saved a file whose path is the module-name with .js
; added.

; The urlang form is controlled by the following parameters:

; (current-urlang-run?                           #t)  ; compile and run (using node)
; (current-urlang-echo?                          #t)  ; print JavaScript to screen
; (current-urlang-console.log-module-level-expr? #t)  ; use conole.log to print module-level exprs

;; Example (factorial):

; > (urlang
;     (urmodule fact                                           ; module name
;       (export fact)                                          ; fact is exported
;       (import + - * = displayln ref console)
;       (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
;       (fact 5)))
;
; "use strict";
; function fact(n){return (((n===0)===false)?(n*(fact((n-1)))):1);};
; console.log((fact(5)));
; exports.fact=fact;

; "120\n"

;; Example (cond-macro and array)

;; SYNTAX (cond [e0 e1 e2 ...] ... [else en]), 
;;   like Racket cond except there is no new scope 

; The urlang macro transformer is an standard (phase 0) Racket function.

; (begin
;   (define-urlang-macro cond
;     (λ (stx)   
;       (syntax-parse stx
;         [(_cond [else e0:Expr e:Expr ...])
;          #'(begin e0 e ...)]
;         [(_cond [e0 e1 e2 ...] clause ...)
;          (syntax/loc stx
;            (if e0 (begin e1 e2 ...) (cond clause ...)))]
;         [(_cond)
;          (raise-syntax-error 'cond "expected an else clause" stx)])))
;   (urlang
;    (urmodule sum-example
;       (export)
;       (import + - * % = === < displayln ref console array)
;       (define (even? x) (=== (% x 2) 0))
;       (var (sum 0) x (a (array 1 2 3 4 5)) (i 0) (n a.length))
;       (while (< i n)
;              (:= x (ref a i))
;              (cond
;                [(even? x)  (:= sum (+ sum (ref a i)))]
;                [else       "skip"])
;              (:= i (+ i 1)))
;      sum)))
;
; "use strict";
; function even_p(x){return ((x%2)===0);};
; var sum=0,x,a=[1,2,3,4,5],i=0,n=a.length;
; while((i<n)){(x=a[i]);(((even_p(x))===false)?"skip":(sum=(sum+a[i])));(i=(i+1));};
; console.log(sum);

; "6\n"

;; See more examples in "urlang-tests.rkt".

;;;
;;; INTERNALS
;;;

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

;; Having Urlang represented as syntax objects allow:

;;  * macros (using full Racket at compile time)
;;  * export of defined names
;;  * easier testing


;;;
;;; GRAMMAR
;;;

;; The expander/parser accepts the syntax objects following the
;; the grammar below.

;; In the grammar below:
;;   x stands for a non-keyword identifier
;;   f stands for an identifier defined as a function
;;   l stands for a label (identifier, not a reserved word)

; <module>            ::= (urmodule <module-name> <module-path> <module-level-form> ...)

; <module-level-form> ::= <export> | <import> | <definition> | <statement> 
; <export>            ::= (export x ...)
; <import>            ::= (import x ...)
; <definition>        ::= (define (f <formal> ...) <body>)
;                      |  (define x <expr>)
; <formal>            ::= x | [x <expr>]

; <statement>         ::= <var-decl> | <block> | <while> | <do-while> | <if>
;                      |  <break> | <continue> | <label> | <sempty> | <expr>
; <var-decl>          ::= (var <var-binding> ...)
; <block>             ::= (block <statement> ...)
; <var-binding>       ::= x | (x e)
; <while>             ::= (while <expr> <statement> ...)
; <do-while>          ::= (do-while <expr> <statement> ...)
; <if>                ::= (sif <expr> <statement> <statement>)
; <empty>             ::= (sempty)
; <label>             ::= (label l <statement>)
; <break>             ::= (break)    | (break l)
; <continue>          ::= (continue) | (continue l)
; <try>               ::= (try <block> <catch>)
;                      |  (try <block> <finally>)
;                      |  (try <block> <catch> <finally>)
; <catch>             ::= (catch   x <statement> ...)
; <finally>           ::= (finally <statement> ...)
; <throw>             ::= (throw <expr>)

; <body>              ::= <statement> ... <expr>

; <expr>              ::= <datum>   | <reference> | <application> | <sequence>
;                      |  <ternary> | <assignment> | <let> | <lambda> | <dot> | <object>
; <ternary>           ::= (if <expr> <expr> <expr>)
; <reference>         ::= x
; <application>       ::= (<expr> <expr> ...)
; <sequence>          ::= (begin <expr> ...)
; <assignment>        ::= (:= x <expr>) | (:= x <expr> <expr>)
; <let>               ::= (let ((x <expr>) ...) <statement> ... <expr>)
; <lambda>            ::= (lambda (<formal> ...) <body>)
; <object>            ::= (object (<property-name> <expr>) ...)
; <property-name>     ::= x | <string> | <number>

; <keyword>           ::= define | begin | urmodule | if | := | ...see code...

; <number>            ::= <fixnum> | <flonum>
; <datum>             ::= <fixnum> | <flonum> | <string> | #t | #f

; <identifier>     an identifier that is not a keyword

; <fixnum>         an integer between -2^53 and 2^53
; <flonum>         an inexact real number represented as an IEEE 754 floating point number
; <module-name>    a symbol or string
; <label>          a JavaScript label


;;;
;;; NOTES
;;;

; Some application are special cases:
;    (ref e0 e1)     becomes  e0[e1]
;    (ref e0 "str")  becomes  e0.str    
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

; Other forms follow standard JavaScript sementics.

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

(define-syntax (debug stx) #'(void))

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

(define-syntax block       (λ (stx) (raise-syntax-error 'block       "used out of context" stx)))
(define-syntax break       (λ (stx) (raise-syntax-error 'break       "used out of context" stx)))
(define-syntax catch       (λ (stx) (raise-syntax-error 'catch       "used out of context" stx)))
(define-syntax continue    (λ (stx) (raise-syntax-error 'continue    "used out of context" stx)))
(define-syntax do-while    (λ (stx) (raise-syntax-error 'do-while    "used out of context" stx)))
(define-syntax export      (λ (stx) (raise-syntax-error 'export      "used out of context" stx)))
(define-syntax finally     (λ (stx) (raise-syntax-error 'finally     "used out of context" stx)))
(define-syntax import      (λ (stx) (raise-syntax-error 'import      "used out of context" stx)))
(define-syntax label       (λ (stx) (raise-syntax-error 'label       "used out of context" stx)))
(define-syntax object      (λ (stx) (raise-syntax-error 'object      "used out of context" stx)))
(define-syntax sempty      (λ (stx) (raise-syntax-error 'sempty      "used out of context" stx)))
(define-syntax sif         (λ (stx) (raise-syntax-error 'sif         "used out of context" stx)))
(define-syntax throw       (λ (stx) (raise-syntax-error 'throw       "used out of context" stx)))
(define-syntax try         (λ (stx) (raise-syntax-error 'try         "used out of context" stx)))
(define-syntax urmodule    (λ (stx) (raise-syntax-error 'urmodule    "used out of context" stx)))
(define-syntax var         (λ (stx) (raise-syntax-error 'var         "used out of context" stx)))
(define-syntax while       (λ (stx) (raise-syntax-error 'while       "used out of context" stx)))
(define-syntax :=          (λ (stx) (raise-syntax-error ':=          "used out of context" stx)))

; Note: Rememember to provide all keywords
(define-literal-set keywords (begin block break catch continue define do-while export finally
                                    if import object label lambda λ let
                                    sempty sif throw try urmodule var while :=))
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

(define (id? v)            (identifier? v))
(define (datum? v)         (or (fixnum? v) (flonum? v) (string? v) (boolean? v)))
(define (module-name? v)   (or (symbol? v) (string? v)))
(define (property-name? v) (or (symbol? v) (string? v) (fixnum? v) (flonum? v)
                               (and (syntax? v) (property-name? (syntax-e v)))))
;;;
;;; URLANG AS NANOPASS LANGUAGE
;;;

(define-language L 
  (entry Module)
  (terminals
   ((id          (f x l)) . => . unparse-id)
   ; f function name
   ; l statement label
   ; x identifier
   ((datum       (d))     . => . unparse-datum)
   (module-name  (mn))
   (property-name (pn)))
  (Module (u)
    (urmodule mn m ...))
  (ModuleLevelForm (m)
    (export x ...)
    (import x ...)
    δ σ)
  #;(Definition (δ)
    (define (f x ...) b)          ; function definition
    (define x e))                 ; variable definition
  (Definition (δ)
    (define (f φ ...) b)          ; function definition
    (define x e))
  (Formal (φ)
    x                      ; parameter name
    [x e])                 ; parameter name and default value
  (Body (b)
    (body σ ... e))
  (VarBinding (vb)
    x
    (binding x e) => (x e))
  (CatchFinally (cf)
    (catch   x σ ...)
    (finally   σ ...)
    (catch-finally x (σ ...) (σ0 ...))) ; See Nanopass Issue #40
  (Statement (σ)
    e                             ; expression
    (var vb ...)                  ; variable definition
    (sif e σ1 σ2)                 ; statement if
    (empty)                       ; the empty statement
    (block      σ ...)            ; block (no new scope in JavaScript)
    (while    e σ ...)
    (do-while e σ ...)
    (break)
    (break l)
    (continue)
    (continue l)
    (label l σ)
    (try (σ ...) cf)
    (throw e))
  (Expr (e)
    x                             ; reference
    (app e0 e ...) => (e0 e ...)  ; application
    (:= x e)                      ; assignment    x = e
    (:= x e0 e1)                  ; assignment    x[e0] = e1
    (begin e ...)                 ; sequence
    (if e0 e1 e2)                 ; ternary
    (let ((x e) ...) b)           ; local binding
    (lambda (x ...) b)            ; anonymous function
    (quote d)                     ; quotation (the parser quotes all datums)
    (object (pn e) ...)))         ; object literal


;;;
;;; GRAMMAR AS SYNTAX CLASSES
;;;

; The above grammar as syntax classes.
; Parsing from syntax-object to nanopass structures will use syntax-parse.
; Defining syntax-classes makes the translation easy.

(define-syntax-class Fixnum #:opaque (pattern d #:fail-unless (fixnum? (syntax-e #'d)) #f))
(define-syntax-class Flonum #:opaque (pattern d #:fail-unless (flonum? (syntax-e #'d)) #f))
(define-syntax-class String #:opaque (pattern d #:fail-unless (string? (syntax-e #'d)) #f))
(define-syntax-class Symbol #:opaque (pattern d #:fail-unless (symbol? (syntax-e #'d)) #f))
(define-syntax-class Boolean #:opaque (pattern (~or #t #f)))

(define-syntax-class Number
  #:description "<number>"
  (pattern (~or d:Fixnum d:Flonum)))


(define-syntax-class Datum
  #:description "<datum>"
  (pattern (~or d:Fixnum d:Flonum d:String bool:Boolean)))

(define-syntax-class ModuleName
  #:description "<module-name>"
  (pattern (~or mn:Symbol mn:String)))  

(define-syntax-class Keyword
  #:literal-sets (keywords)
  (pattern x:identifier
           #:fail-unless (keyword? #'x) "keyword"))

(define-syntax-class Id
  (pattern (~and x:id (~not y:Keyword))))

(define-syntax-class PropertyName
  (pattern (~or (~and x:id (~not y:Keyword))
                d:Number
                s:String)))

#;(define-syntax-class Label
   (pattern x:Id))

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
        (define x:Id e:Expr))))

(define-syntax-class Lambda
  #:literal-sets (keywords)
  (pattern (~or (lambda (x ...) body:Body)
                (λ      (x ...) body:Body))))

(define-syntax-class Statement
  #:literal-sets (keywords)
  (pattern (~or m:Expr
                w:While
                v:VarDecl
                β:Block
                dw:DoWhile
                i:If
                b:Break
                c:Continue
                la:Label)))

(define-syntax-class Block
  #:literal-sets (keywords)
  (pattern (block σ:Statement ...)))

(define-syntax-class VarDecl
  #:literal-sets (keywords)
  (pattern (var vb:VarBinding ...)))

(define-syntax-class VarBinding
  #:literal-sets (keywords)
  (pattern (~or x:Id (x:Id (~or e:Expr e:MacroApplication)))))

(define-syntax-class Formal
  #:literal-sets (keywords)
  (pattern (~or x:Id (x:Id e))
           #:attr xe (if (attribute e) #'(x e) #f)))

(define-syntax-class While
  #:literal-sets (keywords)
  (pattern (while e:Expr σ:Statement ...)))

(define-syntax-class Label
  #:literal-sets (keywords)
  (pattern (label x:Id σ:Statement ...)))

(define-syntax-class If
  #:literal-sets (keywords)
  (pattern (sif e:Expr σ1:Statement σ2:Statement)))

(define-syntax-class Let
  #:literal-sets (keywords)
  (pattern (let ((xl:Id el:Expr) ...) b:Body)))

(define-syntax-class DoWhile
  #:literal-sets (keywords)
  (pattern (do-while e:Expr σ:Statement ...)))

(define-syntax-class Break
  #:literal-sets (keywords)
  (pattern (break))
  (pattern (break la:Id)))

(define-syntax-class Continue
  #:literal-sets (keywords)
  (pattern (continue))
  (pattern (continue la:Id)))

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
                el:Lambda
                e:Object)))

(define-syntax-class Object
  #:literal-sets (keywords)
  (pattern (object [pn:PropertyName e] ...)))  ; e is a an expression or macro application

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
                                    "expected one of: 'module-level, 'statement, 'expression ; got ~a"
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
      [(urmodule mn:ModuleName m ...)
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
  (debug (list 'parse-module-level-form (syntax->datum m)))
  (parameterize ([macro-expansion-context 'module-level])
    (with-output-language (L ModuleLevelForm)
      (syntax-parse m
        #:literal-sets (keywords)
        [ma:MacroApplication    (parse-module-level-form (parse-macro-application #'ma))]
        [(~and ex (export . _)) (parse-export #'ex)]
        [(~and im (import . _)) (parse-import #'im)]
        [(~and d (define . _))  (parse-definition #'d)]
        [σ                      (parse-statement #'σ parse-module-level-form 'module-level-form)]))))

(define (parse-statement σ [context-parse parse-statement] [parent-context 'statement])
  (debug (list 'parse-statement (syntax->datum σ)))
  (parameterize ([macro-expansion-context 'statement])
    (with-output-language (L Statement)
      (syntax-parse σ
        #:literal-sets (keywords)
        [ma:MacroApplication      (define expansion (parse-macro-application #'ma))
                                  ; (displayln (list 'parse-statement 'expansion: expansion))
                                  (parameterize ([macro-expansion-context parent-context])
                                    (context-parse expansion))]
        [(~and b  (break    . _)) (parse-break    #'b)]
        [(~and c  (continue . _)) (parse-continue #'c)]
        [(~and la (label    . _)) (parse-label    #'la)]
        [(~and w  (while    . _)) (parse-while    #'w)]
        [(~and dw (do-while . _)) (parse-do-while #'dw)]
        [(~and v  (var      . _)) (parse-var-decl #'v)]
        [(~and β  (block    . _)) (parse-block    #'β)]
        [(~and i  (sif      . _)) (parse-if       #'i)]
        [(~and se (sempty   . _)) (parse-empty    #'se)]
        [(~and tr (try      . _)) (parse-try      #'tr)]
        [(~and th (throw    . _)) (parse-throw    #'th)]
        [e                        (parse-expr     #'e context-parse parent-context)]))))

(define (parse-try tr)
  ; <try>               ::= (try <block> <catch>)
  ;                      |  (try <block> <finally>)
  ;                      |  (try <block> <catch> <finally>)
  (debug (list 'parse-try (syntax->datum tr)))
  (with-output-language (L Statement)
    (syntax-parse tr
      #:literal-sets (keywords)
      [(try (σ ...)
            (catch x:Id cσ ...))   (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                          [cσ (map parse-statement (syntax->list #'(cσ ...)))]
                                          [c  (with-output-language (L CatchFinally)
                                                `(catch ,#'x ,cσ ...))])
                                     `(try (,σ ...) ,c))]
      [(try (σ ...)
            (finally fσ ...))    (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                        [fσ (map parse-statement (syntax->list #'(fσ ...)))]
                                        [f  (with-output-language (L CatchFinally)
                                              `(finally ,fσ ...))])
                                   `(try (,σ ...) ,f))]
      [(try (σ ...)
            (catch x:Id cσ ...)
            (finally fσ ...))    (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                        [cσ (map parse-statement (syntax->list #'(cσ ...)))]
                                        [fσ (map parse-statement (syntax->list #'(fσ ...)))]
                                        [cf (with-output-language (L CatchFinally)
                                              `(catch-finally ,#'x (,cσ ...) (,fσ ...)))])
                                   `(try (,σ ...) ,cf))])))

(define (parse-throw th)
  (debug (list 'parse-throw (syntax->datum th)))
  (with-output-language (L Statement)
    (syntax-parse th
      #:literal-sets (keywords)
      [(throw e) (let ([e (parse-expr #'e)])
                   `(throw ,e))])))

(define (parse-empty se)
  ; the empty statement
  (debug (list 'parse-empty (syntax->datum se)))
  (with-output-language (L Statement)
    (syntax-parse se
      #:literal-sets (keywords)
      [(sempty) `(empty)])))



(define (parse-continue c)
  (debug (list 'parse-continue (syntax->datum c)))
  (with-output-language (L Statement)
    (syntax-parse c
      #:literal-sets (keywords)
      [(continue)       `(continue)]
      [(continue la:Id) `(continue ,#'la)])))

(define (parse-label la)
  (debug (list 'parse-label (syntax->datum la)))
  (with-output-language (L Statement)
    (syntax-parse la
      #:literal-sets (keywords)
      [(label x:Id σ) (let ([σ (parse-statement #'σ)])
                        `(label ,#'x ,σ))])))

(define (parse-break b)
  (debug (list 'parse-break (syntax->datum b)))
  (with-output-language (L Statement)
    (syntax-parse b
      #:literal-sets (keywords)
      [(break)       `(break)]
      [(break la:Id) `(break ,#'la)])))

(define (parse-macro-application ma)
  (debug (list 'parse-macro-application (syntax->datum ma)))
  (syntax-parse ma
    #:literal-sets (keywords)
    [ma:MacroApplication
     (define mark (make-syntax-introducer))
     (let ((transform (attribute ma.transformer)))
       (mark (transform (mark #'ma))))]))

(define (parse-if i)
  (debug (list 'parse-if (syntax->datum i)))
  (with-output-language (L Statement)
    (syntax-parse i
      #:literal-sets (keywords)
      [(sif e σ1 σ2)
       (let ((e  (parse-expr #'e))
             (σ1 (parse-statement #'σ1))
             (σ2 (parse-statement #'σ2)))
         `(sif ,e ,σ1 ,σ2))])))

(define (parse-let l)
  (debug (list 'parse-let (syntax->datum l)))
  (with-output-language (L Expr)
    (syntax-parse l
      #:literal-sets (keywords)
      [(let ((x:Id e) ...) . body)
       (let ([x  (syntax->list #'(x ...))]
             [e (stx-map parse-expr #'(e ...))]
             (b (parse-body #'body)))
       `(let ((,x ,e) ...) ,b))])))

(define (parse-block β)
  (debug (list 'parse-block (syntax->datum β)))
  (with-output-language (L Statement)
    (syntax-parse β
      #:literal-sets (keywords)
      [(block σ ...)
       (let ((σ (stx-map parse-statement #'(σ ...))))
         `(block ,σ ...))])))

(define (parse-var-decl v)
  (debug (list 'parse-var-decl (syntax->datum v)))
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
      [x:Id     #'x]
      [(x:Id e) `(binding ,#'x ,(parse-expr #'e))])))

(define (parse-while w)
  (debug (list 'parse-while (syntax->datum w)))
  (with-output-language (L Statement)
    (syntax-parse w
      #:literal-sets (keywords)
      [(while e σ ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(while ,e ,σ ...))])))

(define (parse-do-while dw)
  (debug (list 'parse-do-while (syntax->datum dw)))
  (with-output-language (L Statement)
    (syntax-parse dw
      #:literal-sets (keywords)
      [(do-while e σ ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(do-while ,e ,σ ...))])))

(define (parse-definition d)
  (debug (list 'parse-definition (syntax->datum d)))
  (with-output-language (L Definition)
    (syntax-parse d
      #:literal-sets (keywords)
      [(define x:Id e)
       (let ((e (parse-expr #'e)))
         `(define ,#'x ,e))]
      [(define (f:Id φ:Formal ...) . b)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ0 ...) #'((sif (= x0 undefined) (:= x0 e0) (block)) ...)])
             (with-syntax ([(σ ... en) #'b])
               (let ((b (parse-body #'(σ0 ... σ ... en))))
                 `(define (,#'f ,x ...) ,b))))))])))

(define (parse-lambda d)
  (debug (list 'parse-lambda (syntax->datum d)))
  (with-output-language (L Expr)
    (syntax-parse d
      #:literal-sets (keywords)
      [(_lambda (φ:Formal ...) . b)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ0 ...) #'((sif (= x0 undefined) (:= x0 e0) (block)) ...)])
             (with-syntax ([(σ ... en) #'b])
               (let ((b (parse-body #'(σ0 ... σ ... en))))
                 `(lambda (,x ...) ,b))))))])))
      
(define (parse-body b)
  (debug (list 'parse-body (syntax->datum b)))
  (with-output-language (L Body)
    (syntax-parse b
      #:literal-sets (keywords)
      [(σ ... e)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(body ,σ ... ,e))])))

(define (parse-expr e [context-parse parse-expr] [parent-context 'expression])
  (debug (list 'parse-expr (syntax->datum e)))
  (parameterize ([macro-expansion-context 'expression])
    (syntax-parse e
      #:literal-sets (keywords)
      [ma:MacroApplication                    (define expansion (parse-macro-application #'ma))
                                              ; (displayln (list 'parse-expr 'expansion: expansion))
                                              (parameterize ([macro-expansion-context parent-context])
                                              (context-parse expansion))]
      [d:Datum                                (parse-datum       #'d)]
      [r:Reference                            (parse-reference   #'r)]
      [(~and s  (begin . _))                  (parse-sequence    #'s)]
      [(~and t  (if    . _))                  (parse-ternary     #'t)]
      [(~and a  (:=    . _))                  (parse-assignment  #'a)]
      [(~and l  (let   . _))                  (parse-let         #'l)]
      [(~and la (~or (lambda . _) (λ . _)))   (parse-lambda      #'la)]
      [(~and o  (object . _))                 (parse-object      #'o)]
      [(~and a  (e ...)
             (~not (k:keyword . _)))          (parse-application #'a)]
      [_ (raise-syntax-error 'parse-expr (~a "expected an expression, got " e) e)])))

(define (parse-object o)
  (debug (list 'parse-object (syntax->datum o)))
  (with-output-language (L Expr)
    (syntax-parse o
      #:literal-sets (keywords)
      [(object [pn:PropertyName e] ...)
       (let ([pn (syntax->list #'(pn ...))]
             [e  (stx-map parse-expr #'(e  ...))])
         `(object [,pn ,e] ...))])))



(define (parse-application a)
  (debug (list 'parse-application (syntax->datum a)))
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(~and (e0 e ...) (~not ma:MacroApplication))
       (let ([e0 (parse-expr #'e0)]
             [e  (stx-map parse-expr #'(e ...))])
         `(app ,e0 ,e ...))])))

(define (parse-reference r)
  (debug (list 'parse-reference (syntax->datum r)))
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
  (debug (list 'parse-sequence (syntax->datum a)))
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(begin e0)       (parse-expr #'e0)]
      [(begin e0 e ...) (let ([e0 (parse-expr #'e0)]
                              [e  (stx-map parse-expr #'(e ...))])
                          `(begin ,e0 ,e ...))])))

(define (parse-assignment a)
  (debug (list 'parse-assignment (syntax->datum a)))
  (with-output-language (L Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(:= x:Id e)
       (let ((e (parse-expr #'e)))
         `(:= ,#'x ,e))]
      [(:= x:Id e0 e1)
       (let ((e0 (parse-expr #'e0))
             (e1 (parse-expr #'e1)))
         `(:= ,#'x ,e0 ,e1))])))

(define (parse-ternary t)
  (debug (list 'parse-ternary (syntax->datum t)))
  (with-output-language (L Expr)
    (syntax-parse t
      #:literal-sets (keywords)
      [(if e0 e1 e2)
       (let ([e0 (parse-expr #'e0)]
             [e1 (parse-expr #'e1)]
             [e2 (parse-expr #'e2)])
         `(if ,e0 ,e1 ,e2))])))

(define (parse-datum d)
  (with-output-language (L Expr)
    (syntax-parse d
      #:literal-sets (keywords)
      [f:Fixnum  `(quote ,(parse-fixnum  #'f))]
      [f:Flonum  `(quote ,(parse-flonum  #'f))]
      [s:String  `(quote ,(parse-string  #'s))]
      [b:boolean `(quote ,(parse-boolean #'b))])))

(define (parse-fixnum f)  (syntax-e f))
(define (parse-flonum f)  (syntax-e f))
(define (parse-string f)  (syntax-e f))
(define (parse-boolean f) (syntax-e f))

;;;
;;; OPERATORS
;;;

;;; Infix

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

;;; Assignment Operators

(define assignment-operators
  '(+= -= *= /= %= **= <<= >>= >>>= &= ^=))

(define assignment-operators-ids (symbols->ids assignment-operators))

(define (assignment-operator? v)
  (or (and (identifier? v) (assignment-operator? (syntax-e v)))
      (memq v assignment-operators)))

(define-syntax-class AssignmentOperator
  #:opaque
  (pattern x:Id
           #:fail-unless (assignment-operator? (syntax-e #'x)) #f))

;;;
;;; PREDEFINED NAMES AND RESERVED WORDS
;;;

(define predefined-names '(Array array array! arguments console ref undefined new typeof))
; (besides operators)

(define reserved-words-ids (symbols->ids ecma6-reservered-keywords))
(define predefined-ids     (symbols->ids predefined-names))


;;;
;;; DESUGARED URLANG
;;;

;; Desugaring rewrites functions with optional arguments to
;; functions without.

(define-language L- (extends L)
  (Definition (δ)
    (- (define (f φ ...) b))
    (+ (define (f x ...) b))
    ; (- (define x e))
    ; (+ (define x e))
    )
  (Formal (φ)
    (- x)
    (- [x e])))

(define-pass desugar : L (U) -> L- ()
  (definitions)
  (Expr       : Expr       (e) ->  Expr ())
  (Statement  : Statement  (σ) ->  Statement ())
  (Definition : Definition (δ) ->  Definition ()
    [(define (,f ,φ* ...) ,b)
     (match (for/list ([phi (in-list φ*)])
              (nanopass-case (L Formal) phi
                [,x      (list x #f)]
                [[,x ,e] (list x (with-output-language (L- Statement)
                                   (let ([e (Expr e)])
                                     `(sif (app = ,x (quote undefined))
                                           (:= ,x ,e)
                                           (empty)))))]))
       [(list (list x s) ...)
        (let ([s (filter identity s)])
          (nanopass-case (L Body) b
            [(body ,σ ... ,e)
             (let ([σ (map Statement σ)] [e (Expr e)])
               `(define (,f ,x ...)
                  (body ,s ...
                        ,σ ...
                        ,e)))]))])]))

;;;
;;; URLANG ANNOTATED MODULE 
;;;

(define-language L0 (extends L-)  ; L-
  (terminals
   (- ((id (f x l)) . => . unparse-id))
   (+ ((id (f x l)) . => . unparse-id)))
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
   (- ((id (f x l)) . => . unparse-id))
   (+ ((id (f x l)) . => . unparse-id)))
  (Body (b)
    (- (body σ ... e)))
  (AnnotatedBody (ab)
    (+ (annotated-body (x ...) σ ... e)))
  (Definition (δ)
    (- (define (f x ...) b))
    (+ (define (f x ...) ab))
    ;(- (define (f φ ...) b))
    ;(+ (define (f φ ...) ab))
    )
  (Expr (e)
    (- (let ((x e) ...) b))
    (+ (let ((x e) ...) ab))
    (- (lambda (x ...) b))
    (+ (lambda (x ...) ab))))

;;;
;;; ANNOTATE MODULE
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

(define-pass annotate-module : L- (U) -> L0 ()
  (definitions
    (define-free-table  export)
    (define-free-table  import)
    (define-free-table  fun)
    (define-bound-table var)
    (define-free-table  operator)   ; operators 
    (define-free-table  reserved)   ; reserved words
    (define-free-table  predefined) ; predefined names

    (define context      (make-parameter 'module-level))
    (define remove-form  (list 'remove-form))       ; used to remove im- and export forms
    (define (keep? v)    (not (eq? v remove-form)))
    (define (complain msg where) (raise-syntax-error 'annotate-module msg where)))
  (Module : Module (u) ->  Module ()
    [(urmodule ,mn ,[m] ...)
     ;; register predefined names, reserved words, and, operators
     (for ([op infix-operators-ids])       (operator!   op))
     (for ([op assignment-operators-ids])  (operator!   op))
     (for ([op reserved-words-ids])        (reserved!   op))
     (for ([op predefined-ids])            (predefined! op))
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
  ;; Register variables declared with var only, when var is a module-level form.
  ;; All contexts where var-forms can appear need to change the context.
  ;; I.e. function bodies, let and lambda needs to set the context.
  (VarBinding : VarBinding (vb) -> VarBinding ()
    [,x                  (when (eq? (context) 'module-level)
                           (var! x))
                         vb]
    [(binding ,x ,[e])  (when (eq? (context) 'module-level)
                          (var! x))
                        `(binding ,x ,e)])
  (Expr : Expr (δ) -> Expr ()
    ; let and lambda have bodies, so they need special attention
    [(let ((,x ,e) ...) ,b)   (let ([e (parameterize ([context 'rhs])  (map Expr e))]
                                    [b (parameterize ([context 'body]) (Body b))])
                                `(let ((,x ,e) ...) ,b))]                                
    [(lambda (,x ...) ,b)     (let ([b (parameterize ([context 'body]) (Body b))])
                                `(lambda (,x ...) ,b))])
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
;;; α-RENAMING
;;;

; All variables are renamed such that each binding has a unique name.
; This includes macro introduced variables.

(define counter 0)
(define joiner "_")
(define (new-var [prefix "t"])
  (set! counter (+ counter 1))
  (define pre (if (syntax?   prefix) (syntax-e prefix) prefix))
  (datum->syntax #'here (string->symbol (~a pre joiner counter))))
(define (reset-counter! [new-joiner "_"])
  (set! counter 0)
  (set! joiner new-joiner))

(define-pass α-rename : L1 (U) -> L1 ()
  (definitions
    (define-free-table global)  ; free references refer to module-level-defined variables
    (define-bound-table var)    ; references to variables declared with var can be macro introduced
    (define (initial-ρ x [id= #f])
      (cond
        [(var? x) => identity]  ; get (potentially renamed x)
        [(global? x) x]         ; exports, imports, funs are never renamed
        [else        #f]))      ; no other variables are bound
    (define (fid= x y)      (free-identifier=? x y))
    (define (bid= x y)      (bound-identifier=? x y))
    (define (extend ρ original renamed)
      (λ (x id=) (if (id= x original) renamed (ρ x id=))))
    (define (extend* ρ xs) (for/fold ((ρ ρ)) ((x xs)) (extend ρ x x)))
    (define (Statement* σs ρ) (map (λ (σ) (Statement σ ρ)) σs)) ; use same ρ on all statements
    (define (fresh x ρ [orig-x x]) (if (ρ x fid=) (fresh (new-var x) ρ x) x))
    (define (fresh-var x) (if (global? x) (fresh-var (new-var x)) x))
    (define (rename x ρ) (define x* (fresh x ρ)) (values x* (extend ρ x x*)))
    (define (rename* xs ρ) (map2* rename xs ρ))
    (define (lookup x ρ [on-not-found (λ (_) #f)])
      (match (ρ x bid=) [#f (match (ρ x fid=) [#f (on-not-found x)] [y y])] [y y]))
    (define (unbound-error x) (raise-syntax-error 'α-rename "unbound variable" x))
    (define pre-body-ρ (make-parameter #f)))
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
       (parameterize ([pre-body-ρ ρ])
       ; Note: Macros can introduce global variable bindings
       ;       The statement (block (var x) ...) introduces x as a module-level variable.
       ;       This means that module-level variables may have to be renamed.
       ; Note: Macros can't introduce functions, imports or exports (yet).
       ;       No renaming needed for them (yet).
       ; TODO TODO rename vars from x to y in annotations
       (let ((m (map (λ (m) (ModuleLevelForm m ρ)) m)))
         `(urmodule ,mn (,an ...) ,m ...))))])
  (ModuleLevelForm : ModuleLevelForm (m ρ) -> ModuleLevelForm ()
    [,δ (Definition δ ρ)]
    [,σ (Statement  σ ρ)])
  (Definition : Definition  (δ ρ)  ->  Definition  ()
    ; module-level-definitions aren't renamed
    [(define ,x ,e)              (let ((ρ (extend ρ x x))) ; map x to x
                                   (let ((e (Expr e ρ)))
                                     `(define ,x ,e)))]    
    [(define (,f ,x ...) ,ab)    (letv ((x ρ) (rename* x ρ))
                                   (let ([ab (AnnotatedBody ab ρ)])
                                     `(define (,f ,x ...) ,ab)))])
  ; TODO: Change var to use same scope rules as let*.
  ;       In (var [s s]) the second s refers to an outer scope.
  (VarBinding : VarBinding (vb ρ) -> VarBinding ()
    [,x                             (ρ x bid=)]
    [(binding ,x ,[e])   `(binding ,(ρ x bid=) ,e)])
  (AnnotatedBody : AnnotatedBody (b ρ) -> AnnotatedBody ()
    [(annotated-body (,x ...) ,σ ... ,e)
     (parameterize ([pre-body-ρ ρ]) ; need to rename (var ...)
       (letv ((y ρ) (rename* x ρ))  ; extend and rename       ;  NOTE: This means that (var ...) 
         (let ((σ (Statement* σ ρ)) (e (Expr e ρ)))           ;        should not rename again
           `(annotated-body (,y ...) ,σ ... ,e))))])
  (Statement : Statement (σ ρ) -> Statement ()
    [(var ,vb* ...)
     ; Note: At this point the variable x ... have been renamed by AnnotatedBody.
     ;       This means that (ρ x) will return the renamed variable.
     ;       Here we need to handle the right hand sides of [x e] since x is not
     ;       in the scope of e. To get the environment without x ... the parameter pre-body-ρ
     ;       is used.
     ; Note: Since each (var ...) adds variables to the scope, we need to update
     ;       pre-body-ρ so the variables are in scope in the following statements.
     (letv ((vb ρ) (for/fold ([vbs '()] [ρ* (pre-body-ρ)]) ([vb vb*])
                     (nanopass-case (L1 VarBinding) vb
                       [,x                   (let ([x* (ρ x bid=)])
                                               (let ([ρ* (extend ρ* x x*)]) ; already renamed in ρ
                                                 (values (cons x* vbs) ρ*)))]
                       [(binding ,x ,e)   (let ([e (Expr e ρ*)])
                                            (let ([x* (ρ x bid=)])
                                            (let ([ρ* (extend ρ* x x*)])  ; renamed in ρ
                                                (with-output-language (L1 VarBinding)
                                                  (values (cons `(binding ,x* ,e) vbs)
                                                          ρ*)))))])))
       (pre-body-ρ ρ)
       `(var ,(reverse vb) ...))])
  (CatchFinally : CatchFinally (cf ρ) -> CatchFinally ()
    [(catch ,x ,σ ...)                     (letv ((y ρ) (rename x ρ))
                                             (let ((σ (Statement* σ ρ)))
                                               `(catch ,y ,σ ...)))]
    [(finally ,σ ...)                      (let ((σ (Statement* σ ρ)))
                                             `(finally ,σ ...))]
    [(catch-finally ,x (,σ ...) (,σ0 ...)) (letv ((y ρ) (rename x ρ))
                                             (let* ((σ  (Statement* σ  ρ))
                                                    (σ0 (Statement* σ0 ρ)))
                                               `(catch-finally ,x (,σ ...) (,σ0 ...))))])
                            
  ; Expression never change the environment, so only a single return value
  (Expr : Expr (e ρ) -> Expr ()
    ; all expressions that contain an id (x or f) needs consideration
    [,x                         (lookup x ρ unbound-error)]
    [(:= ,x ,[e])               (let ((y (lookup x ρ unbound-error))) `(:= ,y ,e))]
    [(let ((,x ,[e]) ...) ,ab)  (letv ((x ρ) (rename* x ρ))  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(let ((,x ,e) ...) ,ab)))]
    [(lambda (,x ...) ,ab)      (letv ((x ρ) (rename* x ρ))  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(lambda (,x ...) ,ab)))])
  (Module U))

;;;
;;; CODE GENERATION
;;;

(define-pass generate-code : L1 (U) -> * ()
  (definitions
    (define (~parens . t)      (list "(" t ")"))
    (define (~braces . t)      (list "{" t "}"))
    (define (~brackets . t)    (list "[" t "]"))
    (define (~commas ts)       (add-between ts ","))
    (define (~newline t)       (list t "\n"))
    (define (~semi t)          (list t ";"))
    (define (~Statement . t)   (list t ";"))
    (define (~Return t)        (~Statement "return " t))
    (define (~displayln t)     (list "console.log" (~parens t)))
    (define (~top-expr t)      (if (current-urlang-console.log-module-level-expr?)
                                   (~displayln t) t))
    (define (~string t)        (list "\"" t "\""))
    (define (~property-name t) (define v (syntax-e t)) (if (string? v) (~string v) v))
      
    (define (exports.id x)   (format-id x "exports.~a" x)))
  (Module : Module (u) -> * ()
    [(urmodule ,mn (,an ...) ,m ...) (list (~newline (~Statement "\"use strict\""))
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
    [,e (~newline (~Statement (~top-expr (Expr e))))]
    [,σ (~newline (Statement σ))])    
  (Definition : Definition (δ) -> * ()
    [(define ,x ,e)            (let ([e (Expr e)])
                                 (~Statement `(var ,x "=" ,e)))]
    [(define (,f ,x ...) ,ab)  (let ()
                                 #;(define (formal->x φ)
                                     (nanopass-case (L1 Formal) φ
                                       [,x x] [(,x ,e) x]))
                                 (let ((ab (AnnotatedBody ab))
                                       #;[x  (map formal->x φ)])
                                   (~Statement `(function ,f ,(~parens (~commas x))
                                                          ,ab))))])
  (CatchFinally : CatchFinally (cf) -> * ()
    [(catch ,x ,σ ...)                      (let ([σ (map Statement σ)])
                                              (list "catch" (~parens x) (~braces σ)))]
    [(finally  ,σ ...)                      (let ([σ (map Statement σ)])
                                              (list "finally"  (~braces σ)))]
    [(catch-finally ,x (,σ ...) (,σ0 ...))  (let ([σ  (map Statement σ)]
                                                  [σ0 (map Statement σ0)])
                                              (list "catch"   (~parens x) (~braces σ)
                                                    "finally"             (~braces σ0)))])
  (Statement : Statement (σ) -> * ()
    [,e                   (~Statement (Expr e))]
    [(block ,σ ...)       (let ((σ (map Statement σ)))
                            (~braces σ))]
    [(sif ,e ,σ1 (block)) (let ((e (Expr e)) (σ1 (Statement σ1)))
                            (list "if" (~parens "!" (~parens e "===false")) σ1))]
    [(sif ,e ,σ1 ,σ2)     (let ((e (Expr e)) (σ1 (Statement σ1)) (σ2 (Statement σ2)))
                            (~Statement "if" (~parens "!" (~parens e "===false")) σ1 "else " σ2))]
    [(empty)              (~Statement)]
    [(while ,e ,σ ...)    (let ((e (Expr e)) (σ (map Statement σ)))
                            (~Statement "while" (~parens e) (~braces σ)))]
    [(do-while ,e ,σ ...) (let ((e (Expr e)) (σ (map Statement σ)))
                            (~Statement "do" (~braces σ) "while" (~parens e)))]
    [(break)              (~Statement "break")]
    [(break ,l)           (~Statement "break" " " l)]
    [(continue)           (~Statement "continue")]
    [(continue ,l)        (~Statement "continue" " " l)]
    [(label ,l ,σ)        (~Statement l ":" (Statement σ))]
    [(try (,σ ...) ,cf)   (let ((σ (map Statement σ)))
                            (~Statement "try " (~braces σ) (CatchFinally cf)))]
    [(throw ,e)           (let ((e (Expr e)))
                            (~Statement "throw" " " e))]
    [(var ,vb ...)        (match (map VarBinding vb)
                            [(list) (~Statement)]            ; no bindings => empty statement
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
                              [(number? d)  (match d
                                              [+inf.0    "Infinity"]
                                              [-inf.0 "(-Infinity)"]
                                              [+nan.0         "NaN"]
                                              [else               d])]
                              [else (error 'generate-code "expedcted datum, got ~a" d)])]
    [(if ,e0 ,e1 ,e2)       (let ((e0 (Expr e0)) (e1 (Expr e1)) (e2 (Expr e2)))
                              (~parens (~parens e0 "===false") "?" e2 ":" e1))]
    #;[(if ,e0 ,e1 ,e2)       (let ((e0 (Expr e0)) (e1 (Expr e1)) (e2 (Expr e2)))
                              (~parens e0 "?" e1 ":" e2))]
    [(begin ,e ...)         (let ((e (map Expr e)))
                              (~parens (~commas e)))]
    [(:= ,x ,e)             (let ((e (Expr e)))
                              (~parens x "=" e))]
    [(:= ,x ,e0 ,e1)        (let ((e0 (Expr e0)) (e1 (Expr e1)))
                              (~parens x (~brackets e0) "=" e1))]
    [(let ((,x ,e) ...) ,ab) (let ((e (map Expr e)) (ab (AnnotatedBody ab)))
                               (list (~parens (~parens "function" (~parens (~commas x)) ab)
                                              (~parens (~commas e)))))]
    [(lambda (,x ...) ,ab)   (let ((ab (AnnotatedBody ab)))
                               (~parens "function" (~parens (~commas x)) ab))]
    [(object (,pn* ,e*) ...) (~braces (~commas (for/list ([pn pn*] [e e*])
                                                 (list (~property-name pn) ":" (Expr e)))))]
    [(app ,e0 ,e ...)      (cond
                             [(identifier? e0)
                              (define f e0)
                              (define (infix? _)      (infix-operator? f))
                              (define (assignment? _) (assignment-operator? f))
                              (define (pn? _)
                                (nanopass-case (L1 Expr) (second e)
                                  [(quote ,d) (and (property-name? d) (not (number? d)) d)]
                                  [else       #f]))
                              (match (cons (syntax-e f) (map Expr e))
                                [(list 'ref e1 (and (? pn?)
                                                    (app pn? pn)))    (if (identifier? e1)
                                                                          (~a (mangle e1) "." pn)
                                                                              (list e1 "." (~a pn)))]
                                [(list 'ref e1 (and (? pn?) (app pn? pn)))    (list e1 "." (~a pn))]
                                [(list 'ref e1 e2)                          (list e1 (~brackets e2))]
                                [(list 'array e ...)                        (~brackets (~commas e))]
                                [(list 'array e ...)                        (~brackets (~commas e))]
                                [(list 'new    e0 e ...)    (list "new " e0 (~parens   (~commas e)))]
                                [(list 'array! e0 i e)             (list e0 (~brackets i) "=" e)]
                                [(list* 'ref _)
                                 (raise-syntax-error 'ref "(ref expr expr) expected, at " e0)]
                                [(list (? infix?) e1)    (~parens f e1)]                      ; unary
                                [(list (? infix?) e ...) (~parens (add-between e f))]         ; nary
                                [(list (? assignment?) e0 e1)  (~parens e0 f e1)]             ; assign
                                [(list _ e ...)          (~parens f (~parens (~commas e)))])] ; prefix
                             [else ; expression in front
                              (let ((e0 (Expr e0)) (e (map Expr e)))
                                ; improve output, don't wrap e0 in parentheses if possible
                                (define f (if (string? e0) e0 (~parens e0)))
                                (~parens f (~parens (~commas e))))])])
  (Module U))

;;;
;;; EMITTER
;;;

(define-syntax bit-and (λ (stx) (raise-syntax-error 'bit-and "used out of context" stx)))
(define-syntax bit-or  (λ (stx) (raise-syntax-error 'bit-or  "used out of context" stx)))
(define-syntax bit-xor (λ (stx) (raise-syntax-error 'bit-xor "used out of context" stx)))
(define-syntax bit-not (λ (stx) (raise-syntax-error 'bit-not "used out of context" stx)))
(define-syntax ===     (λ (stx) (raise-syntax-error '===     "used out of context" stx)))

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
                 ("=" . "_eq") (">" . "_g") ("<" . "_l"))))
  (string-join (for/list ([c (~a orig)])
                 (hash-ref substitutions (~a c) (~a c))) ""))

(define (mangle id)
  (unless (identifier? id)
    (error 'mangle (~a "expected identifier, got " id)))
  ; DISABLE (define id* (rename-id id))
  ; (displayln (list "  " 'mangle 'id (syntax-e id) 'id* (syntax-e id*)))
  ; (set! id id*)  
  (syntax-parse id
    #:literals (and or not = === bit-and bit-or bit-xor bit-not < > <= >= + - * /)
    ;; Take care of JavaScript operators first
    ;;  - assignment operators
    [ao:AssignmentOperator (symbol->string (syntax-e #'ao))]
    ;;  - infix operators
    [=       "==="]
    [===     "==="]
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
       (annotate-module
        (desugar
         (parse u)))))))
  (if emit? (emit t) t))

(define (expand u)
  (unparse-L (parse u)))

(define (expand+ u)
  (unparse-L1
   (α-rename
    (annotate-bodies
     (annotate-module
      (desugar
       (parse u)))))))

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

(define (run js-tree [delete-tmp? #t])
  (define tmp (make-temporary-file "tmp~a.js"))
  ; (displayln (path->string tmp))
  (with-output-to-file tmp
    (λ () (emit js-tree))
    #:exists 'replace)
  (for ([line (in-port read-line (open-input-string (node/break tmp)))])
    (displayln line))
  (when delete-tmp?
    (delete-file tmp)))

(define (node/break path)
  (define me            (current-thread))
  (define cust          (make-custodian))
  (define (kill)        (custodian-shutdown-all cust))
  (define (on-break _)  (kill) "\"break: node process was killed\"")
  (parameterize ([current-subprocess-custodian-mode 'kill]
                 [subprocess-group-enabled          #t]
                 [current-custodian                 cust])
    (with-handlers ([exn:break? on-break])
      (thread (λ() (thread-send me (node path))))
      (thread-receive))))

(define (node path)
  (with-output-to-string
      (λ ()
        (define p (if (string? path) path (path->string path)))
        (parameterize ([current-subprocess-custodian-mode 'kill])
          (system (string-append "/usr/local/bin/node " " " p))))))

;;;
;;; EVAL
;;;

(define (eval stx)
  (run
   (compile stx #f)))

;;;
;;; COMPILATION
;;;

(define current-urlang-output-file                    (make-parameter #f))
(define current-urlang-run?                           (make-parameter #f))
(define current-urlang-echo?                          (make-parameter #f))
(define current-urlang-console.log-module-level-expr? (make-parameter #f))

(define (urmodule-name->file-name name)
  (match name
    [(? symbol? s) (~a s ".js")]
    [(? string? s) s]
    [_ (error 'urmodule-name->file-name
              "Internal error: expected symbol or string")]))

(define-syntax (urlang stx)
  (define-syntax-class String
    #:opaque (pattern d #:fail-unless (string? (syntax-e #'d)) #f))
  (define-syntax-class Symbol
    #:opaque (pattern d #:fail-unless (symbol? (syntax-e #'d)) #f))
  (define-syntax-class ModuleName
    #:description "<module-name>" (pattern (~or mn:Symbol mn:String)))
  (syntax-parse stx
    #:literals (urmodule)
    [(_urlang (~and urmod (urmodule mn:ModuleName . _)) ...)
     (syntax/loc stx
       (begin
         (let ()
           (define name (syntax-e #'mn))
           (define path (or (current-urlang-output-file)      ; parameter can override module name
                            (urmodule-name->file-name name)))
           (define tree (compile #'urmod #f)) ; #f = don't emit
           (parameterize ([current-urlang-output-file path])
             (with-output-to-file path
               (λ () (emit tree))
               #:exists 'replace))
           (when (current-urlang-echo?)
             (with-input-from-file path
               (λ() (copy-port (current-input-port) (current-output-port)))))
           (when (current-urlang-run?)
             (node/break path)))
         ...))]))
