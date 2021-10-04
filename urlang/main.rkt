#lang racket
(require syntax/strip-context)
;;; TODO    Fix scope of class names => make class declaration module-top-level only ?
;;; TODO  0. put this and super into scope in a class declaration
;;; TODO  1. Is the uncommented (not (keyword? ..))) correct in PropertyName
;;; TODO  2. Write more documentation.

;;;
;;; INTRODUCTION
;;;

; This file contains the Urlang module to JavaScript file compiler.

;;;
;;; INSTALLATION
;;;

;;; [Optional] Node:         Install node and npm.
;;; [Optional] Babel:        sudo npm --global install babel-cli babel-preset-es2015
;;; [Optional] js-beautify   npm -g install js-beautify

;;;
;;; IMPLEMENTATION
;;;


(require "globals.rkt") ; 

(require (only-in racket/unsafe/ops unsafe-fx< unsafe-fx> unsafe-fx+ unsafe-fx- unsafe-fx*)
         (only-in racket/base in-range))


(provide urlang)
;; Parameters
(provide current-urlang-output-file                     ; overrides module-name as output file
         current-urlang-exports-file                    ; overrides name of file to store exports
         current-urlang-run?                            ; run (using node) after compilation
         current-urlang-echo?                           ; echo JavaScript after compilation
         current-urlang-console.log-module-level-expr?  ; call console.log on each module-level expr?
         current-urlang-delete-tmp-file?
         current-urlang-beautify?                       ; process output with js-beautify ?
         current-urlang-babel?                          ; process output with babel ?
         current-urlang-rollup?                         ; generate code suitable for rollup

         current-use-arrows-for-lambda?                 ; use ES6 arrows for lambdas
         )


;; Keywords
(provide keywords) ; a literal set
(provide all-as array as begin block break catch class continue define default do-while dot export
         finally if import import-from
         label lambda λ let new object ref return require sempty sif spread
         throw topblock try urmodule var while :=)
; Keywords from ES6
(provide const let-decl)
; Keywrods from ES7
(provide await)
; Urlang Keywords
(provide define/async) ; ES7 

(provide bit-and bit-not bit-or bit-xor === !==)
;; Compiler 
(provide compile          ; syntax -> *         prints JavaScript
         eval)            ; syntax -> string    compiles, saves, runs
;                                                                - output returned as string
;; Compiler Phases
;;   The types L, L-, L0 and L1 below refer to nanopass languages. I.e. they are structures
;;   representing programs. A tree is represented as nested lists.
(provide parse            ; syntax -> L      parse and expand syntax object into L
         flatten-topblock ; L      -> L      remove topblock (in Racket this is top-level-begin)
         desugar          ; L      -> L-     remove optional arguments
         annotate-module  ; L-     -> L0     annotate module with exports, imports, funs and vars 
         annotate-bodies  ; L0     -> L1     annotate bodies with local variables
         α-rename         ; L1     -> L1     make all variable names unique
         generate-code    ; L1     -> tree   make tree of JavaScript
         emit)            ; tree   -> *      print tree to standard output port
;; Macros 
(provide define-urlang-macro          ; (define-urlang-macro name transformer)
         macro-expansion-context      ; returns one of 'module-level, 'statement, 'expression
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
(provide         Lur         L-         L0         L1
         unparse-Lur unparse-L- unparse-L0 unparse-L1)
;; Datums
(provide fixnum?)
;; Modules
(provide urmodule-name->exports
         mangle)


;;;
;;; IDEAS
;;;

; * Source map
; * Improve error when infix operators are invoked with too few arguments (* 3)
; * Static check of identifiers from NodeJS imported modules.
; * Static check: Is the label in (break label) declared as a label?
; * Static check: Number of arguments of assignment operators (+= x 1 2)
; * Static check: Duplicate declarations of variabels in let and const declarations.

;;;
;;; URLANG
;;;

;; Urlang is a language designed to allow straightforward translation to JavaScript.
;; Think of Urlang as JavaScript with sane syntax and JavaScript semantics.
;; JavaScript in this context is short for ECMAScript 5 in strict mode.
;; Update: Bit by bit we will extend Urlang to handle ES6 as well

;; Although the constructs of Urlang and JavaScript map almost one-to-one,
;; a little sugar was added:
;;   * function definitions allow default arguments
;;   * let expressions

;; Even though the syntax of Urlang is Racket-like, remember that the
;; semantics is standard JavaScript. This means in particular that tail calls
;; build context (unless the JavaScript engine supports TCO/PTC).

;; Examples

; The following examples are compiled using the urlang form.
;     (urlang <module> ...)
; The urlang form compiles the modules. The result of compiling
; a module is saved to a file whose path is the module-name with .js
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
;; compiler framework. The compiler is exported as a function
;;     compile : urlang-module -> JavaScript
;; that compiles an urlang module and produces JavaScript,
;; that can be evaluated by the Node.js platform (or be embedded in a web page).

;; The Urlang module to be compiled can be represented 
;;    1) as a syntax object
;;    2) as a Nanopass structure (representing an Lurlang program)

;; Use 1) to program in Urlang directly.
;; Use 2) if you intend to use Urlang as a compiler backend.
;; [Note: Nanopass is a framework for implementing compilers.]

;; The intended use of Urlang is to use 1) write (generate) a Racket runtime in JavaScript.
;; The middle-end of the Racket-to-JavaScript compiler will produce output as Nanopass
;; structures, so 2) will be used as the backend for the Racket-to-JavaScript compiler.

;; Internally the function expand
;;     expand : syntax -> LUrlang
;; will parse and expand its input and produce an LUrlang representation.

;; Note that `expand` allows the user to extend the input language
;; using define-urlang-macro. An Urlang macro is a syntax to syntax
;; transformation implemented as a normal Racket function.
;; This allows you to use all of the standard Racket macro machinery.

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

;; The expander/parser accepts syntax objects following the the grammar below.

;; In the grammar below:
;;   x stands for a non-keyword identifier
;;   f stands for an identifier defined as a function
;;   l stands for a label (identifier, not a reserved word)

; <module>            ::= (urmodule <module-name> <module-path> <module-level-form> ...)

; <module-level-form> ::= <export> | <import> | <import-from> | <require>
;                      |  <definition> | <statement> | <topblock>
; <export>            ::= (export x ...)
; <import>            ::= (import x ...)
; <import-from>       ::= (import-from <string-literal> <import-spec> ...)
; <import-spec>       ::= x | (as x x) | (all-as x) | (default x)
; <require>           ::= (require <require-spec> ...)
; <topblock>          ::= (topblock <module-level-form> ...)
; <definition>        ::= (define       (f <formal> ...) <body>)
;                      |  (define/async (f <formal> ...) <body>)
;                      |  (define x <expr>)
; <formal>            ::= x | [x <expr>]
; <require-spec>      ::= <module-name>

; <statement>         ::= <var-decl> | <let-decl> | <const-decl> | <block> | <while> | <do-while> 
;                      | <if> | <break> | <continue> | <return> | <label> | <sempty> | <expr>
; <var-decl>          ::= (var   <var-binding> ...)
; <let-decl>          ::= (let-decl   <var-binding> ...)
; <const-decl>        ::= (const <var-binding> ...)
; <var-binding>       ::= x | (x e)
; <block>             ::= (block <statement> ...)
; <while>             ::= (while <expr> <statement> ...)
; <do-while>          ::= (do-while <expr> <statement> ...)
; <if>                ::= (sif <expr> <statement> <statement>)
; <empty>             ::= (sempty)
; <label>             ::= (label l <statement>)
; <break>             ::= (break)    | (break l)
; <continue>          ::= (continue) | (continue l)
; <return>            ::= (return)   | (return e)
; <try>               ::= (try (<statement> ...) <catch>)
;                      |  (try (<statement> ...) <finally>)
;                      |  (try (<statement> ...) <catch> <finally>)
; <catch>             ::= (catch   x <statement> ...)
; <finally>           ::= (finally <statement> ...)
; <throw>             ::= (throw <expr>)

; <body>              ::= <statement> ... <expr>

; <expr>              ::= <datum>   | <reference> | <application> | <sequence>
;                      |  <ternary> | <assignment> | <let> | <lambda> | <await> | <dot> | <object>
;                      |  <array-reference> | <array> | <class> | <spread>
; <ternary>           ::= (if <expr> <expr> <expr>)
; <reference>         ::= x
; <application>       ::= (<expr> <expr> ...)
; <sequence>          ::= (begin <expr> ...)
; <assignment>        ::= (:= x <expr>) | (:= x <expr> <expr>) | (:= (dot <expr> <property-name> ...) <expr>)
; Note: If x in (:= x <expr>) contains periods, it will be parse as  (:= (dot parts-of-x ...) <expr>)
; <let>               ::= (let ((x <expr>) ...) <statement> ... <expr>)
; <lambda>            ::= (lambda (<formal> ...) <body>)
; <async-lambda>      ::= (async <lambda>)
; <await>             ::= (await <expr>) ; ES7
; <array-reference>   ::= (ref <expr> <expr> <expr> ...)
; <array>             ::= (array <expr> ...)
; <dot>               ::= (dot <expr> ...+)
; <new>               ::= (new <constructor-id> <expr> ...)
; <object>            ::= (object (<property-name> <expr>) ...)
; <class>             ::= (class <class-heritage> ((<property-name> x ...) <body>)) ...)
; <spread>            ::= (spread <expr>) ; only in applications
; <class-heritage>    ::= x | (x x)
; <property-name>     ::= x | <keyword> | <string> | <number>

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

;    (ref e0 e1)     becomes  e0[e1]
;    (ref e0 "str")  becomes  e0.str    
;    (array e ...)   becomes  [e,...]

;    (dot e ...)     is property access / chained method calls

; TODO: Fix the comment below.
; Property access with identifier dot notation is rewritten to use bracket syntax
; in the parser.
; Example:  object.property becomes object["property"]
; Note: keywords are allowed as property names pr ES5.


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
  ; syntax: (receive1 e ...)
  ;   evaluate e ... return the first value returned by the last expression.
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
; (define-syntax (debug stx) (syntax-parse stx [(_ e) #'(displayln e)]))

(require (for-syntax racket/syntax))

; tables of symbols
(require "symbol-table.rkt")

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

; Note: After adding a new keyword, remember to update the literal set  keywords  below the
;       the keyword definitions.
;       Also: Remember to provide them!

(define-syntax array         (λ (stx) (raise-syntax-error 'array       "used out of context" stx)))
(define-syntax as            (λ (stx) (raise-syntax-error 'as          "used out of context" stx)))
(define-syntax all-as        (λ (stx) (raise-syntax-error 'all-as      "used out of context" stx)))
(define-syntax block         (λ (stx) (raise-syntax-error 'block       "used out of context" stx)))
(define-syntax break         (λ (stx) (raise-syntax-error 'break       "used out of context" stx)))
(define-syntax catch         (λ (stx) (raise-syntax-error 'catch       "used out of context" stx)))
(define-syntax class         (λ (stx) (raise-syntax-error 'class       "used out of context" stx)))
(define-syntax continue      (λ (stx) (raise-syntax-error 'continue    "used out of context" stx)))
(define-syntax default       (λ (stx) (raise-syntax-error 'default     "used out of context" stx)))
(define-syntax dot           (λ (stx) (raise-syntax-error 'dot         "used out of context" stx)))
(define-syntax do-while      (λ (stx) (raise-syntax-error 'do-while    "used out of context" stx)))
(define-syntax export        (λ (stx) (raise-syntax-error 'export      "used out of context" stx)))
(define-syntax finally       (λ (stx) (raise-syntax-error 'finally     "used out of context" stx)))
(define-syntax import        (λ (stx) (raise-syntax-error 'import      "used out of context" stx)))
(define-syntax import-from   (λ (stx) (raise-syntax-error 'import-from "used out of context" stx)))
(define-syntax label         (λ (stx) (raise-syntax-error 'label       "used out of context" stx)))
(define-syntax object        (λ (stx) (raise-syntax-error 'object      "used out of context" stx)))
(define-syntax new           (λ (stx) (raise-syntax-error 'new         "used out of context" stx)))
(define-syntax ref           (λ (stx) (raise-syntax-error 'ref         "used out of context" stx)))
(define-syntax return        (λ (stx) (raise-syntax-error 'return      "used out of context" stx)))
(define-syntax sempty        (λ (stx) (raise-syntax-error 'sempty      "used out of context" stx)))
(define-syntax sif           (λ (stx) (raise-syntax-error 'sif         "used out of context" stx)))
(define-syntax spread        (λ (stx) (raise-syntax-error 'spread      "used out of context" stx)))
(define-syntax throw         (λ (stx) (raise-syntax-error 'throw       "used out of context" stx)))
(define-syntax topblock      (λ (stx) (raise-syntax-error 'topblock    "used out of context" stx)))
(define-syntax try           (λ (stx) (raise-syntax-error 'try         "used out of context" stx)))
(define-syntax urmodule      (λ (stx) (raise-syntax-error 'urmodule    "used out of context" stx)))
(define-syntax var           (λ (stx) (raise-syntax-error 'var         "used out of context" stx)))
(define-syntax while         (λ (stx) (raise-syntax-error 'while       "used out of context" stx)))
(define-syntax :=            (λ (stx) (raise-syntax-error ':=          "used out of context" stx)))
;;; Keywords added in ES6
(define-syntax const         (λ (stx) (raise-syntax-error 'const       "used out of context" stx)))
(define-syntax let-decl      (λ (stx) (raise-syntax-error 'let-decl    "used out of context" stx)))
;;; Keywords added in ES7
(define-syntax await         (λ (stx) (raise-syntax-error 'await       "used out of context" stx)))
(define-syntax async         (λ (stx) (raise-syntax-error 'async       "used out of context" stx)))
; Note: async is technically not a reserved word in ES7.

;;; Urlang Keywords
(define-syntax define/async  (λ (stx) (raise-syntax-error 'define/async "used out of context" stx)))


; Note: Remember to provide all keywords
(define-literal-set keywords
  (array as all-as begin block break catch class continue define default
         do-while dot export
         finally if import import-from object new label lambda λ
         ref return require sempty sif throw topblock try urmodule
         var while :=
         ; ES6 Keywords
         const spread
         ; ES7 Keywords
         await async
         ; Urlang keywords
         let-decl   ; called let in ES6 (but we have used let for expressions)
         define/async let))
(define keyword? (literal-set->predicate keywords))


;;; EcmaScript 6 Reserved keywords
; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference
; /Lexical_grammar#Reserved_keywords_as_of_ECMAScript_6
(define ecma6-reserved-keywords
  '(break case
          ; class
          catch const continue debugger
          default 
          ; delete ; allow delete expressions
          do else export extends finally for function if import in instanceof let
          new return
          ; super ; HACK temporarily allow (import ... super ...)
          switch
          ; this  ; HACK temporarily allow (import ... this ...)
          throw try typeof var void while with yield
          ; Future Keywords
          enum await
          ; Future Keywords in strict mode
          implements package protected static interface private public
          ; Literals
          null true false))

(define ecma7-reserved-keywords ; incomplete
  '(async await))

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
(define (js-module-name? v) (string? v))
(define (async? v)          (boolean? v))

;;;
;;; URLANG AS NANOPASS LANGUAGE
;;;

(define-language Lur
  (entry Module)
  (terminals
   ((id          (f x l)) . => . unparse-id)
   ; Even though f, x, and, l are specified to be general identifiers, we will use this convention:
   ;   f   function name
   ;   l   statement label
   ;   x   identifier
   ((datum       (d))     . => . unparse-datum)
   (module-name  (mn))
   (js-module-name (js-mn))
   ((property-name (pn)) . => . (λ (v) (if (syntax? v) (unparse-syntax v) v)))
   (async         (a))) ; #t or #f
  (Module (u)
    (urmodule mn m ...))
  (ModuleLevelForm (m)
    (export  x ...)        
    (import  x ...)              ; declare that x ... are declared (builtin or required from Node)
    (import-from js-mn is ...)   ; ES6:   import {is ...} from the module named js-mn (string literal)
    (require rs ...)             ; import from modules compiled by urlang
    (topblock m ...)             ; toplevel block
    ;                            ; (allows macros to expand to more than one module level form)
    δ σ)                         ; definition or statement
  (RequireSpec (rs)
    mn                       ; import all exported identifers from the module with module name mn
    #;(only-in   mn x ...)   ; import x ... from the module named mn
    #;(except-in mn x ...))  ; import all exported identifiers from the module named mn except x ...
  (ImportSpec (is)
    (default x)              ; the default export as x
    (as x1 x2)               ; the member x1 using the name x2 locally
    (all-as x)               ; import all using the name x
    x)                       ; the member named x             
  (Definition (δ)
    (define a (f φ ...) b) ; function definition (async is #t or #f)
    (define x e))
  (Formal (φ)
    x                        ; parameter name
    [x e])                   ; parameter name and default value
  (Body (b)
    (body σ ... e))
  (VarBinding (vb)
    x
    (binding x e) => (x e))
  (CatchFinally (cf)
    (catch   x σ ...)
    (finally   σ ...)
    (catch-finally x (σ ...) (σ0 ...)))
  (Statement (σ)
    e                             ; expression
    (var vb ...)                  ; variable definition
    (let-decl vb ...)             ; variable declaration
    (const vb ...)                ; constant definition
    (sif e σ1 σ2)                 ; statement if
    (empty)                       ; the empty statement
    (block      σ ...)            ; block (no new scope in JavaScript)
    (while    e σ ...)
    (do-while e σ ...)
    (break)
    (break l)
    (continue)
    (continue l)
    (return)                      
    (return e)
    (label l σ)
    (try (σ ...) cf)
    (throw e))
  (LeftHand (lh)
    x 
    (dot e0 pn ...))                     ; dotted identifier
  (Expr (e)
    x                                   ; reference
    (app e0 e ...) => (e0 e ...)        ; application
    ; (:= x e)                          ; assignment    x = e
    (:= x e0 e1)                        ; assignment    x[e0] = e1
    (:= lh e0)                          ; assignment    lh = e1
    (begin e ...)                       ; sequence
    (if e0 e1 e2)                       ; ternary
    (let ((x e) ...) b)                 ; local binding
    (lambda (φ ...) b)                  ; anonymous function
    (await e)                           ; await a promise
    (quote d)                           ; quotation (the parser quotes all datums)
    (ref e0 e1 e* ...)                  ; reference to array index
    (array e ...)                       ; array constructor
    (new x e ...)                       ; new expression
    (object (pn e) ...)                 ; object literal
    (class (x)     (pn e) ...)      ; class : Restriction: e is a lambda-expression
    (class (x0 x1) (pn e) ...)      ; class
    (spread e)
    (dot e pn)))                        ; property access

; <class>             ::= (class x class-heritage? ((<property-name> x ...) <body>)) ...)
; <class-heritage>    ::= x | (x x)

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

(define-syntax-class ECMA6ReservedKeyword
  (pattern x:identifier
           #:fail-unless (member (syntax-e #'x) ecma6-reserved-keywords)
           "ECMA6 reserved keyword"))

(define-syntax-class Id
  (pattern (~and x:id (~not y:Keyword))))

(define-syntax-class NonECMA6ReservedKeyword
  (pattern (~and x:id (~not y:ECMA6ReservedKeyword))))

(define-syntax-class PropertyName
  (pattern (~or (~and x:id #;(~not y:Keyword))  ; TODO TODO is this correct?
                d:Number
                s:String)))

#;(define-syntax-class Label
   (pattern x:Id))

(define-syntax-class Reference
  (pattern x:Id))

(define-syntax-class Application
  (pattern (e0:Expr e:Expr ...)))

(define-syntax-class ArrayReference
  #:literal-sets (keywords)
  (pattern (ref e0:Expr e1:Expr e:Expr ...)))

(define-syntax-class Array
  #:literal-sets (keywords)
  (pattern (array e:ArrayExpr ...)))

(define-syntax-class MacroApplication
  #:literal-sets (keywords)
  (pattern (m:Id . _)
           #:when (macro? #'m)
           #:attr transformer (macro-transformer #'m)))

(define-syntax-class Sequence
  #:literal-sets (keywords)
  (pattern (begin e0:Expr e:Expr ...)))

(define-syntax-class Dot
  #:literal-sets (keywords)
  (pattern (dot e0:Expr e:Expr ...)))

(define-syntax-class Ternary
  #:literal-sets (keywords)
  (pattern (if e0:Expr e1:Expr e2:Expr)))


(define-syntax-class LeftHandIdentifier
  (pattern x:Id))

(define-syntax-class LeftHand
  #:literal-sets (keywords)
  (pattern x:LeftHandIdentifier)
  (pattern (dot e:Expr pn:PropertyName ...)))

(define-syntax-class Assignment
  #:literal-sets (keywords)
  (pattern (~or #;(:= x4:Id e4:Expr)
                (:= x4:Id e4:Expr e5:expr)
                (:= lh:LeftHand e5:Expr))))

(define-syntax-class Definition
  #:literal-sets (keywords)
  (pattern
   (~or (define       (f:Id φ:Formal ...) σ:Statement ... body:Body)
        (define/async (f:Id φ:Formal ...) σ:Statement ... body:Body)
        (define x:Id e:Expr))))

(define-syntax-class Lambda
  #:literal-sets (keywords)
  (pattern (~or (lambda (x ...) body:Body)
                (λ      (x ...) body:Body))))

(define-syntax-class Await
  #:literal-sets (keywords)
  (pattern (await ae:Expr)))

(define-syntax-class Statement
  #:literal-sets (keywords)
  (pattern (~or m:Expr
                w:While
                v:VarDecl
                l:LetDecl
                k:ConstDecl
                β:Block
                dw:DoWhile
                i:If
                b:Break
                c:Continue
                r:Return
                la:Label
                t:Try)))


(define-syntax-class Block
  #:literal-sets (keywords)
  (pattern (block σ:Statement ...)))

(define-syntax-class VarDecl
  #:literal-sets (keywords)
  (pattern (var vb:VarBinding ...)))

(define-syntax-class LetDecl
  #:literal-sets (keywords)
  (pattern (let-decl vb:VarBinding ...)))

(define-syntax-class ConstDecl
  #:literal-sets (keywords)
  (pattern (const vb:VarBinding ...)))

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

(define-syntax-class Try
  #:literal-sets (keywords)
  (pattern (~or (try {σ:Statement ...}
                     (catch x:Id cσ:Statement ...)
                     (finally fσ:Statement ...))
                (try {σ:Statement ...}
                     (finally fσ:Statement ...))
                (try {σ:Statement ...}
                     (catch x:Id cσ:Statement ...)))))

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

(define-syntax-class Return
  #:literal-sets (keywords)
  (pattern (return))
  (pattern (return la:Id)))

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
                e:Await
                e:ArrayReference
                e:Array
                e:New
                e:Object
                e:Dot)))

; Note: Spread expressions are only legal with array expressions or
;       in destructuring arguments.
(define-syntax-class Spread
  #:literal-sets (keywords)
  (pattern (spread se:Expr)))

(define-syntax-class ArrayExpr
  (pattern (~or e:Spread
                e:Expr)))

(define-syntax-class New
  #:literal-sets (keywords)
  (pattern (new c:Id e:Expr ...)))

(define-syntax-class Object
  #:literal-sets (keywords)
  (pattern (object [pn:PropertyName e] ...)))  ; e is a an expression or macro application

; <class>             ::= (class x class-heritage? ((<property-name> x ...) <body>)) ...)
; <class-heritage>    ::= x | (x x)

(define-syntax-class Class
  #:literal-sets (keywords)
  (pattern (~or (class x:Id          [(pn:PropertyName a:Id ...) e] ...)
                (class (x0:Id x1:Id) [(pn:PropertyName a:Id ...) e] ...))))

(define-syntax-class Export
  #:literal-sets (keywords)
  (pattern (export x:Id ...)))

(define-syntax-class Import
  #:literal-sets (keywords)
  ; note this was x:Id, but then the identifier  require  can not be imported.
  (pattern (import x:NonECMA6ReservedKeyword ...)))

(define-syntax-class Require
  #:literal-sets (keywords)
  (pattern (require x:Id ...)))

(define-syntax-class TopBlock
  #:literal-sets (keywords)
  (pattern (topblock mlf:ModuleLevelForm ...)))

(define-syntax-class ModuleLevelForm
  (pattern (~or ex:Export
                im:Import
                rm:Require
                m:Definition
                m:Statement
                m:TopBlock)))

(define-syntax-class Module
  #:literal-sets (keywords)
  (pattern (urmodule mn:ModuleName m:ModuleLevelForm ...)))

;;;
;;; PARSING: FROM SYNTAX OBJECT TO NANOPASS REPRESENTATION
;;;

(define macro-expansion-context
  (make-parameter 'module-level
                  (λ (c) (or (and (member c '(module-level statement expression)) c)
                             (error 'macro-expansion-context
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

(define macros-ht (make-hash)) ; stores all macro transformers

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
  (let ([result (parse-urmodule stx)])
    ;(displayln (list 'parse: result))
    result))

(define (parse-urmodule u)
  (with-output-language (Lur Module)
    (syntax-parse u
      #:literal-sets (keywords)
      [(urmodule mn:ModuleName m ...)
       (let ((m  (stx-map parse-module-level-form #'(m ...)))
             (mn (syntax-e #'mn)))
         `(urmodule ,mn ,m ...))])))

(define (parse-export ex)
  (with-output-language (Lur ModuleLevelForm)
    (syntax-parse ex
      #:literal-sets (keywords)
      [(export x:Id ...)
       (let ([x (syntax->list #'(x ...))])
         `(export ,x ...))])))

(define (parse-import im)
  (with-output-language (Lur ModuleLevelForm)
    (syntax-parse im
      #:literal-sets (keywords)
      [(import x:NonECMA6ReservedKeyword ...)
       (let ([x (syntax->list #'(x ...))])
         `(import ,x ...))])))

(define (parse-import-from imf)
  (with-output-language (Lur ModuleLevelForm)
    (syntax-parse imf
      #:literal-sets (keywords)
      [(import-from d:Datum is* ...)
       (let ([is* (map parse-import-spec (syntax->list #'(is* ...)))]
             [d (syntax->datum #'d)])
         `(import-from ,d ,is* ...))])))

(define (parse-import-spec is)
  (with-output-language (Lur ImportSpec)
    (syntax-parse is
      #:literal-sets (keywords)
      [(default x:NonECMA6ReservedKeyword)
       `(default ,#'x)]
      [(as x1:NonECMA6ReservedKeyword x2:NonECMA6ReservedKeyword)
       `(as ,#'x1 ,#'x2)]
      [(all-as x:NonECMA6ReservedKeyword)
       `(all-as ,#'x)]
      [x:NonECMA6ReservedKeyword
       `,#'x])))

(define (parse-require rm)
  (with-output-language (Lur ModuleLevelForm)
    (syntax-parse rm
      #:literal-sets (keywords)
      [(require rs ...)
       (let ([rs (map parse-require-spec (syntax->list #'(rs ...)))])
         `(require ,rs ...))])))

(define (parse-require-spec rs)
  (with-output-language (Lur RequireSpec)
    (syntax-parse rs
      #:literal-sets (keywords)
      [mn:Id `,(syntax-e #'mn)])))  ; todo : allow syntax-object instead of symbol

(define (parse-topblock tb)
  (with-output-language (Lur ModuleLevelForm)
    (syntax-parse tb
      #:literal-sets (keywords)
      [(topblock m ...)
       (let ([m (map parse-module-level-form (syntax->list #'(m ...)))])
         `(topblock ,m ...))])))

(define (parse-module-level-form m)
  (debug (list 'parse-module-level-form (syntax->datum m)))
  (parameterize ([macro-expansion-context 'module-level])
    (with-output-language (Lur ModuleLevelForm)
      (syntax-parse m
        #:literal-sets (keywords)
        [ma:MacroApplication           (parse-module-level-form (parse-macro-application #'ma))]
        [(~and ex  (export       . _))  (parse-export #'ex)]
        [(~and im  (import       . _))  (parse-import #'im)]
        [(~and rm  (require      . _))  (parse-require #'rm)]
        [(~and d   (define       . _))  (parse-definition #'d)]
        [(~and d   (define/async . _))  (parse-definition #'d)]
        [(~and tb  (topblock     . _))  (parse-topblock #'tb)]
        [(~and imf (import-from  . _))  (parse-import-from #'imf)]
        [σ                       (parse-statement #'σ parse-module-level-form 'module-level-form)]))))

(define (parse-statement σ [context-parse parse-statement] [parent-context 'statement])
  (debug (list 'parse-statement (syntax->datum σ)))
  (parameterize ([macro-expansion-context 'statement])
    (with-output-language (Lur Statement)
      (syntax-parse σ
        #:literal-sets (keywords)
        [ma:MacroApplication      (define expansion (parse-macro-application #'ma))
                                  ; (displayln (list 'parse-statement 'expansion: expansion))
                                  (parameterize ([macro-expansion-context parent-context])
                                    (context-parse expansion))]
        [(~and b  (break    . _)) (parse-break      #'b)]
        [(~and c  (continue . _)) (parse-continue   #'c)]
        [(~and r  (return   . _)) (parse-return     #'r)]
        [(~and la (label    . _)) (parse-label      #'la)]
        [(~and w  (while    . _)) (parse-while      #'w)]
        [(~and dw (do-while . _)) (parse-do-while   #'dw)]
        [(~and v  (var      . _)) (parse-var-decl   #'v)]
        [(~and l  (let-decl . _)) (parse-let-decl   #'l)] ; ES6
        [(~and k  (const    . _)) (parse-const-decl #'k)] ; ES6
        [(~and β  (block    . _)) (parse-block      #'β)]
        [(~and i  (sif      . _)) (parse-if         #'i)]
        [(~and se (sempty   . _)) (parse-empty      #'se)]
        [(~and tr (try      . _)) (parse-try        #'tr)]
        [(~and th (throw    . _)) (parse-throw      #'th)]
        [e                        (parse-expr       #'e context-parse parent-context)]))))

(define (parse-try tr)
  ; <try>               ::= (try <block> <catch>)
  ;                      |  (try <block> <finally>)
  ;                      |  (try <block> <catch> <finally>)
  (debug (list 'parse-try (syntax->datum tr)))
  (with-output-language (Lur Statement)
    (syntax-parse tr
      #:literal-sets (keywords)
      [(try (σ ...)
            (catch x:Id cσ ...))   (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                          [cσ (map parse-statement (syntax->list #'(cσ ...)))]
                                          [c  (with-output-language (Lur CatchFinally)
                                                `(catch ,#'x ,cσ ...))])
                                     `(try (,σ ...) ,c))]
      [(try (σ ...)
            (finally fσ ...))    (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                        [fσ (map parse-statement (syntax->list #'(fσ ...)))]
                                        [f  (with-output-language (Lur CatchFinally)
                                              `(finally ,fσ ...))])
                                   `(try (,σ ...) ,f))]
      [(try (σ ...)
            (catch x:Id cσ ...)
            (finally fσ ...))    (let* ([σ  (map parse-statement (syntax->list #'( σ ...)))]
                                        [cσ (map parse-statement (syntax->list #'(cσ ...)))]
                                        [fσ (map parse-statement (syntax->list #'(fσ ...)))]
                                        [cf (with-output-language (Lur CatchFinally)
                                              `(catch-finally ,#'x (,cσ ...) (,fσ ...)))])
                                   `(try (,σ ...) ,cf))])))

(define (parse-throw th)
  (debug (list 'parse-throw (syntax->datum th)))
  (with-output-language (Lur Statement)
    (syntax-parse th
      #:literal-sets (keywords)
      [(throw e) (let ([e (parse-expr #'e)])
                   `(throw ,e))])))

(define (parse-empty se)
  ; the empty statement
  (debug (list 'parse-empty (syntax->datum se)))
  (with-output-language (Lur Statement)
    (syntax-parse se
      #:literal-sets (keywords)
      [(sempty) `(empty)])))

(define (parse-continue c)
  (debug (list 'parse-continue (syntax->datum c)))
  (with-output-language (Lur Statement)
    (syntax-parse c
      #:literal-sets (keywords)
      [(continue)       `(continue)]
      [(continue la:Id) `(continue ,#'la)])))

(define (parse-return r)
  (debug (list 'parse-return (syntax->datum r)))
  (with-output-language (Lur Statement)
    (syntax-parse r
      #:literal-sets (keywords)
      [(return)   `(return)]
      [(return e) (let ([e  (parse-expr #'e)])
                    `(return ,e))])))

(define (parse-label la)
  (debug (list 'parse-label (syntax->datum la)))
  (with-output-language (Lur Statement)
    (syntax-parse la
      #:literal-sets (keywords)
      [(label x:Id σ) (let ([σ (parse-statement #'σ)])
                        `(label ,#'x ,σ))])))

(define (parse-break b)
  (debug (list 'parse-break (syntax->datum b)))
  (with-output-language (Lur Statement)
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
  (with-output-language (Lur Statement)
    (syntax-parse i
      #:literal-sets (keywords)
      [(sif e σ1 σ2)
       (let ((e  (parse-expr #'e))
             (σ1 (parse-statement #'σ1))
             (σ2 (parse-statement #'σ2)))
         `(sif ,e ,σ1 ,σ2))])))

(define (parse-let l)
  (debug (list 'parse-let (syntax->datum l)))
  (with-output-language (Lur Expr)
    (syntax-parse l
      #:literal-sets (keywords)
      [(let ((x:Id e) ...) . body)
       (let ([x  (syntax->list #'(x ...))]
             [e (stx-map parse-expr #'(e ...))]
             (b (parse-body #'body)))
       `(let ((,x ,e) ...) ,b))])))

(define (parse-block β)
  (debug (list 'parse-block (syntax->datum β)))
  (with-output-language (Lur Statement)
    (syntax-parse β
      #:literal-sets (keywords)
      [(block σ ...)
       (let ((σ (stx-map parse-statement #'(σ ...))))
         `(block ,σ ...))])))

(define (parse-var-decl v)
  (debug (list 'parse-var-decl (syntax->datum v)))
  (with-output-language (Lur Statement)
    (syntax-parse v
      #:literal-sets (keywords)
      [(var vb:VarBinding ...)
       (let ((vb (stx-map parse-var-binding #'(vb ...))))
         `(var ,vb ...))])))

(define (parse-let-decl l) ; ES6
  (debug (list 'parse-let-decl (syntax->datum l)))
  (with-output-language (Lur Statement)
    (syntax-parse l
      #:literal-sets (keywords)
      [(let-decl vb:VarBinding ...)
       (let ((vb (stx-map parse-var-binding #'(vb ...))))
         `(let-decl ,vb ...))])))

(define (parse-const-decl k) ; ES6
  (debug (list 'parse-const-decl (syntax->datum k)))
  (with-output-language (Lur Statement)
    (syntax-parse k
      #:literal-sets (keywords)
      [(const vb:VarBinding ...)
       (let ((vb (stx-map parse-var-binding #'(vb ...))))
         `(const ,vb ...))])))

(define (parse-var-binding vb)
  (with-output-language (Lur VarBinding)
    (syntax-parse vb
      #:literal-sets (keywords)
      [x:Id     #'x]
      [(x:Id e) `(binding ,#'x ,(parse-expr #'e))])))

(define (parse-while w)
  (debug (list 'parse-while (syntax->datum w)))
  (with-output-language (Lur Statement)
    (syntax-parse w
      #:literal-sets (keywords)
      [(while e σ ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(while ,e ,σ ...))])))

(define (parse-do-while dw)
  (debug (list 'parse-do-while (syntax->datum dw)))
  (with-output-language (Lur Statement)
    (syntax-parse dw
      #:literal-sets (keywords)
      [(do-while e σ ...)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(do-while ,e ,σ ...))])))

(define (parse-definition d)
  (debug (list 'parse-definition (syntax->datum d)))
  (with-output-language (Lur Definition)
    (syntax-parse d
      #:literal-sets (keywords)
      [(define x:Id e)
       (let ((e (parse-expr #'e)))
         `(define ,#'x ,e))]
      [(define (f:Id φ:Formal ...))
       (raise-syntax-error 'parse-definition "expected a non-empty body, got: " d)]
      [(define (f:Id φ:Formal ...) . b)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ ... en) #'b])
             (let ([b (parse-body #'(σ ... en))]
                   [φ (stx-map parse-formal #'(φ ...))])
               `(define #f (,#'f ,φ ...) ,b)))))]
      ; identical to the one above apart from #t to indicate an async function
      [(define/async (f:Id φ:Formal ...) . b)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ ... en) #'b])
             (let ([b (parse-body #'(σ ... en))]
                   [φ (stx-map parse-formal #'(φ ...))])
               `(define #t (,#'f ,φ ...) ,b)))))])))

(define (parse-lambda d)
  (debug (list 'parse-lambda (syntax->datum d)))
  (with-output-language (Lur Expr)
    (syntax-parse d
      #:literal-sets (keywords)
      [(_lambda (φ:Formal ...) . b)
       (let ((x (attribute φ.x)))                                           ; all parameters
         (with-syntax ([((x0 e0) ...) (filter identity (attribute φ.xe))])  ; parameters with defaults
           (with-syntax ([(σ ... en) #'b])
             ; Issue #17: Unintuitive that `define` not allowed in body, so give explanation.
             (define δ (for/or ([σ (syntax->list #'(σ ... en))])
                         (syntax-parse σ #:literals (define)
                           [(define . _) σ] [_ #f])))
             (when δ
               (raise-syntax-error
                'parse-lambda
                (~a "Definition not allowed in lambda-body. Definitions work only at the "
                    "module-level.\n Instead use:\n    (var [id lambda-expression])")
                δ))
             ; End Issue 17             
             (let ([b (parse-body #'(σ ... en))]
                   [φ (stx-map parse-formal #'(φ ...))])
               
               `(lambda (,φ ...) ,b)))))])))

(define (parse-await a)
  (debug (list 'await (syntax->datum a)))
  (with-output-language (Lur Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(_await e)
       (let ([e (parse-expr #'e)])
         `(await ,e))])))

(define (parse-spread e)
  (debug (list 'spread (syntax->datum e)))
  (with-output-language (Lur Expr)
    (syntax-parse e
      #:literal-sets (keywords)
      [(_spread e)
       (let ([e (parse-expr #'e)])
         `(spread ,e))])))
  

(define (parse-formal φ)
  (with-output-language (Lur Formal)
    (syntax-parse φ
      [x:Id      `,#'x]
      [[x:Id e]  `[,#'x ,(parse-expr #'e)]])))


      
(define (parse-body b)
  (debug (list 'parse-body (syntax->datum b)))
  (with-output-language (Lur Body)
    (syntax-parse b
      #:literal-sets (keywords)
      [(σ ... e)
       (let ((e (parse-expr #'e))
             (σ (stx-map parse-statement #'(σ ...))))
         `(body ,σ ... ,e))])))

(define (parse-identifier-left-hand x)
  (debug (list 'parse-identifier-left-hand (syntax->datum x)))
  (with-output-language (Lur LeftHand)
    (syntax-parse x
      #:literal-sets (keywords)
      [x:Id
       (match (regexp-split #rx"[.]" (symbol->string (syntax-e #'x)))
         [(list _) `,#'x]       ; standard left hand identifier
         [(list p ...)  ; y.p is short for y["p"]
          (with-syntax ([(p ...)  (map (λ (p) (format-id #'x p #:source #'x)) p)])
            (parse-dotted-left-hand #'(dot p ...)))])])))

(define (parse-dotted-left-hand lh)
  ; parse left hand side of assignment of the form (dot e pn ...).
  ; Note: The input syntax is (dot e ...) but the output have (dot e pn) forms only.
  (debug (list 'parse-dotted-left-hand (syntax->datum lh)))
  (with-output-language (Lur LeftHand)
    (syntax-parse lh
      #:literal-sets (keywords)
      [(dot e0)                       (parse-left-hand #'e0)]
      ; [(dot e0 pn:PropertyName)        `(dot ,(parse-expr #'e0) ,pn)]
      [(dot e0 pn:PropertyName ...)   (let ([pn (syntax->list #'(pn ...))])
                                        `(dot ,(parse-expr #'e0) ,pn ...))]
      [_ ; (dot)
       (raise-syntax-error 'parse-dot (~a "expected a dot expression, got " lh) lh)])))

(define (parse-left-hand lh)
  ; parse left hand side of assignment
  (debug (list 'parse-left-hand (syntax->datum lh)))
  (syntax-parse lh
    #:literal-sets (keywords)
    [x:LeftHandIdentifier  (parse-identifier-left-hand #'x)]
    [(~and lh  (dot . _))  (parse-dotted-left-hand #'lh)]
    [_ (raise-syntax-error 'parse-left-hand (~a "expected an left hand side of an assignment, got " lh) lh)]))



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
      [(~and aw (await . _))                  (parse-await       #'aw)]
      [(~and o  (object . _))                 (parse-object      #'o)]
      [(~and ar (ref . _))                    (parse-array-reference #'ar)]
      [(~and ac (array . _))                  (parse-array       #'ac)]
      [(~and do (dot . _))                    (parse-dot         #'do)]
      [(~and c  (class . _))                  (parse-class       #'c)]
      [(~and sp (spread . _))                 (parse-spread      #'sp)]
      [(~and n  (new . _))                    (parse-new         #'n)]
      [(~and a  (e ...)
             (~not (k:keyword . _)))          (parse-application #'a)]
      [_ (raise-syntax-error 'parse-expr (~a "expected an expression, got " e) e)])))

(define (parse-new n)
  (debug (list 'parse-new (syntax->datum n)))
  (with-output-language (Lur Expr)
    (syntax-parse n
      #:literal-sets (keywords)
      [(new c:Id e:Expr ...)
       (let ([e  (stx-map parse-expr #'(e  ...))])
         `(new ,#'c ,e ...))])))

(define (parse-object o)
  (debug (list 'parse-object (syntax->datum o)))
  (with-output-language (Lur Expr)
    (syntax-parse o
      #:literal-sets (keywords)
      [(object [pn:PropertyName e] ...)
       (let ([pn (syntax->list #'(pn ...))]
             [e  (stx-map parse-expr #'(e  ...))])
         `(object [,pn ,e] ...))])))

(define (parse-class c)
  ; <class>             ::= (class <class-heritage> ((<property-name> x ...) <body>)) ...)
  ; <class-heritage>    ::= x | (x x)
  (debug (list 'parse-class (syntax->datum c)))
  (with-output-language (Lur Expr)
    (syntax-parse c
      #:literal-sets (keywords)
      ; declares class x with method names pn ...
      [(class x:Id [(pn:PropertyName a:Id ...) . b] ...)
       (let* ([pn (syntax->list #'(pn ...))]
              [a  (stx-map syntax->list #'((a ...) ...))]
              [b  (stx-map parse-body #'(b ...))])
         `(class (,#'x) [,pn (lambda (,a ...) ,b)] ...))]
      [(class (x0:Id x1:Id) [(pn:PropertyName a:Id ...) . b] ...)
       ; The class x0 extends the class x1, otherwise as above
       (let* ([pn (syntax->list #'(pn ...))]
              [a  (stx-map syntax->list #'((a ...) ...))]
              [b  (stx-map parse-body #'(b ...))])
         `(class (,#'x0 ,#'x1) [,pn (lambda (,a ...) ,b)] ...))])))

(define (parse-application a)
  (debug (list 'parse-application (syntax->datum a)))
  (with-output-language (Lur Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(~and (e0 e ...) (~not ma:MacroApplication))
       (let ([e0 (parse-expr #'e0)]
             [e  (stx-map parse-expr #'(e ...))])
         `(app ,e0 ,e ...))])))

(define (parse-array-reference ar)
  (debug (list 'parse-array-reference (syntax->datum ar)))
  (with-output-language (Lur Expr)
    (syntax-parse ar
      #:literal-sets (keywords)
      [(ref e0 e1 e ...)
       (let ([e0 (parse-expr #'e0)]
             [e1 (parse-expr #'e1)]
             [e  (stx-map parse-expr #'(e ...))])
         `(ref ,e0 ,e1 ,e ...))])))

(define (parse-array ac)
  (debug (list 'parse-array (syntax->datum ac)))
  (with-output-language (Lur Expr)
    (syntax-parse ac
      #:literal-sets (keywords)
      [(array e ...)
       (let ([e  (stx-map parse-expr #'(e ...))])
         `(array ,e ...))])))

(define (parse-reference r)
  (debug (list 'parse-reference (syntax->datum r)))
  (with-output-language (Lur Expr)
    (syntax-parse r
      #:literal-sets (keywords)
      [x:Id
       (match (regexp-match #rx"(.*)[.](.*)" (symbol->string (syntax-e #'x)))
         [#f `,#'x]       ; standard reference
         [(list y.p y p)  ; y.p is short for y["p"]
          (with-syntax ([y  (format-id #'x y #:source #'x)]
                        [p  (format-id #'x p #:source #'x)])
            (parse-expr #'(dot y p)))])])))

(define (parse-sequence a)
  (debug (list 'parse-sequence (syntax->datum a)))
  (with-output-language (Lur Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(begin e0)       (parse-expr #'e0)]
      [(begin e0 e ...) (let ([e0 (parse-expr #'e0)]
                              [e  (stx-map parse-expr #'(e ...))])
                          `(begin ,e0 ,e ...))])))

(define (parse-dot do)
  ; Note: The input syntax is (dot e ...) but the output have (dot e pn) forms only.
  (debug (list 'parse-dot (syntax->datum do)))
  (with-output-language (Lur Expr)
    (syntax-parse do
      #:literal-sets (keywords)
      [(dot e0)                       (parse-expr #'e0)]
      [(dot e0 (x:Id e ...) e1 ...)   (parse-expr #'(dot ((dot e0 x) e ...) e1 ...))]
      [(dot e0 e1)                   `(dot ,(parse-expr #'e0) ,(parse-expr #'e1))]
      [(dot e0 e1 e2 ...)             (parse-expr #'(dot (dot e0 e1) e2 ...))]
      [(dot)
       (raise-syntax-error 'parse-dot (~a "expected a dot expression, got " do) do)])))

(define (parse-assignment a)
  (debug (list 'parse-assignment (syntax->datum a)))
  (with-output-language (Lur Expr)
    (syntax-parse a
      #:literal-sets (keywords)
      [(:= lh:LeftHand e)
       (let ((lh (parse-left-hand #'lh))
             (e (parse-expr #'e)))
         `(:= ,lh ,e))]
      ; Backward compatibility
       [(:= x:Id e0 e1)
        (let ((e0 (parse-expr #'e0))
              (e1 (parse-expr #'e1)))
          `(:= ,#'x ,e0 ,e1))])))

(define (parse-ternary t)
  (debug (list 'parse-ternary (syntax->datum t)))
  (with-output-language (Lur Expr)
    (syntax-parse t
      #:literal-sets (keywords)
      [(if e0 e1 e2)
       (let ([e0 (parse-expr #'e0)]
             [e1 (parse-expr #'e1)]
             [e2 (parse-expr #'e2)])
         `(if ,e0 ,e1 ,e2))])))

(define (parse-datum d)
  (with-output-language (Lur Expr)
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

;;; Arity

(define (nullary? x) (member x '(+ *)))
(define (unary? x)   (member x '(+ * - /       ; arithmetical
                                   and or not  ; logical
                                   ~)))        ; bitwise not
(define (binary? x)   (member x '(+ - * / %
                                     = == === != !== < > <= >=
                                     and or not
                                     << >> >>>
                                     bit-and bit-or bit-xor bit-not)))
(define (nary? x) (member x '(+ * - / and or bit-and bit-or bit-xor)))

;;;
;;; PREDEFINED NAMES AND RESERVED WORDS
;;;

(define predefined-names '(Array array array! arguments console ref undefined new typeof))
; (besides operators)

(define reserved-words-ids (symbols->ids ecma6-reserved-keywords))
(define predefined-ids     (symbols->ids predefined-names))

;;;
;;; FLATTEN TOPLEVEL BLOCKS
;;;

(define-pass flatten-topblock : Lur (U) -> Lur ()
  ; this pass eliminates topblock
  (definitions
    (define lifted-forms (make-parameter '()))
    (define (add-form m) (lifted-forms (cons m (lifted-forms)))) ; in reverse
    (define (flatten m)
      ; (displayln (list 'flatten m))
      (nanopass-case (Lur ModuleLevelForm) m
        [(export   ,x ...)            (add-form m) '()]
        [(import   ,x ...)            (add-form m) '()]
        [(require  ,rs ...)           (add-form m) '()]
        [(import-from ,js-mn ,is ...) (add-form m) '()]
        [(topblock ,m ...)            (append-map flatten m)]        
        [,δ                           (list δ)]
        [,σ                           (list σ)])))
  (Statement  : Statement  (σ)  ->  Statement ())
  (LeftHand   : LeftHand   (lh) ->  LeftHand  ())
  (ModuleLevelForm : ModuleLevelForm  (m) ->  ModuleLevelForm ()
    [(topblock ,m ...) (append-map flatten m)])
  (Module : Module (u) -> Module ()
    [(urmodule ,mn ,m ...)
     (let ([m     (append-map flatten m)]
           [ex/im (reverse (lifted-forms))])
       `(urmodule ,mn ,ex/im ... ,m ...))]))

;;;
;;; DESUGARED URLANG
;;;

;; Desugaring rewrites functions with optional arguments to
;; functions without.

(define-language L- (extends Lur)
  (Formal (φ)
    (- x)
    (- [x e]))
  (Definition (δ)
    (- (define a (f φ ...) b))
    (+ (define a (f x ...) b))
    ; (- (define x e))
    ; (+ (define x e))
    )
  (Expr (e)
    (- (lambda (φ ...) b))
    (+ (lambda (x ...) b))))


(define-pass desugar : Lur (U) -> L- ()
  (definitions
    (define (L-formal->id+expr φ Expr)
      (nanopass-case (Lur Formal) φ
        [,x      (list x #f)]
        [[,x ,e] (list x (with-output-language (L- Statement)
                           (let ([e (Expr e)])
                             `(sif (app ,#'= ,x ,#'undefined)
                                   (:= ,x ,e)
                                   (empty)))))])))
  (Statement  : Statement  (σ) ->  Statement ())  
  (Expr       : Expr       (e) ->  Expr ()
    [(lambda (,φ* ...) ,b)
     (match (for/list ([φ (in-list φ*)])
              (L-formal->id+expr φ Expr))
       [(list (list x s) ...)
        (let ([s (filter identity s)])
          (nanopass-case (Lur Body) b
            [(body ,σ ... ,e)
             (let ([σ (map Statement σ)] [e (Expr e)])
               `(lambda (,x ...)
                  (body ,s ...
                        ,σ ...
                        ,e)))]))])])
  (Definition : Definition (δ) ->  Definition ()
    [(define ,a (,f ,φ* ...) ,b)
     (match (for/list ([φ (in-list φ*)])
              (L-formal->id+expr φ Expr))
       [(list (list x s) ...)
        (let ([s (filter identity s)])
          (nanopass-case (Lur Body) b
            [(body ,σ ... ,e)
             (let ([σ (map Statement σ)] [e (Expr e)])
               `(define ,a (,f ,x ...)
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
    (- (export  x  ...)
       (import  x  ...)
       (require rs ...)))
  (Annotation (an)
    (+ (export x0 ...) (import x1 ...) (require rs ...) (funs x2 ...) (vars x3 ...)))
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
    (- (define a (f x ...) b))
    (+ (define a (f x ...) ab))
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
;;   * collects all require forms
;;   * imports predefined operators (+, -, *, ...)
;;   * collects all module-level defined function names
;;   * collects all module-level defined variable names
;;   * Adds an annotation to the urmodule form
;;       (annotation
;;         (export x ...) (import x ...) (require rs ...) (funs ...) (vars ...))
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
    (define require-specs '())
    (define (add-require-spec! rs) (set! require-specs (cons rs require-specs)))

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
                   (list `(export  ,(exports) ...)
                         `(import  ,(append (predefineds) (operators) (imports)) ...)
                         `(require ,require-specs ...)
                         `(funs    ,(funs)    ...)
                         `(vars    ,(vars)    ...)))))
         `(urmodule ,mn (,an ...) ,m ...)))])
  (ModuleLevelForm : ModuleLevelForm (m) -> ModuleLevelForm ()
    [(import  ,x ...)             (for-each import! x)            remove-form]
    [(export  ,x ...)             (for-each export! x)            remove-form]
    [(require ,rs ...)            (for-each add-require-spec! rs) remove-form]
    [(import-from ,js-mn ,is ...) (let ([is (map ImportSpec is)])
                                    `(import-from ,js-mn ,is ...))])
  (ImportSpec : ImportSpec (is) -> ImportSpec ()
    ; This introduces variables imported via import-from into the module-level-scope
    [(default ,x) (import! x)  `(default ,x)]
    [(as ,x1 ,x2) (import! x2) `(as ,x1 ,x2)]
    [(all-as ,x)  (import! x)  `(all-as ,x)]
    [,x           (import! x)  `,x])
  
  ;; Register variables declared with var/let/const only, when var/let/const is a module-level form.
  ;; All contexts where var/let/const-forms can appear need to change the context.
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
    [(define ,a (,f ,x0 ...) ,b) (when (fun? f)
                                (raise-syntax-error 'collect "identifier is declared twice as fun" f))
                              (fun! f)
                              (parameterize ([context 'body])
                                (let ((b (Body b)))
                                  `(define ,a (,f ,x0 ...) ,b)))])
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
  (define pre (if (syntax? prefix) (syntax-e prefix) prefix))
  (datum->syntax #'here (string->symbol (~a pre joiner counter))))
(define (reset-counter! [new-joiner "_"])
  (set! counter 0)
  (set! joiner new-joiner))

(define-pass α-rename : L1 (U) -> L1 ()
  (definitions
    ;(define-free-table global)  ; free references refer to module-level-defined variables
    (define-symbol-table global)
    (define-bound-table var)    ; references to variables declared with var can be macro introduced
    (define (initial-ρ x [id= #f])
      (cond
        [(var? x) => identity]  ; get (potentially renamed x)
        [(global? x) x]         ; exports, imports, funs are never renamed
        [else        #f]))      ; no other variables are bound
    (define (fid= x y) (free-identifier=?  x y))
    (define (bid= x y) (bound-identifier=? x y))
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
    (define (unbound-error x)
      (displayln "global:")           (displayln (globals))
      (displayln "var introduced:")   (displayln (map syntax-e (vars)))
      (displayln "unbound variable:") (displayln x)
      (if (memq (syntax-e x) (macro-introduced-identifiers)) ; todo remove when modules arrive
          x
          (raise-syntax-error 'α-rename "(urlang) unbound variable" x)))
    (define pre-body-ρ      (make-parameter #f))
    (define module-name-stx (make-parameter #f)))
  (Annotation : Annotation (an) -> Annotation ()
    [(export  ,x   ...)                      an]
    [(import  ,x   ...) (for-each global! x) an]
    [(require ,rs* ...) (for ([rs rs*])
                          (unless (symbol? rs) (error "internal error"))
                          (for ([sym (urmodule-name->exports rs)])
                            (define x (datum->syntax (module-name-stx) sym))
                            (global! x)))
                          an]
    [(funs    ,x   ...) (for-each global! x) an]
    [(vars    ,x*  ...) (for ((x x*))
                         (let ([y (fresh-var x)])
                           (global! y)
                           (var! x y)))
                        an])
  (Module : Module (u) -> Module ()
    [(urmodule ,mn (,[an] ...) ,m ...)
     (let ((ρ initial-ρ))
       (parameterize ([pre-body-ρ ρ] [module-name-stx mn])
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
    [(define ,a (,f ,x ...) ,ab)  (letv ((x ρ) (rename* x ρ))
                                    (let ([ab (AnnotatedBody ab ρ)])
                                      `(define ,a (,f ,x ...) ,ab)))])
  ; TODO: Change var to use same scope rules as let*.
  ;       In (var [binding s s]) the second s refers to an outer scope.
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
    ; [(:= ,x ,[e])               (let ((y (lookup x ρ unbound-error))) `(:= ,y ,e))]
    [(:= ,lh ,[e])               `(:= ,(LeftHand lh ρ) ,e)]
    [(:= ,x ,[e0] ,[e1])        (let ((y (lookup x ρ unbound-error))) `(:= ,y ,e0 ,e1))]

    [(let ((,x ,[e]) ...) ,ab)  (letv ((x ρ) (rename* x ρ))  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(let ((,x ,e) ...) ,ab)))]
    [(lambda (,x ...) ,ab)      (letv ((x ρ) (rename* x ρ))  ; map x to x
                                  (let ([ab (AnnotatedBody ab ρ)])
                                    `(lambda (,x ...) ,ab)))]
    [(new ,x ,[e] ...)          (let ((y (lookup x ρ unbound-error)))
                                  `(new ,y ,e ...))])

  (LeftHand : LeftHand (lh ρ) -> LeftHand ()
   [,x                         (lookup x ρ unbound-error)]
   [(dot ,e ,pn* ...)          `(dot ,(Expr e ρ) ,pn* ...)])

  (let ()
    ; See explanation in "globals.rkt" and in compiler3.rkt
    ; (displayln "alpha-rename (in urlang/main)")
    ; (displayln (global-variables))
    (for ([v (global-variables)])
      (global! v))
    (global! #'null)  ; Javascript null is a keyword (always available)
    ; (global! #'unsafe-fx<)
    ; Also we need `else` to be know for use in macros
    (Module U)))

;;;
;;; CODE GENERATION
;;;

; This pass produces a tree of datums (mostly strings) and identifiers.
; Emit will will traverse the tree and display all elements.
; The output will be a JavaScript program.

(define current-exports (make-parameter '()))

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
    (define (convert-string-char c)
      (case c
        [(#\backspace) "\\b"]
        [(#\page) "\\f"]
        [(#\newline) "\\n"]
        [(#\return) "\\r"]
        [(#\tab) "\\t"]
        [(#\vtab) "\\v"]
        [(#\nul) "\\000"]
        [(#\") "\\\""]
        [(#\\) "\\\\"]
        [else (string c)]))
    (define (~string t)        `("\"" ,@(for/list ([c t])
                                          (convert-string-char c)) "\""))
    (define (~property-name t) (define v (syntax-e t)) (if (string? v) (~string v) t))
    (define (~ClassMethod pn e)
      (nanopass-case (L1 Expr) e
        [(lambda (,x* ...) ,ab)
         (list pn (~parens (~commas x*)) (AnnotatedBody ab))]))
    
    (define (exports.id x)   (format-id x "exports.~a" x))
    (define current-js-import-module-name (make-parameter #f))
    (define jsmn current-js-import-module-name)
    ; Issue #13 (Rollup.js can't handle reusing the MODULE variable,
    ; so produce MODULEi where i is the import counter.
    (define import-counter 0)
    (define (import-counter++)
      (begin0 import-counter
              (set! import-counter (+ import-counter (if (current-urlang-rollup?) 1 0))))))
  (Module : Module (u) -> * ()
    [(urmodule ,mn (,an ...) ,m ...) (list (~newline (~Statement "\"use strict\""))
                                           (map (λ (an) (Annotation an 'require)) an)
                                           (map ModuleLevelForm m)
                                           (map (λ (an) (Annotation an 'export)) an))])
  (Annotation : Annotation (an type) -> * ()
    [(import ,x ...)   ""]
    [(funs   ,x ...)   ""]
    [(vars   ,x ...)   ""]
    [(export ,x* ...) (match type
                        ['export
                         ; add the exports to current-exports
                         ; (emit will then make a modulename.exports)
                         (current-exports (append x* (current-exports)))
                         ; export the identifiers using the Node convention
                         ; (storing exported values in exports)
                         (for/list ([x x*])
                           (~newline (~Statement (exports.id x) "=" x)))]
                        [_ ""])]
    [(require ,rs ...) (match type
                         ['require (map RequireSpec rs)]
                         [_        '()])])
  (RequireSpec : RequireSpec (rs) -> * ()
    [,mn (define imports (urmodule-name->exports mn))
         (define i (import-counter++))
         (let ([mn.js (urmodule-name->js-file-name mn)])
           (list (~newline (~Statement `(var "MODULE" ,i " = require(\"./" ,mn.js "\")")))
                 (for/list ([x imports]) 
                   (let ([x (mangle (datum->syntax #'ignored x))]) ; mangle expects ids (not symbols)
                     (~newline (~Statement `(var ,x ,(~a " = MODULE"i"[\"" x "\"]"))))))))])    
  (ModuleLevelForm : ModuleLevelForm (m) -> * ()
    [(import-from ,js-mn ,is* ...) (parameterize ([current-js-import-module-name js-mn])
                                     (map ImportSpec  is*))]
    [(topblock ,m* ...)         (map ModuleLevelForm m*)]
    [,δ                         (~newline (Definition δ))]
    [,e                         (~newline (~Statement (~top-expr (Expr e))))]
    [,σ                         (~newline (Statement σ))])
  (ImportSpec : ImportSpec (is) -> * ()
    [(as ,x1 ,x2)   (~newline (~Statement (list "import " (~braces x1 " as " x2)
                                                " from " (~string (jsmn)))))]
    [(all-as ,x)    (~newline (~Statement (list "import " '*  " as " x " from " (~string (jsmn)))))]
    [(default ,x)   (~newline (~Statement (list "import " x " from " (~string (jsmn)))))]
    [,x             (~newline (~Statement (list "import " (~braces x) " from " (~string (jsmn)))))])  
  (Definition : Definition (δ) -> * ()
    [(define ,x ,e)            (let ([e (Expr e)])
                                 (~Statement `(var ,x "=" ,e)))]
    [(define ,a (,f ,x ...) ,ab)  (let ()
                                 #;(define (formal->x φ)
                                     (nanopass-case (L1 Formal) φ
                                       [,x x] [(,x ,e) x]))
                                 (let ((ab (AnnotatedBody ab))
                                       #;[x  (map formal->x φ)])
                                   (~Statement `(,(if a 'async "")
                                                 function ,f ,(~parens (~commas x))
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
    [(return)             (~Statement "return")]
    [(return ,e)          (~Statement "return" " " (Expr e))]
    [(label ,l ,σ)        (~Statement l ":" (Statement σ))]
    [(try (,σ ...) ,cf)   (let ((σ (map Statement σ)))
                            (~Statement "try " (~braces σ) (CatchFinally cf)))]
    [(throw ,e)           (let ((e (Expr e)))
                            (~Statement "throw" " " e))]
    ; the var/let/const cases are the same
    [(var ,vb ...)        (match (map VarBinding vb)
                            [(list) (~Statement)]            ; no bindings => empty statement
                            [(list (list xs es) ...)
                             (~Statement
                              `(var ,(~commas (for/list ([x xs] [e es])
                                                (if e (list x "=" e) x)))))])]
    [(let-decl ,vb ...)   (match (map VarBinding vb)
                            [(list) (~Statement)]            ; no bindings => empty statement
                            [(list (list xs es) ...)
                             (~Statement
                              `(let ,(~commas (for/list ([x xs] [e es])
                                                (if e (list x "=" e) x)))))])]
    [(const ,vb ...)      (match (map VarBinding vb)
                            [(list) (~Statement)]            ; no bindings => empty statement
                            [(list (list xs es) ...)
                             (~Statement
                              `(const ,(~commas (for/list ([x xs] [e es])
                                                (if e (list x "=" e) x)))))])])  
  (VarBinding : VarBinding (vb) -> * ()
    [,x              (list x #f)]
    [(binding ,x ,e) (list x (Expr e))])
  (AnnotatedBody : AnnotatedBody (ab) -> * ()
    [(annotated-body (,x ...) ,σ ... ,e) (let ((σ (map Statement σ)) (e (Expr e)))
                                           (~braces σ (~Return e)))])
  (LeftHand : LeftHand (e) -> * ()
    [,x                x]
    [(dot ,e ,pn* ...) (let ((e (Expr e)))
                         (list (~parens e)
                               (map (λ (pn) (~brackets (list "\"" pn "\"")))
                                    pn*)))])

  (Expr : Expr (e) -> * ()
    [,x x]
    [(quote ,d)             (cond
                              [(string? d)  (~string d)]
                              [(boolean? d) (if d "true" "false")]
                              [(number? d)  (match d
                                              [+inf.0    "Infinity"]
                                              [-inf.0 "(-Infinity)"]
                                              [+nan.0         "NaN"]
                                              [else               d])]
                              [else (error 'generate-code "expected datum, got ~a" d)])]
    [(dot ,e ,pn)            (let ((e (Expr e)))
                               (if (regexp-match #rx"^([$_a-z][$_a-z0-9]*$)" (~a (syntax-e pn)))
                                   (list e "." pn)
                                   (list e (~brackets "\"" pn "\""))))]
    [(if ,e0 ,e1 ,e2)       (let ((e0 (Expr e0)) (e1 (Expr e1)) (e2 (Expr e2)))
                              (~parens (~parens e0 "===false") "?" e2 ":" e1))]
    #;[(if ,e0 ,e1 ,e2)       (let ((e0 (Expr e0)) (e1 (Expr e1)) (e2 (Expr e2)))
                              (~parens e0 "?" e1 ":" e2))]
    [(begin ,e ...)         (let ((e (map Expr e)))
                              (~parens (~commas e)))]
    ;; [(:= ,x ,e)             (let ((e (Expr e)))
    ;;                           (~parens x "=" e))]
    [(:= ,lh ,e)            (let ((e (Expr e)))
                              (~parens (LeftHand lh) "=" e))]
    ; Backward compatibility
    [(:= ,x ,e0 ,e1)        (let ((e0 (Expr e0)) (e1 (Expr e1)))
                              (~parens x (~brackets e0) "=" e1))]
    
    [(let ((,x ,e) ...) ,ab) (let ((e (map Expr e)) (ab (AnnotatedBody ab)))
                               (if (current-use-arrows-for-let?)
                                   (~parens (~parens (~parens (~commas x)) " => " ab)
                                            (~parens (~commas e)))
                                   (list (~parens (~parens "function" (~parens (~commas x)) ab)
                                                  (~parens (~commas e))))))]
    [(lambda (,x ...) ,ab)   (let ((ab (AnnotatedBody ab)))
                               (if (current-use-arrows-for-lambda?)
                                   (~parens (~parens (~commas x)) " => " ab)         ; ES6
                                   (~parens "function" (~parens (~commas x)) ab)))]  ; ES5
    [(await ,e)              (list "await " (Expr e))]
    [(spread ,e)             (list "..." (Expr e))]
    [(array ,e ...)          (~brackets (~commas (map Expr e)))]
    [(new ,x ,e ...)         (~parens "new" " " x (~parens (~commas (map Expr e))))]
    [(object (,pn* ,e*) ...) (parameterize ([current-use-arrows-for-lambda? #f])
                               ; Arrow notations does not work in object literals,
                               ; in the browser they get passed a `Window` instead
                               ; of the the correct this.
                               (~braces (~commas (for/list ([pn pn*] [e e*])
                                                   (list (~property-name pn) ":" (Expr e))))))]
    [(class (,x)
       [,pn* ,e*] ...)     (list "class " x                     (~braces (map ~ClassMethod pn* e*)))]
    [(class (,x0 ,x1)
       [,pn* ,e*] ...)     (list "class " x0 " extends " x1 " " (~braces (map ~ClassMethod pn* e*)))]
    [(ref ,e0 ,e1 ,e ...)  (define (pn? _)
                             (nanopass-case (L1 Expr) e1
                               [(quote ,d) (and (property-name? d) (not (number? d))
                                                (regexp-match #rx"^([$_a-z][$_a-z0-9]*$)"
                                                              (~a (if (syntax? d)
                                                                      (syntax-e d)
                                                                      d)))
                                                d)]
                               [else       #f]))
                           (let loop ([es (map Expr (list* e0 e1 e))])
                             (match es
                               [(list e0 (and (? pn?) (app pn? pn)))
                                (cond 
                                  [(and (identifier? e0)
                                        (identifier? pn))       (~a (mangle e0) "." pn)]
                                  [(identifier? pn)             (list e0 "." (~a pn))]
                                  [else                         (list e0 (~brackets (second es)))])] ; XXX (Expr e1)
                               [(list e0 e1)                    (list e0 (~brackets e1))]
                               [(list* e0 e1 e)                 (let ([e0-ref-e1 (loop (list e0 e1))])
                                                                  (loop (cons e0-ref-e1 e)))]
                               [_ (raise-syntax-error 'ref "internal error" e0)]))]
    [(app ,e0 ,e ...)      (cond
                             [(identifier? e0)
                              (define f e0)
                              (define sym             (syntax-e f))
                              (define (infix? _)      (infix-operator? f))
                              (define (assignment? _) (assignment-operator? f))
                              (define unary-error "internal error: unary operator missing")
                              (match (cons (syntax-e f)  (map Expr e))
                                [(list 'new    e0 e ...) (list "new " e0 (~parens   (~commas e)))]
                                [(list 'array! e0 i e)   (list e0 (~brackets i) "=" e)]
                                [(list (? nullary?))     (match sym ['+ 0] ['* 1]
                                                           [_ (error 'generate-code unary-error)])]
                                [(list (? infix?))       (raise-syntax-error
                                                          'generate-code
                                                          "operator is not unary")]
                                [(list (? unary?) e1)    (match sym
                                                           [(or '+ '* 'and 'or) (~parens e1)]
                                                           [_          (~parens f e1)])]
                                [(list (? infix?) e1)    (raise-syntax-error
                                                          'generate-code
                                                          "operator is not unary" e0)]
                                [(list (? binary?) e1 e2) (~parens (add-between (list e1 e2) f))]
                                [(list (? infix?)  e1 e2) (raise-syntax-error
                                                           'generate-code
                                                           "operator is not binary" e0)]
                                [(list (? nary?) e ...)  (~parens (add-between e f))]
                                [(list (? infix?) e ...) (raise-syntax-error
                                                          'generate-code "operator is not nary" e0)]
                                [(list (? assignment?) e0 e1)  (~parens e0 f e1)]          
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
(define-syntax !==     (λ (stx) (raise-syntax-error '!==     "used out of context" stx)))

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
                 ("?"."_p") ("!"."_e") (":"."_c") 
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
    #:literals (and or not = === !== bit-and bit-or bit-xor bit-not < > <= >= + - * / void null)
    ;; Take care of JavaScript operators first
    ;;  - assignment operators
    [ao:AssignmentOperator (symbol->string (syntax-e #'ao))]
    ;;  - infix operators
    [=       "==="]
    [===     "==="]
    [!==     "!=="]
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
    [void     "Void"]
    [null     "null"] ; TODO: does this affect the rjs compiler?
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
         (flatten-topblock
          (if (syntax? u)
              (parse u)
              u))))))))
  (if emit? (emit t) t))

(define (expand u)
  (unparse-Lur (parse u)))

(define (expand+ u)
  (unparse-L1
   (α-rename
    (annotate-bodies
     (annotate-module
      (desugar
       (flatten-topblock
        (parse u))))))))

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

(define (run js-tree [delete-tmp? (current-urlang-delete-tmp-file?)])
  (define tmp (make-temporary-file "tmp~a.js"))  
  (displayln (path->string tmp))
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
          (system (string-append "/usr/local/bin/node --use-strict"
                                 ; "/usr/local/bin/node --use-strict --harmony" ; for 7.6.0 (TCO)
                                 " " p))))))

;;;
;;; EVAL
;;;

(define (eval stx)
  (run
   (compile stx #f)))

;;;
;;; URLANG MODULES
;;;

;; Compiling an Urlang module m produces two files:
;;   m.js      -- the JavaScript program containing a Node module
;;   m.exports -- a list of symbols exported by m
;; The convention followed by Node is that exported identifiers are
;; exported through the variable  exports.
;; That is, if x is exported from the module m, then m.js contains
;;    exports.x = x
;; To import a Node module m .js in a JavaScript file:
;;    1)   M = require("m.js")    get object holding exported values
;;    2)   M.x  or  M["x"]        reference to the value exported under the name x
;; Urlang modules that import an identifier x from a module M will
;; prefix the program with
;;   M = require("m.js")
;;   x = M.x
;; and the body of the module can now reference the identifier simply as x.

(define (urmodule-name->exports name)
  (define exports-file (urmodule-name->exports-file-name name))
  (unless (file-exists? exports-file)
    (displayln (~a "current directory: " (current-directory)))
    (error 'urmodule-name->exports "exports file not found: \"~a\"" exports-file))
  (define exports (with-input-from-file exports-file read))
  (unless (and exports (list? exports) (andmap symbol? exports))
    (error 'urmodule-name->exports (~a "corrupt contents of exports file \"" exports-file
                                       "\" expected a list of symbols, but read: ~a")
           exports))
  exports)

;;;
;;; COMPILATION
;;;

; 
(define current-use-arrows-for-lambda?                (make-parameter #t))
(define current-use-arrows-for-let?                   (make-parameter #t))

(define current-urlang-output-file                    (make-parameter #f))
(define current-urlang-exports-file                   (make-parameter #f))
(define current-urlang-run?                           (make-parameter #f))
(define current-urlang-echo?                          (make-parameter #t)) ; XXX
(define current-urlang-console.log-module-level-expr? (make-parameter #f))
(define current-urlang-delete-tmp-file?               (make-parameter #t))
(define current-urlang-beautify?                      (make-parameter #f))
(define current-urlang-babel?                         (make-parameter #f))
(define current-urlang-rollup?                        (make-parameter #f)) ; See issue #13

(define (urmodule-name->file-name name extension)
  (match name
    [(? symbol? s) (~a s "." extension)]
    [(? string? s) s]
    [_ (error 'urmodule-name->file-name
              "Internal error: expected symbol or string")]))

(define (urmodule-name->js-file-name name)
  (urmodule-name->file-name name "js"))

(define (urmodule-name->es6-file-name name)
  (urmodule-name->file-name name "es6"))


(define (urmodule-name->exports-file-name name)
  (match name
    [(? symbol? s) (~a s ".exports")]
    [(? string? s) s]
    [_ (error 'urmodule-name->exports-file-name
              "Internal error: expected symbol or string")]))

(define (write-exports exports)
  (displayln (map syntax->datum exports)))

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
           (define name         (syntax-e #'mn))
           (define js-path      (or (current-urlang-output-file)  ; parameter can override module name
                                    (urmodule-name->js-file-name name)))
           (define es6-path     (or (current-urlang-output-file)  ; parameter can override module name
                                    (urmodule-name->es6-file-name name)))
           (define exports-path (or (current-urlang-exports-file) ; parameter can override
                                    (urmodule-name->exports-file-name name)))
           (define-values (tree exports)
             (parameterize ([current-exports '()])
               (values (compile #'urmod #f) (current-exports)))) ; #f = don't emit
           ;; Emit to either file.es6 (if Babel is used) or file.js 
           (define out-path (if (current-urlang-babel?) es6-path js-path))           
           (parameterize ([current-urlang-output-file out-path])
             (with-output-to-file out-path
               (λ () (emit tree))
               #:exists 'replace))
           ; TODO: Only append if not already in path
           (putenv "PATH" (string-append (getenv "PATH") ":/usr/local/bin/"))
           (when (current-urlang-babel?)
             ;; Run Babel on file.es6 and output to file.js
             ; TODO: This simply outputs the result - we need to replace the file.
             (system ; todo: remove full paths here
              (~a
               "/usr/local/bin/babel "
               "--presets '/usr/local/lib/node_modules/babel-preset-es2015,"
                          "/usr/local/lib/node_modules/babel-preset-react,"
                          "/usr/local/lib/node_modules/babel-preset-react-native'"
               ; "--presets '/usr/local/lib/node_modules/babel-preset-react-native' "
               " -o "js-path " " es6-path)))
           (when (current-urlang-beautify?)
             ;; Run the beautifier on file.js
             (system (~a "/usr/local/bin/js-beautify -r  -f " js-path)))
           ;; Write exports to file.exports
           (parameterize ([current-urlang-exports-file exports-path])
             (with-output-to-file exports-path
               (λ () (write-exports exports))
               #:exists 'replace))
           (when (current-urlang-echo?)
             ;; Display result on screen
             (with-input-from-file js-path
               (λ() (copy-port (current-input-port) (current-output-port)))))
           (when (current-urlang-run?)
             ;; Run the resulting program using Node
             (node/break js-path)))
         ...))]))
