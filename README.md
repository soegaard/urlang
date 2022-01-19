# URLANG

> Urlang is JavaScript with a sane syntax

[![Racket](https://img.shields.io/badge/-Racket-darkred?logo=racket)](https://racket-lang.org)
[![Discourse users](https://img.shields.io/discourse/users?label=Discuss%20on%20Racket%20Discourse&logo=racket&server=https%3A%2F%2Fracket.discourse.group)](https://racket.discourse.group/)
[![Racket Discord](https://img.shields.io/discord/571040468092321801?label=Chat%20on%20Racket%20Discord&logo=racket)](https://discord.gg/6Zq8sH5)

Urlang is a language designed to allow straightforward translation to JavaScript.
Think of Urlang as JavaScript with sane syntax and JavaScript semantics.
JavaScript in this context is short for ECMAScript 5 in strict mode.

Although the constructs of Urlang and JavaScript map almost one-to-one,
a little sugar was added:
  * function definitions allow default arguments
  * let expressions

Even though the syntax of Urlang is Racket-like, remember that the
semantics is standard JavaScript. This means in particular that tail calls
build context.


## Examples

The following examples are compiled using the `urlang` form.

    (urlang <module> ...)

The urlang form compiles the modules. The result of compiling
a module is saved a file whose path is the module-name with `.js`
added.

The `urlang` form is controlled by the following parameters:

    (current-urlang-run?                           #t)  ; compile and run (using node)
    (current-urlang-echo?                          #t)  ; print JavaScript to screen
    (current-urlang-console.log-module-level-expr? #t)  ; use conole.log to print module-level exprs


### Example (factorial)

     > (urlang
         (urmodule fact                                           ; module name
           (export fact)                                          ; fact is exported
           (import + - * = displayln ref console)
           (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
           (fact 5)))

The generated JavaScript:

     "use strict";
     function fact(n){return (((n===0)===false)?(n*(fact((n-1)))):1);};
     console.log((fact(5)));
     exports.fact=fact;

The output from Node:

     "120\n"


### Example (`cond`-macro and `array`)

Urlang macro transformers receive and produce standard Racket syntax objects.
This implies that standard tools such as syntax-parse are available.

Consider an Urlang version of `cond`:

    SYNTAX (cond [e0 e1 e2 ...] ... [else en]), 
       like Racket cond except there is no new scope 

 The urlang macro transformer is an standard (phase 0) Racket function.

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
             
 The macro can now we be used:
             
    > (urlang
        (urmodule sum-example
          (define (even? x) (=== (% x 2) 0))
          (var (sum 0) x (a (array 1 2 3 4 5)) (i 0) (n a.length))
          (while (< i n)
            (:= x (ref a i))
            (cond
              [(even? x)  (+= sum (ref a i))]
              [else       "skip"])
            (+= i 1))
          sum))

The generated JavaScript:

    "use strict";
    function even_p(x){return ((x%2)===0);};
    var sum=0,x,a=[1,2,3,4,5],i=0,n=a.length;
    while((i<n)){(x=a[i]);(((even_p(x))===false)?"skip":(sum+=a[i]));(i+=1);};
    console.log(sum);

The output from Node:

    "6\n"

# Overview

The heart of the system is a compiler written using the Nanopass
compiler Framework. The compiler is exported as a function

    compile : urlang-module -> JavaScript

that compiles an urlang module and produces JavaScript,
that can be evaluated by the Node.js platform (or be embedded in a web page).

The Urlang module to be compiled can be represented 

   1. as a syntax object
   2. as a Nanopass structure (representing an Lurlang program)

Use 1) to program in Urlang directly.

Use 2) if you intend to use Urlang as a compiler backend.

[Note: Nanopass is a framework for implementing compilers.]

The intended use of Urlang is to use 1) to write (generate) a Racket runtime in JavaScript.
The middle-end of the Racket-to-JavaScript compiler will produce output as Nanopass
structures, so 2) will be used as the backend for the Racket-to-JavaScript compiler.

Internally the function expand

    expand : syntax -> LUrlang

will parse and expand its input and produce an LUrlang representation.

Note that `expand` allows the user to extend the input language
using define-urlang-macro. An Urlang macro is a syntax to syntax
transformation implemented as a normal Racket function.
This allow you to use all of the standard Racket macro machinery.

Main functions:

    expand : syntax -> Lurlang
        expand the input and produce a fully expanded Urlang program
        represented as a Lurlang structure
        
    compile : syntax ->
        Expand and compile. The output is written to standard out.
        
    eval : syntax -> value
        expand, compile and run the input (an Urlang module represented as a syntax object)
        Running means that `node` is used to run the generated JavaScript.

Having Urlang as a `#lang` language allows

 * macros (using full Racket at compile time)
 * export of defined names
 * easier testing

In the grammar below:

  - `x` stands for a non-keyword identifier
  - `f` stands for an identifier defined as a function

````
 <module>            ::= (urmodule <module-name> <module-path> <module-level-form> ...)

 <module-level-form> ::= <export> | <import> | <definition> | <statement> 
 <export>            ::= (export x ...)
 <import>            ::= (import x ...)
 <definition>        ::= (define (f <formal> ...) <body>)
                      |  (define x <expr>)
 <formal>           ::= x | [x <expr>]

 <statement>         ::= <var-decl> | <block> | <while> | <do-while> | <if> | <break> | <expr> 
 <var-decl>          ::= (var <var-binding> ...)
 <block>             ::= (block <statement> ...)
 <var-binding>       ::= x | (x e)
 <while>             ::= (while <expr> <statement> ...)
 <do-while>          ::= (do-while <expr> <statement> ...)
 <if>                ::= (sif <expr> <statement> <statement>)
 <break>             ::= (break) | (break label)

 <body>              ::= <statement> ... <expr>

 <expr>              ::= <datum>   | <reference> | <application> | <sequence>
                      |  <ternary> | <assignment> | <let> | <lambda> | <dot>
 <ternary>           ::= (if <expr> <expr> <expr>)
 <reference>         ::= x
 <application>       ::= (<expr> <expr> ...)
 <sequence>          ::= (begin <expr> ...)
 <assignment>        ::= (:= x <expr>)
 <let>               ::= (let ((x <expr>) ...) <statement> ... <expr>)
 <lambda>            ::= (lambda (<formal> ...) <body>)

 <keyword>           ::= define | begin | urmodule | if | := | ...se code...

 <datum>             ::= <fixnum> | <string> | #t | #f

 <identifier>     an identifier that is not a keyword
 <fixnum>         an integer between -2^53 and 2^53
 <module-name>    a symbol or string
````

# NOTES

Some application are special cases:

    (ref e0 e1)     becomes  e0[e1]
    (ref e0 "str")  becomes  e0.str    
    (array e ...)   becomes  [e,...]

Property access with dot notation is rewritten to use bracket syntax in the parser.

Example:  `object.property` becomes `object["property"]`


# SEMANTICS

### `(if e0 e1 e2)`
If `e0` evaluates to value strictly equal to false, then `e2` otherwise `e1`.
Note: The JavaScript becomes  `((e0===false) ? e2 : e1)`

### `(var x (y 3))`
Binds `x` to undefined and `y` to `3`.
