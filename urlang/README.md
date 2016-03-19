Urlang
======
Urlang is a language that compiles to strict JavaScript (ES5 aka EcmaScript 5).
The core constructs of Urlang matches the constructs in JavaScript almost one-to-one.

Here are the differences:
  - References to undeclared identifiers signal an error at compile time
    (use DrRacket and you can jump directly to the error)
  - The names identifiers are free to use:  -, *, + etc.
    Names are "mangled" to produce legal JavaScript names in the output.
    For example:  foo-bar becomes foo_bar.
    See the definition of mangle in main.rkt for details.
  - Functions allow optional arguments.
  - Comparisions with = produces === in the JavaScript code.
  - if counts 0 as a true value

The output of an Urlang program should always be a legal JavaScript program.
If you encounter a program that produces an illformed JavaScript program,
please file a bug.

Besides the core constructs, you have the option of adding your own
constructs with define-urlang-macro. See "extra.rkt" for examples.


main.rkt
--------
Usage: (require urlang)

This contains the implementation of the urlang form.
All core urlang forms are implemented here - so look
here for the exact syntax of forms.

The urlang form compiles urmodule forms into JavaScript.
The compiler uses the Nanopass framework.

extra.rkt
---------
Usage: (require urlang/extra)

Contains a few extra urlang forms implemented as macros.
These include the expressions:
  begin0, when, unless, cond, case, letrec, let*
And the statements:
  swhen, sunless, scond

for.rkt
-------
Usage: (require urlang/for)

This implements a racket-like for construct.
Parallel loops:
  for, for/array, for/or for/and, for/sum, for/product
Nested loops:
  for*, ...
Clauses include:
  in-range, in-naturals, in-value, in-array, in-string
See for.rkt for precise syntax
  
html.rkt
--------
Usage: (require urlang/html)

Create html using @-syntax.
Purpose orthogonal to Urlang as such.
Feel free to use whatever way of producing html as you like.


