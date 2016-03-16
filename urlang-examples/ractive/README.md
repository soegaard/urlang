Use Ractive with Urlang
=======================

Ractive is a template-driven UI library.
The simple idea is to give Ractive a template and some data.
Ractive then generates HTML that can replace an existing DOM node.

These examples show how to use the Ractive JS library from Urlang.
Ractive is a lightweight alternative to Angular, React, etc.

The examples are based on  http://www.ractivejs.org/60-second-setup
Thanks to Daniel Prager for the original Racket versions.

ractive-bootstrap-example.rkt
-----------------------------
- uses x-expressions and html-writing to generate html
- uses Urlang to generate JavaScript
- uses Bootstrap for css

ractive-bootstrap-example.html
------------------------------
Pretty-printed output (using js-beautify --html) of ractive-bootstrap-example.rkt

ractive-original.rkt
--------------------
- uses x-expressions and html-writing to generate html
- uses Urlang to generate JavaScript

ractive.rkt
-----------
- uses web-server/templates  to generate html
- uses Urlang to generate JavaScript


    