HTML related utilities
======================

This folder contains a bunch of utilities for creating html-files.
If you want to use all of them at once, use

    (require urlang/html)

otherwise just use, say, (require urlang/html/at-html).

main.rkt
--------
Imports and reexports all other utilies in this folder.

at-html.rkt
-----------
This file makes it possible to construct xml and html
using the scrible at-syntax.

Example:    @h1[class: "red"]{A Title}
         will produce an xml value representing
            <h1 class="red">A Title</h1>

Use ~x  to turn a single  xml value  into a string.
Use ~xs to turn a list of xml values into a string.

