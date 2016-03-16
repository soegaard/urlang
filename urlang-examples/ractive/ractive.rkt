#lang at-exp racket

;;;
;;; Ractive 
;;; 

;;; Ractive is a template-driven UI library.
;;; The simple idea is to give Ractive a template and some data.
;;; Ractive then generates HTML that can replace an existing DOM node.

;;; This example shows how to use the Ractive JS library from Urlang.
;;; Ractive is a lightweight alternative to Angular, React, etc.
;;; The example is based on  http://www.ractivejs.org/60-second-setup
;;; Thanks to Daniel Prager for the original Racket version.


;;; The following Racket libraries are needed
(require urlang                ; to generate JavaScript
         web-server/templates  ; to generate html
         net/sendurl)          ; to send the html to the browser

;;; First, configure Urlang:
(current-urlang-run?                           #f) ; run using Node? no (we are using a browser)
(current-urlang-echo?                          #t) ; print generated JavaScript? yes
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression? no need

;;;
;;; HTML
;;;

; Consider the html file below.
; The contents of  <div id="container"/>  will be replaced
; by the result of combining a template and some data at runtime.

; The Ractive template needs to be stored somewhere in the DOM.
; Here it is stored in the id=template node.

; The Ractive template could be:
;    <p>Hello, {{name}}!</p>
; If the data object given to Ractive contains a name field,
; then the {{name}} will be filled in with the corresponding value.

(define (generate-html ractive-template)
  @~a{<!doctype html>
      <html lang="en-US">
        <head> <meta charset="utf-8"/>
               <title>Ractive Test</title>
               <script src="http://cdn.ractivejs.org/latest/ractive.min.js"></script>
        </head>
        <body>  <h1>Hello Racketeers!</h1>
                <div id="container"/>
                <script id="template" type="text/ractive">
                    @ractive-template
                </script>
                <script>@ractive.js </script>
        </body>
      </html>})

;;;
;;; Urlang / JavaScript
;;;

; In order for Ractive to fill in the template,
; Ractive needs to be activated. This small script,
; will create an Ractive object and pass on the data object:
;   {name: "Racket"}
; The effect will be that "Racket" wil replace {{name}} in the template.


;; The form (urlang ...) compiles all urmodules into JavaScript files.
(urlang
 (urmodule ractive    ; save to ractive.js
   (import Ractive)   ; declare Ractive
   (var [ractive (new Ractive                            ; call Ractive constructor
                      (object [el       "#container"]    ; el can be a node, an ID, or a CSS selector
                              [template "#template"]     ; id of the <script> tag above
                              [data     (object
                                         [name "Racket"])]))])))

;; Read JavaScript from file.
;; Note: Normally one would use <script src="ractive.js"/> in the html.
;;       Here we want an example, that can run without a web-server.
;;       So we simply read in the JavaScript from disk.
(define ractive.js (file->string "ractive.js"))

;; It is not time to call generate-html.
;; The html could be saved to a file and then manually opened in a browser.
;; Under development it is convenient to use send-url/contents to send
;; the web-page directly to the browser.

(send-url/contents
 (generate-html
  @~a{<p>Hello, {{name}}!</p>}))

