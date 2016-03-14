#lang racket

;;; Original Ractive Example

;; This example by Daniel Prager shows how to use the Ractive
;; framework with Urlang. This version uses x-expressions
;; and the html-writing package to generate the html.
;; Compare with the version in "ractive.rkt"

(require html-writing urlang net/sendurl)

(current-urlang-run?                           #f) ; run using Node?
(current-urlang-echo?                          #f) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?

(define (ractive template js-file)
  (xexp->html
   `((html (head (title "Ractive test")
                 (script (@ (src "http://cdn.ractivejs.org/latest/ractive.min.js"))))
          (body
           (div (@ (id "container")))
           (script (@ (id "template") (type "text/ractive"))
                   ,@template)
           (script ,(file->string js-file)))))))

(urlang
 (urmodule ractive
           (import Ractive)
           (var [ractive (new Ractive
                              (object [el "#container"]
                                      [template "#template"]
                                      [data (object [name "Racket"])]))])))

(send-url/contents
 (ractive '((h1 "Ractive test")
            (p "Hello, {{name}}!"))
          "ractive.js"))
