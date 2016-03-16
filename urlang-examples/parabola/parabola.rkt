#lang at-exp racket

;;;
;;; INTRODUCTION
;;;

;; This examples shows:
;;     - how to construct html using Scribble syntax
;;     - use the Bootstrap HTML/CSS framework
;;     - use Urlang to handle events

;;;
;;; HTML
;;;

;; There a lot of options available for constructing html from Racket.
;;   - plain strings can represent html
;;   - x-expressions (an s-expression representation of html)
;;   - templates from web-server/templates
;;   - xml using the representation from scribble/html

;; Here we will give an example of the last option.
;; The scriblle/html collection offers a nice was of writing html
;; using the @-syntax.

;; Example:
;;           > @h1[class: "red" id: "my-title"]{A Title}
;;           (element 'h1 '((class . "red") (id . "my-title")) '("A Title") #f)

;; Those familiar with Scribble will notice that class: and id: are unbound
;; identifiers, and therefore ought to give errors. The scribble/html
;; library fortunately offers a way to turn these class: and id:
;; into 'class and 'id automatically.


;;; Use Scribble syntax to construct xml trees

(require urlang/html)

;;; Conveniency

; In DrRacket (send-url/contents some-html) will display the 
; html in a browser - saves time.

(require net/sendurl)

;;;
;;; Urlang Configuration
;;;

(require urlang)

(current-urlang-run?                           #f) ; run using Node?              No, use browser
(current-urlang-echo?                          #t) ; print generated JavaScript?  
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?  


;;; Urls for JavaScript and CSS libraries used in this example.

(define bootstrap-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css")
(define bootstrap-js  "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js")
(define jquery-js     "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")
(define html5shiv-js  "https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js")
(define respond-js    "https://oss.maxcdn.com/respond/1.4.2/respond.min.js")

(define (script url)   @~a{<script src=@url ></script>})    
(define (link-css url) @~a{<link href=@url rel="stylesheet">})

(define (generate-html body urlang-js-file)
  ;; Given a body (a string) wrap it in a basic html template
  ;; from the Bootstrap project:
  ;;     http://getbootstrap.com/getting-started/#template
  @~a{
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head;
         any other head content must come *after* these tags -->
    <title>Bootstrap 101 Template</title>

    <!-- Bootstrap -->
    @link-css[bootstrap-css]

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!-- WARNING: Respond.js doesn't work if you view the page via file:// -->
    <!--[if lt IE 9]>
    @script[html5shiv-js]
    @script[respond-js]
    <![endif]-->
  </head>
  <body>
    @body
    <!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
    @script[jquery-js]
    <!-- Include all compiled plugins (below), or include individual files as needed -->
    @script[bootstrap-js]
    <script>@file->string[urlang-js-file]</script>
  </body>
</html>})


;;; Bootstrap conainers, fluid containers, and, rows.

; A container has a fixed width
(define (container . contents)       @div[class: "container"]{@contents})
; A fluid container can span the entire width of your viewport
(define (fluid-container . contents) @div[class: "container-fluid"]{@contents})
; A row must be place in a (possibly fluids) container
(define (row . columns)              @container[@div[class: "row"]{@columns}])

;;; The navigation bar

; This navigation is from the Bootstrap jumbotron example
;     http://getbootstrap.com/examples/jumbotron/
(define (navigation-bar)
  (list
   @comment{NAVIGATION BAR}
   @nav[class: "navbar navbar-inverse navbar-fixed-top"]{
    @fluid-container[
      @div[class: "navbar-header"]{
       @comment{Bootstrap supports only button elements in a navbar}
       @button[type:          "button"
               class:         "navbar-toggle collapsed"
               data-toggle:   "collapse"
               data-target:   "#navbar"
               aria-expanded: "false"
               aria-controls: "navbar"]{
         @span[class: "sr-only"]{Toggle navigation}
         @comment{The following three spans will produce an hamburger on narrow screens}
         @span[class: "icon-bar"]{}
         @span[class: "icon-bar"]{}
         @span[class: "icon-bar"]{}}
       @a[class: "navbar-brand" href: "#"]{Urlang}}
      @div[id: "navbar" class: "navbar-collapse collapse"]{}]}))

(define (sample-heading msg)
  ; Here the 4 means: use 4/12 of the screen width. Since 4/12=1/3 we get 3 columns.
  @div[class: "col-md-4"]{
    @h2{@msg}
    @p{Donec id elit non mi porta gravida at eget metus.
       Fusce dapibus, tellus ac cursus commodo, tortor mauris condimentum nibh,
       ut fermentum massa justo sit amet risus.
       Etiam porta sem malesuada magna mollis euismod. Donec sed odio dui.}
    @p{@a[class: "btn btn-default" href: "#" role: "button"]{View details}}})

(define (jumbotron)
  (list
   @comment{JUMBOTRON}
   @comment{Main jumbotron for a primary marketing message or call to action}
   @div[class: "jumbotron"]{
     @container[@h1{Urlang!}
                @p[class: "lead"]{Use Bootstrap with Urlang.}
                @p{This example shows how to use Bootstrap. The html is generated using the at-syntax.
                   Urlang is used to handle events: compute solutions and draw the grah}
                @p{@a[class: "btn btn-primary btn-lg" href: "#" role: "button"]{Learn more}}]}))

(define (the-footer)
  (list @comment{FOOTER}
        @hr[]
        @footer{@p{The Urlang Team}}))

(define (enter-coeffecients)
  @div[class: "col-md-4"]{
    @h2{Coefficients}
    @p{Enter a, b, and, c below.}
    @p{a: @input[id: "input_a" type: "text" value:  "1"] @br[]
       b: @input[id: "input_b" type: "text" value: " 0"] @br[]
       c: @input[id: "input_c" type: "text" value: "-4"]}
    
    @comment{@p{@a[class: "btn btn-default" onclick: "handle_update()"]{Solve}}}})

(define (quadratics-solver)
  @div[class: "col-md-4"]{
    @h2{Roots}
    @p{Equation: ax^2 + bx+c = 0}
    @p{@span[id: "solutions"]}})

(define (graph)
  @div[class: "col-md-4"]{
    @h2{Graph}
    @canvas[id: "graph-canvas" width: 200 height: 200
            style: "border:1px solid"]})

(define (row-of-small-headers)
  (list
   @comment{ROW OF COLUMNS}
   @row[(enter-coeffecients)
        (quadratics-solver)
        (graph)]))


;;; Generate JavaScript

;; The urlang will generate a JavaScript file.
;; The JavaScript will be activated in then the user
;; clicks the compute button.

(require urlang/extra ; make cond  available in urlang
         urlang/for)  ; make "for" available in urlang

(urlang
 (urmodule parabola
   (import $ val)                ; from jquery
   (import parseInt isNaN Math)  ; from javascript

   ; get jquery objects referring to the input fields
   (var [ia ($ "#input_a")] [ib ($ "#input_b")] [ic ($ "#input_c")])
   ; and the span used to display the solutions
   (var [out ($ "#solutions")])

   ; make keyup events in the input fields update solutions and graph
   (ia.keyup handle-update)
   (ib.keyup handle-update)
   (ic.keyup handle-update)
   
   (define (get i) ; i is a jquery object referring to input field
     (var [x (parseInt (i.val) 10)])
     (if (isNaN x) 0 x)) ; handle non-numbers in text field
   
   (define (handle-update)
     ; Use the JavaScript console in your browser to see the messages
     (console.log "on-compute-clicked was clicked")
     ; get the numerical values
     (var [a (get ia)] [b (get ib)] [c (get ic)])
     (out.text (render (solutions a b c)))
     (graph (ref ($ "#graph-canvas") 0) a b c))

   (define (set-keyup-handlers i)
     ; i is a jquery object referring to input field
     (i.keyup handle-update))
   
   (define (solutions a b c)
     (var [d (- (* b b) (* 4 a c))])
     (cond
       [(< d 0) (array)]
       [(= d 0) (array (/ (- b) (* 4 a)))]
       [#t      (array (/ (+ (- b) (- (Math.sqrt d))) (* 2 a))
                       (/ (+ (- b)    (Math.sqrt d))  (* 2 a)))]))
   (define (render xs)
     (var [n xs.length])
     (cond
       [(= n 0) "No solutions"]
       [(= n 1) (+ "One solution:  x = " (ref xs 0))]
       [(= n 2) (+ "Two solutions: x = " (ref xs 0) "  or  x = " (ref xs 1))]))

   (define (graph canvas a b c)
     ; canvas:   0<x<200 and   0<y<200
     ; math:   -10<x< 10 and -10<y< 10
     (var [f (λ (x) (+ (* a x x) (* b x) c))]
          [m (λ (x) (/ (- x 100) 10))]  ; m = to math coordinate
          [w (λ (x) (+ (* x 10) 100))]) ; w = to window coordinate
     ; get the drawing context
     (var [ctx (canvas.getContext "2d")])
     ; add 0.5 to avoid blurry lines (sigh)
     (var [line-to (λ (x y) (ctx.lineTo (+ x 0.5) (+ (- 200 y) 0.5)))]
          [move-to (λ (x y) (ctx.moveTo (+ x 0.5) (+ (- 200 y) 0.5)))])
     ; clear
     (ctx.beginPath)
     (ctx.moveTo 0 0)
     (:= ctx "fillStyle"   "#FFFFFF") ; white
     (:= ctx "strokeStyle" "#FFFFFF")
     
     (ctx.clearRect 0 0 200 200)
     (ctx.stroke)
     ; axes
     (ctx.beginPath)
     (:= ctx "fillStyle"   "#000000") ; black
     (:= ctx "strokeStyle" "#000000")
     (move-to   0 100) (line-to 200 100)
     (move-to 100   0) (line-to 100 200)
     (ctx.stroke)
     ; graph
     (ctx.beginPath)
     (:= ctx "fillStyle"   "#FF0000") ; red
     (:= ctx "strokeStyle" "#FF0000")
     (move-to 0 (w (f (m 0))))
     (for ([x in-range 0 200])
       (line-to x (w (f (m x)))))
     (ctx.stroke))
   ; call update when the script loads to refresh graphs
   (handle-update)))

;;; Test it in the browser

(send-url/contents
 (generate-html
  @~xs{ @navigation-bar[]
        @jumbotron[]
        @comment{Use a container to make the footer and the row the same width}
        @container[@row-of-small-headers[]
                   @the-footer[]]}
  "parabola.js"))
