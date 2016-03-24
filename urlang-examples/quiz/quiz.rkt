#lang at-exp racket
;;;
;;; Quiz
;;;

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
(current-urlang-beautify?                       #t) ; invoke js-beautify 

;;; Urls for JavaScript and CSS libraries used in this example.

;(define bootstrap-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css")
(define bootstrap-css
  "https://cdnjs.cloudflare.com/ajax/libs/bootswatch/3.3.6/cerulean/bootstrap.min.css")
(define bootstrap-js  "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js")
(define jquery-js     "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")
(define html5shiv-js  "https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js")
(define respond-js    "https://oss.maxcdn.com/respond/1.4.2/respond.min.js")
(define mathjax-js   "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")

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
    <!-- Inline CSS -->
    <!-- The body padding-top rule fixes overlap between navigation bar and content -->
    <!-- See http://stackoverflow.com/a/13051843/23567 -->
    <style>
      body { padding-top: 70px; }
    </style>

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
    @script[mathjax-js]    
  </body>
</html>})


;;; Bootstrap containers, fluid containers, and, rows.

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
       @a[class: "navbar-brand" href: "#"]{The Racket Quiz}}
      @div[id: "navbar" class: "navbar-collapse collapse"]{}]}))


(define (html-the-footer)
  (list @comment{FOOTER}
        @hr[]
        @footer{@p{Jens Axel Søgaard, 2016}}))

(define cards
  '(("Short name for: How to Design Books"              "HtDP")
    ("Racket lists are ______ linked lists"             "singly")
    ("The Racket IRC is on ________"                    "freenode")
    ("Racket was formerly known as ___ Scheme"          "PLT")
    ("A list with no elements is called the _____ list" "empty")
    ("Are lists mutable?"                               "no")
    ))

(define  (card->html card)
  (match card
    [(list question answer)
     @div[@div[@h2{Question} @p[question]]
          @div[answer]]]))

(define (html-flash-card)
  ; style: "border:1px solid black"
  @div[class: "col-md-6" ]{
    @div[id: "flash-card-content"]{
      @div[id: "flash-card-question-container"]{
        @div[id: "flash-card-question"]{
          @h2{Flash Card}
          @p{The name of the mathematical exponentiation operator?}
          @p{ $$ c = \sqrt{a^2+b^2} $$}}}
      @html-answer-box[]}})

(define (html-answer-box)
  (list
    @h3{Answer}
    @p{ @input[id: "answer-input" type: "text" value: "" placeholder: "Type here" tabindex: "1"]}))

(define (html-flash-card-row)
  (list @comment{FLASH CARD ROW}
        @div[class: "col-md-3"]{ }
        @html-flash-card
        @div[class: "col-md-3"]{ }))

;;; Generate JavaScript

;; The urlang will generate a JavaScript file.
;; The JavaScript will be activated in then the user
;; clicks the compute button.

(require urlang/extra ; make cond  available in urlang
         urlang/for)  ; make "for" available in urlang


(urlang
 (urmodule quiz
   (import $        ; from jQuery
           window)  ; from the browser

   ; A state is one of:
   ;   "wait-for-answer"
   ;   "answer-was-correct"
   ;   "answer-was-incorrect"
   (define state "wait-for-answer")
   (define current-card 0)

   ;;; The static dom elements
   (define $answer-input       ($ "#answer-input"))    ; with jQuery wrapper
   (define  answer-input       (ref $answer-input 0))  ; without
   (define $question-container ($ "#flash-card-question-container"))
   (define  question-container (ref $question-container 0))
   
   (define (on-next-question)
     (console.log "Next!")
     (:= current-card (% (+ current-card 1) the-cards.length))
     (render-flash-card current-card))

   (define (on-answer-input-enter e)
     ; called when user hits enter
     (var [got      answer-input.value])
     (var [expected (card-number->answer current-card)])
     (cond
       [(= (dot (got.trim) (toLowerCase)) (expected.toLowerCase))
        ;; signal correct answer: background green, then continue
        ($answer-input.css "background-color" "#dff0d8")
        (window.setTimeout on-next-question 500)
        ($answer-input.attr "placeholder" "type here")]
       [else
        ;; signal incorrect answer: background red, reveal answer
        ($answer-input.css "background-color" "#f2dede")
        ($answer-input.val "")
        ($answer-input.attr "placeholder" expected)]))
   
   ;;; Render Flash Card
   (define (render-flash-card card-number)
     (var [$card  ($ (card-number->question card-number))])
     ;; replace the question 
     ($question-container.empty)
     ($question-container.prepend ($card.clone))
     ;; clear the answer input
     ($answer-input.val ""))

   (define (card-number->answer n)
     (var [card-div    (ref the-cards n)])
     (var [answer-node (ref (dot ($ card-div) (children)) 1)])
     (var [answer-text (dot ($ answer-node) (text))])
     answer-text)

   (define (card-number->question n)
     (var [card-div (ref the-cards n)])
     (ref (dot ($ card-div) (children)) 0))   

   ;; Register input handlers
   (dot ($ "#answer-input")
        (keyup (λ (e) (when (= e.keyCode 13) ; enter key
                        (on-answer-input-enter e)))))

   ;; Set focus to input
   (dot ($ "#answer-input") (focus))

   ;; Cards
   (var [the-cards (dot ($ "#card-db") (children))])
   
   ;; Render initial card
   (render-flash-card current-card)
   ))

;;; Test it in the browser

(send-url/contents
 (generate-html
  @~xs{ @navigation-bar[]
        @comment{Use a container to make the footer and the row the same width}
        @container[@html-flash-card-row]
        @container[@html-the-footer]
        @comment{This div has a hidden list of all question-answer pairs}
        @div[id: "card-db" style: "display: none"
             (map card->html cards)]}
  "quiz.js"))
