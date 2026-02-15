#lang at-exp racket
;;; React is a JavaScript library for building user intefaces.
; todo: can we keep urlang-expression as a syntax object?

; https://facebook.github.io/react/docs/tutorial.html

(require urlang urlang/html urlang/extra "../jsx.rkt")
(require net/sendurl)

(current-urlang-run?                           #f) ; run using Node?              
(current-urlang-echo?                          #t) ; print generated JavaScript?  
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?  
(current-urlang-beautify?                      #t) ; invoke js-beautify 

;;;
;;; LIBRARIES
;;;

(define jquery-js
  "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")
(define react-js
  "https://cdnjs.cloudflare.com/ajax/libs/react/15.0.0-rc.2/react.js")
(define react-dom-js
  "https://cdnjs.cloudflare.com/ajax/libs/react/15.0.0-rc.2/react-dom.js")

;;;
;;; HTML
;;;

(define (properties->clauses props)
  (match props
    [(list name: val more ...)
     (let ([name (string-trim (~a name:) ":" #:right? #t)])
       (list* name val
              (properties->clauses more)))]
    [_ '()]))

(require (for-syntax syntax/parse))
(define-syntax (define-components stx)
  (syntax-parse stx
    [(_ name ...)
     (with-syntax ([(name-str ...) (map symbol->string (syntax->datum #'(name ...)))])
       #'(begin
           (define (name . args)
             (define-values (atts body) (attributes+body args))
             (define clauses (map (λ (p) (list (~a (car p)) (cdr p))) atts))
             @ur[(React.createElement name (object ,@clauses) (array ,@body))])
           ...))]))

; TODO: the problem is that jsx calls eval and at that point Comment is not defined yet. Or...

(define-components Comment
  CommentBox
  CommentList 
  CommentForm
  )

#;(define (Comment . args)
    (define-values (atts body) (attributes+body args))
    (define clauses (map (λ (p) (list (~a (car p)) (cdr p))) atts))
    @ur[(React.createElement "Comment" (object ,@clauses) (array ,@body))])

(define page
  @html{@head{@title{React}
              @script[src: jquery-js]
              @script[src: react-js]
              @script[src: react-dom-js]}
        @body{@h1{React Example}
              @p{React is a JavaScript library for building user interfaces}
              @div[id: "example"]
              @script[src: "simple.js"]}})

;; Save the page to disk
(with-output-to-file "simple.html"
  (λ () (displayln (~x page)))
  #:exists 'replace)


;;;
;;; JavaScript
;;;

(urlang
 (urmodule simple ; saved in simple.js
   (import $ setTimeout req React ReactDOM this)
   ;;;
   ;;; jQuery
   ;;;
   (define ($0 selector) (ref ($ selector) 0))

   ;;;
   ;;; Components
   ;;;
   
   (define Comment
     (React.createClass
         (object [render (λ () @jsx[@div[className: "comment"]{
                                      @h2[className: "commentAuthor"]{
                                        @ur[(begin (console.log (+ "X" this.props.author)) this.props.author)]}
                                      @ur[this.props.children]}])])))
   (define CommentForm
     (React.createClass
         (object [render (λ () @jsx[@div[className: "commentForm"]{
                                      Hello, world! I am a CommentForm.}])])))
   (define CommentList
     (React.createClass
         (object [render (λ () (console.log "XXX")
                           @jsx[@div[className: "commentList"]{
                                      @Comment[author: "Pete Hunt"]{This is one comment2}
                                      @Comment[author: "Jordan Walke"]{This is another
                                                        comment2}}])])))
   (define CommentBox
     (React.createClass
         (object [render (λ () (console.log "YYY")
                           @jsx[@div[className: "commentBox"]{
                                      @h1{Comments}
                                                   }])])))   
   ;;;
   ;;; Instantiate Components
   ;;;
   (ReactDOM.render (React.createElement CommentBox) ($0 "#example"))))

 
;(define (start) (send-url/file "react.html"))
;(displayln "Enter (start) in the repl to open the first on first run.")

(send-url/file "simple.html")