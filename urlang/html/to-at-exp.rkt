#lang racket
(provide xexp->at-exp    ; convert an xexp into an at-exp
         html->at-exp)   ; convert a string with html into an at-exp

;;; This file implements two utilities that output at-expressions representing html.
;;; Example:
;;;    > (displayln (html->at-exp "<div id='foo' color='red'><h1>Bar</h1> Baz</div>") )
;;;    @div[id: "foo" color: "red"]{@h1{Bar} Baz}

;;; The intended usage is to convert snippets of html examples found on the web
;;; to at-expressions which work with urlang/html (or Pollen for that matter).

(require html-parsing)

; xexp->at-exp : xexp -> string
;  convert an xexp to an at-exp
(define (xexp->at-exp x)
  (emit (xexp->at-exp-tree x)))

; html->at-exp : string -> string
;  convert a string containing html to an at-exp string
(define (html->at-exp x)
  (xexp->at-exp (html->xexp x)))


; emit : tree -> string
;   display all elements of the tree x into a string, and return it
(define (emit x)
  (define (loop x)
    (match x
      [(list x ...) (map loop x)]
      [x            (display x)]))
  (with-output-to-string
      (λ () (loop x))))


; xexp->at-exp-tree : xexp -> tree
;   build a tree that when emitted will give an at-exp that is equivalent to the xexp
(define (xexp->at-exp-tree x)
  (match x
    [(list '*TOP* more ...) (unparse-top x)]
    [_ (error 'xexp->at-exp "expected (*TOP* <annotation>? <PI>* <comment>* <Element>)")]))

; unparse-top : xexp -> tree
(define (unparse-top x)
  ; <TOP> ::= ( *TOP* <annotations>? <PI>* <comment>* <Element> )
  ; NOTE: <PI> are ignored
  (match x
    [(list '*TOP* (? annotation? a*) ... comment* ... element)
     (list (map unparse-annotation a*)
           ; (map unparse-comment comment*)
           (cond
             [(element? element) (unparse-element element)]
             [else               (unparse-child-of-element element)]))]))

(define (annotation? x)
  ; Note: Annotations beside DECL are ignored.
  (match x [(list '*DECL* _ ...) #t] [_ #f]))

(define (unparse-annotation x)
  (match x
    [(list '*DECL* more ...)
     (list "<!" (add-between more " ") ">\n")]))

(define (comment? x)
  (match x [(list '*COMMENT* _ ...) #t] [_ #f]))

(define (unparse-comment x)
  (match x
    [(list '*COMMENT* comment-string)
     (list "@comment{" comment-string "}")]))

(define (entity? x)
  (match x [(list '*ENTITY* public-id system-id) #t] [_ #f]))

(define (unparse-entity x)
  (match x
    [(list '*ENTITY* public-id system-id)
     (error 'todo-entity)]))

(define (element? x)
  (match x [(list name more ...) #t] [_ #f]))

(define (unparse-element x)
  ; <Element> ::= ( <name> <annot-attributes>? <child-of-element>* )
  (match x
    [(list name {list '@ attribute* ...} child ...)
     (define atts*  (unparse-attributes attribute*))
     (define child* (map unparse-child-of-element child))
     (if (null? child)
         (list "@" name "[" atts* "]")
         (list "@" name "[" atts* "]" "{" child* "}"))]
    ; no annotated attributes:
    [(list name child ...)
     (define child* (map unparse-child-of-element child))
     (list "@" name "{" child* "}")]    
    ))

(define (unparse-attributes xs)
  (add-between (map unparse-attribute xs) " "))

(define (unparse-attribute x)
  (match x
    [(list name)       (list name ":")]
    [(list name value) (list name ": \"" value "\"")]))

(define (unparse-child-of-element x)
  ; <Element> | "character data" | <PI> | <comment> | <entity>
  (match x
    [(? string? ) x] ; "character data"
    [(? comment?) (unparse-comment x)]
    [(? entity?)  (unparse-entity x)]
    [(list '*PI* pi-target annotation ... prossing-instruction-content-string)
     (error 'todo-pi)]
    [_ (unparse-element x)]))
