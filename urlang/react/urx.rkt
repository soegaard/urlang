#lang at-exp racket
;;;
;;; URX
;;;

; This implements urx expressions.
; What jsx expressions are to JavaScript, urx expressions are to Urlang.
; JSX expressions are an extension to JavaScript that makes it easy
; to generate React elements (representing dom elements) at runtime.

; This file (for now) contains both urx expressions.

(require urlang urlang/html
         ; (except-in xml make-element entity element comment cdata attribute? attribute)
         syntax/parse (for-syntax syntax/parse))
(require (for-syntax scribble/html/xml racket/match))

;;; JSX                        URX (AT-expression)    S-EXPR                 Urlang
;;; <div>Foo</div>             @div{Foo}              (div "foo")            (React.createElement "div" null (array "Foo"))
;;; <div foo="bar">Foo</div>   @div[foo: "bar"]{Foo}  (div foo: bar "foo")   (React.createElement "div" (object [foo "bar"]) (array "Foo"))
;;; <Input />                  @Input[]               (Input)                (React.createElement "Input" null)

;;  Note that jsx/urx expressions are allowed in the body too.


;; @list[foo: "foo" bar: "bar"]{baz @42 qux}
;; (foo: "foo" bar: "bar" "baz " 42 " qux")

;; An easy way to see that an urx-expressions reads as:

(define-syntax (urx-test stx)
  (syntax-parse stx
    [(_ urx-expr)
     (syntax/loc stx
       'urx-expr)]))


;; (urx-test @div[foo: "bar" Foo])
;; '(div foo: "bar" Foo)

;; The library scribble/html/xml provides a few tools that will help us.
(require scribble/html/xml
         )

;; First The attribute names are represented as symbols that end with a colon.

;; > (attribute? 'foo:)
;; foo
;; > (attribute? 'foo)
;; #f

;; To make things clearer, we will use `attribute->symbol` when we need
;; to remove a colon.

(define attribute->symbol attribute?)

;; We will need to split a form into the attributes and the body.
;; The functions `attributes+body` and `split-attributes+body` will help
;; us with this.

;; > (attributes+body '(foo: "bar" Foo))
;; '((foo . "bar"))
;; '(Foo)

;; > (split-attributes+body '(foo: "bar" Foo))
;; '(foo: "bar")
;; '(Foo)

(require racket/syntax)
(define-urlang-macro urx
  (λ (stx)
    ; attributes+body : syntax-list -> ...
    (define attribute->symbol attribute?)    
    (define (attributes+body xs) 
      (let loop ([xs xs] [as '()])
        (define a (and (pair? xs)
                       (identifier? (car xs))
                       (attribute->symbol (syntax-e (car xs)))))
        (define a-id (and a (datum->syntax (car xs) a (car xs))))
        ; if a is #f we have seen the last attribute,
        ; otherwise a is the attribute name (without colon).
        (cond [(not a)              (values (reverse as) xs)]
              [(null? (cdr xs))     (error 'attributes+body
                                           "missing attribute value for `~s:'" a)]
              [else (loop (cddr xs)
                          (cons (cons a-id (cadr xs)) as))])))    
    (define (convert urx-expr)
      ; (displayln (list 'urx-convert urx-expr))
      (syntax-parse urx-expr
        #:datum-literals (ur)
        [s:string   #'s]
        [n:number   #'n]
        [i:id       #'i]
        [b:boolean  #'b]
        [(ur more)  #'more]   ; "unquote" for urx expressions (called mustaches in jsx)
        [(tag:id (~optional (~seq #:spread props:id) #:defaults ([props #f]))
                 atts+body ...)
         (define ps (attribute props))
         (define-values (atts body)
           (attributes+body (syntax->list #'(atts+body ...))))
         (define tag-str (symbol->string (syntax-e #'tag)))
         (with-syntax* ([tag              (if (char-lower-case? (string-ref tag-str 0))
                                              tag-str
                                              #'tag)]
                        [((id . val) ...) atts]
                        [(val ...)        (map convert (syntax->list #'(val ...)))]
                        [(content ...)    (map convert body)])
           (with-syntax
             ([atts-expr
               (cond [(and (zero? (length atts)) (not ps)) #'null]
                     [(and (zero? (length atts))      ps)  ps]
                     [(not ps)                             #'(object [id val] ...)]
                     [else
                      (with-syntax ([props ps])
                        ; TODO change assign here to something that works ...
                        #'(Object.assign (object) (object [id val] ...) props))])])
                               
             (match body
               ['() (syntax/loc urx-expr
                      (React.createElement tag atts-expr))]
               [_   (syntax/loc urx-expr
                      (React.createElement tag atts-expr content ...))])))]))
    (syntax-parse stx
      [(_urx urx-expr)
       (define out (convert #'urx-expr))
       ; (displayln (syntax->datum out))
       out])))
