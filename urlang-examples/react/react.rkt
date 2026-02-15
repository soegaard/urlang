#lang at-exp racket
;;; React is a JavaScript library for building user intefaces.
; todo: can we keep urlang-expression as a syntax object?

(require urlang urlang/html urlang/extra "urx.rkt"
         syntax/parse) 
(require net/sendurl)


;;; Urlang settings

(current-urlang-run?                           #f) ; run using Node?              
(current-urlang-echo?                          #t) ; print generated JavaScript?  
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?  
(current-urlang-beautify?                      #t) ; invoke js-beautify


;;;
;;; External JavaScript Libraries
;;;

(define jquery-js
  "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")
(define react-js
  "https://unpkg.com/react@16/umd/react.development.js")
(define react-dom-js
  "https://unpkg.com/react-dom@16/umd/react-dom.development.js")
(define react-bootstrap-js
  "https://unpkg.com/react-bootstrap@next/dist/react-bootstrap.min.js")

(define react-bootstrap-css
  "https://maxcdn.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css")

;;;
;;; HTML
;;;

; script type="module" src="...">

(define page
  @html{@head{@title{React}
              @link[href: react-bootstrap-css rel: "stylesheet"]
              @script[src: jquery-js]
              @script[src: react-js]
              @script[src: react-dom-js]}
        @body{@script[src: react-bootstrap-js]
              @h1{React Example}
              @p{React is a JavaScript library for building user interfaces}
              @div[id: "example"]
              @script[src: "example.js" type: "module"]}})

;; Save the page to disk
(with-output-to-file "react.html"
  (λ () (displayln (~x page)))
  #:exists 'replace)

;;;
;;; JSX
;;;


;; (define-urlang-macro jsx
;;   (λ (stx)
;;     (syntax-parse stx
;;       [(_ at-exp)
;;        (jsx (eval-syntax #'at-exp))])))


;;;
;;; JavaScript
;;;


#;(urlang
 (urmodule example ; saved in react.js
   (import $ setTimeout req React ReactDOM this)
   ;;; A Counter component
   (var [Counter
         (React.createClass
          (object           
           [getInitialState (λ() (object [count 0]))]
           [handleClick     (λ() (this.setState (object [count (+ this.state.count 1)])))]
           [render          (λ() @jsx{@button[onClick: @ur[this.handleClick]]{
                                        @ur[(+ "Click me!   Number of clicks: "
                                               this.state.count)]}})]))])
   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement Counter) ($0 "#example"))))


;;;
;;; REACT HOOKS
;;;

(define-urlang-macro use-state
  (λ (stx)
    (syntax-parse stx
      [(_ state-id set-state-id initial-expr)
       (syntax/loc stx
         (var [state-temp   (React.useState initial-expr)]
              [state-id     (ref state-temp 0)]
              [set-state-id (ref state-temp 1)]))])))




; Example from:  https://reactjs.org/docs/hooks-overview.html


;; import React, { useState } from 'react';

;; function Example() {
;;   // Declare a new state variable, which we'll call "count"
;;   const [count, setCount] = useState(0);

;;   return (
;;     <div>
;;       <p>You clicked {count} times</p>
;;       <button onClick={() => setCount(count + 1)}>
;;         Click me
;;       </button>
;;     </div>
;;   );
;; }



#;(urlang
 (urmodule example ; saved in react.js
   (import $ React ReactDOM)
   (define (Counter)
     (use-state count set-count 0)
     @jsx{@div[@p{You clicked @ur[count] times}
               @button[onClick: @ur[(λ() (set-count (+ count 1)))]]{
                  Click me}]})

   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement Counter) ($0 "#example"))))


;; import React, { useState, useEffect } from 'react';

;; function Example() {
;;   const [count, setCount] = useState(0);

;;   // Similar to componentDidMount and componentDidUpdate:
;;   useEffect(() => {
;;     // Update the document title using the browser API
;;     document.title = `You clicked ${count} times`;
;;   });

;;   return (
;;     <div>
;;       <p>You clicked {count} times</p>
;;       <button onClick={() => setCount(count + 1)}>
;;         Click me
;;       </button>
;;     </div>
;;   );
;; }

#;(urlang
 (urmodule example ; saved in react.js
   (import $ React ReactDOM document.title)

   (var [use-effect React.useEffect])

   (define (Counter)
     (use-state count set-count 0)
     ; the effect function runs after flushing changes to the dom
     (use-effect
      ; An effect function must return either undefined or a cleanup function.
      ; A cleanup function is called when the component "unmounts" (i.e. is removed)
      (λ()
        (:= document.title (+ "You clicked " count " times"))
        undefined))
     
     @jsx{@div[@p{You clicked @ur[count] times}
               @button[onClick: @ur[(λ() (set-count (+ count 1)))]]{
                  Click me}]})

   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement Counter) ($0 "#example"))))

;;;
;;;
;;;


#;(urlang
 (urmodule example ; saved in react.js
   (import $ React ReactDOM document.title)

   (var [use-effect React.useEffect])

   (define (Answer)
     (use-state answer set-answer "")
     ; the effect function runs after flushing changes to the dom
     (use-effect
      ; An effect function must return either undefined or a cleanup function.
      ; A cleanup function is called when the component "unmounts" (i.e. is removed)
      (λ()
        (:= document.title (if (= answer "3") "korrekt" "forkert"))
        undefined))
     
     @jsx{@div[@p{Hvad er 1+2?}
                 @form[@input[id: "answer" type: "text" name: "answer"
                              onInput: @ur[(λ() (set-answer (dot ($0 "#answer") value) )
                                             (console.log answer))] 
                                  ]]]})

   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement Answer) ($0 "#example"))))


;;;
;;;
;;;


; @ur[(spread props)]

#;(urlang
 (urmodule example ; saved in react.js
   (import $ React ReactDOM document.title ReactBootstrap)
   ;(import-from ReactBootstrap Alert)        
   (var [use-effect React.useEffect])

   (define (MyCheckbox props)
     (use-state checked set-checked #f)
     @urx{@div[className: "check" 
                 onClick: (λ() (set-checked (not checked)))]{
               A}})

   (define (Exercise)
     (var [foo "bar"])
     @urx{@div[@h1{Hældning fra to punkter}
               @ReactBootstrap.Alert[variant: "primary"]{A primary alert}
               @p{Hvad er hældningen på linjen, som går gennem (1,2) og (3,4)?}
               @h2{Vælg et svar}
               @ul[@li[key: 1]{A. 1}
                   @li[key: 2]{B. 2}
                   @li[key: 3]{C. @ur[foo]}
                   @li[key: 4]{D. 4}]
               @MyCheckbox[]]})

   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement Exercise) ($0 "#example"))))


(urlang
 (urmodule example ; saved in react.js
   (import Object $ React ReactDOM document.title ReactBootstrap)
   ;(import-from ReactBootstrap Alert)
   ;(import Modal ButtonToolbar Button) ; all from react-bootstrap
   (var [use-effect React.useEffect])
   (var [Modal         ReactBootstrap.Modal]
        [ButtonToolbar ReactBootstrap.ButtonToolbar]
        [Button        ReactBootstrap.Button])

   (define (MyVerticallyCenteredModal props)
     (console.log props)
     @urx{@Modal[#:spread props 
                 centered: #t size: "lg" aria-labelledby: "contained-modal-title-vcenter"]{
            @Modal.Header[closeButton: ""]{
              @Modal.Title[id: "contained-modal-title-vcenter"]{
                Modal heading}}
            @Modal.Body[
             @h4{Centered Modal}
             @p{Cras mattis consectetur purus sit amet fermentum. Cras justo odio,
                dapibus ac facilisis in, egestas eget quam. Morbi leo risus, porta ac
                consectetur ac, vestibulum at eros.}]
            @Modal.Footer[
              @Button[onClick: @ur[props.onHide]]{Close}]}})

   (define (App)
     (use-state modal-show set-modal-show #f)
     @urx[@ButtonToolbar[
            @Button[variant: "primary" onClick: @ur[(λ() (set-modal-show #t))]]{
              Launch vertically centered modal}
            @MyVerticallyCenteredModal[show:   @ur[modal-show]
                                       onHide: @ur[(λ() (set-modal-show #f))]]]])

   (define (MyCheckbox props)
     (use-state checked set-checked #f)
     @urx{@div[className: "check" 
                 onClick: @ur[(λ() (set-checked (not checked)))]]{
               A}})

   (define (Exercise)
     (var [foo "bar"])
     @urx{@div[@h1{Hældning fra to punkter}
               @ReactBootstrap.Alert[variant: "primary"]{A primary alert}
               @p{Hvad er hældningen på linjen, som går gennem (1,2) og (3,4)?}
               @h2{Vælg et svar}
               @ul[@li[key: 1]{A. 1}
                   @li[key: 2]{B. 2}
                   @li[key: 3]{C. @ur[foo]}
                   @li[key: 4]{D. 4}]
               @MyCheckbox[]]})

   (define ($0 selector) (ref ($ selector) 0))
   (ReactDOM.render (React.createElement App) ($0 "#example"))))

;;;
;;;
;;;


;(define (start) (send-url/file "react.html"))
;(displayln "Enter (start) in the repl to open the first on first run.")

;(send-url/file "react.html")

(require racket/runtime-path
         web-server/http/response-structs
         web-server/dispatch web-server/servlet-env web-server/dispatch)
(define-runtime-path here ".")

(serve/servlet (λ (req)
                 (response/output (λ (out) (output-xml page out))))
               #:servlet-path      "/react.html"
               #:launch-browser?   #t
               #:extra-files-paths (list here))
