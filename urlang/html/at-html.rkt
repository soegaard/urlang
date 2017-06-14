#lang racket/base

;; The #lang scribble/html language offers a nice solution for
;; constructing html using at-expressions (aka @-syntax).

;; This file makes it possible to use the machinery without using
;; the #lang scribble/html language.

;; Use (require urlang/html) in your #lang racket  program to get:


;;; Self-quote identifiers that end with colon.

; Example:  class: turns into 'class
(require (only-in scribble/html/main #%top))


;;; Constructors for all standard html tags: h1, p, ...

(require  (except-in scribble/html var object label block))
(require  (prefix-in html: scribble/html))

; A few of the constructors have names that class with Urlang.
; If you need them, then use
;     (require (prefix-in your-favorite-prefix var object label block))

; Two standard constructor is missing from scribble/html
(define/provide-elements/not-empty footer nav canvas)


;;; A way to turn the xml into strings

(require scribble/html/xml)


;;; A few utilies that turns xml(s) into a string

(define ~x xml->string)
(define (~xs . xs) (string-append* (map ~x xs)))

;;; Easy conversion from html or xexps to at-notation
(require "to-at-exp.rkt")

;;; No name clashes

; To prevent and discover name clashes with urlang, we require urlang
; here even though we don't use it for anything.
(require urlang)

(provide ~x ~xs
         #%top
         nav footer canvas
         (all-from-out scribble/html
                       scribble/html/xml
                       "to-at-exp.rkt"))
