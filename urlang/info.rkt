#lang info
;;; This file describes the contents of "urlang" collection in the "urlang" package.

;;; The name of the collection is simply "urlang".
(define collection "urlang")

;;; Version number
(define version    "1.0")

;;; Dependencies needed for runtime.
(define deps       '("base"
                     "html-parsing"
                     "html-writing"
                     "nanopass"
                     "net-lib"
                     "rackunit-lib"
                     "scribble-html-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

;;; Dependencies needed for compile time"
(define build-deps '("base"               
                     "html-writing"          ; for example in urlang-exampes
                     "nanopass"              ; for urlang and urlang-compiler
                     "net-lib"               ; ? detected on build-server
                     "rackunit-lib"          ; for tests
                     "scribble-html-lib"     ; for urlang/html
                     "srfi-lite-lib"         ; ? detected on build-server
                     "web-server-lib"))      ; for urlang/html


(define compile-omit-paths '())

