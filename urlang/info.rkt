#lang info
;;; This file describes the contents of "urlang" collection in the "urlang" package.

;;; The name of the collections is simply "urlang".
(define collection "urlang")

;;; Version number
(define version    "1.0")

;;; Dependencies declared here will need source
(define deps       '())

;;; Dependencies here can be installed as binaries (i.e. zo-files)
(define build-deps '("base"               
                     "html-writing"          ; for example in urlang-exampes
                     "nanopass"              ; for urlang and urlang-compiler
                     "net-lib"               ; ? detected on build-server
                     "rackunit-lib"          ; for tests
                     "scribble-html-lib"     ; for urlang/html
                     "srfi-lite-lib"         ; ? detected on build-server
                     "web-server-lib"))      ; for urlang/html

(define compile-omit-paths '())

