#lang info
;;; This file describes the contents of the Urlang package.
;;; The information is used by the Racket package system.

;;; The Urlang package contains multiple collections (urlang, urlang-doc, compiler etc)
(define collection 'multi)

;;; Version number
(define version    "1.0")

(define deps       '("base"
                     "html-parsing"
                     "html-writing"
                     "nanopass"
                     "net-lib"
                     "rackunit-lib"
                     "scribble-html-lib"
                     "srfi-lite-lib"
                     "web-server-lib"))

(define build-deps '("base"
                     "nanopass"                     
                     "at-exp-lib"
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"
                     "html-writing"
                     "html-parsing"))

(define compile-omit-paths (list "compiler-test"
                                 ; ad hoc files
                                 "urlang/html/tag-comparison.rkt"
                                 "urlang/tmp/"
                                 "urlang/urlang/tmp/"))

(define test-omit-paths (list
                         "compiler-rjs"
                         "compiler-test"
                         "urlang-examples"
                         "urlang-test"
                         "urlang"
                         "README.md"
                         ; these are not commited to Github:
                         "old"
                         "untracked-experiments"
                         "urlang-compiler-examples"))

