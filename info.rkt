#lang info
;;; This file describes the contents of the Urlang package.
;;; The information is used by the Racket package system.

;;; The Urlang package contains multiple collections (urlang, urlang-doc, compiler etc)
(define collection 'multi)

;;; Version number
(define version    "1.0")

;;; Dependencies declared here will need source
(define deps       '())

;;; Dependencies here can be installed as binaries (i.e. zo-files)
(define build-deps '("base"
                     "nanopass"                     
                     "at-exp-lib"
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"
                     "html-writing"))

(define compile-omit-paths (list "compiler-test"
                                 ; ad hoc files
                                 "urlang/html/tag-comparison.rkt"
                                 "urlang/tmp"))

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

