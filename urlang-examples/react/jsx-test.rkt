#lang at-exp racket
(require urlang urlang/html "jsx.rkt" syntax/parse)

#;(define-urlang-macro jsx
  (λ (stx)
    (syntax-parse stx
      [(_ at-exp)
       (jsx (eval-syntax #'at-exp))])))

(current-urlang-echo?     #t)
(current-urlang-beautify? #t)

(urlang
 (urmodule jsx-test
   (import React)
   @jsx{@h1[class: "aheader"]{A Header}}))

