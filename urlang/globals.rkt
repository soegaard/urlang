#lang racket/base

(provide global-variables macro-introduced-identifiers)

(define global-variables             (make-parameter '()))
(define macro-introduced-identifiers (make-parameter '()))

