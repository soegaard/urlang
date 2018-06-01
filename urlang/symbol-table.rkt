#lang racket
(provide define-symbol-table)
;;;
;;; SYMBOL TABLE
;;

; This file contains a symbol table in the style of free-id-table and bound-id-table.
; Symbols are used instead of identifiers.
; Identifiers are accepted as input - but represent the corresponding symbol.

(require (for-syntax racket/syntax syntax/parse))

(struct symbol-table (ht) #:transparent)

(define (make-symbol-table)
  (symbol-table (make-hasheq)))

(define (symbol-table-set! st sym val)
  (hash-set! (symbol-table-ht st) sym val))

(define (symbol-table-symbols st)
  (hash-keys (symbol-table-ht st)))

(define (symbol-table-ref st sym [default #f])
  (hash-ref (symbol-table-ht st) sym #f))

(define (strip x)
  (cond
    [(symbol? x) x]
    [(syntax? x) (strip (syntax->datum x))]
    [else (error 'strip "expected a symbol or an identifier, got ~a" x)]))

(define-syntax (define-symbol-table stx)
  (syntax-parse stx
    [(_define-symbol-table id)
     (with-syntax ([id-st (format-id #'id "~a-st" #'id #:source #'id)]
                   [ids   (format-id #'id "~as"   #'id #:source #'id)]
                   [id!   (format-id #'id "~a!"   #'id #:source #'id)]
                   [id?   (format-id #'id "~a?"   #'id #:source #'id)])
       (syntax/loc stx
         (begin
           (define id-st                (make-symbol-table))
           (define (id! x [v #t])       (symbol-table-set! id-st (strip x) v))
           (define (ids)                (symbol-table-symbols id-st))
           (define (id? x [default #f]) (symbol-table-ref id-st (strip x) default)))))]))

(define-symbol-table foo)








