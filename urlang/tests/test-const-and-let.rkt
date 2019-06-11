#lang racket
(require urlang urlang/extra)

;;;
;;; TEST
;;;

; This file contains tests for the constant and let variable bindings.
; Note: the keyword for ES6 let declarations is   let-decl.
;       we have used the keyword  let  for Racket-like let-expressions.

;;; Note: No output means all tests ran successfully.

;;; Note: You need to install  node  in order to run these tests.
;;;            https://nodejs.org/en/

;;; Running these tests will for each module produce
;;; both a JavaScript file and a file containing names of exports.

;;; Then node (a command line version of the JavaScript engine
;;; used in Chrome) is invoked on the JavaScript file, and the
;;; output is compared with the expected output.

(current-urlang-run?                           #t) ; run using Node?
(current-urlang-echo?                          #t) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t)

(require rackunit)
(define (rs s) (read (open-input-string s))) ; note: only reads FIRST value


(check-equal? (rs (urlang (urmodule test-new1
                            (import this)
                            (const [x 1])
                            ; (const [x 43]) ; TODO: Should be caught by Urlang
                            (const [y 20] [z 300])
                            (let-decl [a 4000] [b 50000])
                            ; (let-decl [a 600000]) ; TODO: catch redeclaration
                            (+ x y z a b))))
              54321)

