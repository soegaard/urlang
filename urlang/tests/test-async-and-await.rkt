#lang racket
(require urlang urlang/extra)

;;;
;;; TEST
;;;

; This file contains tests for the async and await.
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
(current-urlang-echo?                          #f) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #f)

(require rackunit)
(define (rs s) (read (open-input-string s))) ; note: only reads FIRST value


(check-equal? (rs (urlang (urmodule test-async-and-await
                            (import this setTimeout Promise)
                            (define (pause value milliseconds)
                              (new Promise
                                   (λ (resolve reject)
                                     (setTimeout (λ() (resolve value)) milliseconds))))
                            (var [result 0])
                            (define/async (f)
                                (var [promise (await (pause "done" 1000))])
                                (:= result promise))
                            (f)
                            ; don't exit before output
                            (setTimeout (λ () (console.log result)) 2000))))
              'done)

