#lang racket
(require urlang urlang/extra)

;;;
;;; TEST
;;;

; This file contains tests for the new expression:

; (new constructor-id)
; (new constructor-id argument-expression ...)

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


(check-equal? (rs (urlang (urmodule test-add1 (+ 1 2)))) 3)
(check-equal? (rs (urlang (urmodule test-add1 (+ 1))))   1)
(check-equal? (rs (urlang (urmodule test-add1 (+))))     0)
(check-equal? (rs (urlang (urmodule test-add1 (+ 1 2 3)))) 6)

(check-equal? (rs (urlang (urmodule test-add1 (* 2 3)))) 6)
(check-equal? (rs (urlang (urmodule test-add1 (* 2))))   2)
(check-equal? (rs (urlang (urmodule test-add1 (*))))     1)


(check-equal? (rs (urlang (urmodule test-add1 (- 1 2)))) -1)
(check-equal? (rs (urlang (urmodule test-add1 (- 1))))   -1)
;(check-equal? (rs (urlang (urmodule test-add1 (-))))      "error")

;(check-equal? (rs (urlang (urmodule test-add1 (% 1))))   -1)
