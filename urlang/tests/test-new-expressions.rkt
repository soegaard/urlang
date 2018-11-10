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
(current-urlang-echo?                          #f) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t)

(require rackunit)
(define (rs s) (read (open-input-string s))) ; note: only reads FIRST value


(check-equal? (rs (urlang (urmodule test-new1
                            (import this)
                            (define (Car make model year)
                              (:= this "make"   make)
                              (:= this "model"  model)
                              (:= this "year"   year))
                            (define c1 (new Car "Toyota"  "Aygo" 2015))
                            (define c2 (new Car "Renault" "Clio" 2016))
                            ;c1.make
                            ;c2.make
                            ;(:= c1 "year" 2017)
                            ;c1.year
                            c2.year)))
              2016)

