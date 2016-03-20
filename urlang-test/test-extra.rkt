#lang racket

(require rackunit urlang urlang/extra)

(current-urlang-run?                           #t) ; run using Node?
(current-urlang-echo?                          #t) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #t) ; print top-level expression?

(define (rs s) (read (open-input-string s)))

;;;
;;; SCOND
;;;

(current-urlang-console.log-module-level-expr? #f)
(check-equal? (rs (urlang (urmodule scond
                            (var [a 0] [b 0])
                            (scond [#f (:= a 10)] [0 (:= a 1)] [#t (:= a 10)]) ; a=1
                            (scond [#f 4] [else (:= b 10)])                    ; b=10
                            (scond)
                            (console.log (+ a b)))))
              11)

(current-urlang-console.log-module-level-expr? #t)

