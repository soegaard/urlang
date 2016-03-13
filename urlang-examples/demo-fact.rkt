#lang racket
(require urlang)

(urlang
 (urmodule demo-fact                                           ; module name
   (export fact)                                          ; fact is exported
   (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
   (console.log (fact 5))))

;;; Running the program will create a file "demo-fact.js" containing:

;  "use strict";
;  function fact(n){return (((n===0)===false)?(n*(fact((n-1)))):1);};
;  (console.log((fact(5))));
;  exports.fact=fact;

