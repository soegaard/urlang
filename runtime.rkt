#lang racket
(require "urlang.rkt" "urlang-extra.rkt" "for.rkt" syntax/parse syntax/stx)

(current-urlang-run?                           #t)
(current-urlang-echo?                          #t)
(current-urlang-console.log-module-level-expr? #t)

;;; Add  in-list  to for and friends
(define (handle-in-list clause)
  (syntax-parse clause
    #:literals (in-list)
    [[x in-list list-expr]
     #'(([xs list-expr])        ; list of var clauses to create initial state
        (not (null? xs))        ; termination condition
        ([x (car xs)])          ; let bindings (needs to bind x)
        ((:= xs (cdr xs))))]    ; statements to step state forward
    ; version with i as index
    [[x i in-list list-expr]
     #'(([xs list-expr] [i 0])  ; list of var clauses to create initial state
        (not (null? xs))        ; termination condition
        ([x (car xs)])          ; let bindings (needs to bind x)
        ((:= xs (cdr xs))       ; statements to step state forward
         (+= i 1)))]))

(define (handle-in-vector clause)
  (syntax-parse clause
    #:literals (in-vector)
    [[x in-vector vector-expr]
     #'(([xs vector-expr]
         [n (vector-length xs)]
         [i 0])                  ; list of var clauses to create initial state
        (< i n)                  ; termination condition
        ([x (ref xs (+ i 1))])   ; let bindings (needs to bind x)
        ((+= i 1)))]))

(add-clause-handler! 'in-list   handle-in-list)
(add-clause-handler! 'in-vector handle-in-vector)

;;; for/list

(define (expand-for/list stx)
  (syntax-parse stx
    [(_for/list (clause ...) statement-or-break ... expr)
     (syntax/loc stx
       (let ([xs NULL])
         (for (clause ...)
           statement-or-break ...
           (:= xs (cons expr xs)))
         (reverse xs)))]))  ; note: use reverse! ?

(define-urlang-macro for/list expand-for/list)

(define (expand-let-values stx)
  (syntax-parse stx
    [(_let-values ([(id ...) expr] ...) statement ... body-expr)
     (with-syntax ([(t ...)        (generate-temporaries #'(expr ...))])
       (with-syntax ([((t1 ...) ...) (for/list ([t   (in-list (syntax->list #'(t ...)))]
                                                [ids (in-list (syntax->list #'((id ...) ...)))])
                                       (stx-map (λ (id) t) ids))]                                     
                     [((i ...) ...)  (stx-map (λ (ids) (range 1 (+ 1 (length (syntax->list ids)))))
                                              #'((id ...) ...))])
         (syntax/loc stx
           (let ([t expr] ...)
             (let ([id (ref t1 i)] ... ...)
               statement ... body-expr)))))]))

(require racket/syntax)

(define (expand-define-values stx)
  (syntax-parse stx
    [(_define-values (id ...) expr)
     (with-syntax ([t (generate-temporary)])
       (with-syntax ([(i ...) (range 1 (+ 1 (length (syntax->list #'(id ...)))))])
         (syntax/loc stx
           (sbegin
            (var id ...)
            (let ([t expr])
              (:= id (ref t i)) ...)))))]))

(define-urlang-macro let-values    expand-let-values)
(define-urlang-macro define-values expand-define-values)

(define (expand-define/export stx)
  (syntax-parse stx
    [(_define/export (name . formals) . body)
     (syntax/loc stx
       (topblock
        (export name)
        (define (name . formals) . body)))]
    [(_define/export name expr)
     (syntax/loc stx
       (topblock
        (export name)
        (define name expr)))]
    [_ (error 'define/export "bad syntax" stx)]))
     
(define-urlang-macro define/export expand-define/export)

(display
 (urlang
  (urmodule runtime
    (import document window prompt alert parseInt parseFloat)  ; Browser functions
    (import process) ; Only in Node - not in browser?
    (import Array Int8Array Math Number String
            #;new #;typeof)
    ;;; Array
    (define/export (array? v) ; todo : inline array?
      (Array.isArray v))
    ;;; Tags
    (define/export (tag a) (ref a 0))
    (define/export PAIR                   (array "pair"))
    (define/export VECTOR                 (array "vector"))
    (define/export IMMUTABLE-VECTOR       (array "immutable-vector"))
    (define/export BOX                    (array "box"))
    (define/export IMMUTABLE-BOX          (array "immutable-box"))
    (define/export CHAR                   (array "char"))
    (define/export MUTABLE-STRING         (array "mutable-string"))
    (define/export BYTES                  (array "bytes"))
    (define/export MUTABLE-BYTES          (array "mutable-bytes"))
    (define/export SYMBOL                 (array "symbol"))
    (define/export KEYWORD                (array "keyword"))
    (define/export VALUES                 (array "values"))
    (define/export STRUCT-TYPE-DESCRIPTOR (array "struct-type-descriptor"))
    (define/export STRUCT                 (array "struct"))
    
    (define/export VOID (array))    ; singleton
    (define/export NULL (array))    ; singleton

    ;;;
    ;;; OPERATOR PRIMITIVES
    ;;;

    ;; The following primitives have names that are keywords in JavaScript.
    ;; That means we need alternative names.
    ;; Here + is named PRIM+  etc.

    (define/export (PRIM+ a b) ; variadic
      (var [n arguments.length] [args arguments] s)
      (case arguments.length
        [(2)  (+ a b)]
        [(1)  a]
        [(0)  0]
        [else (:= s (+ a b))
              (for ([i in-range 2 n])
                (:= s (+ s (ref args i))))
              s]))

    (define/export (PRIM- a b) ; variadic
      (var [n arguments.length] [args arguments] s)
      (case arguments.length
        [(2)  (- a b)]
        [(1)  (- a)]
        [(0)  0]
        [else (:= s (- a b))
              (for ([i in-range 2 n])
                (:= s (- s (ref args i))))
              s]))

    (define/export (PRIM* a b) ; variadic
      (var [n arguments.length] [args arguments] s)
      (case arguments.length
        [(2)  (* a b)]
        [(1)  a]
        [(0)  1]
        [else (:= s (* a b))
              (for ([i in-range 2 n])
                (:= s (* s (ref args i))))
              s]))

    (define/export (PRIM/ a b) ; variadic
      (var [n arguments.length] [args arguments] s)
      (case arguments.length
        [(2)  (/ a b)]
        [(1)  (/ 1 a)]
        [(0)  (error "PRIM/ expects at least one argument")]
        [else (:= s (/ a b))
              (for ([i in-range 2 n])
                (:= s (/ s (ref args i))))
              s]))
    
    (define/export (PRIM= a b) ; variadic
      (var [n arguments.length] [args arguments] s r)
      (case arguments.length
        [(2)   (=== a b)]
        [(0 1) (error "PRIM= expects at least two arguments")]
        [else (:= s (=== a b))
              (for ([i in-range 2 n])
                (:= s (and s (=== a (ref args i)))))
              s]))

    (define/export (PRIM<= a b) ; variadic
      (var [n arguments.length] [args arguments] s r)
      (case arguments.length
        [(2)   (<= a b)]
        [(0 1) (error "PRIM<= expects at least two arguments")]
        [else (:= s (<= a b))
              (for ([i in-range 2 (- n 1)])
                (:= s  (and s (<= (ref args i) (ref args (+ i 1))))))
              s]))

    (define/export (PRIM>= a b) ; variadic
      (var [n arguments.length] [args arguments] s r)
      (case arguments.length
        [(2)   (>= a b)]
        [(0 1) (error "PRIM>= expects at least two arguments")]
        [else (:= s (>= a b))
              (for ([i in-range 2 (- n 1)])
                (:= s (and s (>= (ref args i) (ref args (+ i 1))))))
              s]))

    (define/export (PRIM> a b) ; variadic
      (var [n arguments.length] [args arguments] s r)
      (case arguments.length
        [(2)   (> a b)]
        [(0 1) (error "PRIM> expects at least two arguments")]
        [else (:= s (> a b))
              (for ([i in-range 2 (- n 1)])
                (:= s  (and s (> (ref args i) (ref args (+ i 1))))))
              s]))

    (define/export (PRIM< a b) ; variadic
      (var [n arguments.length] [args arguments] s r)
      (case arguments.length
        [(2)   (< a b)]
        [(0 1) (error "PRIM< expects at least two arguments")]
        [else (:= s (< a b))
              (for ([i in-range 2 (- n 1)])
                (:= s  (and s (< (ref args i) (ref args (+ i 1))))))
              s]))
    
    ;;;
    ;;; 4.1 Booleans and equality
    ;;;

    ;; Representation: #t and #f are represented as true and false respectively,
    ;; where true and false are JavaScript booleans.
    (define/export (boolean? v)
      (= (typeof v) "boolean"))
    ; (define (not x) (if x #f #t)) ; not is a predefined function
    (define/export (equal? v w) ; TODO: handle cyclic data structures
      (cond
        [(and (boolean? v)          (boolean?        w))   (= v w)]
        [(and (number?  v)          (number?         w))   (= v w)]
        [(and (symbol?  v)          (symbol?         w))   (symbol=? v w)]
        [(and (null?  v)            (null?           w))   #t]
        [(and (pair? v)             (pair?           w)) (and (equal? (unsafe-car v) (unsafe-car w))
                                                              (equal? (unsafe-cdr v) (unsafe-cdr w)))]
        [(and (void?  v)            (void?           w))   #t]
        [(and (char?  v)            (char?           w))   (char=? v w)]
        [(and (immutable-string? v) (immutable-string? w)) (= v w)]
        [(and (mutable-string?   v) (mutable-string? w))   (string=? v w)]
        [(and (vector? v)           (vector? w))           (for/and ([x in-vector v]
                                                                     [y in-vector w])
                                                             (equal? x y))]
        [(and (box? v)              (box? w))              (equal? (unbox v) (unbox w))]
        [(and (bytes?  v)           (bytes?          w))   (bytes=? v w)]
        [(and (keyword?  v)         (keyword?        w))   (keyword=? v w)]
        [#t #f]))
    (define/export (eqv? v w)
      (cond
        [(and (number? v) (number? w)) (= v w)]
        [(and (char?   v) (char?   w)) (= (char->integer v) (char->integer w))]
        [#t                            (= v w)]))
    (define/export (eq? v1 v2)      
      (= v1 v2))
    ; (define (equal/retur v1 v2) ...) TODO
    (define/export (immutable? v)
      ; Note: This follows the spec. It is not a general
      ;       immutability predicate, so immutable pairs are not checked here.
      (or (immutable-string? v)
          (immutable-bytes? v)
          (immutable-vector? v)
          #;(immutable-hash? v)
          (immutable-box? v)))
    ;;;
    ;;; 4.2 Numbers
    ;;;
    (define/export (number? v)   (= (typeof v) "number"))
    (define/export (complex? v)  (number? v))
    (define/export (real? v)     (number? v)) ; +inf.0, -inf.0, +nan.0 are real numbers
    (define/export (rational? v) (and (number? v) (Number.isFinite v)))  
    (define/export (integer? v)  (Number.isInteger v))
    (define/export (exact-integer? v)
      ; http://stackoverflow.com/questions/3885817/how-to-check-that-a-number-is-float-or-integer
      (and (= (typeof v) "number")
           (= v (+ v))
           (= v (bit-or v 0))))
    (define/export (exact-nonnegative-integer? v) (and (exact-integer? v) (>= v 0)))
    (define/export (exact-positive-integer? v)    (and (exact-integer? v) (> v 0)))
    (define/export (inexact-real? v)              (and (real? v) (inexact? v)))
    (define/export (fixnum? v)                    (exact-integer? v))
    (define/export (flonum? v)                    (and (number? v) (not (exact-integer? v))))
    (define/export (double-flonum? v)             (flonum? v))
    (define/export (single-flonum? v)             (flonum? v))
    (define/export (zero? z)                      (= z 0))
    (define/export (positive? x)                  (> x 0))
    (define/export (negative? x)                  (< x 0))
    (define/export (even? n)                      (= 0 (% n 2)))
    (define/export (odd? n)                       (= 1 (% n 2)))
    (define/export (exact? z)                     (exact-integer? z))
    (define/export (inexact? z)                   (and (number? z) (not (exact? z))))
    (define/export (inexact->exact z)      z) ; todo
    (define/export (exact->inexact z)      z) ; todo
    (define/export (real->single-flonum x) x) ; todo 
    (define/export (real->double-flonum x) x) ; todo
    ;;;
    ;;; 4.2.2 Generic Numbers
    ;;;

    ;; Note: JavaScript addition has the name +.
    ;;       Racket generic number operations are prefixed with $,
    ;;       so Racket addition is $+

    (define/export ($+ z1 z2) ; (+ z ...) variadic
      (var [n arguments.length] res)
      (sif (= n 2)
           (:= res (+ z1 z2)) ; fast path
           (sif (= n 1)
                (:= res z1)
                (sif (= n 0)
                     (:= res 0)
                     (block
                      (var [sum (+ z1 z2)] [i 2]
                           ; only reify arguments if needed
                           [args arguments])
                      (while (< i n)
                             (+= sum (ref args i))
                             (+= i 1))
                      (:= res sum)))))
      res)
    (define/export ($- z1 z2) ; (- z1 z2 ...) variadic (at least one argument)
      (var [n arguments.length] res)
      (sif (= n 2)
           (:= res (- z1 z2)) ; fast path
           (sif (= n 1)
                (:= res (- z1))
                (block ; n>= 3
                 (var [sum (- z1 z2)] [i 2]
                      ; only reify arguments if needed
                      [args arguments])
                 (while (< i n)
                        (-= sum (ref args i))
                        (+= i 1))
                 (:= res sum))))
      res)
    (define/export ($* z1 z2) ; (* z ...) variadic
      (var [n arguments.length] res)
      (sif (= n 2)
           (:= res (* z1 z2)) ; fast path
           (sif (= n 1)
                (:= res z1)
                (sif (= n 0)
                     (:= res 1)
                     (block
                      (var [prod (* z1 z2)] [i 2]
                           ; only reify arguments if needed
                           [args arguments])
                      (while (< i n)
                             (*= prod (ref args i))
                             (+= i 1))
                      (:= res prod)))))
      res)
    (define/export ($/ z1 z2) ; (+ z1 z2 ...) variadic (at least one argument)
      (var [n arguments.length] res)
      (sif (= n 2)
           (:= res (/ z1 z2)) ; fast path
           (sif (= n 1)
                (:= res (/ 1 z1))
                (block ; n>= 3
                 (var [quot (/ z1 z2)] [i 2]
                      ; only reify arguments if needed
                      [args arguments])
                 (while (< i n)
                        (/= quot (ref args i))
                        (+= i 1))
                 (:= res quot))))
      res)
    (define/export (quotient n m) 
      (Math.floor (/ n m)))
    (define/export (remainder n m)
      (% n m))
    (define/export (quotient/remainder n m)
      (values (quotient n m) (remainder n m)))
    (define/export (modulo n m) ; TODO : throw exception unless n and m are finite
      (% (+ (% n m) m) m))
    (define/export (add1 z) (+ z 1))
    (define/export (sub1 z) (- z 1))
    (define/export (abs x)  (Math.abs x))
    (define/export (max) ; (max x ...+) variadic
      (Math.max.apply #f arguments))
    (define/export (min) ; (min x ...+) variadic
      (Math.min.apply #f arguments))
    (define/export (gcd n m) ; (gcd n ...)
      (var [args arguments] i g l)
      (case arguments.length
        [(2)  (gcd2 n m)]
        [(1)  n]
        [(0)  0]
        [else (:= l args.length)
              (:= g (ref args 0))
              (for ([i in-range 1 l])
                (:= g (gcd g (ref args i))))
              g]))
    (define/export (gcd2 n m)
      (var a b)
      (swhen (< n 0)  (:= n (- n)))
      (swhen (< m 0)  (:= m (- m)))
      (:= a (if (> n m) n m))
      (:= b (if (< n m) n m))
      (var [res #f])
      (while (not res)
             (cond [(= b 0) (:= res a)]
                   [#t      (%= a b)
                            (cond
                              [(= a 0) (:= res b)]
                              [#t      (%= b a)])]))
      res)
    ; lcm todo
    (define/export (round x) ; break ties in favor of nearest even number
      (var [r (Math.round x)])
      (if (= (% (if (> x 0) x (- x)) 1) 0.5)
          (if (even? r) r (- r 1))
          r))
    (define/export (floor x)    (Math.floor x))
    (define/export (ceiling x)  (Math.ceil x))
    (define/export (truncate x) (Math.trunc x))
    ; (define (numerator q) ; q rational TODO
    (define/export (sqrt x)     (Math.sqrt x))
    (define/export (integer-sqrt n) ; n integer
      (Math.floor (Math.sqrt n)))   ; TODO: handle negative x
    (define/export (integer-sqrt/remainder n)
      (var [s (Math.floor (Math.sqrt n))])
      (values s (- n (* s s))))
    (define/export (expt z w)   (Math.pow z w))
    (define/export (exp z)      (Math.exp z))
    (define/export (log z)      (Math.log z))
    (define/export (sin z)      (Math.sin z))
    (define/export (cos z)      (Math.sin z))
    (define/export (tan z)      (Math.tan z))
    (define/export (asin z)     (Math.asin z))
    (define/export (acos z)     (Math.acos z))
    (define/export (atan y x)
      (case arguments.length
        [(1) (Math.atan y)]
        [(2) (Math.atan y x)]
        [else "error: atan - todo - raise exception"]))
    ;; Complex numbers  (not supported)
    ; make-rectangular - todo
    ; make-polar       - todo
    (define/export (real-part z)  z)  ; todo
    (define/export (image-part z) 0)  ; works only for real numbers :-)
    (define/export (magnitude z)  (Math.abs z))
    (define/export (angle z)
      (cond
        [(> z 0) 0]
        [(< z 0) Math.PI]
        [#t      (error "angle" "undefined for 0")]))
    ; integer-length  -  todo
    (define/export (random k rand-gen)  ; TODO: don't ignore rand-gen
      (Math.floor (* (Math.random) k)))
    (define/export (random-seed k) (Void))            ; can't set the seed in JavaScript ?!?
    (define/export (make-pseudo-random-generator) #f)
    (define/export (pseudo-random-generator? v) (eq? v #f))
    (define/export (current-pseudo-random-generator rand-gen) #f)
    (define/export (pseudo-random-generator->vector rand-gen) (vector))
    (define/export (vector->pseudo-random-generator vec) #f)
    (define/export (vector->pseudo-random-generator! rand-gen vec) (Void))
    (define/export (pseudo-random-generator-vector? v) (vector? v))
    (define/export (number->string z radix)
      (var [num (Number z)])
      (num.toString radix))
    (define/export (string->number s radix) ; radix optional
      (case arguments.length
        [(1) (parseFloat s)]
        [(2) (parseFloat s radix)]
        [else (error "string->number" "expected one or two arguments")]))
    ; real-> decimal-string ; todo
    ; integer-bytes->integer ; todo
    ; integer->integer-bytes ; todo
    ; floating-point-bytes-> real ; todo
    ; real->floating-point-bytes ; todo
    ; system-big-endian? : todo

    ;;; from racket/math
    (define/export pi Math.PI)
    (define/export (nan? x)      (= x +nan.0))
    (define/export (infinite? x) (or (= x +inf.0) (= x -inf.0)))
    
    ;;;  
    ;;; 4.3 Strings
    ;;;

    ; todo: string-copy
    ; todo: string-copy!
    ; todo: string-fill!
    ; todo: list->string
    ; todo: build-string
      
    ;; Representation
    ;;   - immutable Racket strings are represented as JavaScript strings
    ;;   - mutable Racket string are represented as a tagged array:
    ;;       {array MUTABLE-STRING "char0" "char1" ...}
    ;;     where char0 is a javascript string with length 1.
    (define/export (string? v)
      (or (= (typeof v) "string")
          (and (array? v) (= (tag v) MUTABLE-STRING))))
    (define/export (immutable-string? v) (= (typeof v) "string"))
    (define/export (mutable-string? v) (and (array? v) (= (tag v) MUTABLE-STRING)))
    (define/export (make-string k ch) ; ch optional
      (case arguments.length
        [(1) (make-string1 k)]
        [(2) (make-string2 k ch)]
        [else (error "make-string" "expected at one or two argument")]))
    (define/export (make-string1 k)
      (make-string2 k "\u0000"))
    (define/export (make-string2 k ch)
      ; make-string produces a mutable string
      (var [a (Array (+ k 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 1 (+ k 1)])
        (:= a i (ref ch 1)))
      a)      
    (define/export (make-primitive-string n c) ; make primitive js string of length n
      ; http://stackoverflow.com/questions/202605/repeat-string-javascript?rq=1
      (var [s ""])
      (while #t
             (sunless (= (bit-and n 1) 0) (+= s c))
             (>>= n 1)
             (sif (= n 0) (break) (+= c c)))
      s)
    (define/export (string->immutable-string s)
      (cond
        [(= (typeof s) "string") s]
        [#t (var [n+1 s.length] [n (- n+1 1)] [a (Array n)])
            (for ([i in-range 0 n])
              (:= a i (ref s (+ i 1))))
            (String (a.join ""))]))
    (define/export (immutable-string->string str)
      (var [n str.length] [a (Array (+ n 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (ref str i)))
      a)      
    (define/export (string) ; ch ... multiple arguments
      (var [args arguments] [n args.length])
      (var [a (Array (+ n 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (ref (ref args i) 1)))
      a)
    (define/export (string-length s)
      (if (= (typeof s) "string")
          s.length
          (- s.length 1)))
    (define/export (string-ref s i)
      (if (= (typeof s) "string")
          (array CHAR (ref s    i))
          (array CHAR (ref s (+ i 1)))))
    (define/export (string-set! s i c)
      ; (unless (mutable-string? s) (raise ...))
      ; (unless (char? c)           (raise ...))
      (:= s (+ i 1) (ref c 1)))
    (define/export (substring3 str start end) (str.substring start end))
    (define/export (substring2 str start)     (str.substring start (string-length str)))
    (define/export (substring) ; case-lambda
      (case arguments.length
        [(2) (substring2 (ref arguments 0) (ref arguments 1))]
        [(3) (substring3 (ref arguments 0) (ref arguments 1) (ref arguments 2))]
        [else (error "substring" "expected two to three arguments")]))
    (define/export (string=? str1 str2)
      (if (immutable-string? str1)
          (if (immutable-string? str2)
              (= str1 str2)
              (= str1 (string->immutable-string str2)))
          (if (immutable-string? str2)
              (= (string->immutable-string str1) str2)
              (= (string->immutable-string str1) (string->immutable-string str2)))))
    (define/export (string<? str1 str2)
      (if (immutable-string? str1)
          (if (immutable-string? str2)
              (< str1 str2)
              (< str1 (string->immutable-string str2)))
          (if (immutable-string? str2)
              (< (string->immutable-string str1) str2)
              (< (string->immutable-string str1) (string->immutable-string str2)))))
    (define/export (string>? str1 str2)
      (if (immutable-string? str1)
          (if (immutable-string? str2)
              (> str1 str2)
              (> str1 (string->immutable-string str2)))
          (if (immutable-string? str2)
              (> (string->immutable-string str1) str2)
              (> (string->immutable-string str1) (string->immutable-string str2)))))
    (define/export (string<=? str1 str2)
      (if (immutable-string? str1)
          (if (immutable-string? str2)
              (<= str1 str2)
              (<= str1 (string->immutable-string str2)))
          (if (immutable-string? str2)
              (<= (string->immutable-string str1) str2)
              (<= (string->immutable-string str1) (string->immutable-string str2)))))
    (define/export (string>=? str1 str2)
      (if (immutable-string? str1)
          (if (immutable-string? str2)
              (>= str1 str2)
              (>= str1 (string->immutable-string str2)))
          (if (immutable-string? str2)
              (>= (string->immutable-string str1) str2)
              (>= (string->immutable-string str1) (string->immutable-string str2)))))
    (define/export (string-upcase str) ; creates mutable string
      (if (immutable-string? str)
          (immutable-string->string (String (str.toUpperCase)))
          (string-upcase (string->immutable-string str))))
    (define (string-downcase str) ; creates mutable string
      (if (immutable-string? str)
          (immutable-string->string (String (str.toLowerCase)))
          (string-downcase (string->immutable-string str))))         
    (define/export (string->list s) (for/list ([c in-string s]) c))
    (define/export (string-append2 str1 str2) (str1.concat str2))
    (define/export (string-append) ; variadic
      (var [args arguments])
      (var [as (for/array #:length args.length ([a in-array args]) a)])
      (as.join ""))

    ;;;
    ;;; 4.4 Byte Strings
    ;;;

    ;; A byte string is a fixed-length array of bytes.
    ;; A byte is an exact integer between 0 and 255 inclusive.
    ;; A byte string can be mutable or immutable.

    ;; Representation: Byte strings are represented as Int8Array
    ; http://caniuse.com/#feat=typedarrays

    ; bytes-copy!       ; todo
    ; bytes-fill!       ; todo
    ; bytes-append      ; todo
    ; bytes->list       ; todo
    ; list->bytes       ; todo
    ; make-shared-bytes ; todo
    ; shared-bytes      ; todo

    
    (define/export (bytes? v)
      (and (array? v)
           (or (= (tag v) BYTES)
               (= (tag v) MUTABLE-BYTES))))
    (define/export (immutable-bytes? v) (and (array? v) (= (tag v) BYTES)))
    (define/export (mutable-bytes?   v) (and (array? v) (= (tag v) MUTABLE-BYTES)))
    (define/export (bytes->Int8Array bs)
      (ref bs 1))
    (define/export (make-bytes k b) ; b optional
      (case arguments.length
        [(1) (make-bytes1 k)]
        [(2) (make-bytes2 k b)]
        [else (error "make-bytes" "expected one or two arguments")]))
    (define/export (make-bytes1 k) ; b = 0 
      ; Returns a new mutable byte string of length k where each position
      ; in the byte string is initialized with the byte b.
      (var [is (new Int8Array k)])
      (array MUTABLE-BYTES is))
    (define/export (make-bytes2 k b)
      ; Returns a new mutable byte string of length k where each position
      ; in the byte string is initialized with the byte b.
      (var [bs (new Int8Array k)])
      ; (sunless (= b 0) (bs.fill 0 k b))  ; ES6 not supported in Node/Chrome
      (for ([i in-range 0 k])
        (:= bs i b))
      (array MUTABLE-BYTES bs))
    
    (define/export (bytes) ; (bytes b ...)
      ; make new mutable byte string
      (var [args arguments]
           [n    args.length]
           [is   (Int8Array n)])
      (is.set args)
      (array MUTABLE-BYTES is))
    (define/export (bytes->immutable-bytes bs)
      (array BYTES (Int8Array.from (ref bs 1))))
    (define/export (immutable-bytes->bytes bs)
      (array MUTABLE-BYTES (Int8Array.from (ref bs 1))))
    (define/export (byte? v)
      (and (exact-integer? v)
           (<= 0 v 255)))
    (define/export (bytes-length bs)
      (ref (ref bs 1) "length"))
    (define/export (bytes-ref bs k)
      (ref (ref bs 1) k))
    (define/export (bytes-set! bs k b)
      (var [is (ref bs 1)])
      (:= is k b))
    (define/export (subbytes bs start end) ; end is optional
      (case arguments.length
        [(2) (subbytes2 bs start)]
        [(3) (subbytes3 bs start end)]
        [else (error "subbytes" "expected two or three arguments")]))
    (define/export (subbytes2 bs start) ; end = bs.length
      (var [is  (ref bs 1)]
           [end (ref is "length")]
           [k   (- end start)]
           [t   (new Int8Array k)])
      (for ([i in-range start end])
        (:= t i (ref is i)))
      (array MUTABLE-BYTES is))
    (define/export (subbytes3 bs start end)
      (var [is (ref bs 1)]
           [k  (- end start)]
           [t  (new Int8Array k)])
      (for ([i in-range start end])
        (:= t i (ref is i)))
      (array MUTABLE-BYTES is))
    
    (define/export (bytes-copy bs)
      (subbytes2 bs 0))

    ;;; 4.4.2 Byte String Comparisons
    (define/export (bytes=? bs1 bs2)
      (var [n1 (bytes-length bs1)]
           [n2 (bytes-length bs2)]
           [is1 (ref bs1 1)]
           [is2 (ref bs2 1)])
      (and (= n1 n2)
           (for/and ([i in-range 0 n1])
             (= (ref is1 i) (ref is2 i)))))

    ;;;
    ;;; 4.5 Characters
    ;;;

    ; char-utf-8-length TODO
    
    ;; Characters range over Unicode scalar values, which includes characters whose values
    ;; range from #x0 to #x10FFFF, but not including #xD800 to #xDFFF.
    ;; The scalar values are a subset of the Unicode code points.

    ;; Representation:
    ;;      {array CHAR primtive-javascript-string-of-length-1}
    (define/export (char? v)               (and (array? v) (= (tag v) CHAR)))
    (define/export (make-char prim-js-str) (array CHAR prim-js-str))             ; internal
    (define/export (char->integer c)       (var [s (ref c 1)]) (s.charCodeAt 0))
    (define/export (integer->char i)       (String (String.fromCharCode i)))
    (define/export (char=? c1 c2)          (= (ref c1 1) (ref c2 1))) ; todo: variadic
    ; (define (char-alphabetic? c)    TODO
    
    ;;;
    ;;; 4.6 Symbols
    ;;;    

    ; A symbol is like an immutable string, but symbols are normally interned,
    ; so that two symbols with the same character content are normally eq?.
    ; All symbols produced by the default reader (see Reading Symbols) are interned.

    ;; Representation:
    ;;   A interned symbol
    ;;      {array SYMBOL primtive-javascript-string}
    ;;   Non-interned symbol:
    ;;      {array SYMBOL javascript-string-object}

    ;;   For the time being we will assume that the JS implementation
    ;;   interns primitive strings. If this happens to be false, we need
    ;;   to change the representation to do its own interning.
    ;;   Note: String(s) without new in front turns s into a primitive string.

    ; string->unreadable-symbol  ; TODO
    
    (define/export (symbol? v)
      (and (array? v) (= (tag v) SYMBOL)))
    (define/export (symbol-interned? sym) ; symbol? -> boolean?
      (= (typeof (ref sym 1)) "string"))   ; note : an uninterned strings has type "object"
    (define/export (symbol=? sym1 sym2)
      (or (and (symbol-interned? sym1)
               (symbol-interned? sym2)
               (= (ref sym1 1) (ref sym2 1)))
          ; uninterned symbols are only equal to themselves
          (= sym1 sym2)))
      
    (define/export (symbol->string sym)
      ; returns freshly allocated mutable string
      (var [js-str (ref sym 1)]
           [n      js-str.length]
           [a      (Array (+ n 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (ref js-str i)))
      a)
    (define/export (symbol->immutable-string sym)
      ; String returns a primitive (interned by js) string
      (String (ref sym 1)))

    (define symbol-table (array))
    
    (define/export (string->symbol str)
      ; returns interned symbol
      (var [t (typeof str)] r old)
      (:= old (ref symbol-table str))
      (scond
       [(symbol? old)   (:= r old)]
       [(= t "string")  (:= r (array SYMBOL str))  ; primitive string
                        (:= symbol-table str r)]   ; intern it
       [(= t "object")  (var [n+1 t.length] [n (- n+1 1)] [a (Array n)])
                        (for ([i in-range 0 n])
                          (:= a i (ref str (+ i 1))))
                        (:= r (array SYMBOL (String (a.join ""))))
                        (:= symbol-table str r)]   ; intern it
       [#t (error "string->symbol" "expected a string")])      
      r)
    (define/export (string->uninterned-symbol str)
      ; (new String ...) returns a non-primitive string
      (array SYMBOL (new String str)))
    (define/export gensym-counter 0)
    (define/export (gensym base) ; base is optional
      (case arguments.length
        [(0) (gensym0)]
        [(1) (gensym1 base)]
        [else (error "gensym" "expected at most one argument")]))
    (define (gensym0)
      ; returns new uninterned symbol with automatically generated name
      (+= gensym-counter 1)
      (array SYMBOL (new String (+ "g" gensym-counter))))
    (define (gensym1 base)
      (console.log (+ "gensym1: " base))
      ; returns new uninterned symbol with automatically generated name
      (+= gensym-counter 1)
      (array SYMBOL (new String (+ base gensym-counter))))
    
    (define/export (symbol<? a-sym b-sym)
      (string<? (symbol->string a-sym) (symbol->string b-sym)))
    
    (define/export (error who msg)
      (console.log (+ "error: " who ": " msg)))

    ;;;
    ;;; 4.8 Keywords
    ;;;

    ;; A keyword is like an interned symbol, but its printed form starts with #:,
    ;; and a keyword cannot be used as an identifier. Furthermore, a keyword by
    ;; itself is not a valid expression, though a keyword can be quoted to form
    ;; an expression that produces the symbol.

    ;; Representation:
    ;;   {array KEYWORD primitive-js-str}
    ;; Note: The representation assumes primitive JavaScript strings are interned.
    (define/export (keyword? v)
      (and (array? v) (= (tag v) KEYWORD)))
    (define/export (keyword->string key) ; returns mutable string
      (immutable-string->string (ref key 1)))
    (define/export (string->keyword str)
      (var [istr (string->immutable-string str)])
      (array KEYWORD istr))
    (define/export (keyword<? key1 key2)
      (< (ref key1 1) (ref key1 2)))
    (define/export (keyword=? key1 key2)
      (= (ref key1 1) (ref key2 1)))
    
    ;;;
    ;;; 4.9-10 Lists
    ;;;

    ; foldl TODO
    ; foldr TODO

    ;; Representation:
    ;;   A list is either NULL or a pair with a true list? flag.
    ;;   That is, a list is either:
    ;;      NULL or {array PAIR true a d}
    ;;   where d is a list.
    (define/export (pair? v)      (and (array? v) (= (tag v) PAIR)))
    (define/export (null? v) (= v NULL))
    (define/export (cons a d)     (array PAIR (list? d) a d))
    (define/export (unsafe-car p) (ref p 2))
    (define/export car unsafe-car)
    (define/export (unsafe-cdr p) (ref p 3))
    (define/export cdr unsafe-cdr)
    (define/export (cddr v) (cdr (cdr v)))
    (define/export Null NULL)  ;; the name  null  is reserved in EcmaScript 6
    (define/export (list? v)      (or (= v NULL) (and (array? v) (= (tag v) PAIR) (ref v 1))))
    (define/export (list) ; (list v ...)
      ; Note:  [args arguments]  is needed
      (var [args arguments] [n args.length] [n-1 (- n 1)] [xs NULL])
      (for ([i in-range 0 n])
        (:= xs (cons (ref args (- n-1 i)) xs)))
      xs)
    (define/export (list*) ; (list* v ... tail)
      (var [args arguments] [n args.length])
      (cond
        [(= n 0) (error "list*" "expected one or more arguments")]
        [#t      (var [n-2 (- n 2)] [xs (ref args (+ n-2 1))])
                 (for ([i in-range 0 (- n 1)])
                   (:= xs (cons (ref args (- n-2 i)) xs)))
                 xs]))
    (define/export (build-list n proc)
      (var [a (Array n)])
      (for ([i in-range 0 n])
        ; (:= a i (proc i)
        (:= a i ((ref proc 1) proc i))) ; apply closure - TODO: error check
      (array->list a))
    (define/export (length xs)
      (var (n 0))
      (while (pair? xs) (+= n 1) (:= xs (cdr xs)))
      n)
    (define/export (list-ref xs i)
      ;; Return the i'th index of xs.
      ;; Use fast path for i=0 and i=1.
      (var ret)
      (scond [(= i 0) (:= ret (car xs))]
             [(= i 1) (:= ret (car (cdr xs)))]
             [#t      (for ([j in-range 0 (- i 1)])
                        (:= xs (cdr xs)))
                      (:= ret (car xs))])
      ret)
    (define/export (list-tail xs pos)
      (while (not (= pos 0))
             ; TODO ; (swhen (null? xs) (return (error "list-tail" " --- exn:fail:contract ---")))
             (:= xs (cdr xs))
             (-= pos 1))
      xs)
    (define/export (append) ; variadic (append xs ...)
      (var [args arguments] [n args.length])
      (case n
        [(0)  NULL]
        [(1)  (ref args 0)]
        [(2)  (append2 (ref args 0) (ref args 1))]
        [else (var [ret (ref args (- n 1))] [i (- n 2)])
              (while (>= i 0)
                     (:= ret (append2 (ref args i) ret))
                     (-= i 1))
              ret]))
    (define/export (append2 xs ys)
      (var [ret ys])
      (sunless (null? xs) 
               (var [axs (list->array xs)]
                    [n axs.length]
                    [n-1 (- n 1)])
               (for ([i in-range 0 n])
                 (:= ret (cons (ref axs (- n-1 i)) ret))))
      ret)
    (define/export (reverse xs)
      ; Note: for/list uses reverse, so reverse can't use for/list
      (var [result NULL])
      (while (not (null? xs))
             (:= result (cons (car xs) result))
             (:= xs (cdr xs)))
      result)
    (define/export (map proc xs ys zs) ; optional (map proc xs ...+)
      (case arguments.length
        [(2) (for/list ([x in-list xs])
               (proc.call #f x))]
        [(3) (for/list ([x in-list xs] [y in-list ys])
               (proc.call #f x y))]
        [(4) (for/list ([x in-list xs] [y in-list ys] [z in-list zs])
               (proc.call #f x y z))]
        [(0) (error "map" "expected at least two arguments")]        
        [else (/ 1 0)  ])) ; TODO
    (define/export (andmap proc xs ys zs) ; optional (andmap proc xs ...+)
      (case arguments.length
        [(2) (for/and ([x in-list xs])
               (proc.call #f x))]
        [(3) (for/and ([x in-list xs] [y in-list ys])
               (proc.call #f x y))]
        [(4) (for/and ([x in-list xs] [y in-list ys] [z in-list zs])
               (proc.call #f x y z))]
        [(0) (error "andmap" "expected at least two arguments")]
        [else (/ 1 0)  ]))
    (define/export (ormap proc xs ys zs) ; optional (ormap proc xs ...+)
      (case arguments.length
        [(2) (for/or ([x in-list xs])
               (proc.call #f x))]
        [(3) (for/or ([x in-list xs] [y in-list ys])
               (proc.call #f x y))]
        [(4) (for/or ([x in-list xs] [y in-list ys] [z in-list zs])
               (proc.call #f x y z))]
        [(0) (error "ormap" "expected at least two arguments")]
        [else (/ 1 0)  ]))
    (define/export (for-each proc xs ys zs) ; optional (for-each proc xs ...+)
      (case arguments.length
        [(2) (for ([x in-list xs])
               (proc.call #f x))]
        [(3) (for ([x in-list xs] [y in-list ys])
               (proc.call #f x y))]
        [(4) (for ([x in-list xs] [y in-list ys] [z in-list zs])
               (proc.call #f x y z))]
        [(0) (error "map" "expected at least two arguments")]        
        [else (/ 1 0)  ])) ; TODO
    ; foldl TODO
    ; foldr TODO
    (define/export (filter pred xs)
      (var [n (length xs)]
           [a (Array n)]
           [i 0])
      ; fill in array with non-false values
      (for ([x in-list xs])
        (var [t (pred.call #f x)])
        (swhen t
               (:= a i x)
               (+= i 1)))
      ; create list with elements in the array
      (var [xs NULL])
      (while (> i 0)
             (-= i 1)
             (:= xs (cons (ref a i) xs)))
      xs)
    
    (define/export (list->array xs)
      ;; convert list to (non-tagged) JavaScript array
      ; allocate array
      (var a [n (length xs)])
      (:= a (Array n))
      ; fill it
      (for ([x i in-list xs])
        (:= a i x))
      a)
    (define/export (array->list axs)
      ;; convert JavaScript array to Racket list
      (var [n axs.length] [n-1 (- n 1)] [xs NULL])
      (for ([i in-range 0 n])
        (:= xs (cons (ref axs (- n-1 i)) xs)))
      xs)
    (define/export (array-end->list axs from)
      ;; convert JavaScript array to Racket list
      (var [n axs.length] [n-1 (- n 1)] [xs NULL])
      (for ([i in-range 0 (- n-1 from)])
        (:= xs (cons (ref axs (- n-1 i)) xs)))
      xs)
    ;;;
    ;;; racket/list
    ;;;
    (define/export empty NULL)
    (define/export (cons? v)      (pair? v))
    (define/export (empty? v)     (null? v))
    (define/export (first xs)     (car xs))
    (define/export (rest xs)      (cdr xs))
    (define/export (second xs)    (car (cdr xs)))
    (define/export (third xs)     (car (cdr (cdr xs))))
    (define/export (fourth xs)    (car (cdr (cdr (cdr xs)))))
    (define/export (fifth xs)     (car (cdr (cdr (cdr (cdr xs))))))
    (define/export (sixth xs)     (car (cdr (cdr (cdr (cdr (cdr xs)))))))
    (define/export (seventh xs)   (car (cdr (cdr (cdr (cdr (cdr (cdr xs))))))))
    (define/export (eighth xs)    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs)))))))))
    (define/export (ninth xs)     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs))))))))))
    (define/export (tenth xs)     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr xs)))))))))))
    (define/export (last-pair xs) (while (not (null? (cdr xs))) (:= xs (cdr xs))) xs)
    (define/export (last xs)      (car (last-pair xs)))
    (define/export (make-list k v)
      ; Returns a newly constructed list of length k, holding v in all positions.
      (for/list ([i in-range 0 k]) v))
    
    ;;;
    ;;; 4.11 Vectors
    ;;;

    ;; TODO:
    ;;   vector->values
    
    ;; Representation:
    ;;   (array VECTOR           elm0 elm1 ...)  ; mutable vector
    ;;   (array IMMUTABLE-VECTOR elm0 elm1 ...)  ; immutable vector
    
    (define/export (vector? v)
      (and (array? v)
           (or (= (tag v) VECTOR)
               (= (tag v) IMMUTABLE-VECTOR))))
    (define/export (immutable-vector? v) (and (array? v) (= (tag v) IMMUTABLE-VECTOR)))
    (define/export (mutable-vector? v)   (and (array? v) (= (tag v) VECTOR)))
    (define/export (make-vector size v) ; v optional
      (case arguments.length
        [(1) (make-vector2 size 0)]
        [(2) (make-vector2 size v)]
        [else (error "make-vector" "expected one or two arguments")]))
    (define (make-vector2 size v)
      (var [a (Array (+ size 1))])
      (:= a 0 VECTOR)
      (for ([i in-range 1 (+ size 1)])
        (:= a i v))
      a)
    (define/export (vector) ; multiple arguments
      (var [args arguments] [n args.length] [a (Array (+ n 1))])
      (:= a 0 VECTOR)
      (for ([j in-range 0 n])
        (:= a (+ j 1) (ref args j)))
      a)
    (define/export (vector-immutable) ; multiple arguments
      (var [args arguments] [n args.length] [a (Array (+ n 1))])
      (:= a 0 IMMUTABLE-VECTOR)
      (for ([j in-range 0 n])
        (:= a (+ j 1) (ref args j)))
      a)
    (define/export (vector-length v)     (- v.length 1)) ; disregard tag
    (define/export (vector-ref v i)      (ref v (+ i 1)))
    (define/export (vector-set! vec i v) (:= vec (+ i 1) v))
    (define/export (vector->list vec)
      (var [n vec.length] [xs NULL])
      (for ([i in-range 1 n])
        (:= xs (cons (ref vec (- n i)) xs)))
      xs)
    (define/export (list->vector vs)
      (var [n   (length vs)]
           [vec (Array (+ n 1))]
           [i 1])
      (:= vec 0 VECTOR)
      (while (not (null? vs))
             (:= vec i (car vs))
             (+= i 1)
             (:= vs (cdr vs)))
      vec)
    (define/export (vector->immutable-vector vec)
      (var [n vec.length] [a (Array n)])
      (:= a 0 IMMUTABLE-VECTOR)
      (for ([j in-range 1 n])
        (:= a j (ref vec j)))
      vec)
    (define/export (vector-fill! vec v)
      (var [n vec.length])
      (for ([i in-range 1 n])
        (:= vec i v))
      VOID)
    (define/export (vector-copy! dest dest-start src src-start src-end) ; src-start, src-end optional
      (case arguments.length
        [(3) (vector-copy!5 dest dest-start src 0         (vector-length src))]
        [(4) (vector-copy!5 dest dest-start src src-start (vector-length src))]
        [(5) (vector-copy!5 dest dest-start src src-start src-end)]
        [else (error "vector-copy!" "expected 3, 4, or, 5 arguments")]))
    (define (vector-copy!5 dest dest-start src src-start src-end)
      (for ([i in-naturals dest-start]
            [j in-range     src-start src-end])
        (:= dest i (ref src j)))
      VOID)
    (define/export (build-vector n proc)
      (var [a (Array (+ n 1))])
      (:= a 0 VECTOR)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (proc i)))
      a)
    ;;;
    ;;; 4.12 Boxes
    ;;;

    ;; Representation:
    ;;   (array BOX value )

    (define/export (box? v)             (and (array? v)
                                             (or (= (tag v) BOX)
                                                 (= (tag v) IMMUTABLE-BOX))))
    (define/export (box v)              (array BOX v))
    (define/export (box-immutable v)    (array IMMUTABLE-BOX v))
    (define/export (unbox b)            (ref b 1))
    (define/export (set-box! b v)       (:= b 1 v))
    (define/export (box-cas! b old new) (if (eq? (ref b 1) old)
                                     (begin (:= b 1 new) #t)
                                     #f))
    (define/export (immutable-box? v)  (and (array? v) (= (tag v) IMMUTABLE-BOX)))
    
    ;;;
    ;;; 4.17 Procedures
    ;;;

    ;;; Closures:    
    ;;;     Representation:
    ;;;        {array CLOS label value0 ... }
    ;;;     Tagged array.
    ;;;     label       is the (JavaScript) function to call, when the closure is invoked.
    ;;;                 The first argument of label is the closure.
    ;;;     value0 ...  are the values of the free variables
    
    (define/export (closure? v)   (and (Array.isArray v) (= (ref v 0) "CLOS")))    
    (define/export (procedure? v) (or (= (typeof v) "function") (closure? v)))
    (define (js-function? v)      (= (typeof v) "function"))
    (define (invoke f) ; variadic arguments
      (var a lab [args arguments] [n args.length])
      (if (closure? f)
          (begin
            (:= lab (ref f 1))
            (:= a (Array (+ n 1)))
            (array! a 0 f)
            (for ([i in-range 0 n])
              (:= a (+ i 1) (ref args i)))
            (lab.apply #f a))
          (f.apply #f args)))    
    (define/export (apply proc xs) ; variadic
      ; (apply proc v ... lst #:<kw> kw-arg ...) → any
      ; todo: we ignore #:<kw> kw-arg ...  for the moment
      (var [n arguments.length])
      (case n
        [(0 1) (error "procedure?" "expected two or more arguments")]
        [(2)   (apply3 proc NULL xs)]
        [(3)   (apply3 proc (cons (ref arguments 1) NULL) xs)]
        [(4)   (apply3 proc (cons (ref arguments 1) (cons (ref arguments 2) NULL)) xs)]
        [(5)   (apply3 proc (cons (ref arguments 1)
                                  (cons (ref arguments 2) (cons (ref arguments 3) NULL))) xs)]
        [(6)   (apply3 proc (for/list ([i in-range 1 (- n 2)]) (ref arguments i)) xs)]))
    (define/export (apply3 proc vs xs)
      (var [nvs (length vs)]
           [nxs (length xs)]
           [n   (+ nvs nxs)]
           [a   (Array n)])
      (if (js-function? proc)
          (begin
            (for ([i in-range 0 nvs]
                  [v in-list vs])
              (:= a i v))
            (for ([i in-range nvs n]
                  [x in-list xs])
              (:= a i x))
            (proc.apply #f a))
          (if (closure? proc)
              (begin
                (array! a 0 proc)
                (for ([i in-range 0 nvs]
                      [v in-list vs])
                  (:= a (+ i 1) v))
                (for ([i in-range nvs n]
                      [x in-list xs])
                  (:= a (+ i 1) x))                
                ((ref (ref proc 1) "apply") #f a))
              "apply3 - error")))
              
    (define/export new-apply-proc apply) ; todo: support keywords    
    ;;;
    ;;; 4.18 Void
    ;;;

    ;; Representation:
    ;;   The void value is represented as the singleton VOID.
    ;; Note: The name  void  is reserved in EcmaScript 6,
    ;;       so we use the name Void.

    (define/export (void? v) (= v VOID))
    (define/export (Void) VOID) ; variadic

    ;;;
    ;;; 5 Structures
    ;;;

    ;; Representation:
    ;;   A structure is represented as an array tagged with STRUCT
    ;;       (array STRUCT <a-struct-type-descriptor> field0 field1 ...)
    ;;   A struct-type-descriptor is:
    ;;       (array STRUCT-TYPE-DESCRIPTOR name super-type total-field-count
    ;;              init-field-indices auto-field-indices auto-field-values
    ;;              properties inspector immutables guard constructor-name)

    ;;;
    ;;; 5.2 Creating Structure Types
    ;;;
    
    (define/export (struct-type-descriptor-name  std)              (ref std 1))
    (define/export (struct-type-descriptor-super std)              (ref std 2))
    (define/export (struct-type-descriptor-total-field-count  std) (ref std 3))
    (define/export (struct-type-descriptor-init-field-indices std) (ref std 4))
    (define/export (struct-type-descriptor-auto-field-indices std) (ref std 5))
    (define/export (struct-type-descriptor-auto-field-values  std) (ref std 6))
    (define/export (struct-type-descriptor-properties         std) (ref std 7))
    (define/export (struct-type-descriptor-inspector          std) (ref std 8))
    (define/export (struct-type-descriptor-immutables         std) (ref std 9))
    (define/export (struct-type-descriptor-guard              std) (ref std 10))
    (define/export (struct-type-descriptor-constructor-name   std) (ref std 11))

    (define/export (struct-type-descriptor? v) (and (array? v) (= (ref v 0) STRUCT-TYPE-DESCRIPTOR)))
    (define/export (struct? v)                 (and (array? v) (= (ref v 0) STRUCT)))

    (define/export (str-struct-type-descriptor s)
      (+ "(struct-type-descriptor "
         (str (ref s 1)) " " (str (ref s 2))  " " (str (ref s 3)) " " (str (ref s 4)) " "
         (str (ref s 5)) " " (str (ref s 6))  " " (str (ref s 7)) " " (str (ref s 8)) " "
         (str (ref s 9)) " " (str (ref s 10)) " " (str (ref s 11)) ")"))
    (define/export (str-struct s)
      (var [n s.length] [fields (for/array ([i in-range 2 n]) (ref s i))])
      (+ "(struct " (str (ref s 1)) " " (fields.join " ") ")"))
    
    
    (define/export (make-struct-type-descriptor
             name super-type init-field-count auto-field-count
             ; optionals: (handled by make-struct-type)
             auto-field-value props inspector proc-spec immutables
             guard constructor-name
             )
      (var [ifc   init-field-count]
           [afc   auto-field-count]
           [stfc  (if super-type (struct-type-descriptor-total-field-count super-type) 0)])
      (if super-type
          ;; super-case present (the hard case)
          (let ([total-field-count      (+ ifc afc stfc)]
                [new-init-field-indices (for/list ([i in-range 0 ifc]) (+ stfc i))]
                [new-auto-field-indices (for/list ([i in-range 0 afc]) (+ stfc ifc i))]
                [new-auto-field-values  (for/list ([i in-range 0 afc]) auto-field-value)])
            (array
             STRUCT-TYPE-DESCRIPTOR name super-type total-field-count
             (append (struct-type-descriptor-init-field-indices super-type) new-init-field-indices)
             (append (struct-type-descriptor-auto-field-indices super-type) new-auto-field-indices)
             (append (struct-type-descriptor-auto-field-values  super-type) new-auto-field-values)
             NULL ; properties
             #f   ; inspector
             NULL ; immutables
             #f   ; guard
             #f   ; constructor name
             ))
          ;; no super-case (the simple case)
          (let ([total-field-count  (+ ifc afc)]
                [init-field-indices (for/list ([i in-range 0 ifc])        i)]
                [auto-field-indices (for/list ([i in-range 0 afc]) (+ ifc i))]
                [auto-field-values  (for/list ([i in-range 0 afc]) auto-field-value)])
            (array STRUCT-TYPE-DESCRIPTOR name #f total-field-count
                   init-field-indices auto-field-indices auto-field-values
                   NULL ; properties
                   #f   ; inspector
                   NULL ; immutables
                   #f   ; guard
                   #f   ; constructor name
                   ))))

    (define/export (make-struct-type name super-type init-field-count auto-field-count
                              ; optionals: see docs for default value
                              auto-value props inspector proc-spec immutables
                              guard constructor-name)
      ; handle optional arguments, then call do-make-struct-type
      (do-make-struct-type name super-type init-field-count auto-field-count
                           (if (= auto-value       undefined) #f   auto-value)
                           (if (= props            undefined) NULL props)
                           ; TODO use current-inpector
                           (if (= inspector        undefined) #f   inspector) 
                           (if (= proc-spec        undefined) #f   proc-spec)
                           (if (= immutables       undefined) NULL immutables)
                           (if (= guard            undefined) #f   guard)
                           (if (= constructor-name undefined) #f   constructor-name)))
    
    (define/export (do-make-struct-type name super-type init-field-count auto-field-count
                                 auto-field-value ; one values is used in all auto-fields
                                 props inspector proc-spec immutables guard constructor-name)
      (var [super-field-count (if super-type (struct-type-descriptor-total-field-count super-type) 0)]
           [field-count       (+ init-field-count auto-field-count super-field-count)]
           [std               (make-struct-type-descriptor
                               name super-type
                               init-field-count auto-field-count 
                               auto-field-value props inspector proc-spec immutables
                               guard constructor-name)]) ; unique
      (if super-type
          (values
           ;; struct-type
           std
           ;; struct-constructor-procedure
           (λ () (var [args arguments]
                      [n    arguments.length]      ; todo: check that n = field-count
                      [a    (new Array (+ field-count 2))]) ; (+2 is tag and type descriptor)
             (:= a 0 STRUCT)
             (:= a 1 std)
             ;; fill init-fields
             (for ([i in-list (struct-type-descriptor-init-field-indices std)]
                   [j in-range 0 n])
               (:= a (+ i 2) (ref args j)))
             ;; fill auto-fields
             (for ([j in-list (struct-type-descriptor-auto-field-indices std)]
                   [v in-list (struct-type-descriptor-auto-field-values  std)])
               (:= a (+ j 2) v))
             a)
           ;; struct-predicate-procedure
           (λ (v)             (= v std))
           ;; struct-accessor-procedure
           (λ (s index)       (ref s (+ super-field-count index 2)))
           ;; struct-mutator-procedure
           (λ (s index value) (:= s (+ super-field-count index 2) value) VOID))
          ;; ----
          ;; No super-type (easy case)      
          (values
           ;; struct-type
           std
           ;; struct-constructor-procedure
           (λ () (var [args arguments]
                      [n    arguments.length]      ; todo: check that n = field-count
                      [a    (new Array (+ n 2))])
             (:= a 0 STRUCT)
             (:= a 1 std)
             ;; fill init-fields
             (for ([i in-range 0 n])
               (:= a (+ i 2) (ref args i)))
             ;; fill auto-fields
             (for ([j in-range n (+ n auto-field-count)])
               (:= a (+ j 2) auto-field-value))
             ;; done
             a)
           ;; struct-predicate-procedure
           (λ (v)             (= v std))
           ;; struct-accessor-procedure
           (λ (s index)       (ref s (+ index 2)))
           ;; struct-mutator-procedure
           (λ (s index value) (:= s (+ index 2) value) VOID))))

    (define/export (make-struct-field-accessor accessor-proc field-pos
                                               field-name) ; field-name optional
      ; Returns a field accessor that is equivalent to (lambda (s) (accessor-proc s field-pos)).
      ; The accessor-proc must be an accessor returned by make-struct-type.
      ; The name of the resulting procedure for debugging purposes is derived from field-name
      ; and the name of accessor-proc’s structure type if field-name is a symbol.
      (λ (s) (accessor-proc s field-pos))) ; todo add debug info (i.e. infered names)
      
    ;;;
    ;;; 10. Control Flow
    ;;;

    ;;; 10.1 Multiple Values
    
    ;; Representation:
    ;;   Multiple values are passed as a tagged array.
    ;; Note:
    ;;   At some point consider removing the tag.

    (define/export (values? v) (and (array? v) (= (tag v) VALUES)))  ; internal
    
    (define/export (values x) ; (values v ...)  variadic
      (var [args arguments] [n args.length] a)
      (sif (= n 1)
           (:= a x)
           (block
            (:= a (Array (+ n 1)))
            (:= a 0 VALUES)
            (for ([j in-range 0 n])
              (:= a (+ j 1) (ref args j)))))
      a)
    (define/export (call-with-values generator receiver)
      ; generator : (-> any)
      ; receiver  : procedure?
      (var [vals (generator.call #f)])
      (cond
        ;; multiple values
        [(values? vals) (vals.shift) ; removes tag
                        (receiver.apply #f vals)]
        ;; generator produced one value
        [#t             (receiver.apply #f vals)]))

    ;;;
    ;;; 13 INPUT AND OUTPUT
    ;;;

    ;;;
    ;;; 13.5 Writing
    ;;;

    (define/export (displayln v)
      (console.log (str v)))

    (define/export (newline)
      (console.log ""))

    (define/export (display v)
      (process.stdout.write (str v)))

    (define/export (write v)
      (process.stdout.write (str v)))

    (define/export (console-log str)
      (console.log str))

    ;;;
    ;;; 14 REFLECTION AND SECURITY
    ;;;

    ;;;
    ;;; 14.9 Structure Inspectors
    ;;;

    (define/export (current-inspector)
      #f)
    
    ;;;
    ;;; Higher Order
    ;;;

    ;;; FORMAT
    ;; The str functions convert values into strings.
    (define (str-null)      "()")
    (define (str-number v)  (v.toString))
    (define (str-string v)
      (var [s (if (mutable-string? v) (string->immutable-string v) v)])
      (+ "\\\"" s "\\\""))
    (define (str-list v)
      (var [a (for/array ([x in-list v]) (str x))])
      (+ "(" (a.join " ") ")"))
    (define (str-vector v)
      (var [a (for/array ([x in-vector v]) (str x))])
      (+ "#(" (a.join " ") ")"))
    (define (str-pair v)   (+ "(" (str (car v)) " . " (str (cdr v)) ")"))
    (define (str-boolean v)
      (cond [(= v #t) "#t"]
            [(= v #f) "#f"]
            [#t       "str -internal error"]))
    (define (str-symbol  v) (string-append "'"  (string->immutable-string (symbol->string v))))
    (define (str-keyword v)       (+ "#:" (ref v 1)))
    (define (str-void v)          "#<void>")
    (define (str-box v)           (+ "#&" (str (unbox v))))
    (define (str-box-immutable v) (+ "#&" (str (unbox v))))
    (define (str-char v)          (+ "#\\\\" (ref v 1)))
    (define (str-undefined)       "#<js-undefined>")
    (define/export (str v)
      (cond
        [(null? v)                     (str-null)]
        [(string? v)                   (str-string v)]
        [(number? v)                   (str-number v)]
        [(list? v)                     (str-list v)]
        [(pair? v)                     (str-pair v)]
        [(vector? v)                   (str-vector v)]
        [(boolean? v)                  (str-boolean v)]
        [(symbol? v)                   (str-symbol v)]
        [(keyword? v)                  (str-keyword v)]
        [(void? v)                     (str-void v)]
        [(immutable-box? v)            (str-box-immutable v)]
        [(box? v)                      (str-box v)]        
        [(char? v)                     (str-char v)]
        [(struct-type-descriptor? v)   (str-struct-type-descriptor v)]
        [(struct? v)                   (str-struct v)]
        [(= v undefined)               (str-undefined)]
        [#t                            (console.log v)
                                       "str - internal error"]))


    #;("tests"
       ; (str (cons 10 (cons 11 (cons 12 NULL))))
       ; (str (vector 10 11 12))
       ; (str (string-append "foo" "bar"))
       (str (string->list "foobar"))
       (str (for/list ([x in-range 0 5]) x))
       (str (reverse (for/list ([x in-range 0 5]) x)))
       (str (reverse (string->list "foobar")))
       (str (substring "foobar" 2 4))
       (str (string-length "foobar"))
       (str (string-length (string (make-char "a") (make-char "b") (make-char "c"))))
       (str (append2 (string->list "123") (string->list "45")))
       (str (make-list 5 "foo"))
       (str (array->list (list->array (string->list "123"))))
       (str (list-ref (string->list "abcde") 4))
       (str (exact-integer? 42))
       (str (exact-integer? 42.0))
       (str (exact-integer? 42.1))
       ; (str (exact-integer? 42.123))
       (make-bytes2 10 65)
       (bytes-length (make-bytes2 10 65))                        ; 10
       (str (bytes=? (make-bytes2 10 65) (make-bytes2 10 65)))   ; #t
       (str (bytes=? (make-bytes2 10 65) (make-bytes2 10 66))))  ; #f

    #;("even more tests"
    
    (string->symbol "foo")
    (string (make-char "a") (make-char "b") (make-char "c"))
    (string->symbol (string (make-char "a") (make-char "b") (make-char "c")))
    (make-primitive-string 0 "a")
    (make-primitive-string 1 "a")
    (make-primitive-string 2 "a")
    (make-primitive-string 3 "a")
    (make-primitive-string 4 "a")
    (make-primitive-string 5 "a")
    (make-primitive-string 6 "a")
    (make-primitive-string 7 "a")
    (typeof (make-primitive-string 7 "a"))
    (define as (array "a" "b"))
    (as.join "")
    (string->symbol "foo")
    (symbol? (string->symbol "foo"))
    (str (string->symbol "foo"))
    (typeof (string->symbol "foo"))
    (typeof (array 3 4 5))
    (symbol->string (string->symbol "foo"))
    (symbol->immutable-string (string->symbol "foo"))
    (gensym)
    (gensym)
    (gensym "foo")
    (symbol<? "foo" "bar")
    (symbol<? "bar" "foo")
    (string=? "foo" "bar")
    (string=? "foo" "foo")
    (string=? "foo" (immutable-string->string "foo"))
    (string=? "1" "1.0")
    (string-upcase "foo")
    (string-upcase (immutable-string->string "foo"))
    (str (vector->list (vector "a" "b" "c")))
    (str (list->vector (vector->list (vector "a" "b" "c"))))
    (str (vector->immutable-vector (vector "a" "b" "c")))
    (str (let ([v (vector 1 2 3)]) (vector-fill! v 4) v))
    (str (build-vector 5 (λ (x) (+ x 1))))
    (str (string->keyword "foo"))
    (str (apply vector (string->list "abc")))
    (str (list 1 2 3))
    (str (list-tail (list 1 2 3 4 5 6 7 8) 3))
    (str (for/list ([i in-range 0 9]) i))
    (str (list* 1 2 3 (list 4 5)))
    (str (append))
    (str (append (list 1 2 3)))
    (str (append (list 1 2 3) (list 4 5 6)))
    (str (append (list 1 2 3) (list 4 5 6) (list 7 8 9)))
    (str (append (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
    (str (reverse (list)))
    (str (reverse (list 1)))
    (str (reverse (list 1 2)))
    (str (reverse (list 1 2 3)))
    (str (reverse (list 1 2 3 4)))
    (str (map (λ (x)   (+ x 2)) (list 1 2 3 4)))
    (str (map (λ (x y) (+ x y)) (list 1 2 3 4) (list 11 12 13 14)))
    (str (andmap positive? (list 1  2 3)))
    (str (andmap positive? (list 1 -2 3)))
    (str (ormap positive? (list -1 -2 3)))
    (str (ormap positive?  (list -1 -2 -3)))
    (str (filter positive? (list -1 -2 -3 4 5 6 -7 8)))
    (str (last (list 1 2 3 4)))
    (str (box 42))
    (str (box (list 1 2 3)))
    (call-with-values (λ () (values 1 2))
                      (λ (x y) (+ x y)))
    ($+)
    ($+ 1)
    ($+ 1 2)
    ($+ 1 2 3)
    ($- 1)
    ($- 1 2)
    ($- 1 2 3)
    ($*)
    ($* 1)
    ($* 1 2)
    ($* 1 2 3)
    (max 1 2 3 4 2 3 1)
    (str (map round      (list 0.5 1.5 2.5 3.5 4.5 -0.5 -1.5 -2.5 -3.5 +inf.0 -inf.0 +nan.0)))
    (str (map Math.round (list 0.5 1.5 2.5 3.5 4.5 -0.5 -1.5 -2.5 -3.5 +inf.0 -inf.0 +nan.0)))
    (even? -2)
    (floor -4.5)
    (ceiling -4.5)
    (truncate -4.5)
    (str (number->string 123))
    (str (number->string 123 2))
    (str (number->string 123 8))
    (str (number->string 123 16))
    (string->number "123")
    (string->number "123.25")
    (string->number "101" 2)
    (modulo  10    3)  ;  1
    (modulo -10.0  3)  ;  2.0
    (modulo  10.0 -3)  ; -2.0
    (modulo +inf.0 3)  ; contract violoation
    (integer-sqrt 17)
    (integer-sqrt/remainder 17)
    (integer-sqrt -17)
    (integer-sqrt/remainder -17)
    (gcd)       ; 0
    (gcd 12)    ; 12
    (gcd 12 18) ; 6
    (gcd (* 2 3 5 7) (* 2 3 5) (* 2 3 7)) ; 6
    (symbol=? (string->symbol "a") (string->symbol "a"))             ; #t
    (symbol=? (string->symbol "a") (string->uninterned-symbol "a"))  ; #f
    (equal? (list 1 "foo" (string->symbol "a")) (list 1 "foo" (string->symbol "a")))
    (equal? (list 1 "foo" (string->symbol "a")) (list 1 "foo" (string->symbol "b")))
    (let-values ([(a b)   (values 10 11)]
                 [(c d e) (values 20 21 22)])
      (str (list a b c d e)))
    (define foo-sym (string->symbol "foo"))
    (define bar-sym (string->symbol "bar"))
    (let-values ([(struct_colon_foo foo foo? foo-ref foo-set!)
                  (make-struct-type foo-sym #f
                                    2 2 "auto-foo" (list) #f #f (list 0 1) #f foo-sym)])
      (var [foo-a (λ (s) (foo-ref s 0))]
           [foo-b (λ (s) (foo-ref s 1))])
      (var [f (foo 11 12)])
      (str (list (foo-a f) (foo-b f)))
      (let-values ([(struct_colon_bar bar bar? bar-ref bar-set!)
                    (make-struct-type bar-sym struct_colon_foo
                                      2 3 "auto-bar"(list) #f #f (list 112 113) #f bar-sym)])
        (var [g (bar 11 12 13 14)])
        (str (list (foo-a g)
                   (foo-b g)
                   (bar-ref g 4)
                   (bar-ref g 5)))
        (str f))))
  )))