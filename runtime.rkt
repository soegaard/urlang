#lang racket

(require "urlang.rkt" "urlang-extra.rkt" "for.rkt" syntax/parse)

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


(display
 (urlang
  (urmodule runtime
    (import document window prompt alert)  ; Browser functions
    ; (export null? pair? list? cons car cdr)
    (import Array Int8Array Number String
            new typeof)
    ;;; Array
    (define (array? v) ; todo : inline array?
      (Array.isArray v))
    ;;; Tags
    (define (tag a) (ref a 0))
    (define PAIR           (array "pair"))
    (define VECTOR         (array "vector"))
    (define CHAR           (array "char"))
    (define MUTABLE-STRING (array "mutable-string"))
    (define BYTES          (array "bytes"))
    (define MUTABLE-BYTES  (array "mutable-bytes"))
    (define SYMBOL         (array "symbol"))
    ;; Empty
    (define NULL (array))
    (define (null? v) (= v NULL))
    ;;; Pairs
    (define (pair? v)      (and (array? v) (= (tag v) PAIR)))
    (define (list? v)      (or (= v NULL) (and (array? v) (= (tag v) PAIR) (ref v 1))))
    (define (cons a d)     (array PAIR (list? d) a d))
    (define (unsafe-car p) (ref p 2))
    (define (unsafe-cdr p) (ref p 3))
    (define car unsafe-car)
    (define cdr unsafe-cdr)
    ;;; Vectors
    (define (vector? v)       (and (array? v) (= (tag v) VECTOR)))
    (define (vector-length v) (- v.length 1)) ; disregard tag
    (define (vector-ref v i)  (ref v (+ i 1)))      
    (define (vector) ; multiple arguments
      (var [args arguments] [n args.length] [a (Array (+ n 1))])
      (:= a 0 VECTOR)
      (for ([j in-range 0 n])
        (:= a (+ j 1) (ref args j)))
      a)
    ;;;
    ;;; 4.1 Booleans and equality
    ;;;

    ;; Representation: #t and #f are represented as true and false respectively,
    ;; where true and false are JavaScript booleans.
    (define (boolean? v)
      (= (typeof v) "boolean"))
    ; (define (not x) (if x #f #t)) ; not is a predefined function
    (define (equal? v1 v2)
      (= v1 v2))   ; TODO ...
    (define (eqv? v1 v2)  ; TODO
      (= v1 v2))
    (define (eq? v1 v2)
      (= v1 v2))
    ; (define (equal/retur v1 v2) ...) TODO
    #;(define (immutable? v)
        ; Note: This follows the spec. It is not a general
        ;       immutability predicate, so immutable pairs are not checked here.
        (or #t
            #;(immutable-string? v)
            #;(immutable-bytes? v)
            #;(immutable-vector? v)
            #;(immutable-hash? v)
            #;(immutable-box? v)))
    ;;;
    ;;; 4.2 Numbers
    ;;;
    (define (exact-integer? v)
      ; http://stackoverflow.com/questions/3885817/how-to-check-that-a-number-is-float-or-integer
      (and (= (typeof v) "number")
           (= v (+ v))
           (= v (bit-or v 0))))


    ;;;
    ;;; 4.4 Byte Strings
    ;;;

    ;; A byte string is a fixed-length array of bytes.
    ;; A byte is an exact integer between 0 and 255 inclusive.
    ;; A byte string can be mutable or immutable.

    ;; Representation: Byte strings are represented as Int8Array
    ; http://caniuse.com/#feat=typedarrays
    (define (bytes? v)
      (and (array? v)
           (or (= (tag v) BYTES)
               (= (tag v) MUTABLE-BYTES))))
    (define (bytes->Int8Array bs)
      (ref bs 1))    
    (define (make-bytes2 k b) ; b is optional
      ; Returns a new mutable byte string of length k where each position
      ; in the byte string is initialized with the byte b.
      (var [bs (new Int8Array k)])
      ; (sunless (= b 0) (bs.fill 0 k b))  ; ES6 not supported in Node/Chrome
      (for ([i in-range 0 k])
        (:= bs i b))
      (array MUTABLE-BYTES bs))
    (define (make-bytes1 k) ; b = 0 is optional
      ; Returns a new mutable byte string of length k where each position
      ; in the byte string is initialized with the byte b.
      (var [is (new Int8Array k)])
      (array MUTABLE-BYTES is))
    (define (bytes) ; (bytes b ...)
      ; make new mutable byte string
      (var [args arguments]
           [n    args.length]
           [is   (Int8Array n)])
      (is.set args)
      (array MUTABLE-BYTES is))
    (define (bytes->immutable-bytes bs)
      (array BYTES (Int8Array.from (ref bs 1))))
    (define (immutable-bytes->bytes bs)
      (array MUTABLE-BYTES (Int8Array.from (ref bs 1))))
    (define (byte? v)
      (and (exact-integer? v)
           (<= 0 v 255)))
    (define (bytes-length bs)
      (ref (ref bs 1) "length"))
    (define (bytes-ref bs k)
      (ref (ref bs 1) k))
    (define (bytes-set! bs k b)
      (var [is (ref bs 1)])
      (:= is k b))
    (define (subbytes3 bs start end) ; end is optional
      (var [is (ref bs 1)]
           [k  (- end start)]
           [t  (new Int8Array k)])
      (for ([i in-range start end])
        (:= t i (ref is i)))
      (array MUTABLE-BYTES is))
    (define (subbytes2 bs start) ; end is optional
      (var [is  (ref bs 1)]
           [end (ref is "length")]
           [k   (- end start)]
           [t   (new Int8Array k)])
      (for ([i in-range start end])
        (:= t i (ref is i)))
      (array MUTABLE-BYTES is))
    (define (bytes-copy bs)
      (subbytes2 bs 0))
    ; bytes-copy!       ; todo
    ; bytes-fill!       ; todo
    ; bytes-append      ; todo
    ; bytes->list       ; todo
    ; list->bytes       ; todo
    ; make-shared-bytes ; todo
    ; shared-bytes      ; todo

    ;;; 4.4.2 Byte String Comparisons
    (define (bytes=? bs1 bs2)
      (var [n1 (bytes-length bs1)]
           [n2 (bytes-length bs2)]
           [is1 (ref bs1 1)]
           [is2 (ref bs2 1)])
      (and (= n1 n2)
           (for/and ([i in-range 0 n1])
             (= (ref is1 i) (ref is2 i)))))

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

    ; string->unreadable-symbol  ; TODO
    
    (define (symbol? v)
      (and (array? v) (= (tag v) SYMBOL)))
    (define (symbol-interned? sym) ; symbol? -> boolean?
      (= (typeof sym) "string"))   ; note : an uninterned strings has type "object"
    (define (symbol->string sym)
      ; returns freshly allocated mutable string
      (var [js-str (ref sym 1)]
           [n      js-str.length]
           [a      (Array (+ n 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (ref js-str i)))
      a)

    ;    (define (symbol->immutable-string sym)
    ;      (cond
    ;        [(string-interned? sym) (ref sym 1)]
    ;        [else                   (var
    ;         (String 
    (define (string->symbol str)
      ; returns interned symbol
      (var [t (typeof str)] r)
      (scond        
       [(= t "string")  (:= r (array SYMBOL str))] ; primtive string
       [(= t "object")  (var [n+1 t.length] [n (- n+1 1)] [a (Array n)])
                        (for ([i in-range 0 n])
                          (:= a i (ref str (+ i 1))))
                        (:= r (array SYMBOL (String (a.join ""))))]
       [#t (error "string->symbol" "expected a string")])
      r)
    (define (error who msg)
      (console.log (+ "error: " who ": " msg)))
    
    ;;;
    ;;; 4.9-10 Lists
    ;;;

    ;; Representation:
    ;;   A list is either NULL or a pair with a true list? flag.
    ;;   That is, a list is either:
    ;;      NULL or {array PAIR true a d}
    ;;   where d is a list.
    (define (length xs)
      (var (n 0))
      (while (pair? xs) (+= n 1) (:= xs (cdr xs)))
      n)
    (define (list->array xs)
      ;; convert list to (non-tagged) JavaScript array
      ; allocate array
      (var a [n (length xs)])
      (:= a (Array n))
      ; fill it
      (for ([x i in-list xs])
        (:= a i x))
      a)
    (define (array->list axs)
      ;; convert JavaScript array to Racket list
      (var [n axs.length] [n-1 (- n 1)] [xs NULL])
      (for ([i in-range 0 n])
        (:= xs (cons (ref axs (- n-1 i)) xs)))
      xs)
    (define (reverse xs)
      ; Note: for/list uses reverse, so reverse can't use for/list
      (var [result NULL])
      (while (not (null? xs))
             (:= result (cons (car xs) result))
             (:= xs (cdr xs)))
      result)
    (define (append2 xs ys)
      ; note xs and ys are immutable, so ys can be reused.
      (var [ret NULL])
      (scond
       [(null? ys) xs]
       [#t        (var [axs  (list->array xs)]
                       [n axs.length]
                       [n-1 (- n 1)])
                   (:= ret ys)
                   (for ([i in-range 0 n])
                     (:= ret (cons (ref axs (- n-1 i)) ret)))])
      ret)
    (define (make-list k v)
      ; Returns a newly constructed list of length k, holding v in all positions.
      (for/list ([i in-range 0 k]) v))
    (define (list-ref xs i)
      ;; Return the i'th index of xs.
      ;; Use fast path for i=0 and i=1.
      (var ret)
      (scond [(= i 0) (:= ret (car xs))]
             [(= i 1) (:= ret (car (cdr xs)))]
             [#t      (for ([j in-range 0 (- i 1)])
                        (:= xs (cdr xs)))
                      (:= ret (car xs))])
      ret)
    
    ;;;
    ;;; 4.5 Characters
    ;;;
    
    ;; Characters range over Unicode scalar values, which includes characters whose values
    ;; range from #x0 to #x10FFFF, but not including #xD800 to #xDFFF.
    ;; The scalar values are a subset of the Unicode code points.
    (define (make-char js-string) (array CHAR js-string))
    (define (char->string c)      (ref c 1))
    (define (char->integer c)
      (var [s (ref c 1)])
      (s.charCodeAt 0))
    ;;;  
    ;;; Strings
    ;;;
      
    ;; Representation
    ;;   - immutable Racket strings are represented as JavaScript strings
    ;;   - mutable Racket string are represented as a tagged array:
    ;;       {array MUTABLE-STRING "char0" "char1" ...}
    ;;     where char0 is a javascript string with length 1.
    (define (string? v)
      (or (= (typeof v) "string")
          (and (array? v) (= (tag v) MUTABLE-STRING))))
    (define (make-string k ch) ; TODO: make ch optional
      ; make-string produces a mutable string
      (var [a (Array (+ k 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 1 (+ k 1)])
        (:= a i (ref ch 1)))
      a)
    (define (make-primitive-string n c) ; make primitive js string of length n
      ; http://stackoverflow.com/questions/202605/repeat-string-javascript?rq=1
      (var [s ""])
      (while #t
             (sunless (= (bit-and n 1) 0) (+= s c))
             (>>= n 1)
             (sif (= n 0) (break) (+= c c)))
      s)
     ; (define (string->immutable-string s)
     ; (define (fail) (/ 1 0) #;(error 'string->immutable-string "expected string, got" s))
     ; (case (typeof s)
     ;   [("string") s]
     ;   [("array") (if (= (tag v) MUTABLE-STRING)  ; FIX arrays has type object
    (define (string) ; ch ... multiple arguments
      (var [args arguments] [n args.length])
      (var [a (Array (+ n 1))])
      (:= a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (:= a (+ i 1) (ref (ref args i) 1)))
      a)
    (define (string-length s)
      (if (= (typeof s) "string")
          s.length
          (- s.length 1)))
    (define (string-ref s i)
      (if (= (typeof s) "string")
          (array CHAR (ref s    i))
          (array CHAR (ref s (+ i 1)))))
    (define (string-set! s i c)
      ; (unless (mutable-string? s) (raise ...))
      ; (unless (char? c)           (raise ...))
      (:= s (+ i 1) (ref c 1)))
    (define (substring3 str start end) (str.substring start end))
    (define (substring2 str start)     (str.substring start (string-length str)))
    #;(define (substring) ; case-lambda
        (case arguments.length
          [(2) (substring2 (ref arguments 0) (ref arguments 1))]
          [(3) (substring3 (ref arguments 0) (ref arguments 1) (ref arguments 2))]))    
    ; todo: string-copy
    ; todo: string-copy!
    ; todo: string-fill!
    ; todo: string->list
    ; todo: list->string
    ; todo: build-string
    ; todo: string comparisons    
    (define (string->list s)          (for/list ([c in-string s]) c))
    (define (string-append2 str1 str2) (str1.concat str2))
    (define (string-append) ; variadic
      (var [args arguments])
      (var [as (for/array #:length args.length ([a in-array args]) a)])
      (as.join ""))           
    (define (string-downcase str)     (str.toLowerCase))
    (define (string-upcase   str)     (str.toUpperCase))
    
    ;;; Numbers
    (define (number? v) (= (typeof v) "number"))
    
    ;;;
    ;;; Higher Order
    ;;;

    ;;; FORMAT
    ;; The str functions convert values into strings.
    (define (str-null)      "()")
    (define (str-number v)  (v.toString))
    (define (str-string v)  (+ "\\\"" v "\\\""))
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
    (define (str-symbol v) (string-append "'" (symbol->string v)))
    (define (str v)
      (cond
        [(null? v)    (str-null)]
        [(string? v)  (str-string v)]
        [(number? v)  (str-number v)]
        [(list? v)    (str-list v)]
        [(pair? v)    (str-pair v)]
        [(vector? v)  (str-vector v)]
        [(boolean? v) (str-boolean v)]
        [(symbol? v)  (str-symbol v)]
        [#t          "str - internal error"]))

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
  )))