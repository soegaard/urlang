#lang racket

;; TODO Fix the bug below. The second s has not been alpha-renamed.

;> function string__glist(s){
;    return ((function(xs){{
;              var s=s,n=s.length,i=0;
;              while((true&&true&&(i<n))){
;                ((function(c){
;                  (xs=(cons(c,xs)));
;                  return (i+=1);})(s[i]));
;               };undefined;
;             }
;            return xs;
;         })(NULL));};

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
    (export null? pair? list? cons car cdr)
    (import Array Number String typeof)
    ;;; Array
    (define (array? v) ; todo : inline array?
      (Array.isArray v))
    ;;; Tags
    (define (tag a) (ref a 0))
    (define PAIR           (array "pair"))
    (define VECTOR         (array "vector"))
    (define CHAR           (array "char"))
    (define MUTABLE-STRING (array "mutable-string"))
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
      (array! a 0 VECTOR)
      (for ([j in-range 0 n])
        (array! a (+ j 1) (ref args j)))
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
    ;;; 4.4 Byte Strings
    ;;;

    ;; A byte string is a fixed-length array of bytes.
    ;; A byte is an exact integer between 0 and 255 inclusive.
    ;; A byte string can be mutable or immutable.

    
    
    
    ;;;
    ;;; 4.9,4.10 Lists
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
        (array! a i x))
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
      
    ;;; Strings
    ;; Representation
    ;;   - immutable Racket strings are represented as JavaScript strings
    ;;   - mutable Racket string
    ;;       {array MUTABLE-STRING "char0" "char1" ...}
    (define (string? v)
      (or (= (typeof v) "string")
          (and (= (typeof v) "array")
               (= (tag v) MUTABLE-STRING))))
    (define (make-string k ch) ; TODO: make ch optional
      ; make-string produces a mutable string
      (var [a (Array (+ k 1))])
      (array! a 0 MUTABLE-STRING)
      (for ([i in-range 1 (+ k 1)])
        (array! a i ch))
      a)
     ; (define (string->immutable-string s)
     ; (define (fail) (/ 1 0) #;(error 'string->immutable-string "expected string, got" s))
     ; (case (typeof s)
     ;   [("string") s]
     ;   [("array") (if (= (tag v) MUTABLE-STRING)
                       
                       
             
    
    (define (string) ; ch ... multiple arguments
      (var [args arguments] [n args.length])
      (var [a (Array (+ n 1))])
      (array! a 0 MUTABLE-STRING)
      (for ([i in-range 0 n])
        (array! a (+ i 1) (ref args i)))
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
      (array! s (+ i 1) (ref c 1)))
    (define (substring3 str start end) (str.substring start end))
    (define (substring2 str start)     (str.substring start (string-length str)))
    (define substring substring3)      ; todo: handle optional arguments
    ; todo: string-copy
    ; todo: string-copy!
    ; todo: string-fill!
    ; todo: string-append
    ; todo: string->list
    ; todo: list->string
    ; todo: build-string
    ; todo: string comparisons    
    (define (string->list s)          (for/list ([c in-string s]) c))
    (define (string-append str1 str2) (str1.concat str2))
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
    (define (str v)
      (if (null? v) (str-null)
          (if (string? v) (str-string v)
              (if (number? v) (str-number v)
                  (if (list? v) (str-list v)
                      (if (pair? v) (str-pair v)
                          (if (vector? v) (str-vector v)
                              "str - internal error")))))))
      
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
  )))