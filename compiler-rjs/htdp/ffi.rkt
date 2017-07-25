#lang racket
(define assoc->object values)
(define (js-ref  . xs) (cons 'js-ref  xs))
(define (js-set! . xs) (cons 'js-set! xs))
(define (js-call . xs) (cons 'js-call xs))
(define (js-undefined) 'undefined)

;;; Types
(struct ffi-type (name check ex im))

;;; Real
(define (check-real x)  (number? x))
(define (export-real x) x)
(define (import-real x) x)
(define Real (ffi-type "Real" check-real export-real import-real))
;;; Boolean
(define (check-boolean x) #t)
(define (boolify x) (if x #t #f))
(define Boolean (ffi-type "Boolean" check-boolean boolify boolify))
;;; String (accepts symbols too, but returns strings
(define (check-string x)      (or (string? x) (symbol? x)))
(define (export-string x) (if (string? x) (string->immutable-string x) (symbol->string x)))
(define (import-string x) x)
(define String (ffi-type "String" check-string export-string import-string))
;;; Number01 (a number between 0 and 1)
(define (check-number01 x)  (and (number? x) (<= 0 x) (<= x 1)))
(define (export-number01 x) x)
(define (import-number01 x) x)
(define Number01 (ffi-type "Number01" check-number01 export-number01 import-number01))
;;; Dict (an object with properties used as key-value pairs)
(define (export-dict x) (if (list? x) (assoc->object x) x))
(define (import-dict x) (error 'import-dict "TODO") x)
(define Dict (ffi-type "Dict" list? export-dict import-dict))
;;; Void
(define (always-true x) #t)
(define Void           (ffi-type "Void"           always-true void void))
;;; Do Nothing 
(define Any            (ffi-type "Any"            always-true #f #f))
(define CanvasPattern  (ffi-type "CanvasPattern"  always-true #f #f))
(define CanvasGradient (ffi-type "CanvasGradient" always-true #f #f))
(define LineDashArray  (ffi-type "LineDashArray"  always-true #f #f))
(define TextMetrics    (ffi-type "TextMetrics"    always-true #f #f))
(define Path2D         (ffi-type "Path2D"         always-true #f #f))
(define ImageData      (ffi-type "ImageData"      always-true #f #f))
(define Element        (ffi-type "Element"        always-true #f #f))
(define Segments       (ffi-type "Segments"       always-true #f #f))
(define Image          (ffi-type "Image"          always-true #f #f))
; An Image can be one of these:
;   HTMLImageElement (<img>),
;   HTMLVideoElement (<video>),
;   HTMLCanvasElement (<canvas>),
;   CanvasRenderingContext2D,
;   ImageBitmap,
;   ImageData, or a
;   Blob.

(require (for-syntax syntax/parse) syntax/parse)

(begin-for-syntax
  (define-syntax-class Argument ; argument in a method
    (pattern name:id                 #:attr type #f)
    (pattern [name:id ffi-type-name] #:attr type #'ffi-type-name)))

(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_define-interface interface-name
      #:inherits   (super ...)
      #:properties [rkt-prop-name js-prop-name] ...
      #:methods    [rkt-method-name
                    (js-method-name ma:Argument ... (~optional (~seq #:optional optma:Argument ...)
                                                               #:defaults ([(optma.name 1) null])))
                    return-type] ...)
     (define (add-! id)
       (datum->syntax id (string->symbol (string-append (symbol->string (syntax-e id)) "!"))))
     (with-syntax
         ([(rkt-prop-name! ...)     (map add-!          (syntax->list  #'(rkt-prop-name ...)))]
          [(js-prop-name-str ...)   (map symbol->string (syntax->datum #'(rkt-prop-name ...)))]
          [(js-method-name-str ...) (map symbol->string (syntax->datum #'(rkt-method-name ...)))]
          [((arg    ...) ...)       (attribute    ma.name)]
          [((type ...) ...)         (attribute ma.type)]
          [((optarg ...) ...)       (attribute optma.name)]
          [((opttype ...) ...)      (map (λ(x) (if (not x) '() x)) (attribute optma.type))])
         (syntax/loc stx
           (begin
             (define undef (js-undefined))
             ;; Properties
             (define (rkt-prop-name   obj)
               (js-ref  obj js-prop-name-str))
             ...
             (define (rkt-prop-name!  obj val)
               (js-set! obj js-prop-name-str val))
             ...
             (define (rkt-method-name obj arg ... [optarg undef] ...)
               (define who 'rkt-method-name)
               (let ([arg (if type
                              (if ((ffi-type-check type) arg)
                                  (let ([ex (ffi-type-ex type)])
                                    (if ex (ex arg) arg))
                                  (error who "expected ~a for the ~a argument, got: ~a"
                                         (ffi-type-name type) 'arg arg))
                              arg)]
                     ...)
                 (let ([optarg (if (not (eq? optarg undef))
                                   (if opttype
                                       (if ((ffi-type-check opttype) optarg)
                                           (let ([ex (ffi-type-ex opttype)])
                                             (if ex (ex optarg) optarg))
                                           (error who
                                                 "expected ~a for the ~a (optional) argument, got: ~a"
                                                 (ffi-type-name opttype) 'optarg optarg))
                                       optarg)
                                   undef)]
                       ...)
                   (let ([im  (ffi-type-ex return-type)]
                         [val (js-call obj js-method-name-str arg ... optarg ...)])
                     (if im (im val) val)))))
               ...)))]))

;;; HTMLCanvasElement
(define-interface HTMLCanvasElement
  #:inherits   (HTMLElement)
  #:properties
  [canvas-height height]
  [canvas-width  width]
  #:methods    
  [get-context  (getContext [contextType String] [contextAttributes Dict])                   Any]
  [to-blob      (toBlob  callback #:optional [mimeType String] [qualityArguments Number01])  Any]
  [to-data-url  (toDataUrl        #:optional [type     String] [encoderOptions   Number01])  Any])

(define-interface CanvasRenderingContext2D
  #:inherits   ()
  #:properties
  [context-canvas                     canvas]
  [context-fill-style                 fillStyle]
  [context-font                       font]
  [context-global-alpha               globalAlpha]
  [context-global-composite-operation globalCompositeOperation]
  [context-line-cap                   lineCap]
  [context-line-dash-offset           lineDashOffset]
  [context-line-join                  lineJoin]
  [context-line-width                 lineWidth]
  [context-miter-limit                miterLimit]
  [context-shadow-blur                shadowBlur]
  [context-shadow-color               shadowColor]
  [context-shadow-offset-x            shadowOffsetX]
  [context-shadow-offset-y            shadowOffsetY]
  [context-stroke-style               strokeStyle]
  [context-text-align                 textAlign]
  [context-text-baseline              textBaseline]
  #:methods    
  [arc  (arc [x Real] [y Real] [radius Real] [startAngle Real] [endAngle Real]
             #:optional [anticlockwise Boolean])
        Void]
  [arc-to            (arc-to [x1 Real] [y1 Real] [x2 Real] [y2 Real] [radius Real])              Void]
  [begin-path        (begin-path)                                                                Void]
  [bezier-curve-to   (bezier-curve-to [cp1x Real] [cp1y Real]
                                      [cp2x Real] [cp2y Real] [x Real] [ey Real])                Void]
  [clear-rect             (clearRect [x Real] [y Real] [width Real] [height Real])               Void]
  [clip                   (clip)                                                                 Void]
  [clip/rule              (clip               [fillRule String])                                 Void]
  [clip/path/rule         (clip [path Path2D] [fillRule String])                                 Void]
  [close-path             (closePath)                                                            Void]
  [create-image-data      (createImageData [width Real] [height Real])                      ImageData]
  [create-image-data/img  (createImageData [imagedata ImageData])                           ImageData]
  [create-linear-gradient (createLinearGradient [x0 Real] [y0 Real] [x1 Real] [y1 Real])         Void]
  [create-pattern         (create-pattern [image Image] [repetition String])            CanvasPattern]
  [create-radial-gradient (createRadialGradient [x0 Real] [y0 Real] [r0 Real]
                                                [x1 Real] [y1 Real] [r1 Real])         CanvasGradient]
  [draw-focus-if-needed   (drawFocusIfNeeded               [element Element])                    Void]
  [draw-focus-if-needed/path (drawFocusIfNeeded [path Path2D] [element Element])                 Void]
  [draw-image             (drawImage [image Image] [dx Real] [dy Real])                          Void]
  [draw-image/size        (drawImage [image Image] [dx Real] [dy Real]
                                     [dWidth Real] [dHeight Real])                               Void]
  [draw-image/sub         (drawImage [image Image] [sx Real] [sy Real]
                                     [sWidth Real] [sHeight Real]
                                     [dx Real] [dy Real] [dWidth Real] [dHeight Real])           Void]
  [fill                   (fill)                                                                 Void]
  [fill/rule              (fill               [fillRule String])                                 Void]
  [fill/path/rule         (fill [path Path2D] [fillRule String])                                 Void]
  [fill-rect              (fillRect [x Real] [y Real] [width Real] [height Real])                Void]
  [fill-text              (fillText [text String] [x Real] [y Real] #:optional [maxWidth Real])  Void]
  [get-image-data         (getImageData [sx Real] [sy Real] [sw Real] [sh Real])            ImageData]
  [get-line-dash          (getLineDash) LineDashArray]
  [is-point-in-path       (isPointInPath [x Real] [y Real] #:optional [fillRule String])      Boolean]
  [is-point-in-path/path  (isPointInPath [path Path2D]     #:optional [fillRule String])      Boolean]
  [is-point-in-stroke     (isPointInStroke [x Real] [y Real])                                 Boolean]
  [is-point-in-stroke/path(isPointInStroke [path Path2D])                                     Boolean]
  [line-to                (lineTo [x Real] [y Real])                                             Void]
  [measure-text           (measureText [text String])                                     TextMetrics]
  [move-to                (moveTo [x Real] [y Real])                                             Void]
  [put-image-data         (putImageData [imagedata ImageData] [dx Real] [dy Real])               Void]
  [put-image-data/dirty   (putImageData [imagedata ImageData] [dx Real] [dy Real]
                                        [dirtyX Real] [dirtyY Real]
                                        [dirtyWidth Real] [dirtyHeight Real])                    Void]
  [quadratic-curve-to     (quadraticCurveTo [cpx Real] [cpy Real] [x Real] [y Real])             Void]
  [rect                   (rect [x Real] [y Real] [width Real] [height Real])                    Void]
  [restore                (restore)                                                              Void]
  [rotate                 (rotate [angle Real])                                                  Void]
  [save                   (save)                                                                 Void]
  [scale                  (scale [x Real] [y Real])                                              Void]
  [set-line-dash          (setLineDash [segments Segments])                                      Void]
  [set-transform          (setTransform [a Real] [b Real] [c Real] [d Real] [e Real] [f Real])   Void]
  [stroke                 (stroke #:optional [path Path2D])                                      Void]
  [stroke-rect            (strokeRect [x Real] [y Real] [width Real] [height Real])              Void]
  [stroke-text            (strokeText [text String] [x Real] [y Real]#:optional [maxWidth Real]) Void]
  [transform              (transform [a Real] [b Real] [c Real] [d Real] [e Real] [f Real])      Void]
  [translate              (translate [x Real] [y Real])                                         Void])
