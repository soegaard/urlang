#lang at-exp racket
;;;
;;; SPACE INVADERS
;;;

; Jens Axel Søgaard, Feb 2016
;     https://github.com/soegaard/urlang/urlang-examples/space-invaders

; An Urlang remake of a HtDP Space Invader game:
;     https://github.com/soegaard/space-invaders

; Which were inspired by Mary Rose Cook's JavaScript version of Space Invaders.
;     https://github.com/maryrosecook/annotated-code

; The intention of this program is to show how close you can get to the
; original HtDP program using a few macros to implement struct and struct-copy.
; The intention is not to port the JavaScript version by Mary Rose line by line.

; Try it here:
;     http://soegaard.github.io/urlang/space-invaders.html

; Have fun!

;;;
;;; Urlang Configuration
;;;

(require urlang urlang/for urlang/extra syntax/parse syntax/stx racket/syntax)

(current-urlang-run?                           #f) ; run using Node?              No: use browser
(current-urlang-echo?                          #t) ; print generated JavaScript?  Yes
(current-urlang-console.log-module-level-expr? #t) ; print top-level expression?  Yes (see console)
(current-urlang-beautify?                      #t) ; invoke js-beautify           

;;;
;;; Implementation in Urlang
;;;

; hash table from struct name to list of fields
(define structs (make-hash))
(define (register-struct name fields) (hash-set! structs (syntax-e name) fields))
(define (get-fields name)             (hash-ref  structs (syntax-e name) #f))

(define-urlang-macro struct
  (λ (stx)
    (syntax-parse stx
      [(_ name (field ...))
       (define (mangle-id x) (format-id x (mangle x) #:source x))
       (register-struct #'name (syntax->datum #'(field ...)))
       (with-syntax* ([(mfield ...)     (map mangle-id (syntax->list #'(field ...)))]
                      [(pn ...)         (map ~a (syntax->datum #'(mfield ...)))]
                      [(name-field ...) (map (λ (f) (format-id f "~a-~a" #'name f))
                                             (syntax->list #'(field ...)))]
                      [name?            (format-id #'name "~a?" #'name)]
                      [Name             (format-id #'hane (string-upcase (~a (syntax-e #'name))))])
         (syntax/loc stx
           (topblock
            (define (Name field ...) (:= this pn field) ... this)
            (define (name field ...) (new Name field ...))
            (define (name-field x)   (dot x field)) ...
            (define (name? o)        (= o.constructor Name)))))]
      [(_ name parent (field ...))
       (define parent-fields (get-fields #'parent))
       (cond
         [parent-fields
          (with-syntax ([(parent-field ...) parent-fields])
            (syntax/loc stx
              (struct name (parent-field ... field ...))))]
         [else
          (raise-syntax-error 'struct "parent struct not defined" stx)])])))

(define-urlang-macro match-define
  (λ (stx)
    (syntax-parse stx
      [(_ (name field ...) expr)
       (define orig-fields (get-fields #'name))
       (unless orig-fields
         (raise-syntax-error 'match-define "struct name not recognized" stx))
       (with-syntax ([(orig-field ...) orig-fields])
         ; todo: check that the number of fields match          
         (syntax/loc stx
           (var [tmp expr]
                [field (dot tmp orig-field)] ...)))])))

(define-urlang-macro struct-copy
  (λ (stx)
    (syntax-parse stx
      [(_ name struct-expr [field-to-change field-expr] ...)
       (displayln stx)
       (define orig-fields (get-fields #'name))
       (unless orig-fields
         (raise-syntax-error 'match-define "struct name not recognized" stx))
       (with-syntax ([(field-to-change ...) (map ~a (syntax->datum #'(field-to-change ...)))]
                     [(field ...)     orig-fields]
                     [(field-str ...) (map ~a orig-fields)])
         (syntax/loc stx
           (let ()
             (var [copy (Object.create struct-expr)])
             (:= copy field-to-change field-expr) ...
             copy)))])))

(urlang
 (urmodule space-invaders
   (import this Math isFinite Object window document Date)
   ;;;
   ;;; General purpose functions available in Racket
   ;;;
   (define (remainder x y) (% x y))
   (define (filter pred xs)
     (var [j 0] [ys (array)])
     (for ([x in-array xs])
       (when (pred x)
         (:= ys j x)
         (:= j (+ j 1))))
     ys)
   (define (filter-map proc xs)
     (var [j 0] [ys (array)] [t #f])
     (for ([x in-array xs])
       (:= t (proc x))
       (when t
         (:= ys j t)
         (:= j (+ j 1))))
     ys)
   (define (append xs ys)
     (var [zs (array)] [j 0])
     (for ([x in-array xs])
       (:= zs j x)
       (:= j (+ j 1)))
     (for ([y in-array ys])
       (:= zs j y)
       (:= j (+ j 1)))
     zs)
   (define (map f xs)
     (for/array ([x in-array xs]) (f x)))
   ;;;
   ;;; Data Representation
   ;;;
   (struct world        (player invaders bullets))
   (struct body         (x y size))
   (struct invader body (patrol-x speed-x))
   (struct bullet  body (velocity-x velocity-y))
   (struct player  body (dead?))
   ; A world represents the state of the game.
   ; A world consists of a player, a list of invaders and a list of bullets.
   ; The invaders, the bullets and the player are all bodies.
   ; All bodies are drawn as little squares.
   ; A body contains the x and y coordinate og its upper left corner as well as its size.
   ; Invaders move back and forth horisontally - this is called patrolling.
   ; The field patrol-x determines how far the invader has moved from its original position.
   ; A positive speed-x means rightward movement, and a negative sign leftward movement.
   
   ;;; Configuration
   (define width           350)
   (define height          400)
   (define the-player-size  15)
   (define the-bullet-size   3)
   (define the-invader-size 15)
   
   ;;; Smart Constructors
   
   (define (new-bullet x y vx vy)
     (bullet x y the-bullet-size vx vy))
   
   (define (new-invader x y)
     (var [patrol-x 0] [speed-x 0.3])
     (invader x y the-invader-size patrol-x speed-x))
   
   (define (new-player x y)
     (var [size 15] [dead? #f])
     (player x y size dead?))
   
   ;;;
   ;;; MODEL
   ;;;

   ;;; Creation
   
   ; create-world : -> world
   ;  the initial world contains the player and a bunch of invaders
   (define (create-world)
     (world (create-player) (create-invaders) (array)))
   
   ; create-invaders : -> (list body)
   ;   create array of twenty-four invaders
   (define (create-invaders)
     (for/array ([i in-range 0 24])
       (var [x (+ 30 (* 30 (remainder i 8)))]) ; 8 columns
       (var [y (+ 30 (* 30 (remainder i 3)))]) ; 3 rows
       (new-invader x y)))
   
   ; create-player : -> player
   (define (create-player)
     (var [x (/ width 2.)]
          [y (- height (* 2. the-player-size))])
     (new-player x y))
   
   ;;; Updaters
   
   ; update-invader : invader -> invader
   (define (update-invader i)
     (match-define (invader x y size patrol-x speed-x) i)
     ; If the invader is outside the patrol bound, we flip the speed
     (var [speed-x-factor (if (and (<= 0 patrol-x) (<= patrol-x 29)) 1 -1)])
     (var [new-speed-x    (* speed-x-factor speed-x)])
     (var [new-x          (+        x new-speed-x)])
     (var [new-patrol-x   (+ patrol-x new-speed-x)])
     (invader new-x y size new-patrol-x new-speed-x))
   
   ; update-bullet : bullet -> bullet
   (define (update-bullet b)
     (match-define (bullet x y size vx vy) b)
     (bullet (+ x vx) (+ y vy) size vx vy))
   
   ; update-player : world -> world
   (define (update-player w)
     (match-define (world p is bs) w)
     (match-define (player x y size d) p)
     (var [dead? (or d (collisions? p bs))])
     (var [moved-player
           (cond [d                   p]
                 [(key-down? "left")  (player (- x 2.) y size dead?)]
                 [(key-down? "right") (player (+ x 2.) y size dead?)]
                 [else                (player x y size dead?)])])
     (world moved-player is bs))
   
   ; spawn-invader-bullets : world -> world
   ;   maybe create new bullets below an invader
   (define (spawn-invader-bullets w)
     (match-define (world p is bs) w)
     (var [harder (/ (- 24 is.length) 1000)]) ; increases as invaders die
     (var [maybe-spawn-bullet
           (λ (i)
             (match-define (invader x y size _ _) i)
             ; once in a while, if no friends below, create a bullet
             (if (and (> (Math.random) (- 0.995 harder))
                      (not (invader-below? i is)))
                 (bullet x (+ y size 1) 3 (- (Math.random) 0.5) 2.)
                 #f))])
     (var [new-bullets (filter-map maybe-spawn-bullet is)])
     (world p is (append new-bullets bs)))
   
   (define (invader-below? i is)
     (match-define (body x y size) i)
     (for/or ([b in-array is])
       (and (<= x b.x (+ x size))
            (> b.y (+ y size)))))
   
   ; spawn-player-bullet : world -> world
   ;   a non-dead player shoots when space is pressed
   (define (spawn-player-bullet w)
     (match-define (world p is bs) w)
     (cond
       [(player-dead? p) w] ; no shooting, when dead
       [(key-just-down? "space") ; space => new bullet
        (match-define (player x y size d) p)
        (var [b (new-bullet (+ x (/ size 2.)) y 0 -7)])
        (bs.push b) ; sigh: not functional
        (world p is bs)]
       [else w]))
   
   ;;; UPDATES
   
   (define (update w)
     (restart-on-r
      (remove-colliding-bodies
       (spawn-player-bullet
        (spawn-invader-bullets
         (update-player
          (update-invaders
           (update-bullets w))))))))

   (define (update-invaders w)
     (var [is (world-invaders w)])
     (struct-copy world w [invaders (map update-invader is)]))
   
   (define (update-bullets w)
     (struct-copy world w [bullets (map update-bullet (world-bullets w))]))
   
   (define (restart-on-r w)
     (if (key-down? "r")
         (create-world)
         w))

   ;;; Collision
   
   (define (colliding? b1 b2)
     (match-define (body x1 y1 s1) b1)
     (match-define (body x2 y2 s2) b2)
     (not (or (= b1 b2)
              (< (+ x1 s1) x2) (> x1 (+ x2 s2))
              (< (+ y1 s1) y2) (> y1 (+ y2 s2)))))
   
   (define (collisions? x bs)
     (for/or ([b in-array bs]) (colliding? x b)))
   
   (define (inside-screen? b)
     (match-define (body x y size) b)
     (and (< -40 x (+ width  40))
          (< -40 y (+ height 40))))
   
   (define (remove-colliding-bodies w)
     (match-define (world p is bs) w)
     (var [no-bullet-collisons?  (λ (x) (not (collisions? x bs)))]
          [no-invader-collisons? (λ (x) (not (collisions? x is)))])
     (world p 
            (filter no-bullet-collisons? is)
            (filter inside-screen? ; remove non-visible bullets
                    (filter no-invader-collisons? bs))))

   ;;; DRAWING

   ; draw-bodies : (list body) drawing-context -> void
   ;   draw the bodies in the world w to the drawing context dc
   (define (draw-bodies bs dc)
     (for ([b in-array bs])
       (match-define (body x y s) b)
       (var [c (cond [(and (player? b) (player-dead? b)) "purple"]
                     [(player? b)                        "blue"]
                     [(invader? b)                       "red"]
                     [else                               "black"])])
       (:= dc "strokeStyle" c)
       (:= dc "fillStyle" c)
       (fill-rect dc x y s s)))

   (define (fill-rect dc x y w h)
     (dot dc (fillRect (+ x 0.5) (+ y 0.5) w h)))
   
   (define (clear-all dc)
     (:= dc "fillStyle" "white")
     (fill-rect dc 0 0 width height))
   
   (define (draw-world w dc)
     (match-define (world p is bs) w)
     (clear-all dc)
     (draw-bodies (append (array p) (append is bs)) dc))
   
   ;;; GUI STATE

   (define the-world (create-world))

   ;;; Keyboard
   ; The keyboard state is kept in a hash table. 
   ; Use key-down? to find out, whether a key is pressed or not.
   (define the-keyboard  (array))
   (define (key-up! k)   (:= the-keyboard k #f))
   (define (key-down? k) (not (key-up? k)))
   (define (key-up? k)
     (var [state (ref the-keyboard k)])
     (or (= state #f)          ; was released
         (= state undefined))) ; never pressed     
   (define (key-down! k)
     (when (key-up? k) ; prevent two keydowns without a keyup inbetween
       (:= the-keyboard k (Date.now))))
   (define (key-just-down? k)
     (var [old-ts (ref the-keyboard k)])
     (var [new-ts (Date.now)])
     (and (key-down? k)
          (< (- new-ts old-ts) 20)))

   ;;;
   ;;; BIG BANG FRAMEWORK
   ;;;

   ;;; Canvas

   ; Key events sent to the canvas updates the information in the-keyboard.
   ; Paint events calls draw-world. To prevent flicker we suspend flushing
   ; while drawing commences.
   (define (on-keydown event)
     (key-down! (keycode->key event.keyCode)))

   (define (on-keyup event)
     (key-up! (keycode->key event.keyCode)))

   (define (keycode->key k)
     (cond
       [(= k 38) "up"]
       [(= k 40) "down"]
       [(= k 37) "left"]
       [(= k 39) "right"]
       [(= k 32) "space"]
       [(= k 65) "left"]  ; a
       [(= k 83) "down"]  ; s
       [(= k 87) "up"]    ; w
       [(= k 68) "right"] ; d
       [(= k 82) "r"]     ; r
       [else     #f]))

   ;; Attach on-keydown and on-keyup to the canvas
   (var [the-canvas          (document.getElementById "the-canvas")])
   (var [the-drawing-context (the-canvas.getContext "2d")])
   (dot the-canvas (addEventListener "keydown" on-keydown))
   (dot the-canvas (addEventListener "keyup"   on-keyup))
   ;;; Timer
   
   (define (on-timer)
     (:= the-world (update the-world))
     (draw-world the-world the-drawing-context))
   
   ; Start a timer. Each time the timer triggers, the world is updated.
   (var [interval-id (window.setInterval on-timer 20)]) ; in milliseconds
   

   ;;; Keyboard
   ; Let the canvas grab the keyboard focus from the start.
   ; Note: This requires a tabindex attribute in the html.
   (the-canvas.focus)
   ))

;;; Urls for JavaScript and CSS libraries used in this example.

(define (generate-html)
  @~a{
 <!DOCTYPE html>
 <html lang="en">
   <head> <meta charset="utf-8">
          <title>Space Invaders</title>
  </head>
  <body>   
     <h1>Space Invaders in Urlang</h1>
     <p>Use <tt>left</tt> or <tt>right</tt> to move</p>
     <p>Use <tt>space</tt> to shoot.</p>
     <p>Use <tt>r</tt> to restart</p>
     <p>See source code on Github:
        <a href=
   "https://github.com/soegaard/urlang/blob/master/urlang-examples/space-invaders/space-invaders.rkt">
        space-invaders.rkt</a>
     <p><canvas id="the-canvas" width="350" height="400" tabindex="1"/></p>
     <script>@file->string{space-invaders.js}</script>
 </body>
 </html>})


;;; Conveniency

; In DrRacket (send-url/contents some-html) will display the 
; html in a browser - saves time.

(require net/sendurl)

(send-url/contents (generate-html))
