#lang racket

(require html-writing urlang net/sendurl)

(current-urlang-run?                           #f) ; run using Node?
(current-urlang-echo?                          #f) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?

(define jquery "https://ajax.googleapis.com/ajax/libs/jquery/1.12.0/jquery.min.js")
(define bootstrap-css "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css")
(define bootstrap-js "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js")
(define ractive-js "http://cdn.ractivejs.org/latest/ractive.min.js")

(define (ractive title template js-file)
  (xexp->html
   `((html (head (title ,title)
                 (script (@ (src ,jquery)))
                 (link (@ (rel "stylesheet") (href ,bootstrap-css)))
                 (script (@ (src ,bootstrap-js)))
                 (script (@ (src ,ractive-js))))
           (body
            (div (@ (id "container")))
            (script (@ (id "template") (type "text/ractive"))
                    ,@template)
            (script ,(file->string js-file)))))))

(define (make-style xs)
  (string-join
   (for/list ([x xs])
     (match-define (list key val) x)
     (~a key ":" val))
   "; "))

(define template
  `((div (@ (class "container") (style "margin-top: 30px"))
         (div (@ (class "row"))
              (div (@ (class "col-md-6"))
                   (div (@ (class "panel panel-default"))
                        (div (@ (class "panel-heading"))
                             (h1 "Press a button"))
                        (div (@ (class "panel-body"))
                             (button (@ (on-click "grow") (class "btn btn-primary btn-lg"))
                                     "Grow")
                             " "
                             (button (@ (on-click "shrink") (class "btn btn-primary"))
                                     "Shrink")
                             "{{#keep_pressing}}"
                             " Now, keep pressing!"
                             "{{/keep_pressing}}"
                             (h2 "Statistics")
                             (table (@ (class "table table-bordered table-condensed"))
                                    (tbody
                                     (tr (td (b "Clicks")) (td "{{clicks}}"))
                                     (tr (td (b "Size")) (td "{{size}} pixels")))))))
              (div (@ (class "col-md-6"))
                   (div (@ (class "panel panel-default"))
                        (div (@ (class "panel-body"))
                             (p "This totally client-side example was written in "
                                (a (@ (href "https://racket-lang.org/")) "Racket") " using ")
                             (ul
                              (li (a (@ (href "https://github.com/soegaard/urlang")) " Urlang")
                                  " as a Racket-ish stand-in for JavaScript")
                              (li (a (@ (href "http://ractivejs.org")) "ractive")
                                  " as the key JavaScript framework to interact with the DOM")
                              (li (a (@ (href
                                         "https://docs.racket-lang.org/html-writing/index.html"))
                                     "sxml / x-expressions") " instead of html")
                              (li (a (@ (href "http://getbootstrap.com")) "Bootstrap")
                                  " for CSS styling"))
                             (p (@ (style ,(make-style '((color "{{color}}")
                                                         (font-size "{{size}}px")
                                                         (font-family "{{font}}")))))
                                "Hello, {{name}}!"))))))))

(urlang
 (urmodule ractive
           (import Ractive)
           (var [model (object [name "Racket"]
                               [color "purple"]
                               [font "Georgia"]
                               [clicks 0]
                               [size 60]
                               [keep_pressing #f])]
                [ractive (new Ractive
                              (object [el "#container"]
                                      [template "#template"]
                                      [data model]))])
           
           (define (inc-by s amount) (ractive.set s (+ (ractive.get s) amount)))
           (define (resize n)
             (inc-by "size" n)
             (inc-by "clicks" 1)
             (ractive.set "keep_pressing" #t))
           
           (ractive.on (object [grow (λ (e) (resize 5))]
                               [shrink (λ (e) (resize -5))]))))

(send-url/contents
 (ractive "Ractive button example"
          template
          "ractive.js"))