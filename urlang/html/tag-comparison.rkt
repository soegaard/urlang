#lang racket
;;; This is an ad-hoc file used to find the tags missing in scribble/html/html.rkt


(define non-empty-xhtml-tags
  '(a
    abbr
    acronym
    address
    applet
    b
    bdo
    big
    blockquote
    body
    button
    caption
    center
    cite
    code
    colgroup
    dd
    del
    dfn
    dir
    div
    dl
    dt
    em
    fieldset
    font
    form
    frame
    frameset
    h1
    h2
    h3
    h4
    h5
    h6
    head
    html
    i
    iframe
    ins
    kbd
    label
    legend
    li
    menu
    noframes
    noscript
    object
    ol
    optgroup
    option
    p
    pre
    q
    s
    samp
    script
    select
    small
    span
    strike
    strong
    style
    sub
    sup
    table
    tbody
    td
    textarea
    tfoot
    th
    thead
    title
    tr
    tt
    u
    ul
    var))

(define empty-xhtml-tags
  '(base meta link hr br basefont param img area input isindex col))

(define xhtml-tags
  (append non-empty-xhtml-tags
          empty-xhtml-tags))

(define html5-tags
  '(a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    input
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    option
    output
    p
    param
    pre
    progress
    q
    rb
    rp
    rt
    rtc
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    sup
    table
    tbody
    td
    template
    textarea
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr))

(define A (list->set xhtml-tags))
(define B (list->set html5-tags))
(set->list (set-subtract B A))
(length (set->list (set-subtract B A)))

(define new-tags
  '(rb          ; Ruby base
    nav         ; Section with navigational links
    meter       ; Gauge
    data        ; Machine-readable equivalent
    footer      ; Footer for a page or section
    figure      ; Figure with optional caption
    template    ; Template  
    figcaption  ; Caption for figure
    progress    ; Progress bar
    datalist    ; Container for options for combo box control
    output      ; Calculated output value
    bdi         ; Text directionality isolation
    audio       ; Audio player
    time        ; Machine-readable equivalent of date- or time-related data
    aside       ; Sidebar for tangentially related content
    article     ; Contact information for a page or article element
    ruby        ; Ruby annotation(s)
    main        ;
    rt          ; Ruby annotation text
    header      ; Introductory or navigational aids for a page or section
    canvas      ; Scriptable bitmap canvas
    mark        ; Highlight
    video       ; Video player
    rtc         ; Ruby annotation text container 
    rp          ; Parenthesis for ruby annotation text
    section     ; Generic document or application section
    map))
  
(define new-empty-tags
  '(keygen ; Cryptographic key-pair generator form control
    track  ; Timed text track
    wbr    ; Line breaking opportunity
    source ; Media source for video or audio
    embed  ; Plugin
    ))
