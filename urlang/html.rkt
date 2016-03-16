#lang racket
;; This file is intended to be use as
;;     (require urlang/html)
;; and will import all html related utilities in urlang/html/

(require "html/all.rkt")
(provide (all-from-out "html/all.rkt"))