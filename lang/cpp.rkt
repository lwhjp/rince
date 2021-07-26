#lang racket/base

(require racket/port
         racket/string)

(provide preprocess)

(define (preprocess in)
  ;; TODO: preprocess properly
  (define src-lines (call-with-input-file in port->lines))
  (define out-lines
    (map (λ (l)
           (if (regexp-match? #px"^\\s*#" l)
               "//"
               l))
         src-lines))
  (open-input-string
   (string-join out-lines "\n")))
