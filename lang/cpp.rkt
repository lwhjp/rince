#lang racket/base

(require (for-template "../lib.rkt"
                       (rename-in "../lib/stdio.rkt"
                                  [c-prototypes stdio]))
         racket/port
         racket/string)

(provide preprocess)

(define-namespace-anchor ns-here)

(define get-protos
  (let ([ns (namespace-anchor->namespace ns-here)])
    (λ (lib)
      (parameterize ([current-namespace ns])
        (namespace-variable-value lib)))))

(define (preprocess in)
  ;; TODO: preprocess properly (including position)
  (define src-lines (call-with-input-file in port->lines))
  (define-values (out-lines include-decls)
    (for/fold ([out-lines '()]
               [out-decls '()]
               #:result (values (reverse out-lines) (reverse out-decls)))
              ([l src-lines])
      (cond
        [(regexp-match #px"^\\s*#\\s*include\\s*<([^>]+).h>" l)
         => (λ (m)
              (define protos (get-protos (string->symbol (cadr m))))
              (values out-lines
                      (append (reverse (map proto->decl protos)) out-decls)))]
        [(regexp-match? #px"^\\s*#" l)
         (values (cons '("//") out-lines) out-decls)]
        [else
         (values (cons l out-lines) out-decls)])))
  (values
   (open-input-string (string-join out-lines "\n"))
   include-decls))
