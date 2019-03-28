#lang racket/base

(require (for-syntax racket/base)
         racket/provide
         (prefix-in c: "../../lang/syntax.rkt")
         "../../link.rkt")

(provide (filtered-out (λ (id)
                         (and (regexp-match? #rx"^c:" id)
                              (substring id 2)))
                       (all-from-out "../../lang/syntax.rkt"))
         (rename-out [c-module-begin #%module-begin]))

(define-syntax-rule (c-module-begin object)
  (#%plain-module-begin
   (module* main #f
     (define this-object object)
     (define go (linkable->executable this-object))
     (go))))
