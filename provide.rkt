#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax)
         "link.rkt")

(provide c-out)

(define-syntax c-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-case stx ()
       [(_ id ...)
        (with-syntax* ([(int-id) (generate-temporaries #'(id ...))]
                       [obj-id (syntax-local-lift-expression
                                #'(c-object
                                   (extern [int-id id] ...)
                                   (define int-id id) ...))])
          (pre-expand-export
           #'(rename-out [obj-id c-object])
           modes))]))))
