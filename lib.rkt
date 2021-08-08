#lang racket/base

(require (for-syntax racket/base
                     racket/provide-transform
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         "link.rkt")

(provide (for-syntax proto->decl)
         define/prototype
         c-out)

(begin-for-syntax
  (struct identifier-prototype (id type-stx)))

(define-for-syntax (proto->decl proto)
  (with-syntax ([τ (identifier-prototype-type-stx proto)]
                [x (identifier-prototype-id proto)])
    #'(declare (extern) [τ x])))

(define-for-syntax (id->proto: id)
  (format-id id "proto:~a" id))

(define-syntax define/prototype
  (syntax-parser
    [(define/prototype τ (id:id . args) body ...+)
     #'(define/prototype τ id (lambda args body ...))]
    [(define/prototype τ id:id e:expr)
     (with-syntax ([proto: (id->proto: #'id)])
       #'(begin
           (define-syntax proto: (identifier-prototype #'id #'τ))
           (define id e)))]))

(define-syntax c-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-case stx ()
       [(_ id ...)
        (with-syntax* ([(int-id ...) (generate-temporaries #'(id ...))]
                       [obj-id (syntax-local-lift-expression
                                #'(c-object
                                   (extern [int-id id] ...)
                                   (define int-id id) ...))]
                       [(proto: ...) (stx-map id->proto: #'(id ...))])
          (syntax-local-lift-module-end-declaration
           #`(define-for-syntax c-prototypes
               (list (syntax-local-value #'proto:) ...)))
          (pre-expand-export
           #'(combine-out
              (for-syntax c-prototypes)
              (rename-out [obj-id c-object]))
           modes))]))))
