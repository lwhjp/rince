#lang turnstile/base

(require (for-meta 2 racket/base)
         (for-syntax racket/struct))

(provide define-parameterized-type)

(define-syntax define-parameterized-type
  (syntax-parser
    [(_ (τ:id p:id ...))
     (with-syntax ([τ-instance (generate-temporary #'τ)]
                   [τ? (format-id #'τ "~a?" #'τ)]
                   [~τ (format-id #'τ "~~~a" #'τ)]
                   [(~p ...) (generate-temporaries #'(p ...))])
       #'(begin
           (define (τ . args)
             (error 'τ "invalid use of type"))
           (begin-for-syntax
             (struct τ-instance (p ...)
               #:transparent
               #:property prop:procedure
               (λ (this stx)
                 (syntax-case stx ()
                   [_ (identifier? stx)
                    (with-syntax ([(p ...) (struct->list this)])
                      (mk-type
                       (syntax/loc stx
                         (#%plain-app τ 'p ...))))])))
             (define-syntax ~τ
               (pattern-expander
                (λ (stx)
                  (syntax-case stx ()
                    [(_ ~p ...)
                     #`(~describe
                        #:opaque
                        #,(string-append (symbol->string 'τ) " type expression")
                        ((~literal #%plain-app)
                         (~literal τ)
                         ((~literal quote) ~p) ...))])))))
           (define-for-syntax (τ? stx)
             (syntax-parse stx [(~τ p ...) #t] [_ #f]))))]))
