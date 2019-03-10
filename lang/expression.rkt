#lang turnstile/base

;; Typed C expressions

(extends "operator.rkt" #:prefix base:)

(require (for-syntax racket/list
                     racket/match))

(provide
 #%app
 constant)

(define-typed-syntax (#%app f arg ...) ≫
  [⊢ f ≫ f- ⇒ τ_f]
  ; TODO: default argument promotion
  #:with (τ_ret arg^ ...)
  (syntax-parse #'τ_f
    [(~→ τ_arg ... τ_ret)
     (define-values (fixed-arg-types varargs?)
       (match (attribute τ_arg)
         [(list τs ... (? args...?)) (values τs #t)]
         [τs (values τs #f)]))
     (define fixed-arg-count (length fixed-arg-types))
     (unless (>= (stx-length #'(arg ...)) fixed-arg-count)
       (raise-syntax-error #f (format "insufficient arguments: expected at least ~a" fixed-arg-count) this-syntax #'f))
     (define-values (fixed-args extra-args)
       (split-at (attribute arg) fixed-arg-count))
     (when (and (not varargs?) (not (null? extra-args)))
       (raise-syntax-error #f "too many arguments" this-syntax #'f))
     (with-syntax ([(τ_fixed ...) fixed-arg-types]
                   [(fixed-arg ...) fixed-args]
                   [(extra-arg ...) extra-args])
       #'(τ_ret (cast τ_fixed fixed-arg) ... extra-arg ...))]
    [(~unspecified→ τ_ret)
     (cons #'τ_ret (syntax->list #'(arg ...)))])
  --------
  [⊢ (#%plain-app- f- arg^ ...) ⇒ τ_ret])

(define-typed-syntax (constant τ:type v:expr) ≫
  --------
  [≻ (cast τ v)])
