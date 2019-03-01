#lang turnstile/base

;; Typed C expressions

(extends "operator.rkt" #:prefix base)

(provide
 #%app
 constant)

(define-typed-syntax (#%app f arg ...) ≫
  [⊢ f ≫ f- ⇒ (~→ τ_arg ... τ_ret)]
  ; TODO: arity check
  ; TODO: varargs
  ; TODO: default argument promotion
  ; TODO: implicitly cast args
  [⊢ arg ≫ arg- ⇐ τ_arg] ...
  --------
  [⊢ (#%plain-app- f- arg- ...) ⇒ τ_ret])

(define-typed-syntax (constant τ:type v:expr) ≫
  --------
  [≻ (cast τ v)])
