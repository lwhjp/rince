#lang turnstile/base

(extends "scope.rkt")

(require (prefix-in r: (only-in racket/base void))
         racket/stxparam)

(provide empty-statement break continue do for if while)

(define-syntax-rule (empty-statement)
  (#%plain-app r:void))

(define-syntax-parameter break
  (λ (stx)
    (raise-syntax-error #f "invalid use" stx)))

(define-syntax-parameter continue
  (λ (stx)
    (raise-syntax-error #f "invalid use" stx)))

; TODO: check predicate types are scalar

(define-typed-syntax if
  [(_ predicate consequent) ≫
   [⊢ predicate ≫ predicate- ⇒ τ_pred]
   --------
   [≻ (if predicate consequent (empty-statement))]]
  [(_ predicate consequent alternate) ≫
   [⊢ predicate ≫ predicate- ⇒ τ_pred]
   --------
   [≻ (begin
        (when (zero? predicate-) (goto else))
        consequent
        (goto endif)
        #:label else
        alternate
        #:label endif)]])

; TODO: switch

(define-typed-syntax (while pred body) ≫
  [⊢ pred ≫ pred- ⇒ τ_pred]
  --------
  [≻ (begin
       #:label loop
       (when (zero? pred-) (goto end))
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto loop)])])
         body)
       (goto loop)
       #:label end)])

(define-typed-syntax (do body pred) ≫
  [⊢ pred ≫ pred- ⇒ τ_pred]
  --------
  [≻ (begin
       #:label loop
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto cont)])])
         body)
       #:label cont
       (unless (zero? pred-) (goto loop))
       #:label end)])

(define-typed-syntax for
  [(_ (() pred step) body) ≫
   --------
   [≻ (for ((empty-statement) pred step) body)]]
  [(_ (init () step) body) ≫
   --------
   [≻ (for (init 1 step) body)]]
  [(_ (init pred ()) body) ≫
   --------
   [≻ (for (init pred (empty-statement)) body)]]
  [(_ (init pred step) body) ≫
   [⊢ pred ≫ pred- ⇒ τ_pred]
   --------
   [≻ (block
       init
       #:label loop
       (when (zero? pred-) (goto end))
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto cont)])])
         body)
       #:label cont
       step
       (goto loop)
       #:label end)]])
