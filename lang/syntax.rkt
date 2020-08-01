#lang turnstile/base

(extends "scope.rkt")

(require (prefix-in r: (only-in racket/base void))
         racket/stxparam)

(provide
 empty-statement
 break
 continue
 do
 for
 (rename-out [if+ if])
 while)

(define-syntax-rule (empty-statement) (r:void))

(define-syntax-parameter break
  (λ (stx)
    (raise-syntax-error #f "invalid use" stx)))

(define-syntax-parameter continue
  (λ (stx)
    (raise-syntax-error #f "invalid use" stx)))

; TODO: check predicate types are scalar

(define-typed-syntax if+
  [(_ predicate consequent) ≫
   [⊢ predicate ≫ predicate- ⇒ τ_pred]
   --------
   [≻ (if+ predicate consequent (empty-statement))]]
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
  --------
  [≻ (begin
       #:label loop
       (when (__zero? pred) (goto end))
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto loop)])])
         body)
       (goto loop)
       #:label end)])

(define-typed-syntax (do body pred) ≫
  --------
  [≻ (begin
       #:label loop
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto cont)])])
         body)
       #:label cont
       (unless (__zero? pred) (goto loop))
       #:label end)])

(define-typed-syntax for
  [(_ ((~or () init:declaration init)
       (~or () pred)
       (~or () step))
      body) ≫
   --------
   [≻ (block
       (~? init)
       #:label loop
       (~? (when (__zero? pred) (goto end)))
       (syntax-parameterize
           ([break (syntax-rules () [(_) (goto end)])]
            [continue (syntax-rules () [(_) (goto cont)])])
         body)
       #:label cont
       (~? step)
       (goto loop)
       #:label end)]])
