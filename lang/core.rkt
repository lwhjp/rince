#lang turnstile/base

(require (for-syntax racket/list
                     racket/match))

(provide
 (type-out Integer
           Single-Float
           Double-Float
           →
           unspecified→
           args...)
 #%datum+)

; Base types and forms for internal (Racket) representation

(define-base-type Integer)
(define-base-type Single-Float)
(define-base-type Double-Float)

(define-syntax #%datum+
  (syntax-parser
    [(_ . d)
     (define v (syntax-e #'d))
     (define τ
       (cond
         [(exact-integer? v) #'Integer]
         [(single-flonum? v) #'Single-Float]
         [(flonum? v) #'Double-Float]
         [else (type-error #:src #'d #:msg "unsupported literal: ~a" #'d)]))
     (assign-type #'(quote d) τ)]))

(define-type-constructor → #:arity >= 1)
(define-type-constructor unspecified→ #:arity = 1)
(define-base-type args...)

;; lambda and #%app are defined later
