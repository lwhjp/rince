#lang racket/base

(require (for-syntax racket/base))

(provide (for-syntax make-invalid-use))

(define-for-syntax (make-invalid-use)
  (λ (stx)
    (raise-syntax-error #f "invalid use of C keyword" stx)))

(define-syntax-rule (define-keywords/provide id ...)
  (begin
    (provide id ...)
    (define-syntax id (make-invalid-use)) ...))

(define-keywords/provide
  block
  declare
  c:break
  c:continue
  c:do
  c:for
  c:if
  c:while)
