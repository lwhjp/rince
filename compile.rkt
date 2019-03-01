#lang racket/base

(require "lang/parse.rkt")

(provide compile)

(define-namespace-anchor this-namespace)

(define compile-ns (namespace-anchor->empty-namespace this-namespace))

(require racket/runtime-path)
(define-runtime-module-path lang "lang/syntax.rkt")

(namespace-require lang compile-ns)

(define (compile in [source-name #f])
  (eval (c->racket in source-name) compile-ns))
