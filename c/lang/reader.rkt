#lang s-exp syntax/module-reader
rince/c/lang/main
#:read read-c
#:read-syntax read-c-syntax
#:whole-body-readers? #t
(require "../../lang/parse.rkt"
         syntax/strip-context)
(define (read-c in)
    (syntax->datum (read-c-syntax in)))
(define (read-c-syntax src in)
  (list
   (strip-context
    #`(#%module-begin
       #,(c->racket in src)))))
