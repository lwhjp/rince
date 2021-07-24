#lang turnstile/base

(extends "expression.rkt")

(require "scope.rkt"
         "statement.rkt")

(provide
 begin
 block
 declare
 function
 label
 null-statement
 return
 translation-unit
 (rename-out
  [c:break break]
  [c:continue continue]
  [c:do do]
  [c:for for]
  [c:goto goto]
  [c:if if]
  [c:while while]))
