#lang turnstile/base

(extends "scope.rkt")

(require "statement.rkt")

(provide
 label
 null-statement
 (rename-out
  [c:break break]
  [c:continue continue]
  [c:do do]
  [c:for for]
  [c:goto goto]
  [c:if if]
  [c:while while]))
