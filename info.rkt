#lang info
(define deps
  '("base"
    "c-utils"
    "parser-tools-lib"
    "rackunit-lib"
    "turnstile-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))
(define scribblings
  '(("scribblings/rince.scrbl" (multi-page))))
(define raco-commands
  '(("run-c" rince/commands/run-c "compile and execute a single-file C program" 50)))
