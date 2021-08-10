#lang info
(define deps '("rackunit-lib"
               "base"
               "c-utils"
               "parser-tools-lib"
               "turnstile-lib"))
(define raco-commands
  '(("run-c" rince/commands/run-c "compile and execute a single-file C program" 50)))
