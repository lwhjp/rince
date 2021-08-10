#lang racket/base
(require racket/cmdline
         raco/command-name
         rince/compile
         rince/libc
         rince/link)

(define source-file
  (command-line
   #:program (short-program+command-name)
   #:args (source-file)
   source-file))

(define go
  (link/executable
   (list (compile (string->path source-file)))
   (list libc)))

(exit (go))

(module test racket/base)