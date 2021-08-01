#lang racket/base

(require (rename-in "lib/stdio.rkt" [c-object stdio])
         "link.rkt")

(provide libc)

(define libc
  (make-archive
   (list stdio)))
