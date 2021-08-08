#lang racket/base

(require "../lang/rep.rkt"
         "../lib.rkt")

(provide (c-out puts))

(define (char*->string p)
  (list->string
   (let loop ([p p])
     (define c (integer->char (pointer-dereference p)))
     (if (char=? #\null c)
         '()
         (cons c (loop (pointer-inc p 1)))))))

(define/prototype (→ (const (Pointer char)) int)
  (puts s)
  (write-string (char*->string s))
  (newline)
  0)
