#lang racket

(require "../lang/rep.rkt"
         "../provide.rkt")

(provide (c-out puts))

(define (char*->string p)
  (list->string
   (let loop ([p p])
     (define c (integer->char (pointer-dereference p)))
     (if (char=? #\null c)
         '()
         (cons c (loop (pointer-inc p 1)))))))

(define ;(→ (const (Pointer char)) int)
  (puts s)
  (write-string (char*->string s))
  (newline)
  0)
