#lang racket/base

(require racket/format
         racket/generator
         racket/sequence
         "../lang/rep.rkt"
         "../lib.rkt")

(provide (c-out printf
                puts))

(define (char*->string p)
  (list->string
   (let loop ([p p])
     (define c (integer->char (pointer-dereference p)))
     (if (char=? #\null c)
         '()
         (cons c (loop (pointer-inc p 1)))))))

(define/prototype (→ (const (Pointer char)) args... int)
  (printf format . args)
  ; TODO: do this properly
  (define next (sequence->generator args))
  (define (subst s)
    (define v (next))
    (~a (case (string-ref s (sub1 (string-length s)))
          [(#\c) (integer->char v)]
          [(#\s) (char*->string v)]
          [else v])))
  (define str
    (regexp-replace* #rx"% ?[0-9]*[cdfs]" (char*->string format) subst))
  (write-string str)
  (string-length str))

(define/prototype (→ (const (Pointer char)) int)
  (puts s)
  (write-string (char*->string s))
  (newline)
  0)
