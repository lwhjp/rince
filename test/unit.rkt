#lang racket/base

(require racket/runtime-path
         rackunit
         "../compile.rkt"
         "../libc.rkt"
         "../link.rkt")

(define-runtime-path test-files:ok "ok")

(define-simple-check (check-compile+run file)
  (define expected
    (call-with-input-file file
      (λ (in)
        (cond
          [(regexp-match #rx"^// expected: ([0-9]+)" (read-line in))
           => (λ (m) (string->number (cadr m)))]
          [else 0]))))
  (define obj (compile file))
  (define proc (link/executable (list obj) (list libc)))
  (check-equal? (proc) expected))

(define-syntax-rule (make-tests path desc)
  (test-suite
   desc
   (for ([file (in-directory path)]
         #:when (regexp-match? #rx"\\.c$" file))
     (test-case
      (path->string file)
      (check-compile+run file)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests (make-tests test-files:ok "Compile and run unit tests (expected success)")))
