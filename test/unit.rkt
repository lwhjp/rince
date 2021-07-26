#lang racket/base

(require racket/runtime-path
         rackunit
         "../compile.rkt"
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
  (define proc (linkable->executable obj))
  (check-equal? (proc) expected))

(define test-suite:ok
  (test-suite
   "Compile and run unit tests (expected success)"
   (for ([file (in-directory test-files:ok)])
     (test-case
      (path->string file)
      (check-compile+run file)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests test-suite:ok))
