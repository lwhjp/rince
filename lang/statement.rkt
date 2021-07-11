#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/trace
                     syntax/parse
                     syntax/stx)
         (only-in "expression.rkt"
                    [__ensure-scalar ensure-scalar])
         "goto.rkt"
         "keywords.rkt")

(provide (for-syntax expand-function-body)
         label
         null-statement
         c:break c:continue c:do c:for c:goto c:if c:while)

;; C allows jumps into loop bodies, so we need to rewrite loop
;; constructs using 'goto'. Ideally we'd use syntax parameters to
;; make 'break' and 'continue' transform into a 'goto' to the
;; appropriate label, but unfortunately the rewriting here doesn't
;; play nicely with syntax-parameterize.

(define-for-syntax DEBUG #f)

(define-syntax-rule (define-for-syntax/debug-trace (id arg ...) e ...)
  (begin-for-syntax
    (define id
      (let ([id (lambda (arg ...) e ...)])
        (when DEBUG (trace id))
        id))))

(define-syntax-rule (c:goto lbl) (goto lbl))
(define-syntax-rule (null-statement) (void))

(define-for-syntax (expand-function-body stx)
  (expand-statement stx))

(begin-for-syntax
  (define current-break-label (make-parameter #f))
  (define current-continue-label (make-parameter #f)))

(define-for-syntax/debug-trace (expand-statement stx)
  (define (track new-stx) (syntax-track-origin new-stx stx (stx-car stx)))
  (syntax-parse stx
    [((~literal c:break))
     (track
      #`(goto #,(or (current-break-label)
                    (raise-syntax-error #f "break outside of switch or loop" this-syntax))))]
    [((~literal c:continue))
     (track
      #`(goto #,(or (current-continue-label)
                    (raise-syntax-error #f "continue outside of loop" this-syntax))))]
    [((~literal c:do) body predicate)
     (with-syntax ([loop (generate-temporary 'do)]
                   [test (generate-temporary 'test)]
                   [end (generate-temporary 'end)])
       (parameterize ([current-break-label #'end]
                      [current-continue-label #'end])
         (track
          #`(begin
              (label loop)
              #,(expand-statement #'body)
              (label test)
              (unless (zero? (ensure-scalar predicate)) (goto loop))
              (label end)))))]
    [((~literal c:for) ((~or () init)
                        (~or () predicate)
                        (~or () step))
                       body)
     (with-syntax ([loop (generate-temporary 'for)]
                   [next (generate-temporary 'next)]
                   [end (generate-temporary 'end)])
       (parameterize ([current-break-label #'end]
                      [current-continue-label #'next])
         (track
          #`(block
             (~? init)
             (label loop)
             (~? (when (zero? (ensure-scalar predicate)) (goto end)))
             #,(expand-statement #'body)
             (label next)
             (~? step)
             (goto loop)
             (label end)))))]
    [((~literal c:if) predicate consequent)
     (with-syntax ([endif (generate-temporary 'endif)])
       (track
        #`(begin
            (when (zero? (ensure-scalar predicate)) (goto endif))
            #,(expand-statement #'consequent)
            (label endif))))]
    [((~literal c:if) predicate consequent alternate)
     (with-syntax ([else (generate-temporary 'else)]
                   [endif (generate-temporary 'endif)])
       (track
        #`(begin
            (when (zero? (ensure-scalar predicate)) (goto else))
            #,(expand-statement #'consequent)
            (goto endif)
            (label else)
            #,(expand-statement #'alternate)
            (label endif))))]
    [((~literal c:while) predicate body)
     (with-syntax ([loop (generate-temporary 'while)]
                   [end (generate-temporary 'end)])
       (parameterize ([current-break-label #'end]
                      [current-continue-label #'loop])
         (track
          #`(begin
              (label loop)
              (when (zero? (ensure-scalar predicate)) (goto end))
              #,(expand-statement #'body)
              (goto loop)
              (label end)))))]
    [((~or (~literal begin) (~literal block)) item ...)
     ; item could also be a declaration, but that will just pass through as-is
     (track #`(#,(stx-car this-syntax) #,@(stx-map expand-statement #'(item ...))))]
    ; otherwise it's an expression
    [_ this-syntax]))
