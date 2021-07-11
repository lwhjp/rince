#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/syntax
                     syntax/intdef
                     syntax/parse)
         racket/stxparam
         racket/undefined)

(provide
 goto
 label
 label-scope)

(define-for-syntax (invalid-use)
  (λ (stx)
    (raise-syntax-error #f "invalid outside label-scope" stx)))

(define-syntax-parameter goto (invalid-use))

(define-syntax label (invalid-use))

(define-for-syntax introduce-label (make-syntax-introducer))

(define-syntax (label-scope stx)
  (define forms (syntax->list stx))
  (unless forms (raise-syntax-error #f "invalid use" stx))
  (define top-label (introduce-label (generate-temporary 'top)))
  (define def-ctx (syntax-local-make-definition-context))
  (define exp-ctx (list (gensym 'label-scope)))
  (define stop-ids (list #'begin #'define-syntaxes #'define-values #'goto #'label))
  (define-values (parts stx-defs var-defs)
    (let loop ([current-label top-label]
               [forms (cdr forms)]
               [parts '()]
               [exprs '()]
               [stxs '()]
               [vars '()])
      (match forms
        ['()
         (values (reverse (cons (list current-label
                                      (if (null? exprs)
                                          (list #'(void))
                                          (reverse exprs)))
                                parts))
                 (reverse stxs)
                 (reverse vars))]
        [(cons form rest)
         (syntax-parse (local-expand (car forms) exp-ctx stop-ids def-ctx)
           #:literals (begin define-syntaxes define-values label)
           [(begin form ...)
            (loop current-label
                  (append (attribute form) rest)
                  parts
                  exprs
                  stxs
                  vars)]
           [(define-syntaxes ~! (id:id ...) expr:expr)
            (syntax-local-bind-syntaxes (attribute id) #'expr def-ctx)
            (loop current-label
                  rest
                  parts
                  exprs
                  (with-syntax ([(id ...) (map syntax-local-identifier-as-binding (attribute id))])
                    (cons #'[(id ...) expr] stxs))
                  vars)]
           [(define-values ~! (id:id ...) expr:expr)
            (syntax-local-bind-syntaxes (attribute id) #f def-ctx)
            (loop current-label
                  rest
                  parts
                  (cons (syntax/loc this-syntax (set!-values (id ...) expr))
                        exprs)
                  stxs
                  (with-syntax ([(id ...) (map syntax-local-identifier-as-binding (attribute id))]
                                [(v ...) (map (λ (id) #'undefined) (attribute id))])
                    (cons #'[(id ...) (values v ...)] vars)))]
           [(label id:id)
            (with-syntax ([next (introduce-label #'id)])
              (loop #'next
                    rest
                    (cons (list current-label (reverse (cons #'(next) exprs)))
                          parts)
                    '()
                    stxs
                    vars))]
           [_ (loop current-label rest parts (cons this-syntax exprs) stxs vars)])])))
  (define transformed-stx
    (with-syntax ([(stx-def ...) stx-defs]
                  [(var-def ...) var-defs]
                  [((label-id (form ...)) ...) parts]
                  [top top-label])
      (syntax/loc stx
        (let ([goto-prompt (make-continuation-prompt-tag 'label-scope)])
          (call-with-continuation-prompt
           (lambda ()
             (letrec-syntaxes+values
                 (stx-def ...)
                 (var-def ...)
               (syntax-parameterize ([goto (make-goto #'goto-prompt)])
                 (letrec ([label-id (lambda () form ...)] ...)
                   (top)))))
           goto-prompt)))))
  (internal-definition-context-track def-ctx transformed-stx))

(define-for-syntax (make-goto prompt-id)
  (syntax-parser
    [(_ target:id)
     (quasisyntax/loc this-syntax
       (abort-current-continuation
        #,prompt-id
        #,(introduce-label #'target)))]))
