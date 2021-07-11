#lang turnstile/base

(extends "expression.rkt")

(require (for-syntax racket/list
                     racket/match
                     racket/trace
                     syntax/id-table
                     syntax/intdef)
         racket/stxparam
         "../link.rkt"
         (only-in "expression.rkt" [#%datum #%datum+])
         "goto.rkt"
         "keywords.rkt"
         "rep.rkt"
         (only-in "statement.rkt" expand-function-body))

(provide (for-syntax declaration)
         begin block declare function return translation-unit)

(define-for-syntax DEBUG #f)

(define-syntax-rule (define-syntax/trace id expr)
  (define-syntax id
    (let ([id expr])
      (when DEBUG (trace id))
      id)))

(begin-for-syntax
  (define-syntax-rule (call/trace proc arg ...)
    (if DEBUG (trace-call 'proc proc arg ...) (proc arg ...))))

(define-syntax-parameter return (make-invalid-use))

(begin-for-syntax
  (define-syntax-class declaration
    #:attributes ([specifier 1] [x 1] [τ 1] [v 1])
    (pattern
     ((~literal declare) ~!
      (specifier:id ...)
      (~or* [τ:type x:id (~optional v:expr)]
            [fn:function-decl (~bind [x #'fn.f] [τ #'fn.τ])])
      ...))))

(begin-for-syntax
  (struct toplevel (declared-vars)))

(begin-for-syntax
  (struct declared-var (storage type object-id defined?)))

(define-for-syntax (combine-declarations stx new old)
  (match-define (declared-var old-storage old-type old-object-id old-defined?) old)
  (match-define (declared-var new-storage new-type new-object-id new-defined?) new)
  (declared-var
   (cond
     [(eq? new-storage old-storage) old-storage]
     [(eq? 'extern new-storage) old-storage]
     [(and (not new-storage) (eq? 'extern old-storage)) old-storage]
     [else (raise-syntax-error #f "conflicting storage specifier" stx)])
   (cond
     [(type=? new-type old-type) old-type]
     [(and (→? new-type)
           (unspecified→? old-type)
           (type=? (function-return-type new-type) (function-return-type old-type)))
      new-type]
     [(and (→? old-type)
           (unspecified→? new-type)
           (type=? (function-return-type new-type) (function-return-type old-type)))
      old-type]
     [else (raise-syntax-error #f "conflicting type" stx)])
   (cond
     [(bound-identifier=? new-object-id old-object-id) old-object-id]
     [else (error "BUG: object identifier changed")])
   (cond
     [(not (and new-defined? old-defined?)) (or new-defined? old-defined?)]
     [else (raise-syntax-error #f "multiple definitions" stx)])))

(define-syntax/trace translation-unit
  (syntax-parser
    [(_ top-decl ...)
     (define declared-vars (make-bound-id-table))
     (define def-ctx (syntax-local-make-definition-context))
     (define exp-ctx (list (gensym 'translation-unit)))
     (define stop-ids (list #'begin #'declare #'define-syntaxes #'define-values))
     (define lift-ctx (toplevel declared-vars))
     ; Expand, process declarations and collect lifts
     (define expanded-forms
       (let loop ([forms (attribute top-decl)])
         (if (null? forms)
             '()
             (syntax-parse (call/trace local-expand/capture-lifts (car forms) exp-ctx stop-ids def-ctx lift-ctx)
               [((~literal begin) lift ... expanded-form)
                (append
                 (attribute lift)
                 (syntax-parse #'expanded-form
                   [((~literal begin) ~! subform ...) (loop (attribute subform))]
                   [decl:declaration
                    (define specifiers (map syntax-e (attribute decl.specifier)))
                    (filter-map
                     (λ (x τ init-v)
                       (define function? (function-type? τ))
                       (define old-info (bound-id-table-ref declared-vars x #f))
                       (define obj-id
                         (or (and old-info (declared-var-object-id old-info))
                             (with-syntax ([x- (internal-definition-context-introduce def-ctx (generate-temporary x))])
                               (syntax-local-bind-syntaxes (list #'x-) #f def-ctx)
                               #'x-)))
                       (define info
                         (let ([new-info
                                (declared-var
                                 (cond
                                   [(memq 'static specifiers) 'static]
                                   [(memq 'extern specifiers) 'extern]
                                   [function? 'extern]
                                   [else #f])
                                 τ
                                 obj-id
                                 (and init-v #t))])
                           (if old-info
                               (combine-declarations x new-info old-info)
                               new-info)))
                       (bound-id-table-set! declared-vars x info)
                       (define renamer-defs
                         (if old-info
                             '() ; already declared
                             (with-syntax ([x- obj-id]
                                           [x+ (syntax-local-identifier-as-binding x)])
                               (define renamer-target
                                 (if function? #'x- #'(variable-reference . x-)))
                               (define type-renamer
                                 #`(make-variable-like-transformer
                                    (assign-type #'#,renamer-target #'#,τ)))
                               (syntax-local-bind-syntaxes (list #'x+) type-renamer def-ctx)
                               (list #`(define-syntax x+ #,type-renamer)))))
                       (define def
                         (with-syntax ([x- obj-id])
                           (cond
                             [(not init-v) #f]
                             [function? #`(define x- #,init-v)]
                             [else #`(define x- (make-variable (cast #,τ #,init-v)))])))
                       #`(begin #,@renamer-defs #,@(cond [def => list] [else '()])))
                     (attribute decl.x)
                     (map (current-type-eval) (attribute decl.τ))
                     (attribute decl.v))]
                   [((~literal define-syntaxes) (id ...) expr)
                    (syntax-local-bind-syntaxes (attribute id) #'expr def-ctx)
                    (list #`(define-syntaxes #,(map syntax-local-identifier-as-binding (attribute id)) expr))]
                   [_ (raise-syntax-error #f "expected declaration" this-syntax)])
                 (loop (cdr forms)))]))))
     (define tentative-definitions
       (for/list ([(id info) (in-bound-id-table declared-vars)]
                  #:when (and (not (declared-var-defined? info))
                              (not (eq? 'extern (declared-var-storage info)))))
         (bound-id-table-set!
          declared-vars
          id
          (struct-copy declared-var info [defined? #t]))
         #`(define #,(declared-var-object-id info)
             (make-variable (cast #,(declared-var-type info) (#%datum+ . 0))))))
     (define extern-ids
       (filter
        values
        (bound-id-table-map
         declared-vars
         (λ (id info)
           (and (eq? 'extern (or (declared-var-storage info) 'extern))
                #`[#,(declared-var-object-id info) #,id])))))
     (with-syntax ([(extern-id ...) extern-ids]
                   [(form ...) (append expanded-forms tentative-definitions)])
       (internal-definition-context-track
        def-ctx
        (syntax/loc this-syntax
          (c-object
           (extern extern-id ...)
           form ...))))]))

(define-syntax/trace function
  (syntax-parser
    [(_ (specifier:id ...) decl:function-decl
        ; TODO: old-style preamble
        body)
     (define def-ctx (syntax-local-make-definition-context))
     (define stop-ids (list #'begin #'block #'declare #'goto #'label #'return))
     (define-values (arg-spec arg-renamers)
       (cond
         [(attribute decl.arg)
          => (λ (arg-decls)
               (define arg-ids (map (λ (id) (generate-temporary (or id 'arg))) arg-decls))
               (define rest-arg (if (attribute decl.varargs?) #'rest #'()))
               (define arg-renamers
                 (with-syntax
                     ([((arg- arg^ arg+ τ) ...)
                       (for/list ([arg- arg-ids]
                                  [arg+ (attribute decl.arg)]
                                  [τ (attribute decl.τ_arg)]
                                  #:when arg+)
                         (list arg- (generate-temporary arg+) (internal-definition-context-introduce def-ctx arg+) τ))])
                   (define renamers
                     #'(values (make-variable-like-transformer
                                (assign-type #'(variable-reference . arg^) #'τ)) ...))
                   (syntax-local-bind-syntaxes (syntax->list #'(arg^ ...)) #f def-ctx)
                   (syntax-local-bind-syntaxes (syntax->list #'(arg+ ...)) renamers def-ctx)
                   #`(begin
                       (define-values (arg^ ...) (values (make-variable arg-) ...))
                       (define-syntaxes (arg+ ...) #,renamers))))
               (values #`(#,@arg-ids . #,rest-arg) arg-renamers))]
         [else (values #'unspecified #'(begin))]))
     (define expanded-body
       (let loop ([forms #`(#,(expand-function-body #'body))]
                  [def-ctx def-ctx]
                  [exp-ctx (list (gensym 'function))]
                  [introduce-label (λ (stx) stx)])
         ; TODO: can we combine some of this scope logic with translation-unit?
         (syntax-parse forms
           [() '()]
           [(form rest ...)
            (append
             (syntax-parse (call/trace local-expand #'form exp-ctx stop-ids def-ctx)
               [((~literal begin) ~! form ...)
                (loop #'(form ...) def-ctx exp-ctx introduce-label)]
               [((~literal block) ~! form ...)
                (let* ([def-ctx (syntax-local-make-definition-context def-ctx)]
                       [introduce-label (λ (stx) (internal-definition-context-introduce def-ctx (introduce-label stx)))])
                  (loop #'(form ...) def-ctx (cons (gensym 'block) exp-ctx) introduce-label))]
               [((~literal label) . _)
                ;; Flatten label scopes so that we can jump into blocks
                (list (introduce-label this-syntax))]
               [decl:declaration
                (define specifiers (map syntax-e (attribute decl.specifier)))
                (define static? (memq 'static specifiers))
                (cond
                  [(memq 'extern specifiers)
                   ; TODO: handle external-linkage externs
                   ; TODO: (extern) function declarations
                   '()]
                  [else
                   (for/list ([id (in-list (attribute decl.x))]
                              [τ (in-list (map (current-type-eval) (attribute decl.τ)))]
                              [v (in-list (attribute decl.v))])
                     (define init-expr
                       #`(make-variable
                          #,(cond
                              [v #`(initializer #,τ #,v)]
                              [static? #`(static-initializer #,τ)]
                              [else #`(unspecified-initializer #,τ)])))
                     (define obj-id
                       (if static?
                           (syntax-local-lift-expression init-expr)
                           (begin
                             (with-syntax ([x- (internal-definition-context-introduce def-ctx (generate-temporary id))])
                               (syntax-local-bind-syntaxes (list #'x-) #f def-ctx)
                               #'x-))))
                     (with-syntax ([x- obj-id]
                                   [x+ (syntax-local-identifier-as-binding id)])
                       ; FIXME: this duplicates translation-unit
                       (define type-renamer
                         #`(make-variable-like-transformer
                            (assign-type #'(variable-reference . x-) #'#,τ)))
                       (syntax-local-bind-syntaxes (list #'x+) type-renamer def-ctx)
                       #`(begin
                           (define-syntax x+ #,type-renamer)
                           #,@(if static?
                                  '()
                                  (list #`(define x- #,init-expr))))))])]
               [((~or (~literal define-syntaxes) (~literal define-values)) . _)
                (raise-syntax-error #f "not allowed here" this-syntax)]
               [_ (list this-syntax)])
             (loop #'(rest ...) def-ctx exp-ctx introduce-label))])))
     (define λ-stx
       (internal-definition-context-track
        def-ctx
        (quasisyntax/loc this-syntax
          (lambda #,arg-spec
            #,arg-renamers
            (let/ec return-cont
              (syntax-parameterize
                  ([return (make-return #'return-cont #'decl.τ_ret)])
                (label-scope
                 #,@expanded-body
                 'undefined-return)))))))
     #`(declare (specifier ...) [decl.τ decl.f #,λ-stx])]))

(define-for-syntax (make-return ec τ_ret)
  (λ (stx)
    (syntax-parse/typecheck stx
      [(_) ≫
       #:when (void? τ_ret)
       --------
       [≻ (#,ec)]]
      [(_ v) ≫
       #:when (not (void? τ_ret))
       --------
       [≻ (#,ec (cast #,τ_ret v))]])))

;; Tests

(module+ test
  (require (only-in "expression.rkt" #%datum))

  (translation-unit
   (declare () [int x 10])
   (declare () [int y x]))

  (translation-unit
   (declare (extern) [int x])
   (declare () [int y x]))

  (translation-unit
   (declare () [int x 10])
   (function () int (main [void])
     (block
      (declare () [int y x])
      (return y))))

  (translation-unit
   (function () int (main [void])
     (block
      (declare (static) [int x 10])
      (return x))))

  ; TODO: split this into separate test cases; check for error
  (translation-unit
   (declare () [int i1 1])
   (declare (static) [int i2 2])
   (declare (extern) [int i3 3])
   (declare () [int i4])
   (declare (static) [int i5])
   (declare () [int i1])
   ;(declare () [int i2])
   (declare () [int i3])
   (declare () [int i4])
   ;(declare () [int i5])
   (declare (extern) [int i1])
   (declare (extern) [int i2])
   (declare (extern) [int i3])
   (declare (extern) [int i4])
   (declare (extern) [int i5]))
  )
