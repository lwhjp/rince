#lang turnstile/base

(extends "expression.rkt" #:prefix base)

(require (for-syntax racket/list
                     racket/match
                     racket/trace
                     syntax/id-table
                     syntax/intdef)
         racket/stxparam
         "../link.rkt"
         "goto.rkt"
         "rep.rkt")

(provide block declare function goto return translation-unit)

(define-for-syntax DEBUG #f)

(define-syntax-rule (define-syntax/trace id expr)
  (define-syntax id
    (let ([id expr])
      (when DEBUG (trace id))
      id)))

(begin-for-syntax
  (define-syntax-rule (call/trace proc arg ...)
    (if DEBUG (trace-call 'proc proc arg ...) (proc arg ...))))

(define-for-syntax (make-invalid-use [msg "invalid use"])
  (λ (stx)
    (raise-syntax-error #f msg stx)))

(define-syntax block (make-invalid-use))
(define-syntax declare (make-invalid-use))
(define-syntax-parameter return (make-invalid-use))

(begin-for-syntax
  ; TODO: function formals declarator
  ; (can be used in decls and (function) - need help with f(void)
  (define-syntax-class declaration
    #:attributes ([specifier 1] [x 1] [τ 1] [v 1])
    (pattern
     ((~literal declare) ~!
      (specifier:id ...)
      (~or* [τ:type x:id (~optional v:expr)]
            ; TODO: unspecified-args function type
            [τ_ret:type (x:id [τ_arg:type (~optional arg:id)] ...) ; TODO: varargs etc
             (~bind [τ (make-→ #'τ_ret #'([τ_arg (~? arg)] ... ))])])
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
     ; TODO: compatible function types (unspecified args)
     [(type=? new-type old-type) old-type]
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
     (define stop-ids (list #'begin #'declare))
     (define lift-ctx (toplevel declared-vars))
     ; Expand, process declarations and collect lifts
     (define expanded-forms
       (let loop ([forms (attribute top-decl)])
         (if (null? forms)
             '()
             (syntax-parse (call/trace local-expand/capture-lifts (car forms) exp-ctx stop-ids def-ctx lift-ctx)
               [((~literal begin) lift ... expanded-form)
                (define decls
                  (let collect-decls ([form #'expanded-form])
                    (syntax-parse form
                      [((~literal begin) ~! subform ...)
                       (apply append (map collect-decls (attribute subform)))]
                      [_ (list form)])))
                (define defs
                  (apply
                   append
                   (map
                    (syntax-parser
                      [decl:declaration
                       (define specifiers (map syntax-e (attribute decl.specifier)))
                       (filter-map
                        (λ (x τ init-v)
                          (define function? (→? τ))
                          (define old-info (bound-id-table-ref declared-vars x #f))
                          (define obj-id
                            (or (and old-info (declared-var-object-id old-info))
                                (with-syntax ([x- (generate-temporary x)])
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
                                  (define type-renamer
                                    #`(make-variable-like-transformer (assign-type #'x- #'#,τ #:wrap? #f)))
                                  (syntax-local-bind-syntaxes (list #'x+) type-renamer def-ctx)
                                  (list #`(define-syntax x+ #,type-renamer)))))
                          (define def
                            (cond
                              [(not init-v) #f]
                              [function? #`(define #,obj-id #,init-v)]
                              [else #`(define #,obj-id (make-variable (assignment-cast #,τ #,init-v)))]))
                          #`(begin #,@renamer-defs #,@(cond [def => list] [else '()])))
                        (attribute decl.x)
                        (map (current-type-eval) (attribute decl.τ))
                        (attribute decl.v))]
                      [_ (raise-syntax-error #f "expected declaration" this-syntax)])
                    decls)))
                (append (attribute lift) defs (loop (cdr forms)))]))))
     (define tentative-definitions
       (for/list ([(id info) (in-bound-id-table declared-vars)]
                  #:when (and (not (declared-var-defined? info))
                              (not (eq? 'extern (declared-var-storage info)))))
         (bound-id-table-set!
          declared-vars
          id
          (struct-copy declared-var info [defined? #t]))
         #`(define #,(declared-var-object-id info)
             (make-variable (assignment-cast #,(declared-var-type info) 0)))))
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
    ; TODO: varargs etc
    [(_ (specifier:id ...) τ_ret:type
        (f:id [τ_arg:type (~optional arg:id #:defaults ([arg (datum->syntax #'here (gensym))]))] ...)
        ; TODO: old-style preamble
        ((~literal block)
         body ...))
     (define def-ctx (syntax-local-make-definition-context))
     (define stop-ids (list #'begin #'block #'declare #'goto #'return))
     ; TODO: use formals declarator to get arg types and names
     ; ( needed for f(void) )
     ; - should be typed
     (define expanded-body
       (let loop ([forms #'(body ...)]
                  [def-ctx def-ctx]
                  [exp-ctx (list (gensym 'function))])
         ; TODO: can we combine some of this scope logic with translation-unit?
         (syntax-parse forms
           [() '()]
           [(lbl:label rest ...)
            ; Labels get passed on to label-scope
            (append (syntax->list #'lbl) (loop #'(rest ...) def-ctx exp-ctx))]
           [(form rest ...)
            (append
             (syntax-parse (call/trace local-expand #'form exp-ctx stop-ids def-ctx)
               [((~literal begin) ~! form ...)
                (loop #'(form ...) def-ctx exp-ctx)]
               [((~literal block) ~! form ...)
                (let ([def-ctx (syntax-local-make-definition-context def-ctx)])
                  (loop #'(form ...) def-ctx (cons (gensym 'block) exp-ctx)))]
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
                     (define init-expr #`(make-variable (assignment-cast #,τ #,v)))
                     (define obj-id
                       (if static?
                           (syntax-local-lift-expression init-expr)
                           (begin
                             (with-syntax ([x- (generate-temporary id)])
                               (syntax-local-bind-syntaxes (list #'x-) #f def-ctx)
                               #'x-))))
                     (with-syntax ([x- obj-id]
                                   [x+ (syntax-local-identifier-as-binding id)])
                       ; FIXME: this duplicates translation-unit
                       (define type-renamer
                         #`(make-variable-like-transformer (assign-type #'x- #'#,τ #:wrap? #f)))
                       (syntax-local-bind-syntaxes (list #'x+) type-renamer def-ctx)
                       #`(begin
                           (define-syntax x+ #,type-renamer)
                           #,@(if static?
                                  '()
                                  (list #`(define x- #,init-expr))))))])]
               [((~or (~literal define-syntaxes) (~literal define-values)) . _)
                (raise-syntax-error #f "not allowed here" this-syntax)]
               [_ (list this-syntax)])
             (loop #'(rest ...) def-ctx exp-ctx))])))
     (define λ-stx
       (internal-definition-context-track
        def-ctx
        (quasisyntax/loc this-syntax
          (lambda (arg ...)
            (let/ec return-cont
              (syntax-parameterize
                  ([return (make-return #'return-cont #'τ_ret)])
                (label-scope
                 #,@expanded-body
                 'undefined-return)))))))
     ; FIXME: don't include generated arg ids here
     #`(declare (specifier ...) [#,(make-→ #'τ_ret #'([τ_arg arg] ...)) f #,λ-stx])]))

(define-for-syntax (make-return ec τ_ret)
  (syntax-parser
    [(_)
     #:when (void? τ_ret)
     #`(#,ec)]
    [(_ v)
     #:when (not (void? τ_ret))
     #`(#,ec (assignment-cast #,τ_ret v))]))

;; Tests

(module+ test
  (translation-unit
   (declare () [|signed int| x 10])
   (declare () [|signed int| y x]))

  (translation-unit
   (declare (extern) [|signed int| x])
   (declare () [|signed int| y x]))

  (translation-unit
   (declare () [|signed int| x 10])
   (function () |signed int| (main [void])
     (block
      (declare () [|signed int| y x])
      (return y))))

  (translation-unit
   (function () |signed int| (main [void])
     (block
      (declare (static) [|signed int| x 10])
      (return x))))

  ; TODO: split this into separate test cases; check for error
  (translation-unit
   (declare () [|signed int| i1 1])
   (declare (static) [|signed int| i2 2])
   (declare (extern) [|signed int| i3 3])
   (declare () [|signed int| i4])
   (declare (static) [|signed int| i5])
   (declare () [|signed int| i1])
   ;(declare () [|signed int| i2])
   (declare () [|signed int| i3])
   (declare () [|signed int| i4])
   ;(declare () [|signed int| i5])
   (declare (extern) [|signed int| i1])
   (declare (extern) [|signed int| i2])
   (declare (extern) [|signed int| i3])
   (declare (extern) [|signed int| i4])
   (declare (extern) [|signed int| i5]))
  )
