#lang turnstile/base

(extends "basic-types.rkt"
         #:except #%datum)

(require "rep.rkt")

(provide
 (type-out Array
           →
           unspecified→
           args...
           Pointer)
 (except-out (type-out Struct) Struct)
 (rename-out [Struct* Struct])
 (for-syntax struct-tag->info ; FIXME: don't expose this
             function-type?
             function-decl
             make-→
             function-return-type
             scalar-type?
             sizeof/type
             make-conversion)
 define-struct-type
 struct-reference
 ...
 #%datum)

(define-typed-syntax #%datum
  [(_ . s:string) ≫
   #:with cs (list->vector `(,@(bytes->list (string->bytes/latin-1 (syntax-e #'s))) 0))
   --------
   [⊢ (array 'cs '1) ⇒ (Array char)]]
  [(_ . d) ≫
   --------
   [≻ (basic-types:#%datum . d)]])

; Arrays

; TODO: store array size (for multidim arrays)
(define-type-constructor Array
  #:arity = 1)

; Structs

; This needs a bit of extra logic elsewhere to handle
; anonymous struct types and tag declarations.

(define-for-syntax introduce-struct (make-syntax-introducer))

(define-type-constructor Struct
  #:arity = 1)

(define-syntax Struct*
  (syntax-parser
    [(_ tag:id) #`(Struct #,(introduce-struct #'tag))]))

(define-syntax define-struct-type
  (syntax-parser
    [(_ name:id ([τ:type field:id] ...))
     ; TODO: make- helper
     (with-syntax ([tag (introduce-struct #'name)])
       #'(define-syntax (tag stx)
           (syntax-case stx ()
             [_ (mk-type #'(#%plain-app list 'struct-info '(field . τ) ...))])))]))

(define-for-syntax (struct-tag->info tag)
  (syntax-parse tag
    [((~literal #%plain-app) (~literal list) (~datum 'struct-info)
                             ((~literal quote) field-info) ...)
     #'(field-info ...)]
    [_ (raise-syntax-error #f "not a struct tag" #f tag)]))

; FIXME: s.x is only an lvalue if s is also.
(define-typed-syntax (struct-reference s:expr member:id) ≫
  [⊢ s ≫ s- ⇒ (~Struct tag)]
  #:with (i τ)
  (let ([info (struct-tag->info #'tag)]
        [id (syntax-e #'member)])
    (define orig-stx this-syntax)
    (let loop ([fields info]
               [i 0])
      (syntax-parse fields
        [() (raise-syntax-error #f "invalid member specifier" orig-stx #'member)]
        [((field . τ) . _)
         #:when (eq? id (syntax-e #'field))
         (list i #'τ)]
        [(_ . rest) (loop #'rest (add1 i))])))
  --------
  [⊢ (struct-member-reference s- (quote i)) ⇒ τ])

; Unions

; Functions

(define-type-constructor →
  #:arity >= 1)

(define-type-constructor unspecified→
  #:arity = 1)

(define-base-type args...)

(define-for-syntax (function-type? τ)
  (or (→? τ) (unspecified→? τ)))

(begin-for-syntax
  (define-splicing-syntax-class function-decl
    #:attributes (f τ τ_ret [arg 1] [τ_arg 1] varargs?)
    (pattern (~seq τ_ret:type (f:id)
                   (~bind [τ #'(unspecified→ τ_ret)]
                          [(arg 1) #f]
                          [(τ_arg 1) #f]
                          [varargs? #f])))
    (pattern (~seq τ_ret:type (f:id [(~literal void)])
                   (~bind [τ #'(→ τ_ret)]
                          [(arg 1) '()]
                          [(τ_arg 1) '()]
                          [varargs? #f])))
    (pattern (~seq τ_ret:type (f:id [τ_arg:type (~optional arg:id)] ...+)
                   (~bind [τ #'(→ τ_arg ... τ_ret)]
                          [varargs? #f])))
    (pattern (~seq τ_ret:type (f:id [τ_arg:type (~optional arg:id)] ...+ (~literal ...))
                   (~bind [τ #'(→ τ_arg ... args... τ_ret)]
                          [varargs? #t])))))

(define-for-syntax (make-→ τ_ret formals)
  (syntax-parse #`(#,τ_ret #,@formals)
    [(decl:function-decl) #'decl.τ]))

(define-for-syntax (function-return-type τ)
  (syntax-parse τ
    [(~→ _ ... ret) #'ret]
    [(~unspecified→ ret) #'ret]))

; Pointers

(define-type-constructor Pointer
  #:arity = 1)

(define-for-syntax (scalar-type? τ)
  (or (arithmetic-type? τ) (Pointer? τ)))

(define-for-syntax (sizeof/type τ)
  ; TODO: derived types
  (sizeof/basic-type τ))

(define-for-syntax (make-conversion τ_old τ_new stx)
  (cond
    [(type=? τ_old τ_new) stx]
    [(and (Pointer? τ_new) (Array? τ_old))
     ; XXX: cast element type
     #`(array->pointer #,stx)]
    [else (make-conversion/basic τ_old τ_new stx)]))
