#lang racket/base

(require (for-meta 2 racket/base)
         syntax/parse
         turnstile/base
         "rep.rkt")

(provide
 (type-out _Bool
           __int8 __uint8
           char |signed char| |unsigned char|
           __int16 __uint16
           |short int| |unsigned short int|
           __int32 __uint32
           __int64 __uint64
           int |unsigned int|
           |long int| |unsigned long int|
           |long long int| |unsigned long long int|
           float double |long double|
           ;|float _Complex| |double _Complex| |long double _Complex|
           void
           →
           unspecified→
           args...
           const
           Pointer)
 (rename-out [Array* Array])
 (for-syntax Array?
             (rename-out [~Array* ~Array]))
 (except-out (type-out Struct) Struct)
 (rename-out [Struct* Struct])
 (for-syntax signed-integer-type?
             unsigned-integer-type?
             real-floating-type?
             complex-type?
             floating-type?
             corresponding-real-type
             basic-type?
             character-type?
             integer-type?
             real-type?
             arithmetic-type?
             integer-type-width
             integer-type⊂?
             struct-tag->info ; FIXME: don't expose this
             function-type?
             function-decl
             make-→
             function-return-type
             scalar-type?
             sizeof/type)
 ...
 define-struct-type
 struct-reference)

;; TODO: integer type sizes and extended types should be configurable

(define-base-type _Bool)

(define-base-type char)

(define-base-types
  ; standard signed integer types
  |signed char|
  |short int|
  |int|
  |long int|
  |long long int|)

(define-base-types
  ; extended signed integer types (implementation-defined)
  __int8
  __int16
  __int32
  __int64)

(define-for-syntax (signed-integer-type? τ)
  (syntax-parse τ
    [(~or |~signed char|
          |~short int|
          |~int|
          |~long int|
          |~long long int|
          ~__int8
          ~__int16
          ~__int32
          ~__int64)
     #t]
    [_ #f]))

(define-base-types
  ; standard unsigned integer types
  |unsigned char|
  |unsigned short int|
  |unsigned int|
  |unsigned long int|
  |unsigned long long int|)

(define-base-types
  ; extended unsigned integer types (implementation-defined)
  __uint8
  __uint16
  __uint32
  __uint64)

(define-for-syntax (unsigned-integer-type? τ)
  (syntax-parse τ
    [(~or ~_Bool
          |~unsigned char|
          |~unsigned short int|
          |~unsigned int|
          |~unsigned long int|
          |~unsigned long long int|
          ~__uint8
          ~__uint16
          ~__uint32
          ~__uint64)
     #t]
    [_ #f]))

(define-base-types
  ; real floating types
  |float|
  |double|
  |long double|)

(define-for-syntax (real-floating-type? τ)
  (or (|float?| τ)
      (|double?| τ)
      (|long double?| τ)))

(define-base-types
  ; complex types
  |float _Complex|
  |double _Complex|
  |long double _Complex|)

(define-for-syntax (complex-type? τ)
  (or (|float _Complex?| τ)
      (|double _Complex?| τ)
      (|long double _Complex?| τ)))

(define-for-syntax (floating-type? τ)
  (or (real-floating-type? τ)
      (complex-type? τ)))

(define-for-syntax (corresponding-real-type τ)
  (cond
    [(|float _Complex?| τ) ((current-type-eval) #'|float|)]
    [(|double _Complex?| τ) ((current-type-eval) #'|double|)]
    [(|long double _Complex?| τ) ((current-type-eval) #'|long double|)]
    [else τ]))

(define-for-syntax (basic-type? τ)
  (or (char? τ)
      (signed-integer-type? τ)
      (unsigned-integer-type? τ)
      (floating-type? τ)))

(define-for-syntax (character-type? τ)
  (or (char? τ)
      (|signed char?| τ)
      (|unsigned char?| τ)))

; TODO: enumerated types

(define-for-syntax (integer-type? τ)
  (or (char? τ)
      (signed-integer-type? τ)
      (unsigned-integer-type? τ)))

(define-for-syntax (real-type? τ)
  (or (integer-type? τ)
      (real-floating-type? τ)))

(define-for-syntax (arithmetic-type? τ)
  (or (integer-type? τ)
      (floating-type? τ)))

(define-base-type void #:runtime)

(define-for-syntax (integer-type-width τ)
  (syntax-parse τ
    [~_Bool 1]
    [(~or |~char| |~signed char| |~unsigned char| |~__int8| |~__uint8|) 8]
    [(~or |~short int| |~unsigned short int| |~__int16| |~__uint16|) 16]
    [(~or |~__int32| |~__uint32|) 32]
    [_ 64]))

(define-for-syntax (sizeof/basic-type τ)
  (cond
    [(_Bool? τ) 1]
    [(integer-type? τ) (quotient (integer-type-width τ) 8)]
    [(real-floating-type? τ) 8]
    [else 16]))

(define-for-syntax (integer-type⊂? τ1 τ2)
  (define w1 (integer-type-width τ1))
  (define w2 (integer-type-width τ2))
  (define s?1 (signed-integer-type? τ1))
  (define s?2 (signed-integer-type? τ2))
  (cond
    [(and s?1 (not s?2)) #f]
    [(< w1 w2) #t]
    [(= w1 w2) (eq? s?1 s?2)]
    [else #f]))

; Arrays

(define-type-constructor Array
  #:arity = 2)

; TODO: there *must* be a nicer way of doing this
(define-syntax Array*
  (syntax-parser
    [(_ base:type len)
     (with-syntax ([len/τ (mk-type #'(#%plain-app list 'len))])
       #'(Array base len/τ))]))

(begin-for-syntax
  (define-syntax ~Array*
    (pattern-expander
     (λ (stx)
       (syntax-case stx ()
         [(_ base len)
          #'(~Array base
                    ((~literal #%plain-app)
                     (~literal list)
                     ((~literal quote) len)))])))))

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

; Qualifiers

(define-type-constructor const)

; Pointers

(define-type-constructor Pointer
  #:arity = 1)

(define-for-syntax (scalar-type? τ)
  (or (arithmetic-type? τ) (Pointer? τ)))

(define-for-syntax (sizeof/type τ)
  ; TODO: derived types
  (sizeof/basic-type τ))
