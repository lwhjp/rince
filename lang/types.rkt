#lang turnstile/base

;; C types

(require "rep.rkt")

(provide
 (for-syntax function-decl →-ret)
 (type-out void)
           _Bool
           |signed char| |unsigned char|
           |signed short int| |unsigned short int|
           |signed int| |unsigned int|
           |signed long int| |unsigned long int|
           |signed long long int| |unsigned long long int|
           __int8 __uint8
           __int16 __uint16
           __int32 __uint32
           __int64 __uint64
           float double |long double|
 (type-out const
           Struct
           Pointer)
 (for-syntax make-→
             sizeof/type
             function-type?
             arithmetic-type?
             integer-type?
             integer-promote-type
             common-real-type)
 ...
 #%datum
 assignment-cast
 cast
 define-struct-type)

;; Probably needs some rethinking re variance etc.

(define-base-type void)

(define-integer-type _Bool 1 #f)

(define-integer-types
  [|signed char| 8 #t]
  [|unsigned char| 8 #f]
  [|signed short int| 16 #t]
  [|unsigned short int| 16 #f]
  [|signed int| 64 #t]
  [|unsigned int| 64 #f]
  [|signed long int| 64 #t]
  [|unsigned long int| 64 #f]
  [|signed long long int| 64 #t]
  [|unsigned long long int| 64 #f])

(define-integer-types
  [__int8 8 #t]
  [__int16 16 #t]
  [__int32 32 #t]
  [__int64 64 #t]
  [__uint8 8 #f]
  [__uint16 16 #f]
  [__uint32 32 #f]
  [__uint64 64 #f])

(define-syntax float (make-rename-transformer #'Single-Float))
(define-for-syntax float? Single-Float?)
(define-syntax double (make-rename-transformer #'Double-Float))
(define-for-syntax double? Double-Float?)
; FIXME: this should be a distinct type
(define-syntax |long double| (make-rename-transformer #'Double-Float))
(define-for-syntax |long double?| Double-Float?)

(define-type-constructor const #:arity = 1)

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

(define-for-syntax (→-ret τ)
  (syntax-parse τ
    [(~→ _ ... ret) #'ret]
    [(~unspecified→ ret) #'ret]))

(define-for-syntax (function-type? τ)
  (or (→? τ) (unspecified→? τ)))

; TODO: struct, union

(define-for-syntax (arithmetic-type? τ)
  (or (integer-type? τ) (float? τ) (double? τ) (|long double?| τ)))

(define-for-syntax (integer-type? τ)
  (or (Integer? τ) (Constrained-Integer? τ)))

(define-for-syntax integer-type-ranks
  '((_Bool)
    (__int8 __uint8)
    (|signed char| |unsigned char|)
    (__int16 __uint16)
    (|signed short int| |unsigned short int|)
    (__int32 __uint32)
    (__int64 __uint64)
    (|signed int| |unsigned int|)
    (|signed long int| |unsigned long int|)
    (|signed long long int| |unsigned long long int|)))

(define-for-syntax (integer-type-rank τ)
  (define id (Constrained-Integer-id τ))
  (for/first ([r (in-naturals)]
              [types (in-list integer-type-ranks)]
              #:when (memq id types))
    r))

(define-for-syntax (integer-promote-type τ)
  (define r_int (integer-type-rank #'|signed int|))
  (unless (<= (integer-type-rank τ) r_int)
    (error "invalid integer promotion"))
  (if (Constrained-Integer⊂? τ #'|signed int|)
      #'|signed int|
      #'|unsigned int|))

(define-for-syntax (common-real-type τ1 τ2)
  (define (signed->unsigned/type τ) (error 'TODO))
  (cond
    [(or (|long double?| τ1) (|long double?| τ2)) #'|long double|]
    [(or (double? τ1) (double? τ2)) #'double]
    [(or (float? τ1) (float? τ2)) #'float]
    [else
     (let ([τ1 (integer-promote-type τ1)]
           [τ2 (integer-promote-type τ2)])
       (let-values ([(bits1 signed?1) (Constrained-Integer-bits+signed? τ1)]
                    [(bits2 signed?2) (Constrained-Integer-bits+signed? τ2)]
                    [(r1) (integer-type-rank τ1)]
                    [(r2) (integer-type-rank τ2)])
         (cond
           [(type=? τ1 τ2) τ1]
           [(eq? signed?1 signed?2) (if (< r1 r2) τ2 τ1)]
           [(and signed?2 (>= r1 r2)) τ1]
           [(and signed?1 (>= r2 r1)) τ2]
           [(and signed?1 (Constrained-Integer⊂? τ2 τ1) τ1)]
           [(and signed?2 (Constrained-Integer⊂? τ1 τ2) τ2)]
           [signed?1 (signed->unsigned/type τ1)]
           [else (signed->unsigned/type τ2)])))]))

; TODO: check 6.5.16
(define-syntax-rule (assignment-cast τ v) (cast τ v))
