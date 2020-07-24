#lang turnstile/base

(require (prefix-in r: (only-in racket/base void))
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
           void)
 (for-syntax basic-type?
             character-type?
             integer-type?
             real-type?
             arithmetic-type?
             integer-promote
             make-conversion/basic
             common-real-type
             sizeof/basic-type)
 #%datum)

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

(define-for-syntax integer-conversion-ranks
  (list
   (list _Bool?)
   (list __int8? __uint8?)
   (list |signed char?| |unsigned char?| char?)
   (list __int16? __uint16?)
   (list |short int?| |unsigned short int?|)
   (list __int32? __uint32?)
   (list __int64? __uint64?)
   (list int? |unsigned int?|)
   (list |long int?| |unsigned long int?|)
   (list |long long int?| |unsigned long long int?|)))

(define-for-syntax (integer-conversion-rank τ)
  (for/first ([r (in-naturals)]
              [ps (in-list integer-conversion-ranks)]
              #:when (memf (λ (p) (p τ)) ps))
    r))

(define-for-syntax (integer-rank<? τ1 τ2)
  (define r1 (integer-conversion-rank τ1))
  (define r2 (integer-conversion-rank τ2))
  (< r1 r2))

(define-for-syntax (integer-promote τ)
  (define τ-int ((current-type-eval) #'int))
  (cond
    [(integer-rank<? τ-int τ) τ]
    [(integer-type⊂? τ τ-int) τ-int]
    [else ((current-type-eval) #'|unsigned int|)]))

;;;
;;; Conversions
;;;

(define-syntax constrain-value
  (syntax-parser
    [(_ τ:type e)
     (if (floating-type? #'τ.norm)
         #'e
         (with-syntax ([bits (integer-type-width #'τ.norm)])
           (if (signed-integer-type? #'τ.norm)
               #'(constrain-integer/signed bits e)
               #'(constrain-integer/unsigned bits e))))]))

(define-for-syntax (make-conversion/integer τ_old τ_new stx)
  (cond
    [(_Bool? τ_new) #`(->_Bool #,stx)]
    [(integer-type⊂? τ_old τ_new) stx]
    [else #`(constrain-value #,τ_new #,stx)]))

(define-for-syntax (make-conversion/real+integer τ_old τ_new stx)
  (cond
    [(_Bool? τ_new) #`(->_Bool #,stx)]
    [(real-floating-type? τ_old)
     ; TODO: truncation of integer part is UB
     #`(inexact->exact (truncate #,stx))]
    [else #`(real->double-flonum #,stx)]))

(define-for-syntax (make-conversion/real-floating τ_old τ_new stx)
  ; everything is a double-flonum
  stx)

(define-for-syntax (make-conversion/arithmetic τ_old τ_new stx)
  (when (or (complex-type? τ_old) (complex-type? τ_new))
    (error "TODO: complex arithmetic conversion"))
  (define i?-old (integer-type? τ_old))
  (define i?-new (integer-type? τ_new))
  (cond
    [(and i?-old i?-new) (make-conversion/integer τ_old τ_new stx)]
    [(or i?-old i?-new) (make-conversion/real+integer τ_old τ_new stx)]
    [else (make-conversion/real-floating τ_old τ_new stx)]))

(define-for-syntax (make-conversion/basic τ_old τ_new stx)
  (cond
    [(void? τ_new) #`(r:void #,stx)]
    [(void? τ_old) (raise-syntax-error #f stx "invalid conversion from void")]
    [else (make-conversion/arithmetic τ_old τ_new stx)]))

(define-for-syntax (common-real-type τ1 τ2)
  (define real-τ1 (corresponding-real-type τ1))
  (define real-τ2 (corresponding-real-type τ2))
  (cond
    [(|long double?| real-τ1) real-τ1]
    [(|long double?| real-τ2) real-τ2]
    [(double? real-τ1) real-τ1]
    [(double? real-τ2) real-τ2]
    [(float? real-τ1) real-τ1]
    [(float? real-τ2) real-τ2]
    [else
     (let ([p1 (integer-promote τ1)]
           [p2 (integer-promote τ2)])
       (cond
         [(type=? p1 p2) p1]
         [(eq? (signed-integer-type? p1) (signed-integer-type? p2))
          (if (integer-rank<? p1 p2) p2 p1)]
         [else
          (let-values ([(τu τs) (if (signed-integer-type? p1) (values p1 p2) (values p2 p1))])
            (cond
              [(integer-rank<? τs τu) τu]
              [(integer-type⊂? τu τs) τs]
              [else (error "TODO: signed->unsigned type")]))]))]))

#;(define-for-syntax (usual-arithmetic-conversion τ1 τ2)
  (define crt (common-real-type τ1 τ2))
  ; TODO: complex
  (values crt crt))

; TODO: suffixes, different default signedness for decimal/hex/octal
(define-typed-syntax #%datum
  [(_ . n:exact-integer) ≫
   ; all our non-short int types are 64-bit for now.
   #:with (n^ τ)
   (if (>= (integer-length (syntax-e #'n)) 64)
       (list #'(constrain-integer/unsigned '64 'n) #'|unsigned int|)
       (list #'(constrain-integer/signed '64 'n) #'int))
   --------
   [⊢ n^ ⇒ τ]]
  [(_ . f) ≫
   #:when (flonum? (syntax-e #'f))
   --------
   [⊢ 'f ⇒ double]]
  [(_ . c:char) ≫
   --------
   [⊢ (#%plain-app- char->integer 'c) ⇒ int]]
  [(_ . d) ≫
   --------
   [#:error (type-error #:src #'d #:msg "unsupported literal")]])
