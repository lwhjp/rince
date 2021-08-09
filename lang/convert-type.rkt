#lang racket/base

;;;
;;; Type conversions
;;;

(require (prefix-in r: (only-in racket/base void))
         (except-in turnstile/base char? void void?)
         "rep.rkt"
         "types.rkt")

(provide
 (for-syntax integer-promote
             common-real-type
             usual-arithmetic-conversion-types
             make-conversion)
 constrain-value
 decay-array)

(define-for-syntax DEBUG #f)

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

(define-for-syntax (make-conversion τ_old τ_new stx)
  (when DEBUG
    (printf "convert ~a -> ~a\nexpr: ~a\n" (type->str τ_old) (type->str τ_new) stx))
  (syntax-parse #`(#,((current-type-eval) τ_new)
                   #,((current-type-eval) τ_old))
    [(τ_new τ_old)
     #:when (type=? #'τ_new #'τ_old)
     (when DEBUG (displayln "same type"))
     stx]
    [(_ (~const base_old))
     (when DEBUG (displayln "un-const old"))
     (make-conversion #'base_old τ_new stx)]
    [((~const base_new) _)
     (when DEBUG (displayln "un-const new"))
     (make-conversion τ_old #'base_new stx)]
    [((~Pointer _) (~Array base_old _))
     (when DEBUG (displayln "array->pointer"))
     (make-conversion #'(Pointer base_old) τ_new #`(array->pointer #,stx))]
    [((~Pointer ~char) (~Pointer base_old))
     (when DEBUG (displayln "int->raw"))
     ; TODO: handle non-integer types
     #`(integer->raw/pointer #,stx #,(sizeof/type #'base_old) #,(signed-integer-type? #'base_old))]
    [((~Pointer base_new) (~Pointer ~char))
     (when DEBUG (displayln "raw->int"))
     ; TODO: handle non-integer types
     #`(raw->integer/pointer #,stx #,(sizeof/type #'base_new) #,(signed-integer-type? #'base_new))]
    [((~Pointer _) (~Pointer _))
     (when DEBUG (displayln "pun"))
     ; TODO: handle non-integer types
     (make-conversion #'(Pointer char) τ_new
                      (make-conversion τ_old #'(Pointer char) stx))]
    [_ #:when (and (basic-type? τ_old) (basic-type? τ_new))
     (when DEBUG (displayln "basic"))
     (make-conversion/basic τ_old τ_new stx)]
    [_ (raise-syntax-error
        #f
        (format "unsupported type conversion: ~a -> ~a"
                (type->str τ_old)
                (type->str τ_new))
        stx)]))

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

(define-for-syntax (usual-arithmetic-conversion-types τ1 τ2)
  (define crt (common-real-type τ1 τ2))
  ; TODO: complex
  (values crt crt crt))

(define-typed-syntax (decay-array v) ≫
  [⊢ v ≫ v- ⇒ (~Array τ_e _)]
  #:with τ^ #'(Pointer τ_e)
  #:with v^ #'(array->pointer v-)
  --------
  [⊢ v^ ⇒ τ^])
