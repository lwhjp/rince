#lang turnstile/base

;; Representation of C values in racket

(require (for-meta 2 racket/base)
         racket/generic
         racket/math
         "core.rkt"
         "parameterized-type.rkt")

(provide
 (for-syntax Constrained-Integer?
             Constrained-Integer-id
             Constrained-Integer-bits+signed?
             Constrained-Integer⊂?
             sizeof/type)
 (type-out Integer
           Single-Float
           Double-Float
           Array
           Struct
           Pointer
           →
           unspecified→
           args...)
 define-integer-type
 define-integer-types
 object-ref
 set-lvalue!
 lvalue
 unwrap-lvalue
 make-variable
 define-struct-type
 struct-ref
 make-pointer
 pointer-dereference
 pointer-inc
 pointer-diff
 cast
 initializer
 static-initializer
 unspecified-initializer
 #%app
 (rename-out
  [#%datum++ #%datum]))

(define-typed-syntax #%datum++
  [(_ . s:string) ≫
   #:do ((define data
           (list->vector
            (append
             (bytes->list
              (string->bytes/utf-8 (syntax-e #'s)))
             '(0)))))
   --------
   [⊢ (array #,data 1) ⇒ (Array '(#,(vector-length data)))]]
  [(_ . x) ≫
   --------
   [≻ (#%datum+ . x)]])

; TODO: use parameterized type for constrained-integer

(define (Constrained-Integer . args)
  (error "invalid use of constrained integer type"))

(begin-for-syntax
  (struct constrained-integer-type (id bits signed?)
    #:property prop:procedure
    (λ (this stx)
      (syntax-case stx ()
        [_ (identifier? stx)
         (with-syntax ([id (constrained-integer-type-id this)]
                       [bits (constrained-integer-type-bits this)]
                       [signed? (constrained-integer-type-signed? this)])
           (mk-type
            (syntax/loc stx
              (#%plain-app Constrained-Integer 'id bits signed?))))])))
  (define-syntax ~Constrained-Integer
    (pattern-expander
     (λ (stx)
       (syntax-case stx ()
         [(_ bits-pat signed?-pat)
          #'(~describe
             #:opaque
             "constrained integer type"
             ((~literal #%plain-app)
              (~literal Constrained-Integer)
              ((~literal quote) _:id)
              ((~literal quote) bits-pat)
              ((~literal quote) signed?-pat)))])))))

(define-for-syntax (Constrained-Integer? stx)
  (syntax-parse stx [(~Constrained-Integer _ _) #t] [_ #f]))

(define-for-syntax (Constrained-Integer-id τ)
  (syntax-parse ((current-type-eval) τ)
    [((~literal #%plain-app)
      (~literal Constrained-Integer)
      ((~literal quote) id:id)
      _
      _)
     (syntax-e #'id)]))

(define-for-syntax (Constrained-Integer-bits+signed? τ)
  (syntax-parse ((current-type-eval )τ)
    [(~Constrained-Integer bits signed?)
     (values (syntax-e #'bits) (syntax-e #'signed?))]))

(define-for-syntax (Constrained-Integer⊂? τ1 τ2)
  (let ([τ1 ((current-type-eval) τ1)]
        [τ2 ((current-type-eval) τ2)])
    (let-values ([(bits1 signed?1) (Constrained-Integer-bits+signed? τ1)]
                 [(bits2 signed?2) (Constrained-Integer-bits+signed? τ2)])
      (cond
        [(eq? signed?1 signed?2) (<= bits1 bits2)]
        [signed?2 (< bits1 bits2)]
        [else #f]))))

(define-syntax define-integer-type
  (syntax-parser
    [(_ τ:id bits:exact-positive-integer (~and (~or #t #f) signed))
     (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
       #'(begin
           (define-for-syntax (τ? stx) (type=? #'τ stx))
           ; TODO: expander
           (define-syntax τ (constrained-integer-type #'τ bits signed))))]))

(define-syntax-rule
  (define-integer-types [id bits signed?] ...)
  (begin (define-integer-type id bits signed?) ...))

(define (constrain-int/bool v)
  (if (eqv? 0 v) 0 1))

(define (constrain-int/unsigned v bits)
  (let ([mask (sub1 (arithmetic-shift 1 bits))])
    (bitwise-and v mask)))

(define (constrain-int/signed v bits)
  ; signed truncation is implementation-defined
  ; For now, we'll truncate and sign-extend.
  (let ([mask (sub1 (arithmetic-shift 1 bits))])
    (let ([a (bitwise-and v mask)])
      (if (bitwise-bit-set? a (sub1 bits))
          (bitwise-ior a (bitwise-not mask))
          a))))

(define-for-syntax (make-integer-constraint τ v)
  (let-values ([(bits signed?) (Constrained-Integer-bits+signed? τ)])
    (cond
      [(eqv? 1 bits) #`(constrain-int/bool #,v)]
      [signed? #`(constrain-int/signed #,v #,bits)]
      [else #`(constrain-int/unsigned #,v #,bits)])))

; TODO: should we have an "implicit cast" mode? (eg, for integer overflow)
(define-typed-syntax (cast τ:type v:expr) ≫
  [⊢ v ≫ v- ⇒ τ_orig]
  --------
  [⊢ #,(make-cast-expression this-syntax #'τ.norm #'τ_orig #'v-) ⇒ τ])

(define-for-syntax (make-cast-expression stx τ_to τ_from v)
  (define (fail)
    (raise-syntax-error #f "invalid cast" stx))
  ; TODO: more checks here
  (define cast-stx
    (cond
      [(type=? τ_to τ_from) v]
      [(Constrained-Integer? τ_to)
       ; TODO: don't need to constrain subtypes
       (make-integer-constraint
        τ_to
        (cond
          [(or (Integer? τ_from) (Constrained-Integer? τ_from)) v]
          [(or (Single-Float? τ_from) (Double-Float? τ_from)) #`(exact-truncate #,v)]
          [else (fail)]))]
      [(Single-Float? τ_to) #`(real->single-flonum #,v)]
      [(Double-Float? τ_to) #`(real->double-flonum #,v)]
      [(and (Pointer? τ_to) (Array? τ_from))
       ; XXX: cast element type
       #`(array->pointer #,v)]
      [else (fail)]))
  (syntax-track-origin cast-stx stx (stx-car stx)))

(define-for-syntax (sizeof/type τ)
  (syntax-parse τ
    [(~Constrained-Integer bits _) (max 1 (quotient (syntax-e #'bits) 8))]
    [~Single-Float 4]
    [~Double-Float 8]
    [else (raise-syntax-error 'sizeof/type "invalid type" #f τ)]))

; Objects

; base type for objects: ref, to-integer, size

; TODO: use type/syntax info to handle defaults at expansion time
(define-generics object
  (object-ref object)
  #:fast-defaults
  ([box?
    (define object-ref unbox)]
   [number?
    (define object-ref values)]))

; Lvalues

(define-generics lvalue
  (set-lvalue! lvalue v)
  #:fast-defaults
  ([box?
    (define set-lvalue! set-box!)]))

(begin-for-syntax
  (struct lvalue-wrapper (target)
    #:property prop:procedure
    (λ (this stx)
      (syntax/loc stx target))))

(define (lvalue v) (object-ref v))

; Hack: lvalue expressions need to evaluate to the unboxed value, but we
; still need to be able to get at the box for mutation and referencing.
(define-syntax unwrap-lvalue
  (syntax-parser
    [(_ v)
     (syntax-parse (local-expand #'v 'expression '())
       [((~literal #%plain-app-) (~literal lvalue) lv) #'lv]
       [_ (println this-syntax) (raise-syntax-error #f "not an lvalue" #f #'v)])]))

(define make-variable box)

; Arrays

(define-parameterized-type (Array dimensions))

(struct array (data element-size))

(define (array->pointer arr)
  (pointer
   arr
   0
   (λ (p) (vector-ref (array-data (pointer-target p)) (pointer-index p)))
   (λ (p v) (vector-set! (array-data (pointer-target p)) (pointer-index p) v))))

; Structs

; This needs a bit of extra logic elsewhere to handle
; anonymous struct types and tag declarations.

(define-for-syntax introduce-struct-info (make-syntax-introducer))

(define-parameterized-type (Struct tag))

(define-syntax define-struct-type
  (syntax-parser
    [(_ name:id ([τ:type field:id] ...))
     ; TODO: make- helper
     (with-syntax ([info (introduce-struct-info #'name)])
       #'(begin
           (define-syntax info #'((field . τ) ...))))]))

(define-for-syntax (struct-tag->info tag)
  (stx-map
   syntax-e
   (syntax-local-value
    (introduce-struct-info tag)
    (λ () (error "not a struct type:" tag)))))

; FIXME: s.x is only an lvalue if s is also.
(define-typed-syntax (struct-ref s field) ≫
  [⊢ s ≫ s- ⇒ (~Struct tag)]
  #:with (i τ)
  (let ([info (struct-tag->info #'tag)]
        [id (syntax-e #'field)])
    (let loop ([fields info]
               [i 0])
      (cond
        [(null? fields) (raise-syntax-error #f "invalid member specifier" this-syntax #'field)]
        [(eq? id (syntax-e (caar fields))) (list i (cdar fields))]
        [else (loop (cdr fields) (add1 i))])))
  --------
  [⊢ (lvalue (struct-reference s- (quote i))) ⇒ τ])

(struct struct-reference
  (s i)
  #:methods gen:object
  ((define (object-ref ref)
     (vector-ref (struct-reference-s ref) (struct-reference-i ref))))
  #:methods gen:lvalue
  ((define (set-lvalue! ref v)
     (vector-set! (struct-reference-s ref) (struct-reference-i ref) v))))

; Unions

; Pointers

(define-type-constructor Pointer #:arity = 1)

; should this be a generic, with array-pointer, cast-pointer etc?
(struct pointer
  (target index get set!))

(struct reference
  (pointer)
  #:methods gen:object
  ((define (object-ref ref)
     (let ([ptr (reference-pointer ref)])
       ((pointer-get ptr) ptr))))
  #:methods gen:lvalue
  ((define (set-lvalue! ref v)
     (let ([ptr (reference-pointer ref)])
       ((pointer-set! ptr) ptr v)))))

(define (make-pointer lv)
  (cond
    [(box? lv)
     (pointer
      lv
      0
      (λ (p) (object-ref (pointer-target p)))
      (λ (p v) (set-lvalue! (pointer-target p) v)))]
    [(reference? lv) (reference-pointer lv)]
    [else (error 'make-pointer "invalid argument")]))

(define pointer-dereference reference)

(define (pointer-inc p n)
  (struct-copy pointer p [index (+ (pointer-index p) n)]))

(define (pointer-diff p q)
  (- (pointer-index p) (pointer-index q)))

; Initializers

(define-syntax initializer
  (syntax-parser
    [(_ τ v)
     #'(cast τ v)]))

(define-syntax unspecified-initializer
  (syntax-parser
    [(_ (~Array dims)) (error 'TODO)]
    [(_ (~Struct tag))
     (with-syntax ([(τ_e ...) (map cdr (struct-tag->info #'tag))])
       #'(vector (unspecified-initializer τ_e) ...))]
    [(_ τ) #''unspecified]))

(define-syntax static-initializer
  (syntax-parser
    [(_ τ) #'(cast τ (#%datum+ . 0))]))
