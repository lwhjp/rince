#lang turnstile/base

;; Typed C expressions

(extends "derived-types.rkt")

(require (for-syntax racket/list
                     racket/match)
         "derived-types.rkt" ; we really do want #%datum
         "rep.rkt")

(provide
 #%app
 |.| post++
 pre++ sizeof cast
 + - * &
 = +=
 |,|
 ; FIXME: these should not be valid C names
 initializer
 static-initializer
 unspecified-initializer)

(define-syntax define-unary/binary-operators
  (syntax-parser
    [(_ id:id ...)
     (define ((make-add-prefix prefix) stx)
       (format-id stx "~a~a" prefix stx))
     (with-syntax ([(un ...) (map (make-add-prefix 'unary) (attribute id))]
                   [(bi ...) (map (make-add-prefix 'binary) (attribute id))])
       #'(define-syntaxes (id ...)
           (values (make-unary/binary-operator #'un #'bi) ...)))]))

(define-for-syntax (make-unary/binary-operator un-id bin-id)
  (λ (stx)
    (syntax-case stx ()
      [(_ x) (quasisyntax/loc stx (#,un-id x))]
      [(_ x y) (quasisyntax/loc stx (#,bin-id x y))])))

(define-unary/binary-operators
  + - * &)

;; Postfix operators

; TODO: array subscript

(define-typed-syntax (#%app f arg ...) ≫
  [⊢ f ≫ f- ⇒ τ_f]
  ; TODO: default argument promotion
  #:with (τ_ret arg^ ...)
  (syntax-parse #'τ_f
    [(~→ τ_arg ... τ_ret)
     (define-values (fixed-arg-types varargs?)
       (match (attribute τ_arg)
         [(list τs ... (? args...?)) (values τs #t)]
         [τs (values τs #f)]))
     (define fixed-arg-count (length fixed-arg-types))
     (unless (>= (stx-length #'(arg ...)) fixed-arg-count)
       (raise-syntax-error #f (format "insufficient arguments: expected at least ~a" fixed-arg-count) this-syntax #'f))
     (define-values (fixed-args extra-args)
       (split-at (attribute arg) fixed-arg-count))
     (when (and (not varargs?) (not (null? extra-args)))
       (raise-syntax-error #f "too many arguments" this-syntax #'f))
     (with-syntax ([(τ_fixed ...) fixed-arg-types]
                   [(fixed-arg ...) fixed-args]
                   [(extra-arg ...) extra-args])
       #'(τ_ret (cast τ_fixed fixed-arg) ... extra-arg ...))]
    [(~unspecified→ τ_ret)
     (cons #'τ_ret (syntax->list #'(arg ...)))])
  --------
  [⊢ (#%plain-app- f- arg^ ...) ⇒ τ_ret])

(define-syntax-rule (|.| s member) (struct-reference s member))

; TODO: do we need to support &(((struct foo *) 0)->bar) etc?
(define-syntax-rule (-> ps member) (|.| (* ps) member))

(define-typed-syntax (post++ v) ≫
  [⊢ v ≫ v- ⇒ τ]
  #:with x (generate-temporary #'x)
  #:with x+ (assign-type #'x #'τ)
  --------
  [⊢ (lvalue-post-update! v- (λ (x) (+ x+ 1))) ⇒ τ])

;; Unary operators

(define-typed-syntax (pre++ v) ≫
  --------
  [≻ (+= v 1)])

(define-typed-syntax (unary& v) ≫
  [⊢ v ≫ v- ⇒ τ]
  --------
  [⊢ (lvalue->pointer v-) ⇒ (Pointer τ)])

(define-typed-syntax (unary* v) ≫
  [⊢ v ≫ v- ⇒ (~Pointer τ)]
  --------
  [⊢ (pointer-dereference v-) ⇒ τ])

#|
(define-typed-syntax (unary+ v) ≫
  [⊢ v ≫ v- ⇒ τ_v]
  #:with τ^ (integer-promote-type #'τ_v)
  --------
  [≻ (cast τ^ v)])

(define-typed-syntax (unary- v) ≫
  [⊢ v ≫ v- ⇒ τ_v]
  #:with τ^ (integer-promote-type #'τ_v)
  --------
  [≻ (cast τ^ (- (cast τ^ v)))])
|#

(define-typed-syntax sizeof
  [(_ τ:type) ≫
   #:with size (sizeof/type #'τ.norm)
   --------
   [⊢ (quote size) ⇒ int]]
  [(_ e:expr) ≫
   [⊢ e ≫ e- ⇒ τ]
   --------
   [≻ (sizeof τ)]])

;; Cast operators

(define-typed-syntax (cast τ_out:type v) ≫
  #:with τ #'τ_out.norm
  [⊢ v ≫ v- ⇒ τ_v]
  #:with v^ (make-conversion #'τ_v #'τ #'v-)
  --------
  [⊢ v^ ⇒ τ])

;; Arithmetical operators

(define-typed-syntax (binary+ x y) ≫
  [⊢ x ≫ x- ⇒ τ_x]
  [⊢ y ≫ y- ⇒ τ_y]
  #:with expr
  (cond
    [(and (arithmetic-type? #'τ_x) (arithmetic-type? #'τ_y))
     (with-syntax ([τ (common-real-type #'τ_x #'τ_y)])
       (assign-type #'(#%plain-app- constrain-value τ (+- (cast τ x) (cast τ y))) #'τ))]
    [(and (Pointer? #'τ_x) (integer-type? #'τ_y))
     (assign-type #'(#%plain-app- pointer-inc x- y-) #'τ_x)]
    [(and (Pointer? #'τ_y) (integer-type? #'τ_x))
     (assign-type #'(#%plain-app- pointer-inc y- x-) #'τ_y)]
    [else (raise-syntax-error #f "invalid types" this-syntax)])
  --------
  [≻ expr])

(define-typed-syntax (binary- x y) ≫
  [⊢ x ≫ x- ⇒ τ_x]
  [⊢ y ≫ y- ⇒ τ_y]
  #:with expr
  (cond
    [(and (arithmetic-type? #'τ_x) (arithmetic-type? #'τ_y))
     (with-syntax ([τ (common-real-type #'τ_x #'τ_y)])
       (assign-type #'(#%plain-app- constrain-value τ (-- (cast τ x) (cast τ y))) #'τ))]
    [(and (Pointer? #'τ_x) (integer-type? #'τ_y))
     (assign-type #'(#%plain-app- pointer-inc x- (-- y-)) #'τ_x)]
    [(and (Pointer? #'τ_x) (type=? #'τ_x #'τ_y))
     (assign-type #'(#%plain-app- pointer-diff x- y-) #'|long long int|)]
    [else (raise-syntax-error #f "invalid types" this-syntax)])
  --------
  [≻ expr])

;; Comparison operators

;; Assignment

(define-typed-syntax (= x y) ≫
  [⊢ x ≫ x- ⇒ τ]
  ; TODO: unqualify type
  --------
  [⊢ (lvalue-set! x- (cast τ y)) ⇒ τ])

(define-syntax-rule (define-assignment-operator id op)
  (define-typed-syntax (id x y) ≫
    [⊢ x ≫ x- ⇒ τ]
    #:with v (generate-temporary #'v)
    #:with v+ (assign-type #'v #'τ)
    --------
    [⊢ (lvalue-pre-update! x- (λ (v) (op v+ y))) ⇒ τ]))

(define-assignment-operator += +)

;; Comma

(define-typed-syntax (|,| e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ1]
  [⊢ e2 ≫ e2- ⇒ τ2]
  --------
  [⊢ (begin- e1- e2-) ⇒ τ2])

;; Initializers

(define-syntax initializer
  (syntax-parser
    [(_ τ v)
     #'(cast τ v)]))

(define-syntax unspecified-initializer
  (syntax-parser
    [(_ (~Array dims)) (error 'TODO)]
    [(_ (~Struct tag))
     (with-syntax ([((f . τ_e) ...) (struct-tag->info #'tag)])
       #'(#%plain-app vector (unspecified-initializer τ_e) ...))]
    [(_ τ) #''unspecified]))

(define-syntax static-initializer
  (syntax-parser
    [(_ τ) #'(cast τ (#%datum+ . 0))]))
