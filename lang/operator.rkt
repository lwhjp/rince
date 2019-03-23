#lang turnstile/base

;; C operators

(extends "types.rkt" #:prefix base:)

(require "rep.rkt")

(provide
 lvalue
 pointer-inc
 pointer-diff
 sizeof
 |.| -> post++ pre++
 + - * &
 = +=
 |,|)

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

(define-typed-syntax (|.| s:expr member:id) ≫
  --------
  [≻ (struct-ref s member)])

; TODO: do we need to support &(((struct foo *) 0)->bar) etc?
(define-syntax-rule (-> ps member) (|.| (* ps) member))

(define-typed-syntax (post++ v) ≫
  [⊢ v ≫ v- ⇒ τ]
  #:with v^ (generate-temporary #'v)
  #:with v+ (assign-type #'(lvalue v^) #'τ #:wrap? #f)
  --------
  [⊢ (let ([v^ (unwrap-lvalue v-)])
       (begin0
         (lvalue v^)
         (set-lvalue! v^ (assignment-cast τ (+ v+ (cast |signed int| 1))))))
     ⇒ τ])

;; Unary operators

(define-typed-syntax (pre++ v) ≫
  --------
  [≻ (+= v (cast |signed int| 1))])

(define-typed-syntax (unary& v) ≫
  [⊢ v ≫ v- ⇒ τ]
  --------
  [⊢ (make-pointer (unwrap-lvalue v-)) ⇒ (Pointer τ)])

(define-typed-syntax (unary* v) ≫
  [⊢ v ≫ v- ⇒ (~Pointer τ)]
  --------
  [⊢ (lvalue (pointer-dereference v-)) ⇒ τ])

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
   [⊢ (quote size) ⇒ |signed int|]]
  [(_ e:expr) ≫
   [⊢ e ≫ e- ⇒ τ]
   --------
   [≻ (sizeof τ)]])

;; Arithmetical operators

(define-typed-syntax (binary+ x y) ≫
  [⊢ x ≫ x- ⇒ τ_x]
  [⊢ y ≫ y- ⇒ τ_y]
  #:with expr
  (cond
    [(and (arithmetic-type? #'τ_x) (arithmetic-type? #'τ_y))
     (let* ([τ_out (common-real-type #'τ_x #'τ_y)]
            [τ_prim (if (Constrained-Integer? τ_out) #'Integer τ_out)])
       #`(cast #,τ_out #,(assign-type #`(+- (cast #,τ_out x) (cast #,τ_out y)) τ_prim)))]
    [(and (Pointer? #'τ_x) (integer-type? #'τ_y))
     (assign-type #'(pointer-inc x- y-) #'τ_x)]
    [(and (Pointer? #'τ_y) (integer-type? #'τ_x))
     (assign-type #'(pointer-inc y- x-) #'τ_x)]
    [else (raise-syntax-error #f "invalid types" this-syntax)])
  --------
  [≻ expr])

(define-typed-syntax (binary- x y) ≫
  [⊢ x ≫ x- ⇒ τ_x]
  [⊢ y ≫ y- ⇒ τ_y]
  #:with expr
  (cond
    [(and (arithmetic-type? #'τ_x) (arithmetic-type? #'τ_y))
     (let* ([τ_out (common-real-type #'τ_x #'τ_y)]
            [τ_prim (if (Constrained-Integer? τ_out) #'Integer τ_out)])
       #`(cast #,τ_out #,(assign-type #`(-- (cast #,τ_out x) (cast #,τ_out y)) τ_prim)))]
    [(and (Pointer? #'τ_x) (integer-type? #'τ_y))
     (assign-type #'(pointer-inc x- (-- y-)) #'τ_x)]
    [(and (Pointer? #'τ_x) (type=? #'τ_x #'τ_y))
     (assign-type #'(pointer-diff x- y-) #'|signed long long int|)]
    [else (raise-syntax-error #f "invalid types" this-syntax)])
  --------
  [≻ expr])

;; Comparison operators

;; Assignment

(define-typed-syntax (= x y) ≫
  [⊢ x ≫ x- ⇒ τ]
  #:with x^ (generate-temporary #'x)
  #:with x+ (assign-type #'(lvalue x^) #'τ #:wrap? #f)
  --------
  [⊢ (lvalue
      (let ([x^ (unwrap-lvalue x-)])
        (set-lvalue! x^ (assignment-cast τ y))
        x^)) ⇒ τ])

(define-syntax-rule (define-assignment-operator id op)
  (define-typed-syntax (id x y) ≫
    [⊢ x ≫ x- ⇒ τ]
    #:with x^ (generate-temporary #'x)
    #:with x+ (assign-type #'(lvalue x^) #'τ #:wrap? #f)
    --------
    [⊢ (lvalue
        (let ([x^ (unwrap-lvalue x-)])
          (set-lvalue! x^ (assignment-cast τ (op x+ y)))
          x^)) ⇒ τ]))

(define-assignment-operator += +)
         
;; Comma

(define-typed-syntax (|,| e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ1]
  [⊢ e2 ≫ e2- ⇒ τ2]
  --------
  [⊢ (begin- e1- e2-) ⇒ τ2])
