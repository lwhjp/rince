#lang turnstile/base

;; C operators

(extends "types.rkt" #:prefix base)

(require "rep.rkt")

(provide
 lvalue
 pointer-inc
 pointer-diff
 sizeof
 post++ pre++
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

; FIXME: we should really be using v- in the expansion, but that
; gives us an ambiguous binding error (probably the temporary created
; in scope.rkt has the wrong scopes)
; (Also for other operators below)
(define-typed-syntax (post++ ((~literal lvalue) v)) ≫
  [⊢ v ≫ v- ⇒ τ]
  #:with lv- #'lv
  #:with lv+ (assign-type #'lv- #'τ)
  --------
  [⊢ (let ([lv- v])
       (begin0
         (object-ref lv-)
         (set-lvalue! lv- (+ (lvalue lv+) 1)))) ⇒ τ])

;; Unary operators

(define-typed-syntax (pre++ ((~literal lvalue) v)) ≫
  --------
  [≻ (+= (lvalue v) 1)])

(define-typed-syntax (unary& ((~literal lvalue) v)) ≫
  [⊢ v ≫ v- ⇒ τ]
  --------
  [⊢ (make-pointer v) ⇒ (Pointer τ)])

(define-typed-syntax (unary* v) ≫
  [⊢ v ≫ v- ⇒ (~Pointer τ)]
  #:with expr- #'(pointer-dereference v)
  #:with expr+ (assign-type #'expr- #'τ)
  --------
  [≻ (lvalue expr+)])

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
       #`(cast #,τ_out #,(assign-type #'(+- (cast #,τ_out x) (cast #,τ_out y)) τ_prim)))]
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
       #`(cast #,τ_out #,(assign-type #'(-- (cast #,τ_out x) (cast #,τ_out y)) τ_prim)))]
    [(and (Pointer? #'τ_x) (integer-type? #'τ_y))
     (assign-type #'(pointer-inc x- (-- y-)) #'τ_x)]
    [(and (Pointer? #'τ_x) (type=? #'τ_x #'τ_y))
     (assign-type #'(pointer-diff x- y-) #'|signed long long int|)]
    [else (raise-syntax-error #f "invalid types" this-syntax)])
  --------
  [≻ expr])

;; Comparison operators

;; Assignment

(define-typed-syntax (= ((~literal lvalue) x) y) ≫
  [⊢ x ≫ x- ⇒ τ]
  #:with expr- #'(let ([lv x])
                   (set-lvalue! lv (assignment-cast τ y))
                   lv)
  #:with expr+ (assign-type #'expr- #'τ)
  --------
  [≻ (lvalue expr+)])

(define-syntax-rule (define-assignment-operator id op)
  (define-typed-syntax (id ((~literal lvalue) x) y) ≫
    [⊢ x ≫ x- ⇒ τ]
    #:with lv- #'lv
    #:with lv+ (assign-type #'lv- #'τ)
    #:with expr- #'(let ([lv- x])
                     (set-lvalue! lv- (assignment-cast τ (op (lvalue lv+) y)))
                     lv-)
    #:with expr+ (assign-type #'expr- #'τ)
    --------
    [≻ (lvalue expr+)]))

(define-assignment-operator += +)
         
;; Comma

(define-typed-syntax (|,| e1 e2) ≫
  [⊢ e1 ≫ e1- ⇒ τ1]
  [⊢ e2 ≫ e2- ⇒ τ2]
  --------
  [⊢ (begin- e1- e2-) ⇒ τ2])
