#lang racket/base

(require (for-syntax racket/base))

(provide
 ->_Bool
 constrain-integer/unsigned
 constrain-integer/signed
 lvalue-set!
 lvalue-pre-update!
 lvalue-post-update!
 lvalue->pointer
 make-variable
 variable-reference
 array
 array->pointer
 struct-member-reference
 pointer-dereference
 pointer-inc
 pointer-diff)

;; Representation of C values in racket

(define (->_Bool v)
  (if (zero? v) 0 1))

(define (constrain-integer/unsigned bits v)
  (let ([mask (sub1 (arithmetic-shift 1 bits))])
    (bitwise-and mask v)))

(define (constrain-integer/signed bits v)
  ; signed truncation is implementation-defined
  ; For now, we'll truncate and sign-extend.
  (let ([mask (sub1 (arithmetic-shift 1 bits))])
    (let ([a (bitwise-and v mask)])
      (if (bitwise-bit-set? a (sub1 bits))
          (bitwise-ior a (bitwise-not mask))
          a))))

; Lvalues

(struct lvalue (get-proc set!-proc ptr-proc)
  #:constructor-name make-lvalue)

(define-syntax (lvalue-reference stx)
  (syntax-case stx ()
    [(_ . lv) #'((lvalue-get-proc lv))]))

(define-for-syntax (extract-lv lvr)
  (syntax-case (local-expand (syntax-disarm lvr #f) 'expression (list #'lvalue-reference))
               (lvalue-reference)
    [(lvalue-reference . lv) #'lv]
    [_ (raise-syntax-error #f "not an lvalue reference" #f lvr)]))

(define-syntax (lvalue-set! stx)
  (syntax-case stx (lvalue-reference)
    [(_ lvr e)
     (with-syntax ([lve (extract-lv #'lvr)])
       #'(let ([v e])
           ((lvalue-set!-proc lve) v)
           v))]))

(define-syntax (lvalue-pre-update! stx)
  (syntax-case stx (lvalue-reference)
    [(_ lvr proc)
     (with-syntax ([lve (extract-lv #'lvr)])
       #'(let* ([lv lve]
                [v (proc ((lvalue-get-proc lv)))])
           ((lvalue-set!-proc lv) v)
           v))]))

(define-syntax (lvalue-post-update! stx)
  (syntax-case stx (lvalue-reference)
    [(_ lvr proc)
     (with-syntax ([lve (extract-lv #'lvr)])
       #'(let* ([lv lve]
                [v ((lvalue-get-proc lv))])
           ((lvalue-set!-proc lv) (proc v))
           v))]))

(define-syntax (lvalue->pointer stx)
  (syntax-case stx (lvalue-reference)
    [(_ lvr)
     (with-syntax ([lve (extract-lv #'lvr)])
       #'((lvalue-ptr-proc lve)))]))

(define make-variable box)

(define (variable->lvalue v)
  (make-lvalue
   (λ () (unbox v))
   (λ (e) (set-box! v e))
   (λ () (variable->pointer v))))

(define (variable->pointer v)
  (pointer
   (λ (offset)
     (unless (zero? offset)
       (error "illegal derefence of variable pointer with offset:" offset))
     (variable->lvalue v))
   0))

(define-syntax (variable-reference stx)
  (syntax-case stx ()
    [(_ . id)
     #'(lvalue-reference . (variable->lvalue id))]))

; Arrays

(struct array (data element-size))

(define (array->pointer arr [offset 0])
  (pointer
   (λ (offset)
     (make-lvalue
      (λ () (vector-ref (array-data arr) offset))
      (λ (e) (vector-set! (array-data arr) offset e))
      (λ () (array->pointer arr offset))))
   offset))

; Structs

(define (struct-member-lvalue s k)
  (make-lvalue
   (λ () (vector-ref s k))
   (λ (e) (vector-set! s k e))
   (λ () (struct-member-pointer s k))))

(define (struct-member-pointer s k)
  (pointer
   (λ (offset)
     (unless (zero? offset)
       (error "illegal dereference of offset struct member pointer"))
     (struct-member-lvalue s k))
   0))

(define-syntax (struct-member-reference stx)
  (syntax-case stx ()
    [(_ s k) #'(lvalue-reference . (struct-member-lvalue s k))]))

; Unions

; Pointers

(struct pointer (resolve-proc offset))

(define-syntax (pointer-dereference stx)
  (syntax-case stx ()
    [(_ p) #'(lvalue-reference . ((pointer-resolve-proc p) (pointer-offset p)))]))

(define (pointer-inc p n)
  (pointer (pointer-resolve-proc p) (+ (pointer-offset p) n)))

(define (pointer-diff p q)
  (- (pointer-offset p) (pointer-offset q)))
