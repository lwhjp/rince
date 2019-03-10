#lang racket/base

(require (for-syntax racket/base)
         (for-template "syntax.rkt")
         ; for-template doesn't seem to pick up #%app properly
         (prefix-in c: "syntax.rkt")
         racket/match
         racket/string
         racket/stxparam
         racket/syntax
         racket/trace
         c/ast
         c/parse)

(provide c->racket)

(define DEBUG #f)

; TODO: preprocessor directives (need to hack it until c-utils supports them)
(define (c->racket in [source-name #f])
  (*program (parse-program in #:source source-name)))

(when DEBUG (trace c->racket))

(define *program
  (match-lambda
    [(list decls ...)
     #`(translation-unit
        #,@(map *decl decls))]))

(define-syntax-parameter this-node #f)

(define-syntax-rule
  (define/match/wrap id
    [(struct: src member ...) stx-expr] ...)
  (define id
    (let ([id (λ (node)
                (syntax-parameterize ([this-node (make-rename-transformer #'node)])
                  (match node
                    [(struct: src member ...) (src->syntax src stx-expr)] ...)))])
      (when DEBUG (trace id))
      id)))

;; Identifiers

(define (*id node)
  (match node
    [(id:op src name) (src->syntax src name)]
    [else (id->syntax node)]))

;; Expressions

(define/match/wrap *expr
  [(expr:ref src id) (*id id)]
  [(expr:int src value qualifiers) value] ; TODO: qualifiers
  [(expr:float src value qualifiers) values]
  [(expr:char src source wide?) (string-ref source 0)] ; TODO: multi-char constants
  [(expr:string src source wide?) source]
  ; TODO: compound, array-ref
  [(expr:call src function arguments) #`(c:#%app #,(*expr function) #,@(map *expr arguments))]
  ; TODO: member, pointer-member
  [(expr:postfix src expr op) (let ([op (*id op)]) #`(#,(format-id op "post~a" op) #,(*expr expr)))]
  [(expr:prefix src op expr) (let ([op (*id op)]) #`(#,(format-id op "pre~a" op) #,(*expr expr)))]
  [(expr:cast src type expr) #`(cast #,(*type type) #,(*expr expr))]
  [(expr:sizeof src term) #`(sizeof #,(if (type? term) (*type term) (*expr term)))]
  [(expr:unop src op expr) #`(#,(*id op) #,(*expr expr))]
  [(expr:binop src left op right) #`(#,(*id op) #,(*expr left) #,(*expr right))]
  [(expr:assign src left op right) #`(#,(*id op) #,(*expr left) #,(*expr right))]
  [(expr:begin src left right) #`(|,| #,(*expr left) #,(*expr right))]
  [(expr:if src test cons alt) #`(?: #,(*expr test) #,(*expr cons) #,(*expr alt))])

;; Statements

(define/match/wrap *stmt
  [(stmt:label src label stmt) #`(begin #:label #,(*id label) #,(*expr stmt))]
  ; TODO: case, default
  [(stmt:block src items) #`(block #,@(map (λ (item) (if (decl? item) (*decl item) (*stmt item))) items))]
  [(stmt:expr src expr) (*expr expr)]
  [(stmt:if src test cons alt) #`(if #,(*expr test) #,(*stmt cons) #,@(if alt (list (*stmt alt)) '()))]
  ; TODO: switch
  [(stmt:while src test body) #`(while #,(*expr test) #,(*stmt body))]
  [(stmt:do src body test) #`(do #,(*stmt body) #,(*expr test))]
  [(stmt:for src init test update body) #`(for (#,(cond [(expr? init) (*expr init)] [(decl? init) (*decl init)] [else #'()])
                                                #,(if test (*expr test) #'())
                                                #,(if update (*expr update) #'()))
                                            #,(*stmt body))]
  [(stmt:goto src label) #`(goto #,(*id label))]
  [(stmt:continue src) #'(continue)]
  [(stmt:break src) #'(break)]
  [(stmt:return src result) #`(return #,@(if result (list (*expr result)) '()))]
  [(stmt:empty src) #`(empty-statement)])

;; Declarations

(define current-base-type (make-parameter #f))

(define/match/wrap *decl
  ; TODO: typedef
  [(decl:vars src storage-class type declarators)
   #`(declare #,(if storage-class
                    (list (*id storage-class))
                    '())
       #,@(for/list ([ctx (in-list declarators)])
            (define dcl (apply-declarator-context ctx type))
            (define dcl-type (decl:declarator-type dcl))
            (define init
              (cond
                [(decl:declarator-initializer dcl) => *init]
                [else #f]))
            (with-syntax ([x (*id (decl:declarator-id dcl))])
              (cond
                [(type:function? dcl-type)
                 #`[#,(*type (type:function-return dcl-type))
                    (x #,@(map *decl:formal (type:function-formals dcl-type)))]]
                [init #`[#,(*type dcl-type) x #,init]]
                [else #`[#,(*type dcl-type) x]]))))]
  [(decl:function src storage-class inline? return-type declarator preamble body)
   ; TODO: preamble
   (let ([type (decl:declarator-type
                (if (declarator-context? declarator)
                    (apply-declarator-context declarator return-type)
                    declarator))])
     #`(function #,(map *id (filter values (list storage-class inline?)))
         #,(*type return-type)
         (#,(*id (decl:declarator-id declarator))
          #,@(match (type:function-formals type)
               ['() (error "TODO: unspecified formals")]
               [(list formals ...)
                (map (λ (formal)
                       ; formal or ellipsis
                       (if (decl:formal? formal)
                           (*decl:formal formal)
                           (*id formal)))
                     formals)]))
         #,(*stmt body)))])

(define/match/wrap *decl:formal
  [(decl:formal src storage-class type declarator)
   ; TODO: storage class
   (let ([t (if (declarator-context? declarator)
                (decl:declarator-type
                 (apply-declarator-context declarator type))
                (apply-type-context declarator type))]
         [id (if (decl:declarator? declarator)
                 (decl:declarator-id declarator)
                 #f)])
     #`[#,(*type t) #,@(if id (list (*id id)) '())])])

;; Initializers

(define/match/wrap *init
  ; TODO: compound
  [(init:expr src expr) (*expr expr)])

;; Designators

;; Types

(define/match/wrap *type
  [(type:primitive src name)
   (match name
     [(or 'char 'short 'int 'long) (string->symbol (format "signed ~a" name))]
     ['signed '|signed int|]
     ['unsigned '|unsigned int|]
     [(list spec ... (or 'char 'int 'double '_Complex)) (string->symbol (string-join (map symbol->string name)))]
     [(? list?) (string->symbol (string-join (append (map symbol->string name) "int")))]
     [else name])]
  ; TODO: ref, struct, union, enum, array
  [(type:pointer src base qualifiers) #| TODO: qualifiers |# #`(Pointer #,(*type base))]
  [(type:function src return formals)
   ; TODO: formal storage classes
   (let ([formal-decls (match formals
                        ['() (error "TODO: unspecified formals")]
                        [(list formals ...) (map *decl:formal formals)])])
     (make-→ (*type return) formal-decls))]
  [(type:qualified src type qualifiers) #| TODO |# (*type type)])
