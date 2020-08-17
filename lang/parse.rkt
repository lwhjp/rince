#lang racket/base

(require (for-syntax racket/base)
         (for-template (only-in "syntax.rkt" make-→))
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
(define-syntax-parameter this-src #f)

(define-syntax-rule
  (define/match/wrap id
    [(struct: src member ...) body ...] ...)
  (define id
    (let ([id (λ (node)
                (syntax-parameterize ([this-node (make-rename-transformer #'node)])
                  (match node
                    [(struct: src member ...)
                     (syntax-parameterize ([this-src (make-rename-transformer #'src)])
                       body ...)]
                    ...)))])
      (when DEBUG (trace id))
      id)))

(define-syntax-rule
  (quasisyntax/src template)
  (quasisyntax/loc (src->syntax this-src 'here) template))

;; Identifiers

(define (*id node)
  (match node
    [(id:op src name) (src->syntax src name)]
    [else (id->syntax node)]))

;; Expressions

(define/match/wrap *expr
  [(expr:ref src id) (*id id)]
  [(expr:int src value qualifiers) value] ; TODO: qualifiers
  [(expr:float src value qualifiers) value]
  [(expr:char src source wide?) (string-ref source 0)] ; TODO: multi-char constants
  [(expr:string src source wide?) source]
  ; TODO: compound, array-ref
  [(expr:call src function arguments) (quasisyntax/src (#%app #,(*expr function) #,@(map *expr arguments)))]
  [(expr:member src expr label) (quasisyntax/src (|.| #,(*expr expr) #,(*id label)))]
  [(expr:pointer-member src expr label) (quasisyntax/src (-> #,(*expr expr) #,(*id label)))]
  [(expr:postfix src expr op) (let ([op (*id op)]) (quasisyntax/src (#,(format-id op "post~a" op) #,(*expr expr))))]
  [(expr:prefix src op expr) (let ([op (*id op)]) (quasisyntax/src (#,(format-id op "pre~a" op) #,(*expr expr))))]
  [(expr:cast src type expr) (quasisyntax/src (cast #,(*type type) #,(*expr expr)))]
  [(expr:sizeof src term) (quasisyntax/src (sizeof #,(if (type? term) (*type term) (*expr term))))]
  [(expr:unop src op expr) (quasisyntax/src (#,(*id op) #,(*expr expr)))]
  [(expr:binop src left op right) (quasisyntax/src (#,(*id op) #,(*expr left) #,(*expr right)))]
  [(expr:assign src left op right) (quasisyntax/src (#,(*id op) #,(*expr left) #,(*expr right)))]
  [(expr:begin src left right) (quasisyntax/src (|,| #,(*expr left) #,(*expr right)))]
  [(expr:if src test cons alt) (quasisyntax/src (?: #,(*expr test) #,(*expr cons) #,(*expr alt)))])

;; Statements

(define/match/wrap *stmt
  [(stmt:label src label stmt) (quasisyntax/src (begin #:label #,(*id label) #,(*expr stmt)))]
  ; TODO: case, default
  [(stmt:block src items) (quasisyntax/src (block #,@(map (λ (item) (if (decl? item) (*decl item) (*stmt item))) items)))]
  [(stmt:expr src expr) (*expr expr)]
  [(stmt:if src test cons alt) (quasisyntax/src (if #,(*expr test) #,(*stmt cons) #,@(if alt (list (*stmt alt)) '())))]
  ; TODO: switch
  [(stmt:while src test body) (quasisyntax/src (while #,(*expr test) #,(*stmt body)))]
  [(stmt:do src body test) (quasisyntax/src (do #,(*stmt body) #,(*expr test)))]
  [(stmt:for src init test update body) (quasisyntax/src
                                         (for (#,(cond [(expr? init) (*expr init)] [(decl? init) (*decl init)] [else #'()])
                                               #,(if test (*expr test) #'())
                                               #,(if update (*expr update) #'()))
                                           #,(*stmt body)))]
  [(stmt:goto src label) (quasisyntax/src (goto #,(*id label)))]
  [(stmt:continue src) (quasisyntax/src (continue))]
  [(stmt:break src) (quasisyntax/src (break))]
  [(stmt:return src result) (quasisyntax/src (return #,@(if result (list (*expr result)) '())))]
  [(stmt:empty src) (quasisyntax/src (empty-statement))])

;; Declarations

(define current-base-type (make-parameter #f))

(define/match/wrap *decl
  ; TODO: typedef
  [(decl:vars src storage-class type declarators)
   ; TODO: struct declarations can appear in other places too; we'll
   ; probably need to tweak (declare ...) to handle that fully.
   ; TODO: similarly for anonymous structs
   (define struct-defs
     (match type
       [(type:struct src tag (? list? fields))
        (syntax-parameterize ([this-src (make-rename-transformer #'src)])
          (list
           (quasisyntax/src
            (define-struct-type #,(*id tag)
              (#,@(map *decl:member fields))))))]
       [_ '()]))
   (define declarator-stxs
     (for/list ([ctx (in-list declarators)])
       (define dcl (apply-declarator-context ctx type))
       (define dcl-src (decl-src dcl))
       (define dcl-type (decl:declarator-type dcl))
       (define init
         (cond
           [(decl:declarator-initializer dcl) => *init]
           [else #f]))
       (with-syntax ([x (*id (decl:declarator-id dcl))])
         (syntax-parameterize ([this-src (make-rename-transformer #'dcl-src)])
           (cond
             [(type:function? dcl-type)
              (quasisyntax/src
               [#,(*type (type:function-return dcl-type))
                (x #,@(map *decl:formal (type:function-formals dcl-type)))])]
             [init (quasisyntax/src [#,(*type dcl-type) x #,init])]
             [else (quasisyntax/src [#,(*type dcl-type) x])])))))
   (define declaration-stx
     (with-syntax ([(storage-class ...) (if storage-class
                                            (list (*id storage-class))
                                            '())])
       (quasisyntax/src (declare (storage-class ...) #,@declarator-stxs))))
   (if (null? struct-defs)
       declaration-stx
       (quasisyntax/src (begin #,@struct-defs #,declaration-stx)))]
  [(decl:function src storage-class inline? return-type declarator preamble body)
   ; TODO: preamble
   (let ([type (decl:declarator-type
                (if (declarator-context? declarator)
                    (apply-declarator-context declarator return-type)
                    declarator))])
     (quasisyntax/src
       (function #,(map *id (filter values (list storage-class inline?)))
         #,(*type return-type)
         (#,(*id (decl:declarator-id declarator))
          #,@(map (λ (formal)
                    ; formal or ellipsis
                    (if (decl:formal? formal)
                        (*decl:formal formal)
                        (*id formal)))
                  (type:function-formals type)))
         #,(*stmt body))))])

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
     (quasisyntax/src [#,(*type t) #,@(if id (list (*id id)) '())]))])

(define/match/wrap *decl:member
  ; TODO: fully implement this
  [(decl:member src type declarators)
   (match declarators
     [(list (decl:member-declarator _ id #f #f #f))
      (quasisyntax/src [#,(*type type) #,(*id id)])])])

;; Initializers

(define/match/wrap *init
  ; TODO: compound
  [(init:expr src expr) (*expr expr)])

;; Designators

;; Types

(define/match/wrap *type
  [(type:primitive src name)
   (quasisyntax/src
    #,(match name
        [(or 'char 'short 'int 'long) name]
        ['signed 'int]
        ['unsigned '|unsigned int|]
        [(list spec ... (or 'char 'int 'double '_Complex)) (string->symbol (string-join (map symbol->string name)))]
        [(? list?) (string->symbol (string-join (append (map symbol->string name) "int")))]
        [else name]))]
  ; TODO: ref
  [(type:struct src tag #f) #| TODO: inline struct definitions |# (quasisyntax/src (Struct #,(*id tag)))]
  ; TODO: union, enum, array
  [(type:pointer src base qualifiers) #| TODO: qualifiers |# (quasisyntax/src (Pointer #,(*type base)))]
  [(type:function src return formals)
   ; TODO: formal storage classes
   (let ([formal-decls (map *decl:formal formals)])
     (quasisyntax/src #,(make-→ (*type return) formal-decls)))]
  [(type:qualified src type qualifiers) #| TODO |# (*type type)])
