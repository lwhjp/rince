#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/id-set
                     syntax/id-table
                     syntax/parse)
         racket/contract
         racket/match
         racket/set)

(provide (all-defined-out))

; We'll use a really naive approach for now -- possibly an extension of
; racket/unit would be better.

; imports and exports are lists of symbols.
; go is a procedure accepting boxes for (import ... export ...)
; which sets the content of the export boxes.
; FIXME: this will break if toplevel value initializers refer to imports

; can we use linklets?

; possible to use struct/dc?
(define-struct/contract linkable
  ([imports (listof symbol?)]
   [exports (listof symbol?)]
   [go procedure?])) ; dependent arity

;;;
;;; Objects
;;;

(define-struct/contract (linkable:object linkable)
  ())

(define-syntax (extern stx)
  (raise-syntax-error #f "invalid outside of c-object form" stx))

(define-for-syntax (make-box-reference box-id)
  (make-set!-transformer
   (λ (stx)
     (syntax-case stx (set!)
       [(set! id v) #`(set-box! #,box-id v)]
       [id (identifier? #'id) #`(unbox #,box-id)]
       [(id . args) #`((unbox #,box-id) . args)]))))

;; TODO: documentation
(define-syntax (c-object stx)
  (syntax-parse stx
    [(_ form:expr ...)
     (define def-ctx (syntax-local-make-definition-context))
     (define stop-ids (list #'begin #'define-syntaxes #'define-values #'extern))
     (define exp-ctx (list (gensym 'object)))
     (define extern-ids (make-bound-id-table))
     (define defined-ids (mutable-bound-id-set))
     (define expanded-forms
       (let loop ([forms (attribute form)])
         (if (null? forms)
             '()
             (let ([expanded-form (local-expand (car forms) exp-ctx stop-ids def-ctx)])
               (syntax-parse expanded-form
                 #:literals (begin define-syntaxes define-values extern)
                 [(begin sub:expr ...)
                  (loop (append (attribute sub) (cdr forms)))]
                 [(define-syntaxes (id:id ...) expr:expr)
                  (syntax-local-bind-syntaxes (attribute id) #'expr def-ctx)
                  (cons #`(define-syntaxes
                            #,(map syntax-local-identifier-as-binding
                                   (attribute id))
                            expr)
                        (loop (cdr forms)))]
                 [(define-values (id:id ...) expr:expr)
                  (syntax-local-bind-syntaxes (attribute id) #f def-ctx)
                  (for ([id (in-list (attribute id))])
                    (bound-id-set-add! defined-ids id))
                  (cons #`(define-values
                            #,(map syntax-local-identifier-as-binding
                                   (attribute id))
                            expr)
                        (loop (cdr forms)))]
                 [(extern [obj-id:id ext-id:id] ...)
                  (syntax-local-bind-syntaxes (attribute obj-id) #f def-ctx)
                  (for ([obj-id (in-list (attribute obj-id))]
                        [ext-id (in-list (attribute ext-id))])
                    (bound-id-table-set! extern-ids obj-id ext-id))
                  (loop (cdr forms))]
                 [_ (raise-syntax-error #f "expected definition" stx this-syntax)])))))
     (define-values (stx-defs var-defs)
       (let loop ([forms expanded-forms]
                  [stxs '()]
                  [vars '()])
         (if (null? forms)
             (values (reverse stxs) (reverse vars))
             (syntax-case (car forms) (define-syntaxes define-values)
               [(define-syntaxes (id ...) expr)
                (loop (cdr forms) (cons #'[(id ...) expr] stxs) vars)]
               [(define-values (id ...) expr)
                (loop (cdr forms) stxs (cons #'[(id ...) expr] vars))]
               [_ (loop (cdr forms) stxs vars)]))))
     (define-values (import-ids export-ids)
       (let ([extern-ids (immutable-bound-id-set (bound-id-table-keys extern-ids))]
             [defined-ids (immutable-bound-id-set defined-ids)])
         (values (bound-id-set->list (bound-id-set-subtract extern-ids defined-ids))
                 (bound-id-set->list (bound-id-set-intersect defined-ids extern-ids)))))
     ;(printf "externs: ~a\n" (map syntax-e (bound-id-table-keys extern-ids)))
     ;(printf "defines: ~a\n" (bound-id-set-map defined-ids syntax-e))
     (define (extern-id stx) (syntax-e (bound-id-table-ref extern-ids stx)))
     (define import-syms (map extern-id import-ids))
     (define export-syms (map extern-id export-ids))
     (let ([sym (check-duplicates import-syms eq?)])
       (when sym
         (raise-syntax-error #f (format "duplicate import symbol: ~a" sym) stx)))
     (let ([sym (check-duplicates export-syms eq?)])
       (when sym
         (raise-syntax-error #f (format "duplicate export symbol: ~a" sym) stx)))
     (let ([sym (check-duplicates (append import-syms export-syms) eq?)])
       (when sym
         (raise-syntax-error #f (format "symbol appears in both imports and exports: ~a" sym) stx)))
     ;(printf "imports: ~a\n" import-syms)
     ;(printf "exports: ~a\n" export-syms)
     (define object-stx
       (with-syntax ([(import-id ...) import-ids]
                     [(export-id ...) export-ids]
                     [(import-box ...) (generate-temporaries import-ids)]
                     [(export-box ...) (generate-temporaries export-ids)])
         (quasisyntax/loc stx
           (linkable:object
            '#,import-syms
            '#,export-syms
            (lambda (import-box ... export-box ...)
              (letrec-syntaxes+values
                  ([(import-id ...) (values (make-box-reference #'import-box) ...)]
                   #,@stx-defs)
                  (#,@var-defs)
                (set-box! export-box export-id) ...
                (void)))))))
     object-stx]))

;;;
;;; Archives (linked collection of objects)
;;;

(define-struct/contract (linkable:archive linkable)
  ([objects (listof linkable?)]))

(define/contract (link objs [libs '()])
  (->* ((listof linkable?)) ((listof linkable?)) linkable:archive?)
  (define lib-imports (apply set-union (map linkable-imports libs)))
  (define lib-exports (apply set-union (map linkable-exports libs)))
  (let ([unresolved (set-subtract lib-imports lib-exports)])
    (unless (null? unresolved)
      (error 'link "unresolved symbols in libraries: ~a" unresolved)))
  (define obj-exports (apply set-union (map linkable-exports objs)))
  (make-archive (append objs libs) #:exports obj-exports))

(define/contract (make-archive objects #:exports [exports #f])
  (->* ((listof linkable?)) (#:exports (or/c (listof symbol?) #f)) linkable:archive?)
  (define all-imports (apply set-union (map linkable-imports objects)))
  (define all-exports (apply set-union (map linkable-exports objects)))
  (define imports-out (set-subtract all-imports all-exports))
  (define exports-out
    (cond
      [exports (let ([missing (set-subtract exports all-exports)])
                 (if (null? missing)
                     exports
                     (error 'make-archive "specified exports not found: ~a" missing)))]
      [else all-exports]))
  (define internal-symbols
    (set-union (set-subtract all-imports imports-out)
               (set-subtract all-exports exports-out)))
  (linkable:archive
   imports-out
   exports-out
   (λ boxes
     (define ext-symtable
       (make-immutable-hasheq
        (map cons (append imports-out exports-out) boxes)))
     (define symtable
       (for/fold ([symtable ext-symtable])
                 ([sym (in-list internal-symbols)])
         (hash-set symtable sym (box #f))))
     (for ([object (in-list objects)])
       (match-define (linkable imports exports go) object)
       (apply go (map (λ (sym) (hash-ref symtable sym))
                      (append imports exports)))))
   objects))

;;;
;;; Executables (no imports; exports main)
;;;

(define-struct/contract executable
  ([go (unconstrained-domain-> exact-integer?)])
  #:property prop:procedure (struct-field-index go))

(define/contract (link/executable inputs [libs '()])
  (->* ((listof linkable?)) ((listof linkable?)) executable?)
  (linkable->executable (link inputs libs)))

(define/contract (linkable->executable object)
  (-> linkable? executable?)
  ; TODO: atexit, environment etc
  (define imports (linkable-imports object))
  (define exports (linkable-exports object))
  (unless (null? imports)
    (error 'linkable->executable "unresolved symbols: ~a" imports))
  (unless (memq 'main exports)
    (error 'linkable->executable "main not defined"))
  (define (apply/main main args)
    ; TODO: argc, argv
    (cond
      [(procedure-arity-includes? main 2) (main 0 #f)]
      [(procedure-arity-includes? main 0) (main)]
      [else (error "main: invalid arity")]))
  (executable
   (λ args
     (define boxes (map (λ (sym) (cons sym (box #f))) exports))
     (apply (linkable-go object) (map cdr boxes))
     (apply/main (unbox (cdr (assq 'main boxes))) args))))

; TODO: linkable->unit
