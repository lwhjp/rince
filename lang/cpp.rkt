#lang racket/base

(require (for-template "../lib.rkt"
                       (rename-in "../lib/stdio.rkt"
                                  [c-prototypes stdio]))
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/list
         racket/match
         racket/port
         racket/string)

(provide preprocess)

(define-namespace-anchor ns-here)

(define get-protos
  (let ([ns (namespace-anchor->namespace ns-here)])
    (λ (lib)
      (parameterize ([current-namespace ns])
        (namespace-variable-value lib)))))

(define-tokens preprocessing-tokens
  (header-name identifier pp-number character-constant string-literal punctuator whitespace other-char))

(define-lex-abbrevs
  [header-name (:or (:: #\< (:+ (:~ #\newline #\>)) #\>)
                    (:: #\" (:+ (:~ #\newline #\")) #\"))]
  [identifier (:: identifier-nondigit (:* (:or identifier-nondigit digit)))]
  [identifier-nondigit (:or nondigit universal-character-name)]
  [nondigit (:or #\_ (:/ #\a #\z #\A #\Z))]
  [digit (:/ #\0 #\9)]
  [universal-character-name (:: #\\ (:or #\u #\U) (:** 1 2) (:= 4 hexadecimal-digit))]
  [octal-digit (:/ #\0 #\7)]
  [hexadecimal-digit (:/ #\0 #\9 #\a #\f #\A #\F)]
  [pp-number (:: (:or digit (:: #\. digit))
                 (:* (:or digit identifier-nondigit (:: (char-set "eEpP") (char-set "+-")) #\.)))]
  [character-constant (:: (:? #\L) #\' c-char-sequence #\')]
  [c-char-sequence (:+ c-char)]
  [c-char (:or (:~ #\' #\\ #\newline) escape-sequence)]
  [escape-sequence (:or (:: #\\ (char-set "'\"?\\abfnrtv"))
                        (:: #\\ (:** 1 3 octal-digit))
                        (:: #\\ (:+ hexadecimal-digit))
                        universal-character-name)]
  [string-literal (:: (:? #\L) #\" s-char-sequence #\")]
  [s-char-sequence (:+ s-char)]
  [s-char (:or (:~ #\" #\\ #\newline) escape-sequence)]
  [punctuator (:or (char-set "[](){}.&*+-~!/%<>^|?:;=,#")
                   "->" "++" "--" "<<" ">>" "<=" ">=" "==" "!=" "&&" "||"
                   "..." "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "^=" "|="
                   "##" "<:" ":>" "<%" "%>" "%:" "%:%:")])

(define cpp-lex
  (lexer
   [(eof) #f]
   [header-name (token-header-name lexeme)]
   [identifier (token-identifier lexeme)]
   [pp-number (token-pp-number lexeme)]
   [character-constant (token-character-constant lexeme)]
   [string-literal (token-string-literal lexeme)]
   [punctuator (token-punctuator lexeme)]
   [(:+ whitespace) (token-whitespace lexeme)]
   [any-char (token-other-char lexeme)]))

(define (string->tokens str)
  (call-with-input-string str
   (λ (in)
     (let loop ()
       (cond
         [(cpp-lex in) => (λ (t) (cons (token-value t) (loop)))]
         [else '()])))))

(define (skip-ws-until tok toks)
  (cond
    [(null? toks) '()]
    [(equal? tok (car toks)) toks]
    [(regexp-match-exact? #px#"\\s+" (car toks)) (cdr toks)]
    [else (cons (car toks) (skip-ws-until tok (cdr toks)))]))

(define (skip-ws toks) (skip-ws-until #f toks))

(define (preprocess in)
  ;; TODO: preprocess properly (including position)
  (define src-lines (call-with-input-file in port->lines))
  (define macros (make-hash))
  (define-values (out-lines include-decls)
    (for/fold ([out-lines '()]
               [out-decls '()]
               #:result (values (reverse out-lines) (reverse out-decls)))
              ([l src-lines])
      (cond
        [(regexp-match #px"^\\s*#\\s*include\\s*<([^>]+).h>" l)
         => (λ (m)
              (define protos (get-protos (string->symbol (cadr m))))
              (values out-lines
                      (append (reverse (map proto->decl protos)) out-decls)))]
        [(regexp-match #px"^\\s*#\\s*define\\s*(.*)" l)
         => (λ (m)
              (match (skip-ws (string->tokens (cadr m)))
                [(list id "(" rest ...)
                 (define-values (arg-ids es)
                   (let loop ([toks rest]
                              [arg-ids '()])
                     (case (car toks)
                       [(")") (values (reverse arg-ids) (cdr toks))]
                       [(",") (loop (cdr toks) arg-ids)]
                       [else (loop (cdr toks) (cons (car toks) arg-ids))])))
                 (hash-set! macros id (make-apply-macro arg-ids es))]
                [(list id es ...)
                 (hash-set! macros id (λ (toks) (append es (cdr toks))))]
                [_ (error "invalid #define")])
              (values out-lines out-decls))]
        [(regexp-match? #px"^\\s*#" l)
         (values (cons "//" out-lines) out-decls)]
        [else
         (values (cons l out-lines) out-decls)])))
  (values
   (open-input-string (apply-macros macros (string-join out-lines "\n")))
   include-decls))

(define (apply-macros macros code)
  (define out-toks
    (let loop ([toks (string->tokens code)])
      (cond
        [(null? toks) '()]
        [(hash-ref macros (car toks) #f) => (λ (m) (loop (m toks)))]
        [else (cons (car toks) (loop (cdr toks)))])))
  (string-append* out-toks))

(define (make-apply-macro arg-ids macro-es)
  (λ (toks)
    (let/ec return
      (define (abort) (return toks))
      (define-values (arg-es rest)
        (let next-arg ([arg-es '()]
                       [toks (cddr toks)])
          (let next-tok ([arg-toks '()]
                         [paren 0]
                         [toks toks])
            (match (car toks)
              ["(" (next-tok (cons (car toks) arg-toks) (add1 paren) (cdr toks))]
              [")" (if (zero? paren)
                         (values (reverse (cons (reverse arg-toks) arg-es)) (cdr toks))
                         (next-tok (cons (car toks) arg-toks) (sub1 paren) (cdr toks)))]
              ["," #:when (zero? paren) (next-arg (cons (reverse arg-toks) arg-es) (cdr toks))]
              [t (next-tok (cons t arg-toks) paren (cdr toks))]))))
      (unless (= (length arg-ids) (length arg-es)) (abort))
      (let ([subs (make-hash (map cons arg-ids arg-es))])
        (append
         (append*
          (map (λ (t)
                 (hash-ref subs t (λ () (list t))))
               macro-es))
         rest)))))
