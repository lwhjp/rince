#lang scribble/manual

@require[(for-label rince/lang/syntax)]

@title{Internals}

This section describes the internals of Rince as a reference for
developers. The interfaces here may change at any time. Do not use
these in external code.

@section{Parenthetical C}

@declare-exporting[rince/lang/syntax]
The parser transforms C syntax into the S-expressions described here.

@defform[#:literals (=)
         (declare maybe-storage-class base-type decl ...)
         #:grammar
         [(maybe-storage-class (code:line) #:extern #:static #:auto #:register)
          (decl declarator (code:line declarator #,(tt "=") initializer))]]{
 Declares type tags and variables. A declarator is a type expression
 with the variable identifier in place of the base type.

 For example:
 @tabular[#:sep @hspace[1]
 (list (list @bold{C} @bold{Racket})
       (list @tt|{extern int x, y = 1;}|
             @racket[(declare #:extern int x y #,(tt "=") 1)])
       (list @tt|{int * const p;}|
             @racket[(declare int (const (ptr p)))])
       (list @tt|{int (*f)(int);}|
             @racket[(declare int (ptr (function f ([int]))))])
       (list @tt|{struct { int x; } s;}|
             @racket[(declare (struct ([int x])) s)])
       (list @tt|{struct foo { int x; };}|
             @racket[(declare (struct foo ([int x])))])
       (list @tt|{struct foo;}|
             @racket[(declare (struct foo))])
       (list @tt|{int f(void);}|
             @racket[(declare int (function f ([void])))]))
 ]
}

@defform*[((function ret-type args)
           (function maybe-storage-class ret-type id args body))
          #:grammar
          [(args (arg ...) (arg ... ...))
           (arg [type] [base-type declarator])]]{
 The first form specifies a function type or forms part of a
 declarator.

 The second form defines a function.

 As in C, an @emph{unspecified} argument list is @racket[()], and an
 @emph{empty} argument list is @racket[([void])]. Unspecified
 trailing arguments are represented by @racket[...] at the end
 of the list.
}

@defform*[((struct maybe-tag (member-decl ...))
           (struct tag))
          #:grammar
          [(member-decl [base-type declarator ...+])]]{
 Specifies a struct type.

 The first form defines a (possibly anonymous) struct type.

 The second form refers to a previously defined struct type, or (as
 the base type in a declaration) declares a new struct tag.
}
