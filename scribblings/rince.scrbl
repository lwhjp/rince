#lang scribble/manual
@title{Rince: Racket-INtegrated C Environment}

(or Rince Is Not a C Emulator)

This is an (in-progress) implementation of C semantics for Racket, comprising
a runtime library and C->Racket compiler to allow running C programs on top
of Racket (no FFI!).

@table-of-contents[]

@section{Overview}

@(require (for-label (except-in racket/base compile)
                     rince/compile
                     rince/link))

There is a simple @racketmodfont{#lang} @racketmodname[rince/c] for standalone programs.

Rince is not a static analyzer, but it could serve as a tool
to instrument C programs at runtime, in a similar manner to valgrind.

@subsection{Getting started}

Still very much a work-in-progress. Much is missing---you won't be able
to run anything but toy examples for now.

Try this:

@codeblock{
#lang racket
(require rince/compile rince/link)

(define src #<<END
int main(int argc, char **argv)
{
    const char *str = "test";
    const char *p = str;
    while (*p) ++p;
    return p - str;
}
END
)

(define obj (compile src))

(define proc (linkable->executable obj))
}

@racket[(proc)] should return @racket[4], the length of @tt{"test"}.

@subsection{Running a standalone C program}

You can use @tt{raco run-c} to compile and execute a standalone C program:

@verbatim[#:indent 4]|{raco run-c tests/ok/puts.c}|

@include-section["internal.scrbl"]
