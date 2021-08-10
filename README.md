# RINCE

### Racket-INtegrated C Environment

(or Rince Is Not a C Emulator)

This is an (in-progress) implementation of C semantics for Racket, comprising
a runtime library and C->Racket compiler to allow running C programs on top
of Racket (no FFI!).

There is a simple `#lang rince/c` for standalone programs.

Rince is not a static analyzer, but it could serve as a tool
to instrument C programs at runtime, in a similar manner to valgrind.

##### Getting started

Still very much a work-in-progress. Much is missing - you won't be able
to run anything but toy examples for now.

Try this:

```racket
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
```

`(proc)` should return `4`, the length of `"test"`.

##### Running a standalone C program

Install this folder as a package:

    raco pkg install -n rince .

Now you can use `raco run-c` to compile and execute a standalone C program:

    raco run-c tests/ok/puts.c
