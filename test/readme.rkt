#lang racket/base

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

(module+ test
  (require rackunit)
  (define obj (compile src))
  (define proc (linkable->executable obj))
  (check-equal? (proc) 4))
