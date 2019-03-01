(Roughly in descending order of priority)

- C code must be safe: unsafe operations must only be used
when the compiler can prove they are safe.

- Must support conforming programs.

- Should (optionally) support non-conforming real-world code where reasonably possible.

- Should be configurable (size of types; arithmetic; anything implementation-defined) - an "evil C" mode with unusual parameters would catch a lot of portability issues.

- Should (optionally) catch undefined behavior and warn/fail at runtime.

- Should interoperate with racket code (either using racket
types or marshalling/contracts).

- Should perform reasonably well - but we're clearly not going to get close to native binaries.
