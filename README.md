# Commodore 64 BASIC interpreter

An interpreter for **Commodore 64 BASIC V2**, written in Clojure in a purely functional style. It runs classic line-numbered BASIC programs from an interactive prompt.

Built for *Lenguajes Formales* (FIUBA, University of Buenos Aires), 2023. The course provided the interpreter's skeleton: the REPL loop, the shunting-yard evaluator and file loading. My work was implementing the core functions it relies on.

## What I implemented

- **Lexical validation:** reserved words, operators, rejecting invalid tokens, and typing variables as float, integer (`%`) or string (`$`).
- **Program loading:** inserting and replacing numbered lines, expanding multi-variable `NEXT`, counting statements, and finding where execution continues after `GOTO`/`GOSUB`/`RETURN`.
- **Execution:** assignments, reading `DATA`, and BASIC-style error messages.
- **Expressions:** preprocessing (variable substitution), telling unary from binary `+`/`-` and the 2- and 3-argument `MID$`, operator precedence and arity, and C64-style number formatting.
- A test suite of 21 test groups and 83 assertions.

## Running it

Requires [Leiningen](https://leiningen.org/).

```sh
lein run     # starts the BASIC prompt
lein test    # runs the test suite
```

Sample programs are included: `FIBO.BAS`, `PRIMES.BAS`, `GCD.BAS`, `BIN-DEC.BAS`, `HEX.BAS`, `SINE.BAS`, `STARS.BAS`, `NATO.BAS`, `NAME.BAS`, `SUM.BAS`.
