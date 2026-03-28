# MiniML

[Русская версия](./README.md)

[![CI](https://github.com/Vlm326/MiniML/actions/workflows/ci.yml/badge.svg)](https://github.com/Vlm326/MiniML/actions/workflows/ci.yml)
![OCaml](https://img.shields.io/badge/OCaml-4.14%20%7C%205.2-fc7a03)
![Dune](https://img.shields.io/badge/build-Dune-8d5fd3)
![Formatter](https://img.shields.io/badge/formatter-ocamlformat-2a5ada)
![License](https://img.shields.io/badge/license-Apache%202.0-green)
[![Stars](https://img.shields.io/github/stars/Vlm326/MiniML?style=flat)](https://github.com/Vlm326/MiniML/stargazers)
[![Forks](https://img.shields.io/github/forks/Vlm326/MiniML?style=flat)](https://github.com/Vlm326/MiniML/network/members)
[![Issues](https://img.shields.io/github/issues/Vlm326/MiniML)](https://github.com/Vlm326/MiniML/issues)
[![Last commit](https://img.shields.io/github/last-commit/Vlm326/MiniML)](https://github.com/Vlm326/MiniML/commits/main)

MiniML is a small OCaml study project: an interpreter for a compact ML-like language with its own AST, lexer, parser, evaluator, and command-line interface.

The project currently includes:
- integers and booleans
- arithmetic, logical, and comparison operators
- `if ... then ... else`
- `let` and `let rec`
- anonymous functions and function application
- REPL mode and file execution

## Example

Example program:

```ml
let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
in
fact 5
```

Result:

```text
120
```

REPL example:

```text
> let add = fun x -> fun y -> x + y in add 3 4;;
7
```

## Build And Run

Requirements:
- OCaml
- Dune
- opam

Build:

```bash
dune build
```

Run the REPL:

```bash
dune exec MiniML
```

Run a program from a file:

```bash
dune exec MiniML -- path/to/program.ml
```

Run tests:

```bash
dune runtest
```

## Contributing

If you want to help the project, you can:
- open an issue with a bug report or idea
- submit a pull request with a focused change
- add tests or improve documentation

## License

The project is distributed under the Apache License 2.0.

## Tooling

Continuous Integration is provided by GitHub Actions. Formatting is checked with `ocamlformat`.
