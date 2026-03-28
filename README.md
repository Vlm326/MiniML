# MiniML

[English README](./README.en.md)

[![CI](https://github.com/Vlm326/MiniML/actions/workflows/ci.yml/badge.svg)](https://github.com/Vlm326/MiniML/actions/workflows/ci.yml)
![OCaml](https://img.shields.io/badge/OCaml-4.14%20%7C%205.2-fc7a03)
![Dune](https://img.shields.io/badge/build-Dune-8d5fd3)
![Formatter](https://img.shields.io/badge/formatter-ocamlformat-2a5ada)
![License](https://img.shields.io/badge/license-Apache%202.0-green)
[![Stars](https://img.shields.io/github/stars/Vlm326/MiniML?style=flat)](https://github.com/Vlm326/MiniML/stargazers)
[![Forks](https://img.shields.io/github/forks/Vlm326/MiniML?style=flat)](https://github.com/Vlm326/MiniML/network/members)
[![Issues](https://img.shields.io/github/issues/Vlm326/MiniML)](https://github.com/Vlm326/MiniML/issues)
[![Last commit](https://img.shields.io/github/last-commit/Vlm326/MiniML)](https://github.com/Vlm326/MiniML/commits/main)

MiniML - это небольшой учебный проект на OCaml: интерпретатор компактного ML-подобного языка со своим AST, лексером, парсером, вычислителем и CLI.

Сейчас в проекте есть:
- целые числа и булевы значения
- арифметические, логические и сравнительные операции
- `if ... then ... else`
- `let` и `let rec`
- анонимные функции и применение функций
- REPL и запуск программы из файла

## Грамматика языка

Ниже приведена BNF-грамматика MiniML, соответствующая текущей реализации парсера:

```bnf
<program> ::= <expr>

<expr> ::= <if-expr>
         | <let-expr>
         | <fun-expr>
         | <or-expr>

<if-expr> ::= "if" <expr> "then" <expr> "else" <expr>

<let-expr> ::= "let" <ident> "=" <expr> "in" <expr>
             | "let" "rec" <ident> <ident> "=" <expr> "in" <expr>
             | "let" "rec" <ident> "=" "fun" <ident> "->" <expr> "in" <expr>

<fun-expr> ::= "fun" <ident> "->" <expr>

<or-expr> ::= <and-expr>
            | <and-expr> "||" <or-expr>

<and-expr> ::= <rel-expr>
             | <rel-expr> "&&" <and-expr>

<rel-expr> ::= <add-expr>
             | <add-expr> "=" <add-expr>
             | <add-expr> "<>" <add-expr>
             | <add-expr> "<" <add-expr>
             | <add-expr> "<=" <add-expr>
             | <add-expr> ">" <add-expr>
             | <add-expr> ">=" <add-expr>

<add-expr> ::= <mul-expr>
             | <add-expr> "+" <mul-expr>
             | <add-expr> "-" <mul-expr>

<mul-expr> ::= <unary-expr>
             | <mul-expr> "*" <unary-expr>
             | <mul-expr> "/" <unary-expr>

<unary-expr> ::= "-" <unary-expr>
               | "not" <unary-expr>
               | <app-expr>

<app-expr> ::= <atom>
             | <app-expr> <app-arg>

<app-arg> ::= <atom>
            | <fun-expr>
            | <if-expr>
            | <let-expr>

<atom> ::= <int>
         | "true"
         | "false"
         | <ident>
         | "(" <expr> ")"
```

Лексические элементы:

```bnf
<ident> ::= letter { letter | digit | "_" }
<int> ::= digit { digit }
```

Приоритет операций, от более высокого к более низкому:
- применение функции
- унарные `-` и `not`
- `*` и `/`
- `+` и `-`
- операции сравнения
- `&&`
- `||`

## Пример использования

Пример программы:

```ml
let rec fact n =
  if n = 0 then 1 else n * fact (n - 1)
in
fact 5
```

Результат:

```text
120
```

Пример в REPL:

```text
> let add = fun x -> fun y -> x + y in add 3 4;;
7
```

## Сборка и запуск

Требования:
- OCaml
- Dune
- opam

Сборка:

```bash
dune build
```

Запуск REPL:

```bash
dune exec MiniML
```

Запуск программы из файла:

```bash
dune exec MiniML -- path/to/program.ml
```

Запуск тестов:

```bash
dune runtest
```

## Как помочь проекту

Если хочешь помочь проекту, можно:
- открыть issue с описанием бага или идеи
- прислать pull request с небольшим сфокусированным изменением
- добавить тесты и улучшить документацию

## Лицензия

Проект распространяется под лицензией Apache License 2.0.

## Инструменты

Для Continuous Integration используется GitHub Actions. Форматирование проверяется через `ocamlformat`.
