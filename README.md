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
