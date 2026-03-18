(lang dune 3.14)

(name MiniML)

(generate_opam_files true)

(source
 (github Vlm326/MiniML))

(authors "Vladislav <morozvv75@gmail.com>")

(maintainers "Vladislav <morozvv75@gmail.com>")

(license "Apache-2.0")

(documentation "https://github.com/Vlm326/MiniML#readme")

(package
 (name MiniML)
 (synopsis "A small MiniML interpreter in OCaml")
 (description "Lexer, parser and evaluator for a tiny ML-like language")
 (depends
  ocaml
  (alcotest :with-test))
 (tags
  ("ocaml" "interpreter" "miniml" "compiler-construction")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
