module Lexer : sig
  type token =
    | INT of int
    | BOOL of bool
    | IDENT of string
    | TRUE
    | FALSE
    | IF
    | THEN
    | ELSE
    | LET
    | REC
    | IN
    | FUN
    | PLUS
    | MINUS
    | STAR
    | SLASH
    | LPAREN
    | RPAREN
    | EQ
    | NEQ
    | LT
    | LE
    | GT
    | GE
    | AND
    | OR
    | NOT
    | ARROW
    | EOF

  type lexbuf = { mutable pos : int; len : int; buf : string }

  val from_string : string -> lexbuf
  val peek : lexbuf -> char option
  val get : lexbuf -> char option
  val skip_whitespaces : lexbuf -> unit
  val read_int : lexbuf -> int
  val read_ident : lexbuf -> string
  val keywords : string -> token
  val next_token : lexbuf -> token
end
