(*
 * Copyright 2026 Vladislav <morozvv75@gmail.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

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
