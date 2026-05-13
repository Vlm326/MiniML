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

module Parser : sig
  type parser = {
    lexbuf : Lexer.Lexer.lexbuf;
    mutable current_token : Lexer.Lexer.token;
  }

  val next : parser -> unit
  val from_string : string -> parser
  val parse : parser -> Ast.expr
  val parse_expr : parser -> Ast.expr
  val expect : Lexer.Lexer.token -> parser -> unit
  val parse_if : parser -> Ast.expr
  val parse_let : parser -> Ast.expr
  val parse_fun : parser -> Ast.expr
  val parse_or : parser -> Ast.expr
  val parse_and : parser -> Ast.expr
  val parse_rel : parser -> Ast.expr
  val parse_add : parser -> Ast.expr
  val parse_mul : parser -> Ast.expr
  val parse_unary : parser -> Ast.expr
  val parse_app : parser -> Ast.expr
  val parse_atom : parser -> Ast.expr
end
