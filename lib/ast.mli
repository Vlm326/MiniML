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

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | If of expr * expr * expr
  | BinOp of binop * expr * expr
  | UnOp of unop * expr

and binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
and unop = Neg | Not

type rec_state = Unevaluated | Evaluating | Evaluated of value

and value =
  | IntValue of int
  | BoolValue of bool
  | FunValue of string * expr * env
  | RecValue of string * expr * env ref * rec_state ref

and env = (string * value) list
