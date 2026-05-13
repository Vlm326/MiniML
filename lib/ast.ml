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

(* Синтаксическое дерево выражений MiniML *)
type expr =
  (* Целое число *)
  | Int of int
  (* Булево значение *)
  | Bool of bool
  (* Переменная *)
  | Var of string
  (* Функция: параметр и тело *)
  | Fun of string * expr
  (* Применение функции к аргументу *)
  | App of expr * expr
  (* Локальное связывание *)
  | Let of string * expr * expr
  (* Рекурсивное связывание *)
  | LetRec of string * expr * expr
  (* Условное выражение *)
  | If of expr * expr * expr
  (* Бинарная операция *)
  | BinOp of binop * expr * expr
  (* Унарная операция *)
  | UnOp of unop * expr

(* Бинарные операции *)
and binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or

(* Унарные операции *)
and unop = Neg | Not

(* Состояние вычисления рекурсивного связывания *)
type rec_state = Unevaluated | Evaluating | Evaluated of value

(* Значения времени выполнения *)
and value =
  | IntValue of int
  | BoolValue of bool
  (* Замыкание обычной функции *)
  | FunValue of string * expr * env
  (* Рекурсивное связывание, вычисляемое по требованию *)
  | RecValue of string * expr * env ref * rec_state ref

(* Окружение: имя -> значение *)
and env = (string * value) list
