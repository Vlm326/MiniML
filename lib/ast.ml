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
  (* Рекурсивное связывание функции *)
  | LetRec of string * string * expr * expr
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

(* Значения времени выполнения *)
type value =
  | IntValue of int
  | BoolValue of bool
  (* Замыкание обычной функции *)
  | FunValue of string * expr * env
  (* Замыкание рекурсивной функции *)
  | RecFunValue of string * string * expr * env

(* Окружение: имя -> значение *)
and env = (string * value) list
