type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * string * expr * expr
  | If of expr * expr * expr
  | BinOp of binop * expr * expr
  | UnOp of unop * expr

and binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge | And | Or
and unop = Neg | Not

type value =
  | IntValue of int
  | BoolValue of bool
  | FunValue of string * expr * env
  | RecFunValue of string * string * expr * env

and env = (string * value) list
