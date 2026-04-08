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

type rec_state =
  | Unevaluated
  | Evaluating
  | Evaluated of value

and value =
  | IntValue of int
  | BoolValue of bool
  | FunValue of string * expr * env
  | RecValue of string * expr * env ref * rec_state ref

and env = (string * value) list
