open Ast
open Parser

let rec eval (environment : env) (expression : expr) : value =
  match expression with
  | Int n -> IntValue n
  | Bool b -> BoolValue b
  | Var x -> (
      try List.assoc x environment
      with Not_found -> failwith ("Unbound variable: " ^ x))
  | Fun (name, body) -> FunValue (name, body, environment)
  | Let (x, e1, e2) ->
      let v1 = eval environment e1 in
      eval ((x, v1) :: environment) e2
  | LetRec (f, x, e1, e2) ->
      let rec_closure = RecFunValue (f, x, e1, environment) in
      eval ((f, rec_closure) :: environment) e2
  | If (cond, then_branch, else_branch) -> (
      match eval environment cond with
      | BoolValue true -> eval environment then_branch
      | BoolValue false -> eval environment else_branch
      | _ -> failwith "Condition must be boolin")
  | App (e1, e2) -> (
      match eval environment e1 with
      | FunValue (x, body, env') ->
          let v2 = eval environment e2 in
          eval ((x, v2) :: env') body
      | RecFunValue (f, x, body, env') ->
          let v2 = eval environment e2 in
          eval ((f, RecFunValue (f, x, body, env')) :: (x, v2) :: env') body
      | _ -> failwith "Cannot apply non-function")
  | BinOp (op, e1, e2) ->
      let v1 = eval environment e1 in
      let v2 = eval environment e2 in
      eval_binop op v1 v2
  | UnOp (op, e) ->
      let v = eval environment e in
      eval_unop op v

and eval_binop op v1 v2 =
  match (op, v1, v2) with
  | Add, IntValue n1, IntValue n2 -> IntValue (n1 + n2)
  | Sub, IntValue n1, IntValue n2 -> IntValue (n1 - n2)
  | Mul, IntValue n1, IntValue n2 -> IntValue (n1 * n2)
  | Div, IntValue n1, IntValue n2 ->
      if n2 = 0 then failwith "you cannot divide by zero" else IntValue (n1 / n2)
  | Eq, IntValue n1, IntValue n2 -> BoolValue (n1 = n2)
  | Eq, BoolValue n1, BoolValue n2 -> BoolValue (n1 = n2)
  | Neq, IntValue n1, IntValue n2 -> BoolValue (n1 <> n2)
  | Neq, BoolValue n1, BoolValue n2 -> BoolValue (n1 <> n2)
  | Lt, IntValue n1, IntValue n2 -> BoolValue (n1 < n2)
  | Le, IntValue n1, IntValue n2 -> BoolValue (n1 <= n2)
  | Gt, IntValue n1, IntValue n2 -> BoolValue (n1 > n2)
  | Ge, IntValue n1, IntValue n2 -> BoolValue (n1 >= n2)
  | And, BoolValue n1, BoolValue n2 -> BoolValue (n1 && n2)
  | Or, BoolValue n1, BoolValue n2 -> BoolValue (n1 || n2)
  | _ -> failwith "unreacheble"

and eval_unop op v =
  match (op, v) with
  | Neg, IntValue n -> IntValue (-n)
  | Not, BoolValue b -> BoolValue (not b)
  | _ -> failwith "Type error in unary operation"

let rec value_to_string = function
  | IntValue n -> string_of_int n
  | BoolValue true -> "true"
  | BoolValue false -> "false"
  | FunValue _ -> "<fun>"
  | RecFunValue _ -> "<fun>"

let run code =
  let p = Parser.from_string code in
  let e = Parser.parse p in
  let v = eval [] e in
  value_to_string v
