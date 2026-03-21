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
