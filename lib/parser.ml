module Parser = struct
  open Ast
  open Lexer

  type parser = { lexbuf : Lexer.lexbuf; mutable current_token : Lexer.token }

  let next parser = parser.current_token <- Lexer.next_token parser.lexbuf
  let from_string s = { lexbuf = Lexer.from_string s; current_token = EOF }

  let rec parse parser =
    next parser;
    parse_expr parser

  and parse_expr parser = parse_if parser

  and expect t parser =
    if parser.current_token = t then next parser
    else
      failwith
        (Printf.sprintf "Expected token: "
        ^
        match t with
        | Lexer.INT _ -> "INT"
        | Lexer.BOOL _ -> "BOOL"
        | Lexer.IDENT _ -> "IDENT"
        | Lexer.TRUE -> "TRUE"
        | Lexer.FALSE -> "FALSE"
        | Lexer.IF -> "IF"
        | Lexer.THEN -> "THEN"
        | Lexer.ELSE -> "ELSE"
        | Lexer.LET -> "LET"
        | Lexer.REC -> "REC"
        | Lexer.IN -> "IN"
        | Lexer.FUN -> "FUN"
        | Lexer.PLUS -> "PLUS"
        | Lexer.MINUS -> "MINUS"
        | Lexer.STAR -> "STAR"
        | Lexer.SLASH -> "SLASH"
        | Lexer.LPAREN -> "LPAREN"
        | Lexer.RPAREN -> "RPAREN"
        | Lexer.EQ -> "EQ"
        | Lexer.NEQ -> "NEQ"
        | Lexer.LT -> "LT"
        | Lexer.LE -> "LE"
        | Lexer.GT -> "GT"
        | Lexer.GE -> "GE"
        | Lexer.AND -> "AND"
        | Lexer.OR -> "OR"
        | Lexer.NOT -> "NOT"
        | Lexer.ARROW -> "ARROW"
        | Lexer.EOF -> "EOF")

  and parse_if parser =
    match parser.current_token with
    | Lexer.IF ->
        next parser;
        let cond = parse_expr parser in
        expect Lexer.THEN parser;
        let then_branch = parse_expr parser in
        expect Lexer.ELSE parser;
        let else_branch = parse_expr parser in
        If (cond, then_branch, else_branch)
    | _ -> parse_let parser

  and parse_let parser =
    match parser.current_token with
    | Lexer.LET -> (
        next parser;
        match parser.current_token with
        | Lexer.REC -> (
            next parser;
            let name =
              match parser.current_token with
              | Lexer.IDENT s ->
                  next parser;
                  s
              | _ -> failwith "Expected IDENT"
            in
            match parser.current_token with
            | Lexer.EQ -> (
                next parser;
                match parser.current_token with
                | Lexer.FUN -> (
                    next parser;
                    match parser.current_token with
                    | Lexer.IDENT param ->
                        next parser;
                        expect Lexer.ARROW parser;
                        let body = parse_expr parser in
                        expect Lexer.IN parser;
                        let rest = parse_expr parser in
                        LetRec (name, param, body, rest)
                    | _ -> failwith "Expected parameter in let rec")
                | _ ->
                    let body = parse_expr parser in
                    expect Lexer.IN parser;
                    let rest = parse_expr parser in
                    LetRec (name, name, body, rest))
            | _ ->
                let param =
                  match parser.current_token with
                  | Lexer.IDENT s ->
                      next parser;
                      s
                  | _ -> failwith "Expected parameter in let rec"
                in
                expect Lexer.EQ parser;
                let body = parse_expr parser in
                expect Lexer.IN parser;
                let rest = parse_expr parser in
                LetRec (name, param, body, rest))
        | Lexer.IDENT name ->
            next parser;
            expect Lexer.EQ parser;
            let value = parse_expr parser in
            expect Lexer.IN parser;
            Let (name, value, parse_expr parser)
        | _ -> failwith "Expected IDENT or REC")
    | _ -> parse_fun parser

  and parse_fun parser =
    match parser.current_token with
    | Lexer.FUN -> (
        next parser;
        match parser.current_token with
        | Lexer.IDENT x ->
            next parser;
            expect Lexer.ARROW parser;
            let body = parse_expr parser in
            Fun (x, body)
        | _ -> failwith "Expected parameter in fun")
    | _ -> parse_or parser

  and parse_or parser =
    let e = parse_and parser in
    match parser.current_token with
    | OR ->
        next parser;
        BinOp (Or, e, parse_or parser)
    | _ -> e

  and parse_and parser =
    let e = parse_rel parser in
    match parser.current_token with
    | AND ->
        next parser;
        BinOp (And, e, parse_and parser)
    | _ -> e

  and parse_rel parser =
    let e = parse_add parser in
    match parser.current_token with
    | EQ ->
        next parser;
        BinOp (Eq, e, parse_add parser)
    | NEQ ->
        next parser;
        BinOp (Neq, e, parse_add parser)
    | LT ->
        next parser;
        BinOp (Lt, e, parse_add parser)
    | LE ->
        next parser;
        BinOp (Le, e, parse_add parser)
    | GT ->
        next parser;
        BinOp (Gt, e, parse_add parser)
    | GE ->
        next parser;
        BinOp (Ge, e, parse_add parser)
    | _ -> e

  and parse_add parser =
    let e = parse_mul parser in
    match parser.current_token with
    | PLUS ->
        next parser;
        BinOp (Add, e, parse_add parser)
    | MINUS ->
        next parser;
        BinOp (Sub, e, parse_add parser)
    | _ -> e

  and parse_mul parser =
    let e = parse_unop parser in
    match parser.current_token with
    | STAR ->
        next parser;
        BinOp (Mul, e, parse_mul parser)
    | SLASH ->
        next parser;
        BinOp (Div, e, parse_mul parser)
    | _ -> e

  and parse_unop parser =
    match parser.current_token with
    | MINUS ->
        next parser;
        UnOp (Neg, parse_unop parser)
    | _ -> parse_unary parser

  and parse_unary parser =
    match parser.current_token with
    | MINUS ->
        next parser;
        UnOp (Neg, parse_unary parser)
    | NOT ->
        next parser;
        UnOp (Not, parse_unary parser)
    | _ -> parse_app parser

  and parse_app parser =
    let rec parse_app_arg parser =
      match parser.current_token with
      | Lexer.FUN -> parse_fun parser
      | Lexer.IF -> parse_if parser
      | Lexer.LET -> parse_let parser
      | _ -> parse_atom parser
    in
    let rec loop e =
      match parser.current_token with
      | Lexer.TRUE | Lexer.FALSE | Lexer.IDENT _ | Lexer.LPAREN | Lexer.FUN
      | Lexer.IF | Lexer.LET | Lexer.INT _ ->
          let arg = parse_app_arg parser in
          loop (App (e, arg))
      | _ -> e
    in
    let e = parse_atom parser in
    loop e

  and parse_atom parser =
    match parser.current_token with
    | INT n ->
        next parser;
        Int n
    | TRUE ->
        next parser;
        Bool true
    | FALSE ->
        next parser;
        Bool false
    | IDENT x ->
        next parser;
        Var x
    | LPAREN ->
        next parser;
        let e = parse_expr parser in
        expect Lexer.RPAREN parser;
        e
    | _ ->
        failwith
          (Printf.sprintf "Unexpected token: %s"
             (match parser.current_token with
             | INT _ -> "INT"
             | IDENT _ -> "IDENT"
             | _ -> "?"))
end
