open MiniML

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let fail msg = Alcotest.fail msg
let protect f = try Ok (f ()) with exn -> Error exn

let expect_run code expected () =
  check_string ("program result for " ^ code) expected (Eval.run code)

let expect_failure code expected_message () =
  match protect (fun () -> Eval.run code) with
  | Error (Failure msg) ->
      check_string ("failure for " ^ code) expected_message msg
  | Error exn ->
      fail
        (Printf.sprintf "Expected Failure for `%s`, got `%s`" code
           (Printexc.to_string exn))
  | Ok value ->
      fail
        (Printf.sprintf "Expected failure for `%s`, got successful value `%s`"
           code value)

let rec string_of_expr = function
  | Ast.Int n -> Printf.sprintf "Int(%d)" n
  | Ast.Bool b -> Printf.sprintf "Bool(%b)" b
  | Ast.Var name -> Printf.sprintf "Var(%s)" name
  | Ast.Fun (arg, body) -> Printf.sprintf "Fun(%s,%s)" arg (string_of_expr body)
  | Ast.App (fn, arg) ->
      Printf.sprintf "App(%s,%s)" (string_of_expr fn) (string_of_expr arg)
  | Ast.Let (name, value, body) ->
      Printf.sprintf "Let(%s,%s,%s)" name (string_of_expr value)
        (string_of_expr body)
  | Ast.LetRec (name, arg, value, body) ->
      Printf.sprintf "LetRec(%s,%s,%s,%s)" name arg (string_of_expr value)
        (string_of_expr body)
  | Ast.If (cond, then_branch, else_branch) ->
      Printf.sprintf "If(%s,%s,%s)" (string_of_expr cond)
        (string_of_expr then_branch)
        (string_of_expr else_branch)
  | Ast.BinOp (op, left, right) ->
      let op_name =
        match op with
        | Ast.Add -> "Add"
        | Ast.Sub -> "Sub"
        | Ast.Mul -> "Mul"
        | Ast.Div -> "Div"
        | Ast.Eq -> "Eq"
        | Ast.Neq -> "Neq"
        | Ast.Lt -> "Lt"
        | Ast.Le -> "Le"
        | Ast.Gt -> "Gt"
        | Ast.Ge -> "Ge"
        | Ast.And -> "And"
        | Ast.Or -> "Or"
      in
      Printf.sprintf "BinOp(%s,%s,%s)" op_name (string_of_expr left)
        (string_of_expr right)
  | Ast.UnOp (op, expr) ->
      let op_name = match op with Ast.Neg -> "Neg" | Ast.Not -> "Not" in
      Printf.sprintf "UnOp(%s,%s)" op_name (string_of_expr expr)

let check_expr name expected actual =
  check_string name (string_of_expr expected) (string_of_expr actual)

let collect_tokens source =
  let lexbuf = Lexer.Lexer.from_string source in
  let rec loop acc =
    let token = Lexer.Lexer.next_token lexbuf in
    match token with
    | Lexer.Lexer.EOF -> List.rev (token :: acc)
    | _ -> loop (token :: acc)
  in
  loop []

let token_name = function
  | Lexer.Lexer.INT n -> Printf.sprintf "INT(%d)" n
  | Lexer.Lexer.BOOL b -> Printf.sprintf "BOOL(%b)" b
  | Lexer.Lexer.IDENT s -> Printf.sprintf "IDENT(%s)" s
  | Lexer.Lexer.TRUE -> "TRUE"
  | Lexer.Lexer.FALSE -> "FALSE"
  | Lexer.Lexer.IF -> "IF"
  | Lexer.Lexer.THEN -> "THEN"
  | Lexer.Lexer.ELSE -> "ELSE"
  | Lexer.Lexer.LET -> "LET"
  | Lexer.Lexer.REC -> "REC"
  | Lexer.Lexer.IN -> "IN"
  | Lexer.Lexer.FUN -> "FUN"
  | Lexer.Lexer.PLUS -> "PLUS"
  | Lexer.Lexer.MINUS -> "MINUS"
  | Lexer.Lexer.STAR -> "STAR"
  | Lexer.Lexer.SLASH -> "SLASH"
  | Lexer.Lexer.LPAREN -> "LPAREN"
  | Lexer.Lexer.RPAREN -> "RPAREN"
  | Lexer.Lexer.EQ -> "EQ"
  | Lexer.Lexer.NEQ -> "NEQ"
  | Lexer.Lexer.LT -> "LT"
  | Lexer.Lexer.LE -> "LE"
  | Lexer.Lexer.GT -> "GT"
  | Lexer.Lexer.GE -> "GE"
  | Lexer.Lexer.AND -> "AND"
  | Lexer.Lexer.OR -> "OR"
  | Lexer.Lexer.NOT -> "NOT"
  | Lexer.Lexer.ARROW -> "ARROW"
  | Lexer.Lexer.EOF -> "EOF"

let check_tokens name expected actual =
  let join tokens = String.concat ";" (List.map token_name tokens) in
  check_string name (join expected) (join actual)

let parse source =
  let parser = Parser.Parser.from_string source in
  Parser.Parser.parse parser

let expect_parse source expected () = check_expr source expected (parse source)

let expect_parse_failure source expected_message () =
  match protect (fun () -> parse source) with
  | Error (Failure msg) ->
      check_string ("parse failure for " ^ source) expected_message msg
  | Error exn ->
      fail
        (Printf.sprintf "Expected parse Failure for `%s`, got `%s`" source
           (Printexc.to_string exn))
  | Ok expr ->
      fail
        (Printf.sprintf "Expected parse failure for `%s`, got `%s`" source
           (string_of_expr expr))

let lexer_tests =
  [
    ( "keywords and operators",
      `Quick,
      fun () ->
        check_tokens "token stream"
          [
            Lexer.Lexer.LET;
            Lexer.Lexer.REC;
            Lexer.Lexer.IDENT "fact";
            Lexer.Lexer.EQ;
            Lexer.Lexer.FUN;
            Lexer.Lexer.IDENT "n";
            Lexer.Lexer.ARROW;
            Lexer.Lexer.IF;
            Lexer.Lexer.IDENT "n";
            Lexer.Lexer.LE;
            Lexer.Lexer.INT 1;
            Lexer.Lexer.THEN;
            Lexer.Lexer.TRUE;
            Lexer.Lexer.ELSE;
            Lexer.Lexer.FALSE;
            Lexer.Lexer.AND;
            Lexer.Lexer.NOT;
            Lexer.Lexer.FALSE;
            Lexer.Lexer.OR;
            Lexer.Lexer.IDENT "x_1";
            Lexer.Lexer.NEQ;
            Lexer.Lexer.INT (-42);
            Lexer.Lexer.EOF;
          ]
          (collect_tokens
             "let rec fact = fun n -> if n <= 1 then true else false && not \
              false || x_1 <> -42") );
    ( "comments and whitespace",
      `Quick,
      fun () ->
        check_tokens "comment skipping"
          [
            Lexer.Lexer.INT 1;
            Lexer.Lexer.PLUS;
            Lexer.Lexer.INT 2;
            Lexer.Lexer.EOF;
          ]
          (collect_tokens "  # ignore me\n1 \n\t+ 2 # and me\n") );
    ( "single ampersand is rejected",
      `Quick,
      fun () ->
        match protect (fun () -> collect_tokens "&") with
        | Error (Failure msg) ->
            check_string "lexer error" "Unexpected character: &" msg
        | _ -> fail "Expected lexer failure on single `&`" );
    ( "single pipe is rejected",
      `Quick,
      fun () ->
        match protect (fun () -> collect_tokens "|") with
        | Error (Failure msg) ->
            check_string "lexer error" "Unexpected character: |" msg
        | _ -> fail "Expected lexer failure on single `|`" );
    ( "unexpected character is rejected",
      `Quick,
      fun () ->
        match protect (fun () -> collect_tokens "@") with
        | Error (Failure msg) ->
            check_string "lexer error" "Unexpected character: @" msg
        | _ -> fail "Expected lexer failure on `@`" );
    ( "helpers advance the lexbuf",
      `Quick,
      fun () ->
        let digits = Lexer.Lexer.from_string "123abc" in
        check_int "read_int" 123 (Lexer.Lexer.read_int digits);
        let ident = Lexer.Lexer.from_string "name_42!" in
        check_string "read_ident" "name_42" (Lexer.Lexer.read_ident ident);
        let peek_buf = Lexer.Lexer.from_string "xy" in
        Alcotest.(check (option char))
          "peek before get" (Some 'x')
          (Lexer.Lexer.peek peek_buf);
        Alcotest.(check (option char))
          "get first char" (Some 'x') (Lexer.Lexer.get peek_buf);
        Alcotest.(check (option char))
          "peek after get" (Some 'y')
          (Lexer.Lexer.peek peek_buf) );
  ]

let parser_tests =
  [
    ( "precedence",
      `Quick,
      expect_parse "1 + 2 * 3"
        (Ast.BinOp
           (Ast.Add, Ast.Int 1, Ast.BinOp (Ast.Mul, Ast.Int 2, Ast.Int 3))) );
    ( "application associates left",
      `Quick,
      expect_parse "f x y"
        (Ast.App (Ast.App (Ast.Var "f", Ast.Var "x"), Ast.Var "y")) );
    ( "function application accepts let argument",
      `Quick,
      expect_parse "f (let x = 1 in x)"
        (Ast.App (Ast.Var "f", Ast.Let ("x", Ast.Int 1, Ast.Var "x"))) );
    ( "if expression",
      `Quick,
      expect_parse "if true then 1 else 2"
        (Ast.If (Ast.Bool true, Ast.Int 1, Ast.Int 2)) );
    ( "function literal",
      `Quick,
      expect_parse "fun x -> x + 1"
        (Ast.Fun ("x", Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Int 1))) );
    ( "let rec with inline parameter",
      `Quick,
      expect_parse
        "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 4"
        (Ast.LetRec
           ( "fact",
             "n",
             Ast.If
               ( Ast.BinOp (Ast.Eq, Ast.Var "n", Ast.Int 0),
                 Ast.Int 1,
                 Ast.BinOp
                   ( Ast.Mul,
                     Ast.Var "n",
                     Ast.App
                       ( Ast.Var "fact",
                         Ast.BinOp (Ast.Sub, Ast.Var "n", Ast.Int 1) ) ) ),
             Ast.App (Ast.Var "fact", Ast.Int 4) )) );
    ( "let rec with fun keyword",
      `Quick,
      expect_parse "let rec inc = fun x -> x + 1 in inc 2"
        (Ast.LetRec
           ( "inc",
             "x",
             Ast.BinOp (Ast.Add, Ast.Var "x", Ast.Int 1),
             Ast.App (Ast.Var "inc", Ast.Int 2) )) );
    ( "boolean operators",
      `Quick,
      expect_parse "not false || true && false"
        (Ast.BinOp
           ( Ast.Or,
             Ast.UnOp (Ast.Not, Ast.Bool false),
             Ast.BinOp (Ast.And, Ast.Bool true, Ast.Bool false) )) );
    ( "trailing token is rejected",
      `Quick,
      expect_parse_failure "1 < 2 < 3" "Unexpected trailing token: LT" );
    ( "missing let name is rejected",
      `Quick,
      expect_parse_failure "let = 1 in 2" "Expected IDENT or REC" );
    ( "missing if then branch separator",
      `Quick,
      expect_parse_failure "if true 1 else 2" "Expected THEN, got ELSE" );
    ( "bare let rec value is rejected",
      `Quick,
      expect_parse_failure "let rec f = 1 in f"
        "let rec requires a function definition, use `let rec f x = ... in \
         ...` or `let rec f = fun x -> ... in ...`" );
    ( "unexpected token in atom",
      `Quick,
      expect_parse_failure ")" "Unexpected token in expression: RPAREN" );
  ]

let eval_tests =
  [
    ( "curried addition",
      `Quick,
      expect_run "let add = fun x -> fun y -> x + y in add 3 4" "7" );
    ( "recursive factorial",
      `Quick,
      expect_run
        "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5" "120"
    );
    ( "lexical scoping",
      `Quick,
      expect_run "let x = 10 in let f = fun y -> x + y in let x = 100 in f 5"
        "15" );
    ("boolean equality", `Quick, expect_run "true = false" "false");
    ("boolean inequality", `Quick, expect_run "true <> false" "true");
    ( "integer comparisons",
      `Quick,
      expect_run "1 < 2 && 2 <= 2 && 3 > 2 && 3 >= 3" "true" );
    ("unary minus over arithmetic", `Quick, expect_run "-(1 + 2)" "-3");
    ("not false", `Quick, expect_run "not false" "true");
    ("if false branch", `Quick, expect_run "if false then 1 else 2" "2");
    ("function values stringify as fun", `Quick, expect_run "fun x -> x" "<fun>");
    ( "recursive function values stringify as fun",
      `Quick,
      expect_run "let rec id x = x in id" "<fun>" );
    ( "division by zero fails",
      `Quick,
      expect_failure "10 / 0" "you cannot divide by zero" );
    ( "if condition type error",
      `Quick,
      expect_failure "if 1 then 2 else 3"
        "Condition of if-expression must be bool, got int" );
    ( "applying non-function fails",
      `Quick,
      expect_failure "1 2" "Cannot apply value of type int as a function" );
    ("unbound variable fails", `Quick, expect_failure "x" "Unbound variable: x");
    ( "binary type mismatch on addition",
      `Quick,
      expect_failure "1 + true"
        "Type error in binary operation +: got int and bool" );
    ( "binary type mismatch on equality",
      `Quick,
      expect_failure "1 = false"
        "Type error in binary operation =: got int and bool" );
    ( "binary type mismatch on and",
      `Quick,
      expect_failure "1 && true"
        "Type error in binary operation &&: got int and bool" );
    ( "unary type mismatch",
      `Quick,
      expect_failure "not 1" "Type error in unary operation not: got int" );
  ]

let () =
  Alcotest.run "MiniML"
    [ ("lexer", lexer_tests); ("parser", parser_tests); ("eval", eval_tests) ]
