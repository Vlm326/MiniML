module Lexer = struct
  type token =
    | INT of int
    | BOOL of bool
    | IDENT of string
    | TRUE
    | FALSE
    | IF
    | THEN
    | ELSE
    | LET
    | REC
    | IN
    | FUN
    | PLUS
    | MINUS
    | STAR
    | SLASH
    | LPAREN
    | RPAREN
    | EQ
    | NEQ
    | LT
    | LE
    | GT
    | GE
    | AND
    | OR
    | NOT
    | ARROW
    | EOF

  type lexbuf = { mutable pos : int; len : int; buf : string }

  let from_string s = { pos = 0; len = String.length s; buf = s }

  let peek lexer =
    if lexer.pos < lexer.len then Some lexer.buf.[lexer.pos] else None

  let get lexer =
    if lexer.pos < lexer.len then (
      lexer.pos <- lexer.pos + 1;
      Some lexer.buf.[lexer.pos - 1])
    else None

  let skip_whitespaces lexer =
    let rec loop () =
      match peek lexer with
      | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
          let _ = get lexer in
          loop ()
      | Some '#' ->
          let rec skip_line () =
            match peek lexer with
            | Some '\n' | None -> ()
            | _ ->
                let _ = get lexer in
                skip_line ()
          in
          let _ = skip_line () in
          loop ()
      | _ -> ()
    in
    loop ()

  let read_int lexer =
    let rec loop acc =
      match peek lexer with
      | Some c when c >= '0' && c <= '9' ->
          let n = int_of_string (String.make 1 c) in
          let _ = get lexer in
          loop ((acc * 10) + n)
      | _ -> acc
    in
    loop 0

  let read_ident lexer =
    let rec loop acc =
      match peek lexer with
      | Some c
        when (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9')
             || c = '_' ->
          let _ = get lexer in
          loop (acc ^ String.make 1 c)
      | _ -> acc
    in
    loop ""

  let keywords s =
    match s with
    | "if" -> IF
    | "then" -> THEN
    | "else" -> ELSE
    | "let" -> LET
    | "rec" -> REC
    | "in" -> IN
    | "fun" -> FUN
    | "true" -> TRUE
    | "false" -> FALSE
    | "not" -> NOT
    | _ -> IDENT s

  let read_binop lexer =
    match peek lexer with
    | Some '+' ->
        let _ = get lexer in
        PLUS
    | Some '-' ->
        let _ = get lexer in
        MINUS
    | Some '*' ->
        let _ = get lexer in
        STAR
    | Some '/' ->
        let _ = get lexer in
        SLASH
    | Some '=' ->
        let _ = get lexer in
        EQ
    | Some c when c = '<' -> (
        let _ = get lexer in
        match peek lexer with
        | Some '>' ->
            let _ = get lexer in
            NEQ
        | Some '=' ->
            let _ = get lexer in
            LE
        | _ -> LT)
    | Some '>' -> (
        let _ = get lexer in
        match peek lexer with
        | Some '=' ->
            let _ = get lexer in
            GE
        | _ -> GT)
    | Some '&' -> (
        let _ = get lexer in
        match peek lexer with
        | Some '&' ->
            let _ = get lexer in
            AND
        | _ -> EOF)
    | Some '|' -> (
        let _ = get lexer in
        match peek lexer with
        | Some '|' ->
            let _ = get lexer in
            OR
        | _ -> EOF)
    | _ -> EOF

  let next_token lexer =
    skip_whitespaces lexer;
    match peek lexer with
    | None -> EOF
    | Some c -> (
        match c with
        | ')' ->
            let _ = get lexer in
            RPAREN
        | '(' ->
            let _ = get lexer in
            LPAREN
        | '+' ->
            let _ = get lexer in
            PLUS
        | '*' ->
            let _ = get lexer in
            STAR
        | '/' ->
            let _ = get lexer in
            SLASH
        | '=' ->
            let _ = get lexer in
            EQ
        | '-' -> (
            let _ = get lexer in
            match peek lexer with
            | Some '>' ->
                let _ = get lexer in
                ARROW
            | Some d when d >= '0' && d <= '9' ->
                let n = read_int lexer in
                INT (-n)
            | _ -> MINUS)
        | c when c >= '0' && c <= '9' ->
            let n = Char.code c - Char.code '0' in
            let _ = get lexer in
            let rec loop acc =
              match peek lexer with
              | Some d when d >= '0' && d <= '9' ->
                  let digit = int_of_string (String.make 1 d) in
                  let _ = get lexer in
                  loop ((acc * 10) + digit)
              | _ -> acc
            in
            INT (loop n)
        | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
            let _ = get lexer in
            let s = read_ident lexer in
            let s = String.make 1 c ^ s in
            keywords s
        | '<' -> (
            let _ = get lexer in
            match peek lexer with
            | Some '>' ->
                let _ = get lexer in
                NEQ
            | Some '=' ->
                let _ = get lexer in
                LE
            | _ -> LT)
        | '>' -> (
            let _ = get lexer in
            match peek lexer with
            | Some '=' ->
                let _ = get lexer in
                GE
            | _ -> GT)
        | _ ->
            let _ = get lexer in
            read_binop lexer)
end
