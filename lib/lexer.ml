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
  let peek l = if l.pos < l.len then Some l.buf.[l.pos] else None

  let get l =
    if l.pos < l.len then (
      l.pos <- l.pos + 1;
      Some l.buf.[l.pos - 1])
    else None

  let skip_whitespaces l =
    let rec loop () =
      match peek l with
      | Some ' ' | Some '\t' | Some '\n' | Some '\r' ->
          let _ = get l in
          loop ()
      | Some '#' ->
          let rec skip_line () =
            match peek l with
            | Some '\n' | None -> ()
            | _ ->
                let _ = get l in
                skip_line ()
          in
          let _ = skip_line () in
          loop ()
      | _ -> ()
    in
    loop ()

  let read_int l =
    let rec loop acc =
      match peek l with
      | Some c when c >= '0' && c <= '9' ->
          let n = int_of_string (String.make 1 c) in
          let _ = get l in
          loop ((acc * 10) + n)
      | _ -> acc
    in
    loop 0

  let read_ident l =
    let rec loop acc =
      match peek l with
      | Some c
        when (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9')
             || c = '_' ->
          let _ = get l in
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

  let read_binop l =
    match peek l with
    | Some '+' ->
        let _ = get l in
        PLUS
    | Some '-' ->
        let _ = get l in
        MINUS
    | Some '*' ->
        let _ = get l in
        STAR
    | Some '/' ->
        let _ = get l in
        SLASH
    | Some '=' ->
        let _ = get l in
        EQ
    | Some '<' -> (
        let _ = get l in
        match peek l with
        | Some '>' ->
            let _ = get l in
            NEQ
        | Some '=' ->
            let _ = get l in
            LE
        | _ -> LT)
    | Some '>' -> (
        let _ = get l in
        match peek l with
        | Some '=' ->
            let _ = get l in
            GE
        | _ -> GT)
    | Some '&' -> (
        let _ = get l in
        match peek l with
        | Some '&' ->
            let _ = get l in
            AND
        | _ -> EOF)
    | Some '|' -> (
        let _ = get l in
        match peek l with
        | Some '|' ->
            let _ = get l in
            OR
        | _ -> EOF)
    | _ -> EOF

  let next_token l =
    skip_whitespaces l;
    match peek l with
    | None -> EOF
    | Some c -> (
        match c with
        | ')' ->
            let _ = get l in
            RPAREN
        | '(' ->
            let _ = get l in
            LPAREN
        | '+' ->
            let _ = get l in
            PLUS
        | '*' ->
            let _ = get l in
            STAR
        | '/' ->
            let _ = get l in
            SLASH
        | '=' ->
            let _ = get l in
            EQ
        | '-' -> (
            let _ = get l in
            match peek l with
            | Some '>' ->
                let _ = get l in
                ARROW
            | Some d when d >= '0' && d <= '9' ->
                let n = read_int l in
                INT (-n)
            | _ -> MINUS)
        | c when c >= '0' && c <= '9' ->
            let _ = get l in
            let rest = read_int l in
            let n = Char.code c - Char.code '0' in
            INT ((n * 10) + rest)
        | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
            let _ = get l in
            let s = read_ident l in
            let s = String.make 1 c ^ s in
            keywords s
        | _ ->
            let _ = get l in
            read_binop l)
end
