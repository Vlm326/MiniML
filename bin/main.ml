open MiniML

let rec read_file fname =
  let ch = open_in fname in
  let rec loop acc =
    try loop (input_line ch :: acc)
    with End_of_file ->
      close_in ch;
      String.concat "\n" (List.rev acc)
  in
  loop []

let read_repl_input () =
  print_string "> ";
  flush stdout;
  let rec loop acc =
    let line = input_line stdin in
    let idx = try Some (String.index line ';') with Not_found -> None in
    match idx with
    | Some i when i + 1 < String.length line && line.[i + 1] = ';' ->
        let before = String.sub line 0 i in
        String.concat "\n" (List.rev (before :: acc))
    | _ -> loop (line :: acc)
  in
  try loop [] with End_of_file -> ""

let read_file fname =
  let ch = open_in fname in
  let rec loop acc =
    try loop (input_line ch :: acc)
    with End_of_file ->
      close_in ch;
      String.concat "\n" (List.rev acc)
  in
  loop []

let () =
  let code =
    if Array.length Sys.argv > 1 then
      let fname = Sys.argv.(1) in
      read_file fname
    else read_repl_input ()
  in
  try
    let result = Eval.run code in
    print_endline result
  with
  | Failure msg -> prerr_endline ("Error: " ^ msg)
  | e -> prerr_endline ("Error: " ^ Printexc.to_string e)
