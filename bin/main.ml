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
    try
      let line = input_line stdin in
      let idx = try Some (String.index line ';') with Not_found -> None in
      match idx with
      | Some i when i + 1 < String.length line && line.[i + 1] = ';' ->
          let before = String.sub line 0 i in
          String.concat "\n" (List.rev (before :: acc))
      | _ -> loop (line :: acc)
    with End_of_file ->
      if acc = [] then raise End_of_file else String.concat "\n" (List.rev acc)
  in
  loop []

let run_and_print code =
  try
    let result = Eval.run code in
    print_endline result
  with
  | Failure msg -> prerr_endline ("Error: " ^ msg)
  | e -> prerr_endline ("Error: " ^ Printexc.to_string e)

let rec repl () =
  let code = read_repl_input () |> String.trim in
  if code = "" then repl ()
  else if code = "quit" || code = ":quit" then ()
  else (
    run_and_print code;
    repl ())

let () =
  if Array.length Sys.argv > 1 then
    let fname = Sys.argv.(1) in
    run_and_print (read_file fname)
  else try repl () with End_of_file -> ()
