open MiniML

let expect_run code expected =
  let actual = Eval.run code in
  if actual <> expected then
    failwith
      (Printf.sprintf "Expected `%s`, got `%s` for `%s`" expected actual code)

let expect_failure code expected_message =
  try
    let _ = Eval.run code in
    failwith (Printf.sprintf "Expected failure for `%s`" code)
  with
  | Failure msg when msg = expected_message -> ()
  | Failure msg ->
      failwith
        (Printf.sprintf "Expected failure `%s`, got `%s`" expected_message msg)
  | exn ->
      failwith
        (Printf.sprintf "Expected Failure, got `%s`"
           (Printexc.to_string exn))

let () =
  expect_run "let add = fun x -> fun y -> x + y in add 3 4" "7";
  expect_failure "1 < 2 < 3" "Unexpected trailing token: LT";
  expect_failure "let rec f = 1 in f"
    "let rec requires a function definition, use `let rec f x = ... in ...` or `let rec f = fun x -> ... in ...`"
