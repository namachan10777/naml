let () =
  let ic = open_in Sys.argv.(1) in
  let src = really_input_string ic (in_channel_length ic) in
  let _, typed = Lex.f "test.ml" src |> Parser.f |> Ast.f |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env in
  close_in ic ;
  print_endline @@ Typing.show typed
