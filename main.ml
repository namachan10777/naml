let () =
  let ic = open_in Sys.argv.(1) in
  let src = really_input_string ic (in_channel_length ic) in
  let alpha = Ast.f "test.ml" src |> Alpha.f Alpha.pervasive_env in
  print_endline @@ Ast.show alpha;
  let _, typed = Ast.f "test.ml" src |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env in
  close_in ic ;
  print_endline @@ Typing.show typed
