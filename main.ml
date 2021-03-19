let () =
  let ic = open_in Sys.argv.(1) in
  let src = really_input_string ic (in_channel_length ic) in
  let env, lets = Flatlet.typing_module Flatlet.pervasives Sys.argv.(1) src in
  let funs, ast = Flat.f @@ Closure.f Closure.pervasives lets in
  close_in ic ;
  print_endline @@ Flat.show_funs_t funs;
  print_endline @@ Flat.show_stmts_t ast
