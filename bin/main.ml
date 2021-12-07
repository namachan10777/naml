let () =
  let ic = open_in Sys.argv.(1) in
  let txt = really_input_string ic (in_channel_length ic) in
  let tokens = Naml.Lex.f Sys.argv.(1) txt in
  let ast = Naml.Parser.f tokens in
  print_endline @@ Naml.Parser.show ast
