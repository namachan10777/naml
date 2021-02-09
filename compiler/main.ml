let () =
    if Array.length Sys.argv != 2 then print_endline "compiler <filename>"
    else
      let ic = open_in Sys.argv.(1) in
      let src = really_input_string ic (in_channel_length ic) in
      let typed = Typing.f @@ Alpha.f @@ Ast.f Sys.argv.(1) src in
      print_endline @@ Typing.show typed ;
      let closure = Closure.f typed in
      print_endline @@ Closure.show_inst_t closure ;
      let ir = Ir.f closure in
      print_endline @@ Ir.show_insts_t ir
