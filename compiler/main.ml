let () =
    if Array.length Sys.argv != 2 then print_endline "compiler <filename>"
    else
      let ic = open_in Sys.argv.(1) in
      let src = really_input_string ic (in_channel_length ic) in
      print_endline @@ Closure.show_inst_t @@ List.rev @@ Closure.f @@ Typing.f
      @@ Alpha.f
      @@ Ast.f Sys.argv.(1) src
