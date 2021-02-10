let () =
    if Array.length Sys.argv != 3 then
      print_endline "compiler <typing|asmgen> <filename>"
    else
      let ic = open_in Sys.argv.(2) in
      let src = really_input_string ic (in_channel_length ic) in
      let typed = Typing.f @@ Alpha.f @@ Ast.f Sys.argv.(2) src in
      close_in ic ;
      match Sys.argv.(1) with
      | "typing" -> print_endline @@ Typing.show typed
      | "asmgen" ->
          let closure = Closure.f typed in
          let asm_src = Ir.f closure in
          let oc = open_out "out.s" in
          output_string oc asm_src ;
          close_out oc ;
          exit @@ Sys.command "gcc out.s"
      | mode -> Printf.printf "unsupported mode \"%s\"" mode
