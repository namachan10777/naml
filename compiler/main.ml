let () =
    if Array.length Sys.argv != 2 then print_endline "compiler <filename>"
    else
      let ic = open_in Sys.argv.(1) in
      let src = really_input_string ic (in_channel_length ic) in
      let typed = Typing.f @@ Alpha.f @@ Ast.f Sys.argv.(1) src in
      let closure = Closure.f typed in
      let asm_src = Ir.f closure in
      print_endline asm_src
