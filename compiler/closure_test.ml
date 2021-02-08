let test a b =
    let ast = Closure.f @@ Typing.f @@ Alpha.f @@ Ast.f "test.ml" a in
    if ast = b then ()
    else (
      Printf.printf "left:\n%s\nright:\n%s" (Closure.show_inst_t ast)
        (Closure.show_inst_t b) ;
      failwith @@ "test failed: " ^ a )

module C = Closure
module T = Types

let () =
    test "let x = 1 + 2"
      [ C.LetInt (T.VidSSA 3, 2)
      ; C.LetInt (T.VidSSA 2, 1)
      ; C.LetCall (T.Vid 16, T.Vid 0, [T.VidSSA 2; T.VidSSA 3])
      ; C.End ]
