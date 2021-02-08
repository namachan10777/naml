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
      ; C.End ] ;
    test "let f x = x + 2"
      [ C.LetClosure
          ( T.Vid 16
          , [T.Vid 17]
          , [ C.LetInt (T.VidSSA 7, 2)
            ; C.LetCall (T.VidSSA 8, T.Vid 0, [T.Vid 17; T.VidSSA 7]) ]
          , T.VidSSA 8
          , "_f_1" )
      ; C.End ]
