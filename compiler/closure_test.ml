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
      ; C.End ] ;
    test "let f x = let g y = x + y in g"
      [ C.LetClosure
          ( T.Vid 16
          , [T.Vid 17]
          , [ C.LetClosure
                ( T.VidSSA 14
                , [T.VidSSA 12; T.Vid 19]
                , [C.LetCall (T.VidSSA 13, T.Vid 0, [T.VidSSA 12; T.Vid 19])]
                , T.VidSSA 13
                , "_f_1_g_2" )
            ; C.LetCall (T.Vid 18, T.VidSSA 14, [T.Vid 17]) ]
          , T.Vid 18
          , "_f_1" )
      ; C.End ] ;
    test "let rec f x = f x"
      [ C.LetClosure
          ( T.Vid 16
          , [T.Vid 17]
          , [C.LetCall (T.VidSSA 17, T.Vid 16, [T.Vid 17])]
          , T.VidSSA 17
          , "_f_1" )
      ; C.End ]
