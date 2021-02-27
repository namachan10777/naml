let f s = s |> Ast.f "test.ml" |> Alpha.f Tbl.empty

let () =
    match f "let x = 1 and y = 2" with
    | Ast.Let
        ( [ (Ast.PVar (id1, _), _, Ast.Int (1, _))
          ; (Ast.PVar (id2, _), _, Ast.Int (2, _)) ]
        , Ast.Never
        , _ ) ->
        ()
    | _ -> failwith "ast test 1 failed"
