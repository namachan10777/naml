let f s = s |> Ast.f "test.ml" |> Alpha.f Tbl.empty

let () =
    ( match f "let x = 1 and y = 2" with
    | Ast.Let
        ( [ (Ast.PVar (id1, _), _, Ast.Int (1, _))
          ; (Ast.PVar (id2, _), _, Ast.Int (2, _)) ]
        , Ast.Never
        , _ ) ->
        ()
    | _ -> failwith "ast test 1 failed" ) ;
    match f "let x = let x = 2 in x" with
    | Ast.Let
        ( [ ( Ast.PVar (id, _)
            , _
            , Ast.Let ([(Ast.PVar (id2, _), _, _)], Ast.Var (id2', _), _) ) ]
        , _
        , _ )
      when id2 = id2' ->
        ()
    | _ -> failwith "ast test 1 failed"
