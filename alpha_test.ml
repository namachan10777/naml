let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f [] 
let nw = Lex.nowhere

let () =
    ( match f "let x = 1 and y = 2 in 1" with
    | Ast.Let
        ( [ (Ast.PVar (id1, _), _, Ast.Int (1, _))
          ; (Ast.PVar (id2, _), _, Ast.Int (2, _)) ]
        , Ast.Int (1, _)
        , _ ) when id1 <> id2 ->
        ()
    | _ -> failwith "ast test 1 failed" ) ;
    (match f "let x = let x = 2 in x in x" with
    | Ast.Let
        ( [ ( Ast.PVar (id, _)
            , _
            , Ast.Let ([(Ast.PVar (id2, _), _, _)], Ast.Var (id2', _), _) ) ]
        , Ast.Var (id', _)
        , _ ) when id = id' && id2 = id2' ->
        ()
    | _ -> failwith "ast test 2 failed");
    (match f "let rec x = let rec x = 2 in x in x" with
    | Ast.LetRec
        ( [ ( id
            , _
            , Ast.LetRec ([(id2, _, _)], Ast.Var (id2', _), _) ) ]
        , Ast.Var (id', _)
        , _ ) when id = id' && id2 = id2' ->
        ()
    | _ -> failwith "ast test 3 failed");
    (try f "let x = 1 and x = 2 in 1" |> ignore; failwith "let boundary check" with
    | Alpha.Error _ -> ()
    | _ -> failwith "let boundary check");
    (try f "let rec x = 1 and y = x in 1" |> ignore; failwith "matual-recursion is allowed for only function" with
    | Alpha.Error _ -> ()
    | _ -> failwith "matual-recursion is allowed for only function");
