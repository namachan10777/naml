let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f Alpha.pervasive_env
let f_s s = Lex.f "test.ml" s |> Parser.f |> Ast.f |> Alpha.f Alpha.pervasive_env
let nw = Lex.nowhere


let () =
    ( match f "let x = 1 and y = 2 in 1" with
    | Ast.Let
        ( [ (Ast.PVar (id1, _), _, Ast.Int (1, _))
          ; (Ast.PVar (id2, _), _, Ast.Int (2, _)) ]
        , Ast.Int (1, _)) when id1 <> id2 ->
        ()
    | _ -> failwith "ast test 1 failed" ) ;
    (match f "let x = let x = 2 in x in x" with
    | Ast.Let
        ( [ ( Ast.PVar (id, _)
            , _
            , Ast.Let ([(Ast.PVar (id2, _), _, _)], Ast.Var (id2', _)) ) ]
        , Ast.Var (id', _)) when id = id' && id2 = id2' ->
        ()
    | _ -> failwith "ast test 2 failed");
    (match f "let rec x = let rec x = 2 in x in x" with
    | Ast.LetRec
        ( [ ( id
            , _
            , Ast.LetRec ([(id2, _, _)], Ast.Var (id2', _)) ) ]
        , Ast.Var (id', _)) when id = id' && id2 = id2' ->
        ()
    | _ -> failwith "ast test 3 failed");
    (try f "let x = 1 and x = 2 in 1" |> ignore; failwith "let boundary check" with
    | Alpha.Error _ -> ()
    | _ -> failwith "let boundary check");
    (try f "let rec x = 1 and y = x in 1" |> ignore; failwith "matual-recursion is allowed for only function" with
    | Alpha.Error _ -> ()
    | _ -> failwith "matual-recursion is allowed for only function");
    ( match f "let rec x = 1 and y z = x + z in 1" with
    | Ast.LetRec ([
        (id_x, _, Ast.Int (1, _));
        (id_y, _, Ast.Fun (id_z, Ast.App (Ast.App (_, Ast.Var (id_x', _), _), Ast.Var (id_z', _), _), p));
    ], Ast.Int (1, _)) when id_x = id_x' && id_z = id_z' -> ()
    | _ -> failwith "ast test 1 failed" ) ;
    (match f "let x | x = 1 in x" with
    | Ast.Let ([Ast.POr (Ast.PVar (id1, _), [Ast.PVar (id2, _)], _), _, Ast.Int (1, _)], Ast.Var (id3, _)) when id1 = id2 && id2 = id3 -> ()
    | _ -> failwith "ast or pat test failed");
    (match f "match [] with [] -> 0 | [x] -> x" with
    | Ast.Match (Ast.CtorApp (emp_id1, _, []), [
        (Ast.PCtorApp (emp_id2, [], _), _, Ast.Bool (true, _), Ast.Int (0, _));
        (Ast.PCtorApp (emp_id3, [
            Ast.PVar (x_id1, _);
            Ast.PCtorApp (emp_id4, [], _);
        ], _), _, Ast.Bool(true, _), Ast.Var (x_id2, _));
    ]) when emp_id1 = emp_id2 && emp_id2 = emp_id4 && x_id1 = x_id2 -> ()
    | _ -> failwith "alpha ctor failed");
    (let emp_id1 = Id.lookup  ["[]"] (List.map Util.fst Pervasives.ctors) in
    match f "let x = [] in 0" with
    | Ast.Let ([
        (Ast.PVar (x, _), _, Ast.CtorApp (emp_id2, _, []));
    ], Ast.Int (0, _)) when emp_id1 = emp_id2 -> ()
    | _ -> failwith "alpha CtorApp failed");
    (let list_id1 = Id.lookup  ["list"] (List.map fst Pervasives.types) in
    match f_s "type 'a t = 'a list and t2 = int t" with
    | Ast.Type ([
        (tid1, _, [("a", _)], Ast.Alias (Ast.TApp ([Ast.TVar ("a", _)], list_id2, _)));
        (t2id, _, [], Ast.Alias (Ast.TApp ([Ast.TInt _], tid2, _)));
    ], Ast.Never) when tid1 = tid2 && list_id1 = list_id2 -> ()
    | _ -> failwith "alpha type alias");
    (match f_s "type t = A of int let x = A 1" with
    | Ast.Type ([
        (tid1, _, [], Ast.Variant ([aid1, _, [Ast.TInt _]]));
    ],
    Ast.Let ([xid, _, Ast.CtorApp (aid2, _, [Ast.Int (1, _)])], Ast.Never)) when aid1 = aid2 -> ()
    | _ -> failwith "alpha type ctoR")
