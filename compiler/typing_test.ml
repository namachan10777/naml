let test src expected =
    Printf.printf "testing \"%s\"..." src ;
    let lexed = Lex.lex src @@ Lex.initial_pos "test.ml" in
    let s, _ = Parser.parse_expr lexed in
    let ast = Ast.of_parser_t s in
    let typed = Typing.f ast in
    if typed = expected then Printf.printf "ok\n"
    else (
      Printf.printf "left: \n%s\n" @@ Typing.show typed ;
      Printf.printf "right: \n%s\n" @@ Typing.show expected ;
      failwith "test failed" )

let unify_test name a b =
    Printf.printf "testing \"%s\"\"..." name ;
    let tbl = ref [] in
    let a = Typing.generalize_ty tbl 0 a in
    let b = Typing.generalize_ty tbl 0 b in
    if Types.eq b a then Printf.printf "ok\n"
    else (
      Printf.printf "left: \n%s\n" @@ Types.show a ;
      Printf.printf "right: \n%s\n" @@ Types.show b ;
      failwith "test failed" )

module Ty = Types
module T = Typing

let () =
    test "0" (Typing.Int 0) ;
    test "not" (Typing.Var ["not"]) ;
    test "not true" (Typing.App (Typing.Var ["not"], [Typing.Bool true])) ;
    test "1+2" (Typing.App (Typing.Var ["+"], [Typing.Int 1; Typing.Int 2])) ;
    test "let x = 1 in x"
      (Typing.Let ([(Typing.PVar ("x", Ty.Int), Typing.Int 1)], Typing.Var ["x"])) ;
    test "let x = 1 + 1 in x"
      (Typing.Let
         ( [ ( Typing.PVar ("x", Ty.Int)
             , Typing.App (Typing.Var ["+"], [Typing.Int 1; Typing.Int 1]) ) ]
         , Typing.Var ["x"] )) ;
    test "let id = fun x -> x in id 1; id true"
      (Typing.Let
         ( [ ( Typing.PVar ("id", Ty.Fun ([Ty.Poly 0], Ty.Poly 0))
             , Typing.Fun ([("x", Ty.Poly 0)], Typing.Var ["x"], Ty.Poly 0) ) ]
         , Typing.App
             ( Typing.Var [";"]
             , [ Typing.App (Typing.Var ["id"], [Typing.Int 1])
               ; Typing.App (Typing.Var ["id"], [Typing.Bool true]) ] ) )) ;
    test
      "let mk_pair x y = (x, y) in let a = mk_pair 1 in let b = mk_pair true \
       false in a"
      (T.Let
         ( [ ( T.PVar
                 ( "mk_pair"
                 , Ty.Fun
                     ([Ty.Poly 0; Ty.Poly 1], Ty.Tuple [Ty.Poly 0; Ty.Poly 1])
                 )
             , T.Fun
                 ( [("x", Ty.Poly 0); ("y", Ty.Poly 1)]
                 , T.Tuple ([T.Var ["x"]; T.Var ["y"]], [Ty.Poly 0; Ty.Poly 1])
                 , Ty.Tuple [Ty.Poly 0; Ty.Poly 1] ) ) ]
         , T.Let
             ( [ ( T.PVar
                     ("a", Ty.Fun ([Ty.Poly 0], Ty.Tuple [Ty.Int; Ty.Poly 0]))
                 , T.App (T.Var ["mk_pair"], [T.Int 1]) ) ]
             , T.Let
                 ( [ ( T.PVar ("b", Ty.Tuple [Ty.Bool; Ty.Bool])
                     , T.App (T.Var ["mk_pair"], [T.Bool true; T.Bool false]) )
                   ]
                 , Typing.Var ["a"] ) ) )) ;
    test "let f x = let g y = x = y in g in f"
      (T.Let
         ( [ ( T.PVar ("f", Ty.Fun ([Ty.Poly 0], Ty.Fun ([Ty.Poly 0], Ty.Bool)))
             , T.Fun
                 ( [("x", Ty.Poly 0)]
                 , T.Let
                     ( [ ( T.PVar ("g", Ty.Fun ([Ty.Poly 0], Ty.Bool))
                         , T.Fun
                             ( [("y", Ty.Poly 0)]
                             , T.App (T.Var ["="], [T.Var ["x"]; T.Var ["y"]])
                             , Ty.Bool ) ) ]
                     , T.Var ["g"] )
                 , Ty.Fun ([Ty.Poly 0], Ty.Bool) ) ) ]
         , T.Var ["f"] )) ;
    test "let rec fact n = if n = 1 then 1 else n * fact (n-1) in fact 5"
      (T.LetRec
         ( [ ( ["fact"]
             , Ty.Fun ([Ty.Int], Ty.Int)
             , T.Fun
                 ( [("n", Ty.Int)]
                 , T.If
                     ( T.App (T.Var ["="], [T.Var ["n"]; T.Int 1])
                     , T.Int 1
                     , T.App
                         ( T.Var ["*"]
                         , [ T.Var ["n"]
                           ; T.App
                               ( T.Var ["fact"]
                               , [T.App (T.Var ["-"], [T.Var ["n"]; T.Int 1])]
                               ) ] ) )
                 , Ty.Int ) ) ]
         , T.App (T.Var ["fact"], [T.Int 5]) )) ;
    test "let x, y = 1, 2 in x"
      (T.Let
         ( [ ( T.PTuple
                 ([T.PVar ("x", Ty.Int); T.PVar ("y", Ty.Int)], [Ty.Int; Ty.Int])
             , T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int]) ) ]
         , T.Var ["x"] )) ;
    test "let f x = [x] in f 1; f true"
      (T.Let
         ( [ ( T.PVar
                 ("f", Ty.Fun ([Ty.Poly 0], Ty.Variant ([Ty.Poly 0], ["list"])))
             , T.Fun
                 ( [("x", Ty.Poly 0)]
                 , T.CtorApp
                     ( ["::"]
                     , [ T.Var ["x"]
                       ; T.Ctor (["[]"], Ty.Variant ([Ty.Poly 0], ["list"])) ]
                     , Ty.Variant ([Ty.Poly 0], ["list"]) )
                 , Ty.Variant ([Ty.Poly 0], ["list"]) ) ) ]
         , T.App
             ( T.Var [";"]
             , [ T.App (T.Var ["f"], [T.Int 1])
               ; T.App (T.Var ["f"], [T.Bool true]) ] ) )) ;
    test "let x = match (1, 2) with (x, y) -> x + y in x"
      (T.Let
         ( [ ( T.PVar ("x", Ty.Int)
             , T.Match
                 ( T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int])
                 , Ty.Tuple [Ty.Int; Ty.Int]
                 , [ ( T.PTuple
                         ( [T.PVar ("x", Ty.Int); T.PVar ("y", Ty.Int)]
                         , [Ty.Int; Ty.Int] )
                     , T.App (T.Var ["+"], [T.Var ["x"]; T.Var ["y"]]) ) ]
                 , Ty.Int ) ) ]
         , T.Var ["x"] ))

let () =
    let t1 = Types.Int in
    let t2 = T.fresh 1 in
    T.unify t1 t2 |> ignore ;
    unify_test "unify int u" t1 t2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    unify_test "unify u u " u1 (T.unify u1 u2) ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    let t = Types.Int in
    T.unify u1 t |> ignore ;
    T.unify u2 t |> ignore ;
    unify_test "unify 2" u1 u2 ;
    let t1 = T.fresh 0 in
    let t2 = T.fresh 1 in
    let t3 = T.fresh 2 in
    let t4 = T.fresh 3 in
    let t5 = t4 in
    T.unify t1 t2 |> ignore ;
    T.unify t3 t4 |> ignore ;
    T.unify t1 t3 |> ignore ;
    unify_test "unify 3" t1 t2 ;
    unify_test "unify 3" t3 t4 ;
    unify_test "unify 3" t1 t3 ;
    unify_test "unify 3" t2 t4 ;
    unify_test "unify 3" t2 t3 ;
    unify_test "unify 3" t1 t4 ;
    unify_test "unify 3" t1 t5 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    let u3 = T.unify u1 u2 in
    T.unify Ty.Int u1 |> ignore ;
    unify_test "unify 4" u1 u3 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    let u3 = T.unify u1 u2 in
    T.unify Ty.Int u3 |> ignore ;
    unify_test "unify 5" u1 u3 ;
    let u1 = T.fresh 0 in
    let u2 = u1 in
    T.unify u1 Ty.Int |> ignore ;
    unify_test "unify 6" u2 Ty.Int

let () =
    let def1 = (["t1"], [], Ast.Alias (Ast.TTuple [Ast.TInt; Ast.TInt])) in
    let def2 = (["t2"], [], Ast.Alias (Ast.TApp ([], ["t1"]))) in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "simple conv" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "chain" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def1 =
        (["t1"], ["a"], Ast.Alias (Ast.TTuple [Ast.TVar "a"; Ast.TVar "a"]))
    in
    let def2 =
        ( ["t2"]
        , []
        , Ast.Alias
            (Ast.TTuple
               [Ast.TApp ([Ast.TInt], ["t1"]); Ast.TApp ([Ast.TInt], ["t1"])])
        )
    in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "higher alias" def ([], Types.Tuple [Poly 0; Poly 0]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "higher alias" def
      ( []
      , Types.Tuple
          [ Types.Tuple [Types.Int; Types.Int]
          ; Types.Tuple [Types.Int; Types.Int] ] ) ;
    let def1 = (["t1"], [], Ast.Alias (Ast.TApp ([], ["t1"]))) in
    ( try
        Typing.canonical_type_def [] [def1; def2] def1 |> ignore ;
        failwith "test failure"
      with
    | Failure msg ->
        Test.assert_eq "recursive detection" msg "recursive definition"
    | e -> raise e ) ;
    let def1 =
        ( ["t1"]
        , ["a"; "b"]
        , Ast.Alias
            (Ast.TTuple
               [ Ast.TApp ([Ast.TVar "a"], ["list"])
               ; Ast.TApp ([Ast.TVar "b"], ["list"]) ]) )
    in
    let def2 =
        (["t2"], ["a"], Ast.Alias (Ast.TApp ([Ast.TInt; Ast.TVar "a"], ["t1"])))
    in
    let def =
        Typing.canonical_type_def Types.pervasive_types [def1; def2] def2
    in
    Test.assert_eq "chain to defined type" def
      ( []
      , Types.Tuple
          [ Types.Variant ([Types.Int], ["list"])
          ; Types.Variant ([Types.Poly 0], ["list"]) ] ) ;
    let def1 =
        ( ["t1"]
        , ["a"]
        , Ast.Variant
            [("A", [Ast.TVar "a"]); ("B", [Ast.TApp ([Ast.TInt], ["t2"])])] )
    in
    let def2 = (["t2"], ["a"], Ast.Alias (Ast.TApp ([Ast.TVar "a"], ["t1"]))) in
    let def =
        Typing.canonical_type_def Types.pervasive_types [def1; def2] def1
    in
    Test.assert_eq "matual recursive variatn" def
      ( [ (["A"], ([Poly 0], [Poly 0], ["t1"]))
        ; (["B"], ([Types.Variant ([Types.Int], ["t1"])], [Poly 0], ["t1"])) ]
      , Types.Variant ([Poly 0], ["t1"]) )
