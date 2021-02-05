let test src expected =
    Printf.printf "testing \"%s\"..." src ;
    let lexed = Lex.lex src @@ Lex.initial_pos "test.ml" in
    let s, _ = Parser.parse_expr lexed in
    let ast = Ast.of_parser_t s in
    let env = Alpha.init () in
    let alpha = Alpha.of_expr env ast in
    let typed = Typing.f alpha in
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
    test "not" (Typing.Var 12) ;
    test "not true" (Typing.App (Typing.Var 12, [Typing.Bool true])) ;
    test "1+2" (Typing.App (Typing.Var 0, [Typing.Int 1; Typing.Int 2])) ;
    test "let x = 1 in x"
      (Typing.Let ([(Typing.PVar (15, Ty.Int), Typing.Int 1)], Typing.Var 15)) ;
    test "let x = 1 + 1 in x"
      (Typing.Let
         ( [ ( Typing.PVar (15, Ty.Int)
             , Typing.App (Typing.Var 0, [Typing.Int 1; Typing.Int 1]) ) ]
         , Typing.Var 15 )) ;
    test "let id = fun x -> x in id 1; id true"
      (Typing.Let
         ( [ ( Typing.PVar (15, Ty.Fun ([Ty.Poly 0], Ty.Poly 0))
             , Typing.Fun ([(16, Ty.Poly 0)], Typing.Var 16, Ty.Poly 0) ) ]
         , Typing.App
             ( Typing.Var 8
             , [ Typing.App (Typing.Var 15, [Typing.Int 1])
               ; Typing.App (Typing.Var 15, [Typing.Bool true]) ] ) )) ;
    test
      "let mk_pair x y = (x, y) in let a = mk_pair 1 in let b = mk_pair true \
       false in a"
      (T.Let
         ( [ ( T.PVar
                 ( 15
                 , Ty.Fun
                     ([Ty.Poly 0; Ty.Poly 1], Ty.Tuple [Ty.Poly 0; Ty.Poly 1])
                 )
             , T.Fun
                 ( [(16, Ty.Poly 0); (17, Ty.Poly 1)]
                 , T.Tuple ([T.Var 16; T.Var 17], [Ty.Poly 0; Ty.Poly 1])
                 , Ty.Tuple [Ty.Poly 0; Ty.Poly 1] ) ) ]
         , T.Let
             ( [ ( T.PVar
                     (18, Ty.Fun ([Ty.Poly 0], Ty.Tuple [Ty.Int; Ty.Poly 0]))
                 , T.App (T.Var 15, [T.Int 1]) ) ]
             , T.Let
                 ( [ ( T.PVar (19, Ty.Tuple [Ty.Bool; Ty.Bool])
                     , T.App (T.Var 15, [T.Bool true; T.Bool false]) ) ]
                 , Typing.Var 18 ) ) )) ;
    let f, x, g, y = (15, 16, 17, 18) in
    test "let f x = let g y = x = y in g in f"
      (T.Let
         ( [ ( T.PVar (f, Ty.Fun ([Ty.Poly 0], Ty.Fun ([Ty.Poly 0], Ty.Bool)))
             , T.Fun
                 ( [(x, Ty.Poly 0)]
                 , T.Let
                     ( [ ( T.PVar (g, Ty.Fun ([Ty.Poly 0], Ty.Bool))
                         , T.Fun
                             ( [(y, Ty.Poly 0)]
                             , T.App (T.Var 7, [T.Var x; T.Var y])
                             , Ty.Bool ) ) ]
                     , T.Var g )
                 , Ty.Fun ([Ty.Poly 0], Ty.Bool) ) ) ]
         , T.Var f )) ;
    let fact, n = (15, 16) in
    test "let rec fact n = if n = 1 then 1 else n * fact (n-1) in fact 5"
      (T.LetRec
         ( [ ( fact
             , Ty.Fun ([Ty.Int], Ty.Int)
             , T.Fun
                 ( [(n, Ty.Int)]
                 , T.If
                     ( T.App (T.Var 7, [T.Var n; T.Int 1])
                     , T.Int 1
                     , T.App
                         ( T.Var 2
                         , [ T.Var n
                           ; T.App
                               ( T.Var fact
                               , [T.App (T.Var 1, [T.Var n; T.Int 1])] ) ] ) )
                 , Ty.Int ) ) ]
         , T.App (T.Var fact, [T.Int 5]) )) ;
    test "let x, y = 1, 2 in x"
      (T.Let
         ( [ ( T.PTuple
                 ([T.PVar (15, Ty.Int); T.PVar (16, Ty.Int)], [Ty.Int; Ty.Int])
             , T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int]) ) ]
         , T.Var 15 )) ;
    test "let f x = [x] in f 1; f true"
      (T.Let
         ( [ ( T.PVar (15, Ty.Fun ([Ty.Poly 0], Ty.Variant ([Ty.Poly 0], 5)))
             , T.Fun
                 ( [(16, Ty.Poly 0)]
                 , T.CtorApp
                     ( 2
                     , [T.Var 16; T.Ctor (3, Ty.Variant ([Ty.Poly 0], 5))]
                     , Ty.Variant ([Ty.Poly 0], 5) )
                 , Ty.Variant ([Ty.Poly 0], 5) ) ) ]
         , T.App
             ( T.Var 8
             , [T.App (T.Var 15, [T.Int 1]); T.App (T.Var 15, [T.Bool true])] )
         )) ;
    test "let x = match (1, 2) with (x, y) -> x + y in x"
      (T.Let
         ( [ ( T.PVar (15, Ty.Int)
             , T.Match
                 ( T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int])
                 , Ty.Tuple [Ty.Int; Ty.Int]
                 , [ ( T.PTuple
                         ( [T.PVar (16, Ty.Int); T.PVar (17, Ty.Int)]
                         , [Ty.Int; Ty.Int] )
                     , T.App (T.Var 0, [T.Var 16; T.Var 17]) ) ]
                 , Ty.Int ) ) ]
         , T.Var 15 )) ;
    let length, l, x = (15, 16, 17) in
    test
      "let rec length l = match l with [] -> 0 | x :: [] -> 1 + length l in ()"
      (T.LetRec
         ( [ ( length
             , Ty.Fun ([Ty.Variant ([Ty.Poly 0], 5)], Ty.Int)
             , T.Fun
                 ( [(l, Ty.Variant ([Ty.Poly 0], 5))]
                 , T.Match
                     ( T.Var l
                     , Ty.Variant ([Ty.Poly 0], 5)
                     , [ ( T.PCtor
                             ( []
                             , [Ty.Poly 0]
                             , Alpha.lookup ["[]"] Alpha.pervasive_cenv )
                         , T.Int 0 )
                       ; ( T.PCtor
                             ( [ T.PVar (x, Ty.Poly 0)
                               ; T.PCtor
                                   ( []
                                   , [Ty.Poly 0]
                                   , Alpha.lookup ["[]"] Alpha.pervasive_cenv )
                               ]
                             , [Ty.Poly 0]
                             , Alpha.lookup ["::"] Alpha.pervasive_cenv )
                         , T.App
                             ( T.Var 0
                             , [T.Int 1; T.App (T.Var length, [T.Var l])] ) ) ]
                     , Ty.Int )
                 , Ty.Int ) ) ]
         , T.Tuple ([], []) ))

let () =
    let t1 = Types.Int in
    let t2 = T.fresh 1 in
    T.unify t1 t2 ;
    unify_test "unify int u" t1 t2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify u1 u2 ;
    unify_test "unify u u " u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    let t = Types.Int in
    T.unify u1 t ;
    T.unify u2 t ;
    unify_test "unify 2" u1 u2 ;
    let t1 = T.fresh 0 in
    let t2 = T.fresh 1 in
    let t3 = T.fresh 2 in
    let t4 = T.fresh 3 in
    let t5 = t4 in
    T.unify t1 t2 ;
    T.unify t3 t4 ;
    T.unify t1 t3 ;
    unify_test "unify 3" t1 t2 ;
    unify_test "unify 3" t3 t4 ;
    unify_test "unify 3" t1 t3 ;
    unify_test "unify 3" t2 t4 ;
    unify_test "unify 3" t2 t3 ;
    unify_test "unify 3" t1 t4 ;
    unify_test "unify 3" t1 t5 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify u1 u2 ;
    T.unify Ty.Int u1 ;
    unify_test "unify 4" u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify u1 u2 ;
    T.unify Ty.Int u1 ;
    unify_test "unify 5" u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = u1 in
    T.unify u1 Ty.Int ;
    unify_test "unify 6" u2 Ty.Int

let () =
    let def1 = (6, 0, Alpha.Alias (Alpha.TTuple [Alpha.TInt; Alpha.TInt])) in
    let def2 = (7, 0, Alpha.Alias (Alpha.TApp ([], 6))) in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "simple conv" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "chain" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def1 =
        (6, 1, Alpha.Alias (Alpha.TTuple [Alpha.TVar 0; Alpha.TVar 0]))
    in
    let def2 =
        ( 7
        , 0
        , Alpha.Alias
            (Alpha.TTuple
               [Alpha.TApp ([Alpha.TInt], 6); Alpha.TApp ([Alpha.TInt], 6)]) )
    in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "higher alias" def ([], Types.Tuple [Poly 0; Poly 0]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "higher alias" def
      ( []
      , Types.Tuple
          [ Types.Tuple [Types.Int; Types.Int]
          ; Types.Tuple [Types.Int; Types.Int] ] ) ;
    let def1 = (6, 0, Alpha.Alias (Alpha.TApp ([], 6))) in
    ( try
        Typing.canonical_type_def [] [def1; def2] def1 |> ignore ;
        failwith "test failure"
      with
    | T.CyclicType -> ()
    | e -> raise e ) ;
    let def1 =
        ( 6
        , 2
        , Alpha.Alias
            (Alpha.TTuple
               [Alpha.TApp ([Alpha.TVar 0], 5); Alpha.TApp ([Alpha.TVar 1], 5)])
        )
    in
    let def2 =
        (7, 1, Alpha.Alias (Alpha.TApp ([Alpha.TInt; Alpha.TVar 0], 6)))
    in
    let def = Typing.canonical_type_def T.pervasive_tenv [def1; def2] def2 in
    Test.assert_eq "chain to defined type" def
      ( []
      , Types.Tuple
          [Types.Variant ([Types.Int], 5); Types.Variant ([Types.Poly 0], 5)] ) ;
    let def1 =
        ( 6
        , 1
        , Alpha.Variant
            [(4, [Alpha.TVar 0]); (5, [Alpha.TApp ([Alpha.TInt], 7)])] )
    in
    let def2 = (7, 1, Alpha.Alias (Alpha.TApp ([Alpha.TVar 0], 6))) in
    let def = Typing.canonical_type_def T.pervasive_tenv [def1; def2] def1 in
    Test.assert_eq "matual recursive variatn" def
      ( [ (4, ([Poly 0], [Poly 0], 6))
        ; (5, ([Types.Variant ([Types.Int], 6)], [Poly 0], 6)) ]
      , Types.Variant ([Poly 0], 6) )
