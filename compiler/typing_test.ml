let nw = Lex.nowhere

let test src expected =
    Printf.printf "testing \"%s\"..." src ;
    let lexed = Lex.lex src @@ Lex.initial_pos "test.ml" in
    let s, _, _ = Parser.parse_expr lexed in
    let ast = Ast.of_parser_t s in
    let env = Alpha.init () in
    let alpha = Alpha.of_expr "" env ast in
    let typed = Typing.f alpha in
    if typed = expected then Printf.printf "ok\n"
    else (
      Printf.printf "left: \n%s\n" @@ Typing.show typed ;
      Printf.printf "right: \n%s\n" @@ Typing.show expected ;
      failwith "test failed" )

let test_stmt src expected =
    Printf.printf "testing \"%s\"..." src ;
    let lexed = Lex.lex src @@ Lex.initial_pos "test.ml" in
    let s = Parser.parse_stmts lexed in
    let ast = Ast.of_parser_t s in
    let env = Alpha.init () in
    let alpha = Alpha.of_expr "" env ast in
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

let lv n = Alpha.lookup Lex.nowhere n Alpha.pervasive_venv

let v n = Typing.Var (lv n)

let vn n = Types.Vid (n + List.length Alpha.pervasive_venv)

let v' n = Typing.Var (vn n)

let lt n = Alpha.lookup Lex.nowhere n Alpha.pervasive_tenv

let tn n = Types.Tid (n + List.length Alpha.pervasive_tenv)

let lc n = Alpha.lookup Lex.nowhere n Alpha.pervasive_cenv

let cn n = Types.Cid (n + List.length Alpha.pervasive_cenv)

let poly n = Types.Poly (Types.Pid n)

let pos i = ("test.ml", 1, i, i)

let () =
    test "0" (Typing.Int 0) ;
    test "not" (v ["not"]) ;
    test "not true" (Typing.App (v ["not"], [Typing.Bool true])) ;
    test "1+2" (Typing.App (v ["+"], [Typing.Int 1; Typing.Int 2])) ;
    test "let x = 1 in x"
      (Typing.Let ([(Typing.PVar (vn 0, Ty.Int), Typing.Int 1)], v' 0)) ;
    test "let x = 1 + 1 in x"
      (Typing.Let
         ( [ ( Typing.PVar (vn 0, Ty.Int)
             , Typing.App (v ["+"], [Typing.Int 1; Typing.Int 1]) ) ]
         , v' 0 )) ;
    test "let id = fun x -> x in id 1; id true"
      (Typing.Let
         ( [ ( Typing.PVar (vn 0, Ty.Fun ([poly 0], poly 0))
             , Typing.Fun ([(vn 1, poly 0)], v' 1, poly 0, "_id_1", pos 19) ) ]
         , Typing.App
             ( v [";"]
             , [ Typing.App (v' 0, [Typing.Int 1])
               ; Typing.App (v' 0, [Typing.Bool true]) ] ) )) ;
    test
      "let mk_pair x y = (x, y) in let a = mk_pair 1 in let b = mk_pair true \
       false in a"
      (T.Let
         ( [ ( T.PVar
                 (vn 0, Ty.Fun ([poly 0; poly 1], Ty.Tuple [poly 0; poly 1]))
             , T.Fun
                 ( [(vn 1, poly 0); (vn 2, poly 1)]
                 , T.Tuple ([v' 1; v' 2], [poly 0; poly 1])
                 , Ty.Tuple [poly 0; poly 1]
                 , "_mk_pair_1"
                 , pos 5 ) ) ]
         , T.Let
             ( [ ( T.PVar (vn 3, Ty.Fun ([poly 0], Ty.Tuple [Ty.Int; poly 0]))
                 , T.App (v' 0, [T.Int 1]) ) ]
             , T.Let
                 ( [ ( T.PVar (vn 4, Ty.Tuple [Ty.Bool; Ty.Bool])
                     , T.App (v' 0, [T.Bool true; T.Bool false]) ) ]
                 , v' 3 ) ) )) ;
    let f, x, g, y = (0, 1, 2, 3) in
    test "let f x = let g y = x = y in g in\n   f"
      (T.Let
         ( [ ( T.PVar (vn f, Ty.Fun ([poly 0], Ty.Fun ([poly 0], Ty.Bool)))
             , T.Fun
                 ( [(vn x, poly 0)]
                 , T.Let
                     ( [ ( T.PVar (vn g, Ty.Fun ([poly 0], Ty.Bool))
                         , T.Fun
                             ( [(vn y, poly 0)]
                             , T.App (v ["="], [v' x; v' y])
                             , Ty.Bool
                             , "_f_1_g_2"
                             , pos 15 ) ) ]
                     , v' g )
                 , Ty.Fun ([poly 0], Ty.Bool)
                 , "_f_1"
                 , pos 5 ) ) ]
         , v' f )) ;
    let fact, n = (0, 1) in
    test "let rec fact n = if n = 1 then 1 else n * fact (n-1) in\n   fact 5"
      (T.LetRec
         ( [ ( vn fact
             , Ty.Fun ([Ty.Int], Ty.Int)
             , T.Fun
                 ( [(vn n, Ty.Int)]
                 , T.If
                     ( T.App (v ["="], [v' n; T.Int 1])
                     , T.Int 1
                     , T.App
                         ( v ["*"]
                         , [ v' n
                           ; T.App (v' fact, [T.App (v ["-"], [v' n; T.Int 1])])
                           ] ) )
                 , Ty.Int
                 , "_fact_1"
                 , pos 9 ) ) ]
         , T.App (v' fact, [T.Int 5]) )) ;
    test "let x,\n   y = 1, 2 in x"
      (T.Let
         ( [ ( T.PTuple
                 ( [T.PVar (vn 0, Ty.Int); T.PVar (vn 1, Ty.Int)]
                 , [Ty.Int; Ty.Int] )
             , T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int]) ) ]
         , v' 0 )) ;
    test "let f x = [x] in f 1; f true"
      (T.Let
         ( [ ( T.PVar
                 (vn 0, Ty.Fun ([poly 0], Ty.Variant ([poly 0], lt ["list"])))
             , T.Fun
                 ( [(vn 1, poly 0)]
                 , T.CtorApp
                     ( lc ["::"]
                     , [ v' 1
                       ; T.CtorApp
                           (lc ["[]"], [], Ty.Variant ([poly 0], lt ["list"]))
                       ]
                     , Ty.Variant ([poly 0], lt ["list"]) )
                 , Ty.Variant ([poly 0], lt ["list"])
                 , "_f_1"
                 , pos 5 ) ) ]
         , T.App
             (v [";"], [T.App (v' 0, [T.Int 1]); T.App (v' 0, [T.Bool true])])
         )) ;
    test "let x = match (1, 2) with (x, y) -> x + y in x"
      (T.Let
         ( [ ( T.PVar (vn 0, Ty.Int)
             , T.Match
                 ( T.Tuple ([T.Int 1; T.Int 2], [Ty.Int; Ty.Int])
                 , Ty.Tuple [Ty.Int; Ty.Int]
                 , [ ( T.PTuple
                         ( [T.PVar (vn 1, Ty.Int); T.PVar (vn 2, Ty.Int)]
                         , [Ty.Int; Ty.Int] )
                     , T.App (v ["+"], [v' 1; v' 2]) ) ]
                 , Ty.Int ) ) ]
         , v' 0 )) ;
    let length, l, x = (0, 1, 2) in
    test
      "let rec length l =\n\
      \   match l with [] -> 0 | x :: [] -> 1 + length l in ()"
      (T.LetRec
         ( [ ( vn length
             , Ty.Fun ([Ty.Variant ([poly 0], lt ["list"])], Ty.Int)
             , T.Fun
                 ( [(vn l, Ty.Variant ([poly 0], lt ["list"]))]
                 , T.Match
                     ( v' l
                     , Ty.Variant ([poly 0], lt ["list"])
                     , [ ( T.PCtor
                             ( []
                             , [poly 0]
                             , Alpha.lookup Lex.nowhere ["[]"]
                                 Alpha.pervasive_cenv )
                         , T.Int 0 )
                       ; ( T.PCtor
                             ( [ T.PVar (vn x, poly 0)
                               ; T.PCtor
                                   ( []
                                   , [poly 0]
                                   , Alpha.lookup Lex.nowhere ["[]"]
                                       Alpha.pervasive_cenv ) ]
                             , [poly 0]
                             , Alpha.lookup Lex.nowhere ["::"]
                                 Alpha.pervasive_cenv )
                         , T.App (v ["+"], [T.Int 1; T.App (v' length, [v' l])])
                         ) ]
                     , Ty.Int )
                 , Ty.Int
                 , "_length_1"
                 , pos 9 ) ) ]
         , T.Tuple ([], []) )) ;
    let map, f, l, x, xs = (0, 1, 2, 3, 4) in
    let l1_ty = Ty.Variant ([poly 1], lt ["list"]) in
    let l2_ty = Ty.Variant ([poly 0], lt ["list"]) in
    let f_ty = Ty.Fun ([poly 1], poly 0) in
    let map_ty = Ty.Fun ([f_ty; l1_ty], l2_ty) in
    test
      "let rec map f l = match l with x :: xs -> (f x)\n\
      \   :: map f xs | [] -> []  in map"
      (T.LetRec
         ( [ ( vn map
             , map_ty
             , T.Fun
                 ( [(vn f, f_ty); (vn l, l1_ty)]
                 , T.Match
                     ( v' l
                     , l1_ty
                     , [ ( T.PCtor
                             ( [T.PVar (vn x, poly 1); T.PVar (vn xs, l1_ty)]
                             , [poly 1]
                             , lc ["::"] )
                         , T.CtorApp
                             ( lc ["::"]
                             , [ T.App (v' f, [v' x])
                               ; T.App (v' map, [v' f; v' xs]) ]
                             , l2_ty ) )
                       ; ( T.PCtor ([], [poly 1], lc ["[]"])
                         , T.CtorApp (lc ["[]"], [], l2_ty) ) ]
                     , l2_ty )
                 , l2_ty
                 , "_map_1"
                 , pos 9 ) ) ]
         , v' map )) ;
    test_stmt
      "type 'a t = A of 'a * ('a t) | B let rec total l = match l with | A (x, \
       xs) -> x + total xs | B -> 0"
      (Typing.LetRec
         ( [ ( vn 0
             , Types.Fun ([Types.Variant ([Types.Int], tn 0)], Types.Int)
             , Typing.Fun
                 ( [(vn 1, Types.Variant ([Types.Int], tn 0))]
                 , Typing.Match
                     ( v' 1
                     , Types.Variant ([Types.Int], tn 0)
                     , [ ( Typing.PCtor
                             ( [ Typing.PVar (vn 2, Int)
                               ; Typing.PVar
                                   (vn 3, Types.Variant ([Types.Int], tn 0)) ]
                             , [Int]
                             , cn 0 )
                         , Typing.App
                             (v ["+"], [v' 2; Typing.App (v' 0, [v' 3])]) )
                       ; (Typing.PCtor ([], [Int], cn 1), Typing.Int 0) ]
                     , Int )
                 , Int
                 , "_total_1"
                 , pos 42 ) ) ]
         , Typing.Never ))

let () =
    let t1 = Types.Int in
    let t2 = T.fresh 1 in
    T.unify nw t1 t2 ;
    unify_test "unify int u" t1 t2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify nw u1 u2 ;
    unify_test "unify u u " u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    let t = Types.Int in
    T.unify nw u1 t ;
    T.unify nw u2 t ;
    unify_test "unify 2" u1 u2 ;
    let t1 = T.fresh 0 in
    let t2 = T.fresh 1 in
    let t3 = T.fresh 2 in
    let t4 = T.fresh 3 in
    let t5 = t4 in
    T.unify nw t1 t2 ;
    T.unify nw t3 t4 ;
    T.unify nw t1 t3 ;
    unify_test "unify 3" t1 t2 ;
    unify_test "unify 3" t3 t4 ;
    unify_test "unify 3" t1 t3 ;
    unify_test "unify 3" t2 t4 ;
    unify_test "unify\n   3" t2 t3 ;
    unify_test "unify 3" t1 t4 ;
    unify_test "unify 3" t1 t5 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify nw u1 u2 ;
    T.unify nw Ty.Int u1 ;
    unify_test "unify 4" u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = T.fresh 0 in
    T.unify nw u1 u2 ;
    T.unify nw Ty.Int u1 ;
    unify_test "unify 5" u1 u2 ;
    let u1 = T.fresh 0 in
    let u2 = u1 in
    T.unify nw u1 Ty.Int ;
    unify_test "unify 6" u2 Ty.Int

let () =
    let def1 =
        ( tn 0
        , nw
        , 0
        , Alpha.Alias (Alpha.TTuple ([Alpha.TInt nw; Alpha.TInt nw], nw)) )
    in
    let def2 = (tn 1, nw, 0, Alpha.Alias (Alpha.TApp ([], tn 0, nw))) in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "simple conv" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "chain" def ([], Types.Tuple [Types.Int; Types.Int]) ;
    let def1 =
        ( tn 0
        , nw
        , 1
        , Alpha.Alias
            (Alpha.TTuple ([Alpha.TVar (0, nw); Alpha.TVar (0, nw)], nw)) )
    in
    let def2 =
        ( tn 1
        , nw
        , 0
        , Alpha.Alias
            (Alpha.TTuple
               ( [ Alpha.TApp ([Alpha.TInt nw], tn 0, nw)
                 ; Alpha.TApp ([Alpha.TInt nw], tn 0, nw) ]
               , nw )) )
    in
    let def = Typing.canonical_type_def [] [def1; def2] def1 in
    Test.assert_eq "higher alias" def ([], Types.Tuple [poly 0; poly 0]) ;
    let def = Typing.canonical_type_def [] [def1; def2] def2 in
    Test.assert_eq "higher alias" def
      ( []
      , Types.Tuple
          [ Types.Tuple [Types.Int; Types.Int]
          ; Types.Tuple [Types.Int; Types.Int] ] ) ;
    let def1 = (tn 0, nw, 0, Alpha.Alias (Alpha.TApp ([], tn 0, nw))) in
    ( try
        Typing.canonical_type_def [] [def1; def2] def1 |> ignore ;
        failwith "test failure"
      with
    | T.CyclicType _ -> ()
    | e -> raise e ) ;
    let def1 =
        ( tn 0
        , nw
        , 2
        , Alpha.Alias
            (Alpha.TTuple
               ( [ Alpha.TApp ([Alpha.TVar (0, nw)], lt ["list"], nw)
                 ; Alpha.TApp ([Alpha.TVar (1, nw)], lt ["list"], nw) ]
               , nw )) )
    in
    let def2 =
        ( tn 1
        , nw
        , 1
        , Alpha.Alias
            (Alpha.TApp ([Alpha.TInt nw; Alpha.TVar (0, nw)], tn 0, nw)) )
    in
    let def = Typing.canonical_type_def T.pervasive_tenv [def1; def2] def2 in
    Test.assert_eq "chain to defined type" def
      ( []
      , Types.Tuple
          [ Types.Variant ([Types.Int], lt ["list"])
          ; Types.Variant ([poly 0], lt ["list"]) ] ) ;
    let def1 =
        ( tn 0
        , nw
        , 1
        , Alpha.Variant
            [ (cn 0, nw, [Alpha.TVar (0, nw)])
            ; (cn 1, nw, [Alpha.TApp ([Alpha.TInt nw], tn 1, nw)]) ] )
    in
    let def2 =
        (tn 1, nw, 1, Alpha.Alias (Alpha.TApp ([Alpha.TVar (0, nw)], tn 0, nw)))
    in
    let def = Typing.canonical_type_def T.pervasive_tenv [def1; def2] def1 in
    Test.assert_eq "matual recursive variatn" def
      ( [ (cn 0, ([poly 0], [poly 0], tn 0))
        ; (cn 1, ([Types.Variant ([Types.Int], tn 0)], [poly 0], tn 0)) ]
      , Types.Variant ([poly 0], tn 0) )
