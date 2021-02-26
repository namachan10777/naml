module P = Parser

let rec eq_pat l r =
    match (l, r) with
    | P.PInt (i1, _), P.PInt (i2, _) -> i1 = i2
    | P.PVar (id1, _), P.PVar (id2, _) -> Id.approx id1 id2
    | P.PParen p1, P.PParen p2 -> eq_pat p1 p2
    | P.PTuple (ps1, _), P.PTuple (ps2, _) ->
        List.for_all (fun (a, b) -> eq_pat a b) @@ Util.zip ps1 ps2
    | P.PAs (ps1, _), P.PAs (ps2, _) ->
        List.for_all (fun (a, b) -> eq_pat a b) @@ Util.zip ps1 ps2
    | P.PEmp _, P.PEmp _ -> true
    | P.PCons (l1, r1, _), P.PCons (l2, r2, _) -> eq_pat l1 l2 && eq_pat r1 r2
    | P.PCtorApp (id1, p1, _), P.PCtorApp (id2, p2, _) ->
        Id.approx id1 id2 && eq_pat p1 p2
    | P.PCtor (id1, _), P.PCtor (id2, _) -> Id.approx id1 id2
    | a, b ->
        Printf.printf "%s\n%s" (P.show_pat_t a) (P.show_pat_t b) ;
        false

let rec eq_ty l r =
    match (l, r) with
    | P.TInt _, P.TInt _ -> true
    | P.TTuple (ts1, _), P.TTuple (ts2, _) ->
        List.for_all (fun (a, b) -> eq_ty a b) @@ Util.zip ts1 ts2
    | P.TApp (ts1, name1, _), P.TApp (ts2, name2, _) ->
        Id.approx name1 name2
        && (List.for_all (fun (a, b) -> eq_ty a b) @@ Util.zip ts1 ts2)
    | P.TParen t1, P.TParen t2 -> eq_ty t1 t2
    | P.TVar (id1, _), P.TVar (id2, _) -> id1 = id2
    | a, b ->
        Printf.printf "%s\n%s" (P.show_ty_t a) (P.show_ty_t b) ;
        false

let rec eq_tydef l r =
    match (l, r) with
    | P.Alias ty1, P.Alias ty2 -> eq_ty ty2 ty1
    | P.Variant defs1, P.Variant defs2 ->
        List.for_all (fun ((name1, _, tys1), (name2, _, tys2)) ->
            Id.approx name1 name2
            && (List.for_all (fun (t1, t2) -> eq_ty t1 t2) @@ Util.zip tys1 tys2))
        @@ Util.zip defs1 defs2
    | a, b ->
        Printf.printf "%s\n%s" (P.show_tydef_t a) (P.show_tydef_t b) ;
        false

let rec eq l r =
    match (l, r) with
    | P.Never, P.Never -> true
    | P.Int (i1, _), P.Int (i2, _) -> i1 = i2
    | P.Bool (b1, _), P.Bool (b2, _) -> b1 = b2
    | P.Neg (e1, _), P.Neg (e2, _) -> eq e1 e2
    | P.Var (id1, _), P.Var (id2, _) -> Id.approx id1 id2
    | P.Paren e1, P.Paren e2 -> eq e1 e2
    | P.Emp _, P.Emp _ -> true
    | P.Ctor (id1, _), P.Ctor (id2, _) -> Id.approx id1 id2
    | P.If (c1, t1, e1, _), P.If (c2, t2, e2, _) ->
        eq c1 c2 && eq t1 t2 && eq e1 e2
    | P.Assign (a1, v1, _), P.Assign (a2, v2, _) -> eq a1 a2 && eq v1 v2
    | P.ArrayAssign (a1, i1, v1, _), P.ArrayAssign (a2, i2, v2, _) ->
        eq a1 a2 && eq i1 i2 && eq v1 v2
    | P.Add (l1, r1, _), P.Add (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Sub (l1, r1, _), P.Sub (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Mul (l1, r1, _), P.Mul (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Div (l1, r1, _), P.Div (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Mod (l1, r1, _), P.Mod (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.And (l1, r1, _), P.And (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Or (l1, r1, _), P.Or (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Eq (l1, r1, _), P.Eq (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Neq (l1, r1, _), P.Neq (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Gret (l1, r1, _), P.Gret (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Less (l1, r1, _), P.Less (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Seq (l1, r1, _), P.Seq (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Index (l1, r1, _), P.Index (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.Cons (l1, r1, _), P.Cons (l2, r2, _) -> eq l1 l2 && eq r1 r2
    | P.App (f1, a1, _), P.App (f2, a2, _) -> eq f1 f2 && eq a1 a2
    | P.Pipeline (a1, f1, _), P.Pipeline (a2, f2, _) -> eq f1 f2 && eq a1 a2
    | P.Tuple (es1, _), P.Tuple (es2, _) ->
        List.for_all (fun (a, b) -> eq a b) @@ Util.zip es1 es2
    | P.Let (defs1, e1, is_top), P.Let (defs2, e2, is_top') ->
        eq e1 e2 && is_top = is_top'
        && List.for_all (fun ((pat1, _, def1), (pat2, _, def2)) ->
               eq_pat pat1 pat2 && eq def1 def2)
           @@ Util.zip defs1 defs2
    | P.LetRec (defs1, e1, is_top), P.LetRec (defs2, e2, is_top') ->
        eq e1 e2 && is_top = is_top'
        && List.for_all (fun ((id1, _, def1), (id2, _, def2)) ->
               Id.approx id1 id2 && eq def1 def2)
           @@ Util.zip defs1 defs2
    | P.Fun (arg1, e1, _), P.Fun (arg2, e2, _) ->
        eq e1 e2 && Id.approx arg1 arg2
    | P.Match (e1, arms1), P.Match (e2, arms2) ->
        eq e1 e2
        && List.for_all (fun ((pat1, _, guard1, e1), (pat2, _, guard2, e2)) ->
               eq_pat pat1 pat2 && eq guard1 guard2 && eq e1 e2)
           @@ Util.zip arms1 arms2
    | P.Type (defs1, e1), P.Type (defs2, e2) ->
        List.for_all (fun ((name1, _, targs1, ty1), (name2, _, targs2, ty2)) ->
            Id.approx name1 name2
            && List.for_all (fun ((t1, _), (t2, _)) -> t1 = t2)
               @@ Util.zip targs1 targs2
            && eq_tydef ty1 ty2)
        @@ Util.zip defs1 defs2
    | a, b -> false

let test name src right =
    let left = P.parse @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    if eq right left then ()
    else
      failwith
      @@ Printf.sprintf "\"%s\" left  : %s\nright : %s\n" name (P.show left)
           (P.show right)

let test_ty name src right =
    let left =
        match P.parse_ty @@ Lex.lex src @@ Lex.initial_pos "test.ml" with
        | t, _, [(Lex.Eof, _)] -> t
        | _ -> failwith "parse failed"
    in
    if eq_ty right left then ()
    else
      failwith
      @@ Printf.sprintf "\"%s\" left  : %s\nright : %s\n" name
           (P.show_ty_t left) (P.show_ty_t right)

let test_stmts name src right =
    let left = P.parse_stmts @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    if eq right left then ()
    else
      failwith
      @@ Printf.sprintf "\"%s\" left  : %s\nright : %s\n" name (P.show left)
           (P.show right)

let p i = ("test.ml", 1, i, i)

let nw = Lex.nowhere

let () =
    test "parse_add" "0+1+2"
      (P.Add (P.Add (P.Int (0, nw), P.Int (1, nw), nw), P.Int (2, nw), nw)) ;
    test "parse_mul" "0*1*2"
      (P.Mul (P.Mul (P.Int (0, nw), P.Int (1, nw), nw), P.Int (2, nw), nw)) ;
    test "unary_minus" "1+-2"
      (P.Add (P.Int (1, nw), P.Neg (P.Int (2, nw), nw), nw)) ;
    test "mod" "3\n mod 2"
      (P.Mod
         (P.Int (3, nw), P.Int (2, ("test.ml", 2, 6, 8)), ("test.ml", 2, 2, 4))) ;
    test "add_r" "0+(1+2)"
      (P.Add
         (P.Int (0, nw), P.Paren (P.Add (P.Int (1, nw), P.Int (2, nw), nw)), nw)) ;
    test "4arith" "3*(200+300)/4-5"
      (P.Sub
         ( P.Div
             ( P.Mul
                 ( P.Int (3, nw)
                 , P.Paren (P.Add (P.Int (200, nw), P.Int (300, nw), nw))
                 , nw )
             , P.Int (4, nw)
             , nw )
         , P.Int (5, nw)
         , nw )) ;
    test "bool" "3>2 && not (1 < 0)"
      (P.And
         ( P.Gret (P.Int (3, nw), P.Int (2, nw), nw)
         , P.App
             ( P.Var (Id.from_strlist ["not"], nw)
             , P.Paren (P.Less (P.Int (1, nw), P.Int (0, nw), nw))
             , nw )
         , nw )) ;
    test "ref" "ref 1"
      (P.App (P.Var (Id.from_strlist ["ref"], nw), P.Int (1, nw), nw)) ;
    test "bool2" "true &&\n false || 1 = 2 && false"
      (P.Or
         ( P.And (P.Bool (true, nw), P.Bool (false, nw), nw)
         , P.And
             (P.Eq (P.Int (1, nw), P.Int (2, nw), nw), P.Bool (false, nw), nw)
         , nw )) ;
    test "let\n   simple" "let x = 1 in\n x"
      (P.Let
         ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
         , P.Var (Id.from_strlist ["x"], nw)
         , false )) ;
    test "let rec" "let rec x\n = 1 in x"
      (P.LetRec
         ( [(Id.from_strlist ["x"], nw, P.Int (1, nw))]
         , P.Var (Id.from_strlist ["x"], nw)
         , false )) ;
    test "let add left" "1 + let x = 1 in x"
      (P.Add
         ( P.Int (1, nw)
         , P.Let
             ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
             , P.Var (Id.from_strlist ["x"], nw)
             , false )
         , nw )) ;
    test "let add right" "(let x = 1\n   in x) + 1"
      (P.Add
         ( P.Paren
             (P.Let
                ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
                , P.Var (Id.from_strlist ["x"], nw)
                , false ))
         , P.Int (1, nw)
         , nw )) ;
    test "let complex" "let\n x = let y = 1 in y in let z = x in z"
      (P.Let
         ( [ ( P.PVar (Id.from_strlist ["x"], nw)
             , nw
             , P.Let
                 ( [(P.PVar (Id.from_strlist ["y"], nw), nw, P.Int (1, nw))]
                 , P.Var (Id.from_strlist ["y"], nw)
                 , false ) ) ]
         , P.Let
             ( [ ( P.PVar (Id.from_strlist ["z"], nw)
                 , nw
                 , P.Var (Id.from_strlist ["x"], nw) ) ]
             , P.Var (Id.from_strlist ["z"], nw)
             , false )
         , false )) ;
    test "let and" "let x = 1 and\n   y = 2 in y"
      (P.Let
         ( [ (P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))
           ; (P.PVar (Id.from_strlist ["y"], nw), nw, P.Int (2, nw)) ]
         , P.Var (Id.from_strlist ["y"], nw)
         , false )) ;
    test "let rec and" "let rec\n x = 1 and y = 2 in y"
      (P.LetRec
         ( [ (Id.from_strlist ["x"], nw, P.Int (1, nw))
           ; (Id.from_strlist ["y"], nw, P.Int (2, nw)) ]
         , P.Var (Id.from_strlist ["y"], nw)
         , false )) ;
    test "letfun" "let add\n   x y = x + y in add"
      (P.Let
         ( [ ( P.PVar (Id.from_strlist ["add"], nw)
             , nw
             , P.Fun
                 ( Id.from_strlist ["x"]
                 , P.Fun
                     ( Id.from_strlist ["y"]
                     , P.Add
                         ( P.Var (Id.from_strlist ["x"], nw)
                         , P.Var (Id.from_strlist ["y"], nw)
                         , nw )
                     , nw )
                 , nw ) ) ]
         , P.Var (Id.from_strlist ["add"], nw)
         , false )) ;
    Parser.count := 0 ;
    test "letfun_pat" "let add (x, y) (z, w) = x + y in add"
      (P.Let
         ( [ ( P.PVar (Id.from_strlist ["add"], nw)
             , nw
             , P.Fun
                 ( Id.from_strlist ["<anonymous2>"]
                 , P.Fun
                     ( Id.from_strlist ["<anonymous1>"]
                     , P.Match
                         ( P.Var (Id.from_strlist ["<anonymous2>"], nw)
                         , [ ( P.PParen
                                 (P.PTuple
                                    ( [ P.PVar (Id.from_strlist ["x"], nw)
                                      ; P.PVar (Id.from_strlist ["y"], nw) ]
                                    , nw ))
                             , nw
                             , P.Bool (true, nw)
                             , P.Match
                                 ( P.Var (Id.from_strlist ["<anonymous1>"], nw)
                                 , [ ( P.PParen
                                         (P.PTuple
                                            ( [ P.PVar
                                                  (Id.from_strlist ["z"], nw)
                                              ; P.PVar
                                                  (Id.from_strlist ["w"], nw) ]
                                            , nw ))
                                     , nw
                                     , P.Bool (true, nw)
                                     , P.Add
                                         ( P.Var (Id.from_strlist ["x"], nw)
                                         , P.Var (Id.from_strlist ["y"], nw)
                                         , nw ) ) ] ) ) ] )
                     , nw )
                 , nw ) ) ]
         , P.Var (Id.from_strlist ["add"], nw)
         , false )) ;
    test "letrecfun" "let rec add x\n y = x + y in add"
      (P.LetRec
         ( [ ( Id.from_strlist ["add"]
             , nw
             , P.Fun
                 ( Id.from_strlist ["x"]
                 , P.Fun
                     ( Id.from_strlist ["y"]
                     , P.Add
                         ( P.Var (Id.from_strlist ["x"], nw)
                         , P.Var (Id.from_strlist ["y"], nw)
                         , nw )
                     , nw )
                 , nw ) ) ]
         , P.Var (Id.from_strlist ["add"], nw)
         , false )) ;
    Parser.count := 0 ;
    test "letrecfun_pat" "let rec add (x, y) (z,\n   w) = x + y in add"
      (P.LetRec
         ( [ ( Id.from_strlist ["add"]
             , nw
             , P.Fun
                 ( Id.from_strlist ["<anonymous2>"]
                 , P.Fun
                     ( Id.from_strlist ["<anonymous1>"]
                     , P.Match
                         ( P.Var (Id.from_strlist ["<anonymous2>"], nw)
                         , [ ( P.PParen
                                 (P.PTuple
                                    ( [ P.PVar (Id.from_strlist ["x"], nw)
                                      ; P.PVar (Id.from_strlist ["y"], nw) ]
                                    , nw ))
                             , nw
                             , P.Bool (true, nw)
                             , P.Match
                                 ( P.Var (Id.from_strlist ["<anonymous1>"], nw)
                                 , [ ( P.PParen
                                         (P.PTuple
                                            ( [ P.PVar
                                                  (Id.from_strlist ["z"], nw)
                                              ; P.PVar
                                                  (Id.from_strlist ["w"], nw) ]
                                            , nw ))
                                     , nw
                                     , P.Bool (true, nw)
                                     , P.Add
                                         ( P.Var (Id.from_strlist ["x"], nw)
                                         , P.Var (Id.from_strlist ["y"], nw)
                                         , nw ) ) ] ) ) ] )
                     , nw )
                 , nw ) ) ]
         , P.Var (Id.from_strlist ["add"], nw)
         , false )) ;
    test "fun" "fun x\n   y z -> x + y + z"
      (P.Fun
         ( Id.from_strlist ["x"]
         , P.Fun
             ( Id.from_strlist ["y"]
             , P.Fun
                 ( Id.from_strlist ["z"]
                 , P.Add
                     ( P.Add
                         ( P.Var (Id.from_strlist ["x"], nw)
                         , P.Var (Id.from_strlist ["y"], nw)
                         , nw )
                     , P.Var (Id.from_strlist ["z"], nw)
                     , nw )
                 , nw )
             , nw )
         , nw )) ;
    test "fun" "(fun x y z -> x + y + z) 1"
      (P.App
         ( P.Paren
             (P.Fun
                ( Id.from_strlist ["x"]
                , P.Fun
                    ( Id.from_strlist ["y"]
                    , P.Fun
                        ( Id.from_strlist ["z"]
                        , P.Add
                            ( P.Add
                                ( P.Var (Id.from_strlist ["x"], nw)
                                , P.Var (Id.from_strlist ["y"], nw)
                                , nw )
                            , P.Var (Id.from_strlist ["z"], nw)
                            , nw )
                        , nw )
                    , nw )
                , nw ))
         , P.Int (1, nw)
         , nw )) ;
    Parser.count := 0 ;
    test "fun pat" "fun x, y\n   -> x + y"
      (P.Fun
         ( Id.from_strlist ["<anonymous1>"]
         , P.Match
             ( P.Var (Id.from_strlist ["<anonymous1>"], nw)
             , [ ( P.PTuple
                     ( [ P.PVar (Id.from_strlist ["x"], nw)
                       ; P.PVar (Id.from_strlist ["y"], nw) ]
                     , nw )
                 , nw
                 , P.Bool (true, nw)
                 , P.Add
                     ( P.Var (Id.from_strlist ["x"], nw)
                     , P.Var (Id.from_strlist ["y"], nw)
                     , nw ) ) ] )
         , nw )) ;
    Parser.count := 0 ;
    test "fun pat" "fun (x, y) (z, w) -> 0"
      (P.Fun
         ( Id.from_strlist ["<anonymous2>"]
         , P.Fun
             ( Id.from_strlist ["<anonymous1>"]
             , P.Match
                 ( P.Var (Id.from_strlist ["<anonymous2>"], nw)
                 , [ ( P.PParen
                         (P.PTuple
                            ( [ P.PVar (Id.from_strlist ["x"], nw)
                              ; P.PVar (Id.from_strlist ["y"], nw) ]
                            , nw ))
                     , nw
                     , P.Bool (true, nw)
                     , P.Match
                         ( P.Var (Id.from_strlist ["<anonymous1>"], nw)
                         , [ ( P.PParen
                                 (P.PTuple
                                    ( [ P.PVar (Id.from_strlist ["z"], nw)
                                      ; P.PVar (Id.from_strlist ["w"], nw) ]
                                    , nw ))
                             , nw
                             , P.Bool (true, nw)
                             , P.Int (0, nw) ) ] ) ) ] )
             , nw )
         , nw )) ;
    test "cons" "1 + 2 :: 3 :: [] = []"
      (P.Eq
         ( P.Cons
             ( P.Add (P.Int (1, nw), P.Int (2, nw), nw)
             , P.Cons (P.Int (3, nw), P.Emp nw, nw)
             , nw )
         , P.Emp nw
         , nw )) ;
    test "pipeline" "1 |> f |> g"
      (P.Pipeline
         ( P.Pipeline (P.Int (1, nw), P.Var (Id.from_strlist ["f"], nw), nw)
         , P.Var (Id.from_strlist ["g"], nw)
         , nw )) ;
    test "@@" "f @@ g @@ 1"
      (P.App
         ( P.Var (Id.from_strlist ["f"], nw)
         , P.App (P.Var (Id.from_strlist ["g"], nw), P.Int (1, nw), nw)
         , nw )) ;
    test "seq" "1; 2 |> f; 3"
      (P.Seq
         ( P.Int (1, nw)
         , P.Seq
             ( P.Pipeline (P.Int (2, nw), P.Var (Id.from_strlist ["f"], nw), nw)
             , P.Int (3, nw)
             , nw )
         , nw )) ;
    test "match1" "match x\n   with\n y -> 0 | z -> match z with a -> a"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PVar (Id.from_strlist ["y"], nw)
             , nw
             , P.Bool (true, nw)
             , P.Int (0, nw) )
           ; ( P.PVar (Id.from_strlist ["z"], nw)
             , nw
             , P.Bool (true, nw)
             , P.Match
                 ( P.Var (Id.from_strlist ["z"], nw)
                 , [ ( P.PVar (Id.from_strlist ["a"], nw)
                     , nw
                     , P.Bool (true, nw)
                     , P.Var (Id.from_strlist ["a"], nw) ) ] ) ) ] )) ;
    test "match_when" "match x\n with y when is_prime y -> 0 | z -> 1"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PVar (Id.from_strlist ["y"], nw)
             , nw
             , P.App
                 ( P.Var (Id.from_strlist ["is_prime"], nw)
                 , P.Var (Id.from_strlist ["y"], nw)
                 , nw )
             , P.Int (0, nw) )
           ; ( P.PVar (Id.from_strlist ["z"], nw)
             , nw
             , P.Bool (true, nw)
             , P.Int (1, nw) ) ] )) ;
    test "match2" "match x with y -> let x = 1\n   in x | z ->\n 1"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PVar (Id.from_strlist ["y"], nw)
             , nw
             , P.Bool (true, nw)
             , P.Let
                 ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
                 , P.Var (Id.from_strlist ["x"], nw)
                 , false ) )
           ; ( P.PVar (Id.from_strlist ["z"], nw)
             , nw
             , P.Bool (true, nw)
             , P.Int (1, nw) ) ] )) ;
    test "match nested" "match match [] with [] -> [] with [] ->\n   []"
      (P.Match
         ( P.Match (P.Emp nw, [(P.PEmp nw, nw, P.Bool (true, nw), P.Emp nw)])
         , [(P.PEmp nw, nw, P.Bool (true, nw), P.Emp nw)] )) ;
    test "tuple1" "1, 2" (P.Tuple ([P.Int (1, nw); P.Int (2, nw)], nw)) ;
    test "tuple1" "1+2, 2+3"
      (P.Tuple
         ( [ P.Add (P.Int (1, nw), P.Int (2, nw), nw)
           ; P.Add (P.Int (2, nw), P.Int (3, nw), nw) ]
         , nw )) ;
    test "tuple1" "1, 2, 3, 4"
      (P.Tuple ([P.Int (1, nw); P.Int (2, nw); P.Int (3, nw); P.Int (4, nw)], nw)) ;
    test "tuple nest" "1, (3, 4)"
      (P.Tuple
         ( [ P.Int (1, nw)
           ; P.Paren (P.Tuple ([P.Int (3, nw); P.Int (4, nw)], nw)) ]
         , nw )) ;
    test "pattern_tuple" "match x with (x, y) -> x"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PParen
                 (P.PTuple
                    ( [ P.PVar (Id.from_strlist ["x"], nw)
                      ; P.PVar (Id.from_strlist ["y"], nw) ]
                    , nw ))
             , nw
             , P.Bool (true, nw)
             , P.Var (Id.from_strlist ["x"], nw) ) ] )) ;
    test "pattern_tuple" "match x with (x,\n   (y,\n z)) -> x"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PParen
                 (P.PTuple
                    ( [ P.PVar (Id.from_strlist ["x"], nw)
                      ; P.PParen
                          (P.PTuple
                             ( [ P.PVar (Id.from_strlist ["y"], nw)
                               ; P.PVar (Id.from_strlist ["z"], nw) ]
                             , nw )) ]
                    , nw ))
             , nw
             , P.Bool (true, nw)
             , P.Var (Id.from_strlist ["x"], nw) ) ] )) ;
    test "pattern_cons" "match x with x :: [] -> x"
      (P.Match
         ( P.Var (Id.from_strlist ["x"], nw)
         , [ ( P.PCons (P.PVar (Id.from_strlist ["x"], nw), P.PEmp nw, nw)
             , nw
             , P.Bool (true, nw)
             , P.Var (Id.from_strlist ["x"], nw) ) ] )) ;
    test "parse_list1" "[1]" (P.Cons (P.Int (1, nw), P.Emp nw, nw)) ;
    test "parse_list2" "[1;]" (P.Cons (P.Int (1, nw), P.Emp nw, nw)) ;
    test "parse_list3" "[1;2;3]"
      (P.Cons
         ( P.Int (1, nw)
         , P.Cons (P.Int (2, nw), P.Cons (P.Int (3, nw), P.Emp nw, nw), nw)
         , nw )) ;
    test "parse_list4" "[1;2;3;]"
      (P.Cons
         ( P.Int (1, nw)
         , P.Cons (P.Int (2, nw), P.Cons (P.Int (3, nw), P.Emp nw, nw), nw)
         , nw )) ;
    test "parse_list5" "1 :: [2;3]"
      (P.Cons
         ( P.Int (1, nw)
         , P.Cons (P.Int (2, nw), P.Cons (P.Int (3, nw), P.Emp nw, nw), nw)
         , nw )) ;
    test "parse_list1" "match [] with [1] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons (P.PInt (1, nw), P.PEmp nw, nw)
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_pattern_list2" "match [] with [1;] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons (P.PInt (1, nw), P.PEmp nw, nw)
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_pattern_list3" "match [] with [1;2;3] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons
                 ( P.PInt (1, nw)
                 , P.PCons
                     ( P.PInt (2, nw)
                     , P.PCons (P.PInt (3, nw), P.PEmp nw, nw)
                     , nw )
                 , nw )
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_pattern_list4" "match [] with [1;2;3;] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons
                 ( P.PInt (1, nw)
                 , P.PCons
                     ( P.PInt (2, nw)
                     , P.PCons (P.PInt (3, nw), P.PEmp nw, nw)
                     , nw )
                 , nw )
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_pattern_list5" "match [] with 1 ::\n\n   [2;3] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons
                 ( P.PInt (1, nw)
                 , P.PCons
                     ( P.PInt (2, nw)
                     , P.PCons (P.PInt (3, nw), P.PEmp nw, nw)
                     , nw )
                 , nw )
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_as" "match [] with 1, 2 as x -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PAs
                 ( [ P.PTuple ([P.PInt (1, nw); P.PInt (2, nw)], nw)
                   ; P.PVar (Id.from_strlist ["x"], nw) ]
                 , nw )
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "parse_as" "match [] with X 1 :: [] -> []"
      (P.Match
         ( P.Emp nw
         , [ ( P.PCons
                 ( P.PCtorApp (Id.from_strlist ["X"], P.PInt (1, nw), nw)
                 , P.PEmp nw
                 , nw )
             , nw
             , P.Bool (true, nw)
             , P.Emp nw ) ] )) ;
    test "let\n\n   x, y = z in x" "let x, y = z in x"
      (P.Let
         ( [ ( P.PTuple
                 ( [ P.PVar (Id.from_strlist ["x"], nw)
                   ; P.PVar (Id.from_strlist ["y"], nw) ]
                 , nw )
             , nw
             , P.Var (Id.from_strlist ["z"], nw) ) ]
         , P.Var (Id.from_strlist ["x"], nw)
         , false )) ;
    test "app1" "1 + f 2"
      (P.Add
         ( P.Int (1, nw)
         , P.App (P.Var (Id.from_strlist ["f"], nw), P.Int (2, nw), nw)
         , nw )) ;
    test "app2" "- f 2"
      (P.Neg (P.App (P.Var (Id.from_strlist ["f"], nw), P.Int (2, nw), nw), nw)) ;
    test "app3" "f 1 + 2"
      (P.Add
         ( P.App (P.Var (Id.from_strlist ["f"], nw), P.Int (1, nw), nw)
         , P.Int (2, nw)
         , nw )) ;
    test "app4" "f 1 2"
      (P.App
         ( P.App (P.Var (Id.from_strlist ["f"], nw), P.Int (1, nw), nw)
         , P.Int (2, nw)
         , nw )) ;
    test "app5" "f 1 2 3"
      (P.App
         ( P.App
             ( P.App (P.Var (Id.from_strlist ["f"], nw), P.Int (1, nw), nw)
             , P.Int (2, nw)
             , nw )
         , P.Int (3, nw)
         , nw )) ;
    test "ctor" "Leaf (1, 2)"
      (P.App
         ( P.Ctor (Id.from_strlist ["Leaf"], nw)
         , P.Paren (P.Tuple ([P.Int (1, nw); P.Int (2, nw)], nw))
         , nw )) ;
    test "if" "if f x then 1 else let x = 1\n   in\n x"
      (P.If
         ( P.App
             ( P.Var (Id.from_strlist ["f"], nw)
             , P.Var (Id.from_strlist ["x"], nw)
             , nw )
         , P.Int (1, nw)
         , P.Let
             ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
             , P.Var (Id.from_strlist ["x"], nw)
             , false )
         , nw )) ;
    test "if_nested"
      "if if x then\n\
      \   true else\n\
      \  false then true else if y then true else false"
      (P.If
         ( P.If
             ( P.Var (Id.from_strlist ["x"], nw)
             , P.Bool (true, nw)
             , P.Bool (false, nw)
             , nw )
         , P.Bool (true, nw)
         , P.If
             ( P.Var (Id.from_strlist ["y"], nw)
             , P.Bool (true, nw)
             , P.Bool (false, nw)
             , nw )
         , nw )) ;
    test "assign" "a := 1 + 2"
      (P.Assign
         ( P.Var (Id.from_strlist ["a"], nw)
         , P.Add (P.Int (1, nw), P.Int (2, nw), nw)
         , nw )) ;
    test "assign" "a.(0) <- 1 + 2"
      (P.ArrayAssign
         ( P.Var (Id.from_strlist ["a"], nw)
         , P.Int (0, nw)
         , P.Add (P.Int (1, nw), P.Int (2, nw), nw)
         , nw )) ;
    test "ref\n   array" "a.(0) <-\n a.(0)"
      (P.ArrayAssign
         ( P.Var (Id.from_strlist ["a"], nw)
         , P.Int (0, nw)
         , P.Index (P.Var (Id.from_strlist ["a"], nw), P.Int (0, nw), nw)
         , nw )) ;
    test "dot access" "X.Y.z" (P.Var (Id.from_strlist ["X"; "Y"; "z"], nw)) ;
    test "arr assign" "(getarr 0).(1+1) <- 2"
      (P.ArrayAssign
         ( P.Paren
             (P.App (P.Var (Id.from_strlist ["getarr"], nw), P.Int (0, nw), nw))
         , P.Add (P.Int (1, nw), P.Int (1, nw), nw)
         , P.Int (2, nw)
         , nw )) ;
    test "dot array assign" "X.y.(1) <- 1 + 1"
      (P.ArrayAssign
         ( P.Var (Id.from_strlist ["X"; "y"], nw)
         , P.Int (1, nw)
         , P.Add (P.Int (1, nw), P.Int (1, nw), nw)
         , nw )) ;
    test "unit" "let () = () in ()"
      (P.Let
         ([(P.PTuple ([], nw), nw, P.Tuple ([], nw))], P.Tuple ([], nw), false)) ;
    test_ty "tuple1" "t * t"
      (P.TTuple
         ( [ P.TApp ([], Id.from_strlist ["t"], nw)
           ; P.TApp ([], Id.from_strlist ["t"], nw) ]
         , nw )) ;
    test_ty "tuple2" "t *\n   (t * t)"
      (P.TTuple
         ( [ P.TApp ([], Id.from_strlist ["t"], nw)
           ; P.TParen
               (P.TTuple
                  ( [ P.TApp ([], Id.from_strlist ["t"], nw)
                    ; P.TApp ([], Id.from_strlist ["t"], nw) ]
                  , nw )) ]
         , nw )) ;
    test_ty "tid" "M1.t * M2.t"
      (P.TTuple
         ( [ P.TApp ([], Id.from_strlist ["M1"; "t"], nw)
           ; P.TApp ([], Id.from_strlist ["M2"; "t"], nw) ]
         , nw )) ;
    test_ty "higher type" "t list list"
      (P.TApp
         ( [ P.TApp
               ( [P.TApp ([], Id.from_strlist ["t"], nw)]
               , Id.from_strlist ["list"]
               , nw ) ]
         , Id.from_strlist ["list"]
         , nw )) ;
    test_ty "tapp2" "(a * a)\n list"
      (P.TApp
         ( [ P.TParen
               (P.TTuple
                  ( [ P.TApp ([], Id.from_strlist ["a"], nw)
                    ; P.TApp ([], Id.from_strlist ["a"], nw) ]
                  , nw )) ]
         , Id.from_strlist ["list"]
         , nw )) ;
    test_ty "('a list)" "('a list)"
      (P.TApp ([P.TVar ("a", nw)], Id.from_strlist ["list"], nw)) ;
    test_ty "tapp2" "(a, a) list"
      (P.TApp
         ( [ P.TApp ([], Id.from_strlist ["a"], nw)
           ; P.TApp ([], Id.from_strlist ["a"], nw) ]
         , Id.from_strlist ["list"]
         , nw )) ;
    test_ty "tvar" "'a\n   list"
      (P.TApp ([P.TVar ("a", nw)], Id.from_strlist ["list"], nw)) ;
    test_stmts "let stmt" "let x = 1"
      (P.Let
         ( [(P.PVar (Id.from_strlist ["x"], nw), nw, P.Int (1, nw))]
         , P.Never
         , true )) ;
    test_stmts "letfun stmt" "let\n   add x y =\n x + y"
      (P.Let
         ( [ ( P.PVar (Id.from_strlist ["add"], nw)
             , nw
             , P.Fun
                 ( Id.from_strlist ["x"]
                 , P.Fun
                     ( Id.from_strlist ["y"]
                     , P.Add
                         ( P.Var (Id.from_strlist ["x"], nw)
                         , P.Var (Id.from_strlist ["y"], nw)
                         , nw )
                     , nw )
                 , nw ) ) ]
         , P.Never
         , true )) ;
    test_stmts "letrec and" "let rec f = 1\n   and g = 2"
      (P.LetRec
         ( [ (Id.from_strlist ["f"], nw, P.Int (1, nw))
           ; (Id.from_strlist ["g"], nw, P.Int (2, nw)) ]
         , P.Never
         , true )) ;
    test_stmts "let\n and" "let f = 1 and g = 2"
      (P.Let
         ( [ (P.PVar (Id.from_strlist ["f"], nw), nw, P.Int (1, nw))
           ; (P.PVar (Id.from_strlist ["g"], nw), nw, P.Int (2, nw)) ]
         , P.Never
         , true )) ;
    P.count := 0 ;
    test_stmts "letfun and" "let add (x, y) (z,\n w) = x + y and add2 = add"
      (P.Let
         ( [ ( P.PVar (Id.from_strlist ["add"], nw)
             , nw
             , P.Fun
                 ( Id.from_strlist ["<anonymous2>"]
                 , P.Fun
                     ( Id.from_strlist ["<anonymous1>"]
                     , P.Match
                         ( P.Var (Id.from_strlist ["<anonymous2>"], nw)
                         , [ ( P.PParen
                                 (P.PTuple
                                    ( [ P.PVar (Id.from_strlist ["x"], nw)
                                      ; P.PVar (Id.from_strlist ["y"], nw) ]
                                    , nw ))
                             , nw
                             , P.Bool (true, nw)
                             , P.Match
                                 ( P.Var (Id.from_strlist ["<anonymous1>"], nw)
                                 , [ ( P.PParen
                                         (P.PTuple
                                            ( [ P.PVar
                                                  (Id.from_strlist ["z"], nw)
                                              ; P.PVar
                                                  (Id.from_strlist ["w"], nw) ]
                                            , nw ))
                                     , nw
                                     , P.Bool (true, nw)
                                     , P.Add
                                         ( P.Var (Id.from_strlist ["x"], nw)
                                         , P.Var (Id.from_strlist ["y"], nw)
                                         , nw ) ) ] ) ) ] )
                     , nw )
                 , nw ) )
           ; ( P.PVar (Id.from_strlist ["add2"], nw)
             , nw
             , P.Var (Id.from_strlist ["add"], nw) ) ]
         , P.Never
         , true )) ;
    test_stmts "type variant" "type t =\n   Leaf of\n int | Node of t * t"
      (P.Type
         ( [ ( Id.from_strlist ["t"]
             , nw
             , []
             , P.Variant
                 [ (Id.from_strlist ["Leaf"], nw, [P.TInt nw])
                 ; ( Id.from_strlist ["Node"]
                   , nw
                   , [ P.TApp ([], Id.from_strlist ["t"], nw)
                     ; P.TApp ([], Id.from_strlist ["t"], nw) ] ) ] ) ]
         , P.Never )) ;
    test_stmts "type variant and"
      "type t = Leaf of int | Node of t\n   * t and 'a\n a_t = int"
      (P.Type
         ( [ ( Id.from_strlist ["t"]
             , nw
             , []
             , P.Variant
                 [ (Id.from_strlist ["Leaf"], nw, [P.TInt nw])
                 ; ( Id.from_strlist ["Node"]
                   , nw
                   , [ P.TApp ([], Id.from_strlist ["t"], nw)
                     ; P.TApp ([], Id.from_strlist ["t"], nw) ] ) ] )
           ; (Id.from_strlist ["a_t"], nw, [("a", nw)], P.Alias (P.TInt nw)) ]
         , P.Never )) ;
    test_stmts "type option" "type 'a t =\n   Some of 'a |\n None"
      (P.Type
         ( [ ( Id.from_strlist ["t"]
             , nw
             , [("a", nw)]
             , P.Variant
                 [ (Id.from_strlist ["Some"], nw, [P.TVar ("a", nw)])
                 ; (Id.from_strlist ["None"], nw, []) ] ) ]
         , P.Never )) ;
    test_stmts "type result" "type ('a, 'b)\n   t = Ok of 'a |\n Err of 'b"
      (P.Type
         ( [ ( Id.from_strlist ["t"]
             , nw
             , [("a", nw); ("b", nw)]
             , P.Variant
                 [ (Id.from_strlist ["Ok"], nw, [P.TVar ("a", nw)])
                 ; (Id.from_strlist ["Err"], nw, [P.TVar ("b", nw)]) ] ) ]
         , P.Never ))
