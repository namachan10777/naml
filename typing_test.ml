let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env
let f_s s = Lex.f "test.ml" s |> Parser.f |> Ast.f |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env


let assert_eq name a b =
    if a = b
    then ()
    else failwith @@ "typing unittest failed " ^ name

let unify_unk_unk () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 1 in
    Typing.unify u1 u2;
    assert_eq "unify to low level" (!Typing.store).(0) (Typing.Unknown (0, 0, [0;1]));
    assert_eq "unify to low level" (!Typing.store).(1) (Typing.Unknown (0, 0, [0;1]));
    Typing.init ();
    let u1 = Typing.fresh 1 in
    let u2 = Typing.fresh 0 in
    Typing.unify u1 u2;
    assert_eq "unify to low level" (!Typing.store).(0) (Typing.Unknown (0, 1, [0;1]));
    assert_eq "unify to low level" (!Typing.store).(1) (Typing.Unknown (0, 1, [0;1]))

let unify_unk_ty () =
    Typing.init ();
    let u = Typing.fresh 0 in
    Typing.unify u Typing.TInt;
    Typing.unify Typing.TInt u;
    assert_eq "unify unknown and int" (!Typing.store).(0) (Typing.Just (Typing.TInt, [0]))

let unify_unk_just () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 0 in
    Typing.unify u1 Typing.TInt;
    Typing.unify u1 u2;
    assert_eq "unify unknown and just" (!Typing.store).(0) (Typing.Just (Typing.TInt, [0; 1]));
    assert_eq "unify unknown and just" (!Typing.store).(0) (Typing.Just (Typing.TInt, [0; 1]));
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 0 in
    Typing.unify u1 Typing.TInt;
    Typing.unify u2 u1;
    assert_eq "unify unknown and just" (!Typing.store).(0) (Typing.Just (Typing.TInt, [1; 0]));
    assert_eq "unify unknown and just" (!Typing.store).(0) (Typing.Just (Typing.TInt, [1; 0]))

let unify_fun () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 0 in
    let f1 = Typing.TFun (u1, Typing.TInt) in
    let f2 = Typing.TFun (Typing.TInt, u2) in
    Typing.unify f1 f2;
    assert_eq "unify fun" (!Typing.store).(0) (Typing.Just (Typing.TInt, [0]));
    assert_eq "unify fun" (!Typing.store).(1) (Typing.Just (Typing.TInt, [1]))

let cycle_check () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    (try Typing.unify (Typing.TFun (u1, u1)) u1; failwith "unexpected unify success" with
    | Typing.CyclicType -> ())

let cannot_unify () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 0 in
    (try Typing.unify Typing.TInt Typing.TStr; failwith "unexpected unify success" with
    | Typing.UnifyError -> ());
    (try Typing.unify (Typing.TFun (Typing.TStr, u1)) (Typing.TFun (Typing.TInt, u2)); failwith "unexpected unify success" with
    | Typing.UnifyError -> ());
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 0 in
    Typing.unify u1 Typing.TInt;
    (try Typing.unify u1 Typing.TStr; failwith "unexpected unify success" with
    | Typing.UnifyError -> ());
    Typing.unify u2 Typing.TStr;
    (try Typing.unify u1 u2; failwith "unexpected unify success" with
    | Typing.UnifyError -> ())

let test_generalize () =
    Typing.init ();
    let u1 = Typing.fresh 1 in
    let u2 = Typing.fresh 2 in
    let t = Typing.TTuple [u1; u2; u1] in
    let g1 = Typing.gen_ty [] 0 t in
    let g2 = Typing.gen_ty [] 1 t in
    let g3 = Typing.gen_ty [] 2 t in
    assert_eq "full generalize 1" g1 (Typing.TTuple [Typing.Poly 0; Typing.Poly 1; Typing.Poly 0]);
    assert_eq "partial generalize 1" g2 (Typing.TTuple [u1; Typing.Poly 1; u1]);
    assert_eq "not generalize 1" g3 t

let test_instantiate () =
    Typing.init ();
    let u1 = Typing.fresh 1 in
    let u2 = Typing.fresh 1 in
    Typing.init ();
    let t = Typing.TTuple [Typing.Poly 0; Typing.Poly 1; Typing.Poly 0] in
    let i = Typing.inst_ty (1, ref []) t in
    assert_eq "not generalize 1" i (Typing.TTuple [u1; u2; u1])

module T = Typing
module Ty = Types

let expect_unify_error expr =
    try f expr |> ignore; failwith (Printf.sprintf "expect unify error \"%s\"" expr) with
    | Typing.UnifyError -> ()

let expect_unify_error expr =
    try f expr |> ignore; failwith (Printf.sprintf "expect unify error \"%s\"" expr) with
    | Typing.UnifyError -> ()

let test_typing () =
    Typing.init ();
    (match f "1" with
    | (ty, t) when ty = Typing.TInt -> ()
    | _ -> failwith "typing int failed");
    (match f "let id x = x in id true; id 1" with
    | (ty, Typing.Let ([(Typing.PVar (_, (Typing.TFun (Typing.Poly tag, Typing.Poly tag') as id_ty), _), id_ty'), _, _], _)) when 
        id_ty = id_ty' && tag = tag' && (T.dereference ty) = (0, Types.Int) -> ()
    |_ -> failwith "typing int failed");
    (match f "let x = 1 and y = 2 in x + y" with
    | (Typing.TInt, Typing.Let ([(Typing.PVar (_, Typing.TInt, _), _), _, _; (Typing.PVar (_, Typing.TInt, _), _), _, _], _)) -> ()
    | _ -> failwith "typing let and");
    (match f "let make_pair = fun x -> let f = fun y -> (x, y) in f in let pair = make_pair 42 in let pair_with_true = make_pair true in let pair2 = pair_with_true 13 in pair2" with
    | ( ty, _) when (T.dereference ty) = (0, Ty.Tuple [Ty.Bool; Ty.Int]) -> ()
    | _ -> failwith "typing makepair");
    (match f "let (x, y) = 1, true in y, x" with
    | (ty, t) when (T.dereference ty) = (0, Ty.Tuple [Ty.Bool; Ty.Int]) -> ()
    | _ -> failwith "typing let tuple");
    (match f "let rec fact n = if n = 0 then 1 else n * fact (n-1) in fact" with
    | (ty, _) when (T.dereference ty) = (0, Ty.Fun (Ty.Int, Ty.Int)) -> ()
    | (_, t) -> failwith "typing let tuple");
    (match f "let f x = match x with  (1, y) -> 0 | (x, y) -> x in f" with
    | (_, Let ([(_, ty), _, _], _)) when (T.dereference ty) = (1, Ty.Fun (Ty.Tuple [Ty.Int; Ty.Poly 0], Ty.Int)) -> ()
    | (_, t) -> failwith "typing let tuple");
    let list_id = Id.lookup ["list"] (List.map fst Pervasives.types) in
    (match f_s "let rec map f l = match l with x :: xs -> f x :: map f xs | [] -> []" with
    | (_, T.LetRec([_, _, (_, ty)], _))
        when (T.dereference ty)
            = (2, Ty.Fun (Ty.Fun (Ty.Poly 0, Ty.Poly 1), Ty.Fun (Ty.Variant ([Ty.Poly 0], list_id), Ty.Variant ([Ty.Poly 1], list_id)))) -> ()
    | (_, t) -> failwith "typing let tuple");
    (match f_s "type ('a, 'b) t1 = 'a * 'b and 'b t2 = A of ('b, int) t1 let x = A (1, 2)" with
    | (_, T.Let([_, _, (_, T.TVariant ([T.TInt], (_, "t2", _)))], _)) -> ()
    | (_, t) -> failwith "typing let tuple");
    (match f_s "type ('a, 'b) t1 = 'a * 'b type 'b t2 = A of ('b, int) t1 let x = A (1, 2)" with
    | (_, T.Let([_, _, (_, T.TVariant ([T.TInt], (_, "t2", _)))], _)) -> ()
    | (_, t) -> failwith "typing let tuple");
    (match f "let x = ref (fun x -> x) in (!x) 1; (!x)" with
    | (ty, _) when (Typing.dereference ty) = (0, Types.Fun (Types.Int, Types.Int)) -> ()
    | (_, _) -> failwith "typing let tuple");
    expect_unify_error "1 + true";
    expect_unify_error "(fun x -> x) 1 2";
    expect_unify_error "let (x, y) = 1 in x";
    expect_unify_error "let (1, 2) = (1, true) in 1";
    expect_unify_error "let r = ref [] in 1 :: !r; true :: !r";
    expect_unify_error "(fun f -> f 1; f true) (fun x -> x)"

let () =
    unify_unk_unk ();
    unify_unk_ty ();
    unify_unk_just ();
    unify_fun ();
    cycle_check ();
    cannot_unify ();
    test_instantiate ();
    test_generalize ();
    test_typing ()
