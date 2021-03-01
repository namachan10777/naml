let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f Alpha.pervasive_env |> Typing.f 0 Typing.pervasive_env
let f_s s = Ast.f "test.ml" s |> Alpha.f Alpha.pervasive_env

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
    let g1 = Typing.gen_ty (0, ref []) t in
    let g2 = Typing.gen_ty (1, ref []) t in
    let g3 = Typing.gen_ty (2, ref []) t in
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

let test_typing () =
    Typing.init ();
    (match f "1" with
    | (ty, t) when ty = Typing.TInt -> ()
    | _ -> failwith "typing int failed")

let () =
    unify_unk_unk ();
    unify_unk_ty ();
    unify_unk_just ();
    unify_fun ();
    cannot_unify ();
    test_instantiate ();
    test_generalize ();
    test_typing ()
