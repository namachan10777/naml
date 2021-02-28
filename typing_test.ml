let assert_eq name a b =
    if a = b
    then ()
    else failwith @@ "typing unittest failed " ^ name

let unify_unk_unk () =
    Typing.init ();
    let u1 = Typing.fresh 0 in
    let u2 = Typing.fresh 1 in
    Typing.unify u1 u2;
    assert_eq "unify to low level" (!Typing.store).(0) (Typing.Unknown (0, [0;1]));
    assert_eq "unify to low level" (!Typing.store).(1) (Typing.Unknown (0, [0;1]));
    Typing.init ();
    let u1 = Typing.fresh 1 in
    let u2 = Typing.fresh 0 in
    Typing.unify u1 u2;
    assert_eq "unify to low level" (!Typing.store).(0) (Typing.Unknown (0, [0;1]));
    assert_eq "unify to low level" (!Typing.store).(1) (Typing.Unknown (0, [0;1]))

let () =
    unify_unk_unk ()
