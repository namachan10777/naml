let assert_eq a b =
    if a = b
    then ()
    else failwith "id test failed"

let () =
    assert_eq "a(0)" @@ Id.show ([], "a", 0);
    assert_eq "Mod1.a(0)" @@ Id.show (["Mod1"], "a", 0);
    assert_eq "Mod1.Mod2.a(0)" @@ Id.show (["Mod1"; "Mod2"], "a", 0);
    assert_eq "Mod1.Mod2.Mod3.a(0)" @@ Id.show (["Mod1"; "Mod2"; "Mod3"], "a", 0);
