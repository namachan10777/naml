let assert_eq name ty str =
    if Types.show ty <> str
    then failwith @@ Printf.sprintf "but representation (\"%s\" <> \"%s\")" (Types.show ty) str
    else ()

let () =
    assert_eq "fun" (Types.Fun (Types.Fun (Types.Int, Types.Int), Types.Fun (Types.Int, Types.Int))) "(int -> int) -> int -> int";
    assert_eq "tuple order" (Types.Tuple [Types.Int; Types.Bool; Types.Str]) "int * bool * string";
    assert_eq "nested tuple" (Types.Tuple [Types.Int; Types.Tuple [Types.Int; Types.Str]]) "int * (int * string)";
    assert_eq "unit" (Types.Tuple []) "unit";
    assert_eq "variant arg0" (Types.Variant ([], ([], "hoge", 0))) "hoge(0)";
    assert_eq "variant arg1" (Types.Variant ([Types.Int], ([], "hoge", 0))) "int hoge(0)";
    assert_eq "variant arg3" (Types.Variant ([Types.Int; Types.Bool; Types.Str], ([], "hoge", 0))) "(int, bool, string) hoge(0)";
    assert_eq "variant chain" (Types.Variant ([Types.Variant ([], ([], "fuga", 1))], ([], "hoge", 0))) "fuga(1) hoge(0)";
