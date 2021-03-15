let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env |> snd |> Closure.f
let f_s s = Ast.f "test.ml" s |> Alpha.f Alpha.pervasive_env |> Typing.f Typing.pervasive_env |> snd |> Closure.f

module C = Closure

let assert_eq a b =
    if a = b
    then ()
    else
        failwith @@ "closure unittest failed "

let add = C.Var (fst (Tbl.lookup ["+"] Alpha.pervasive_var_env |> Tbl.expect ""), Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int)))
let neg = C.Var (fst (Tbl.lookup ["<neg>"] Alpha.pervasive_var_env |> Tbl.expect ""), Types.Fun (Types.Int, Types.Int))
let eq = C.Var (fst (Tbl.lookup ["="] Alpha.pervasive_var_env |> Tbl.expect ""), Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Bool)))
let i_i_i = Types.Fun (Types.Int, (Types.Fun (Types.Int, Types.Int)))
let i_i_b = Types.Fun (Types.Int, (Types.Fun (Types.Int, Types.Bool)))
let i_i = Types.Fun (Types.Int, Types.Int)
let x_id = ([], "x", 0)
let y_id = ([], "y", 1)
let x = C.Var (x_id, Types.Int)
let y = C.Var (y_id, Types.Int)
let cap4_id = ([], "<cap>", 4)
let cap5_id = ([], "<cap>", 5)
let cap6_id = ([], "<cap>", 6)
let cap4 = C.Var (cap4_id, i_i_b)
let cap5 = C.Var (cap5_id, i_i_i)
let cap6 = C.Var (cap6_id, i_i)

let () =
    Id.global_impure_count := 0;
    assert_eq (f "fun x -> fun y -> (- x + y) = 0")
    (C.App (
        C.App (
            C.App (
                C.Fun (
                    [cap4_id, i_i_b; cap5_id, i_i_i; cap6_id, i_i;x_id, Types.Int; y_id, Types.Int],
                    C.App (
                        C.App (
                            cap4,
                            C.App (
                                C.App (
                                    cap5,
                                    C.App (
                                        cap6,
                                        x,
                                        Types.Int,
                                        Types.Int
                                    ),
                                    Types.Int,
                                    Types.Fun (Types.Int, Types.Int)
                                ),
                                y,
                                Types.Int,
                                Types.Int
                            ),
                            Types.Int,
                            Types.Fun (Types.Int, Types.Bool)
                        ),
                        C.Int 0,
                        Types.Int,
                        Types.Bool
                    ),
                    Types.Bool
                ),
                eq,
                i_i_b,
                Types.Fun (i_i_i, Types.Fun (i_i, i_i_b))
            ),
            add,
            i_i_i,
            Types.Fun (i_i, i_i_b)
        ),
        neg,
        i_i,
        i_i_b
    ))
