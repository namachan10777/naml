let f s = s |> Lex.f "test.ml" |> Parser.parse |> Ast.of_t |> Alpha.f Alpha.pervasive_env |> Typing.f 0 Typing.pervasive_env |> snd |> Closure.f
let f_s s = Ast.f "test.ml" s |> Alpha.f Alpha.pervasive_env |> Typing.f 0 Typing.pervasive_env |> snd |> Closure.f

module C = Closure

let add = C.Var (fst (Tbl.lookup ["+"] Alpha.pervasive_var_env |> Tbl.expect ""), Types.Fun (Types.Int, Types.Fun (Types.Int, Types.Int)))

let () =
    (match f "fun x -> fun y -> x + y" with
    | C.Fun (
        [x_id, Types.Int; y_id, Types.Int],
        C.App (
            C.App (add, C.Var (x_id', _), Types.Int, Types.Fun (Types.Int, Types.Int)),
            C.Var (y_id', _),
            Types.Int,
            Types.Int
        ),
        Types.Int
    ) -> ()
    | _ -> failwith "function shrink test failed")
