let test src expected =
    Printf.printf "testing \"%s\"...\n" src;
    let s, _ = Parser.parse_expr @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    let ast = Ast.of_parser_t s in
    let typed = Typing.f ast in
    if typed = expected
    then ()
    else begin
        print_endline "-------------------------------------";
        Printf.printf "left: \n%s\n" @@ Typing.show typed;
        Printf.printf "right: \n%s\n" @@ Typing.show expected;
        failwith "test failed"
    end

let unify_test name a b =
    Printf.printf "testing \"%s\"\"...\n" name;
    if b = a 
    then ()
    else begin
        print_endline "-------------------------------------";
        Printf.printf "left: \n%s\n" @@ Types.show a;
        Printf.printf "right: \n%s\n" @@ Types.show b;
        failwith "test failed"
    end

module Ty = Types
module T = Typing

let () =
    test "0" (Typing.Int 0);
    test "not" (Typing.Var ["not"]);
    test "not true" (
        Typing.App (
            Typing.Var ["not"],
            [Typing.Bool true]
        )
    );
    test "1+2" (
        Typing.App (
            Typing.Var ["+"],
            [Typing.Int 1; Typing.Int 2]
        )
    );
    test "let x = 1 in x" (
        Typing.Let (
            [Typing.PVar ("x", Ty.Int), Typing.Int 1],
            Typing.Var ["x"]
        )
    );
    test "let x = 1 + 1 in x" (
        Typing.Let (
            [Typing.PVar ("x", Ty.Int), Typing.App (Typing.Var ["+"], [Typing.Int 1; Typing.Int 1])],
            Typing.Var ["x"]
        )
    );
    test "let id = fun x -> x in id 1; id true" (
        Typing.Let (
            [
                Typing.PVar ("id", Ty.Fun ([Ty.Poly 0], Ty.Poly 0)),
                Typing.Fun (["x", Ty.Poly 0], Typing.Var ["x"], Ty.Poly 0)
            ],
            Typing.App (Typing.Var [";"], [
                Typing.App (Typing.Var ["id"], [Typing.Int 1]);
                Typing.App (Typing.Var ["id"], [Typing.Bool true]);
            ])
        )
    );
    test "let mk_pair x y = (x, y) in let a = mk_pair 1 in let b = mk_pair true false in a"
    (T.Let (
        [
            T.PVar ("mk_pair",Ty.Fun ([Ty.Poly 0; Ty.Poly 1], Ty.Tuple [Ty.Poly 0; Ty.Poly 1])),
            T.Fun (["x", Ty.Poly 0; "y", Ty.Poly 1], T.Tuple ([T.Var ["x"]; T.Var ["y"]], [Ty.Poly 0; Ty.Poly 1]),
            Ty.Tuple [Ty.Poly 0; Ty.Poly 1]);
        ],
        T.Let ([
            T.PVar ("a", Ty.Fun ([Ty.Poly 0], Ty.Tuple [Ty.Int; Ty.Poly 0])),
            T.App (T.Var ["mk_pair"], [T.Int 1])
        ],
        T.Let ([
            T.PVar ("b", Ty.Tuple ([Ty.Bool; Ty.Bool])),
            T.App (T.Var ["mk_pair"], [T.Bool true; T.Bool false])
        ], Typing.Var ["a"]))
    ))(*;
    test "let f x = let g y = x = y in g in f"
    (T.Let (
        [
            T.PVar ("f", Ty.Fun ([Ty.Poly 0], Ty.Fun ([Ty.Poly 0], Ty.Bool))),
            T.Let ([
                    T.PVar ("g", Ty.Fun ([Ty.Poly 0], Ty.Bool)),
                    T.App (T.Var ["="], [T.Var ["x"]; T.Var ["y"]])
                ],
                T.Var ["g"]
            );
        ],
        T.Var ["f"]
    ))*)

let () =
    let t1 = T.fresh 0 in
    let t2 = T.fresh 1 in
    let t3 = T.fresh 2 in
    let t4 = T.fresh 3 in
    T.unify t1 t2 |> ignore;
    T.unify t3 t4 |> ignore;
    T.unify t1 t3 |> ignore;
    unify_test "unify 3" t1 t2;
    unify_test "unify 3" t3 t4;
    unify_test "unify 3" t1 t3;
    unify_test "unify 3" t2 t4;
    unify_test "unify 3" t2 t3;
    unify_test "unify 3" t1 t4
