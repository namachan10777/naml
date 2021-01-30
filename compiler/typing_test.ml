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
            Ty.Fun ([Ty.Poly 0; Ty.Poly 1], Ty.Tuple [Ty.Poly 0; Ty.Poly 1]));
        ],
        T.Let ([
            T.PVar ("a", Ty.Fun ([Ty.Poly 0], Ty.Tuple [Ty.Int; Ty.Poly 0])),
            T.App (T.Var ["mk_pair"], [T.Int 1])
        ], T.Int 0)
    ))
