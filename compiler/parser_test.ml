let () =
    Test.assert_eq "parse_add" (Parser.parse @@ Lex.lex "0+1+2" (Lex.initial_pos "test.ml"))
    (Parser.Add (Parser.Add(Parser.Int 0, Parser.Int 1), Parser.Int 2));
    Test.assert_eq "parse_mul" (Parser.parse @@ Lex.lex "0*1*2" (Lex.initial_pos "test.ml"))
    (Parser.Mul (Parser.Mul(Parser.Int 0, Parser.Int 1), Parser.Int 2));
    Test.assert_eq "unary_minus" (Parser.parse @@ Lex.lex "1+-2" (Lex.initial_pos "test.ml"))
    (Parser.Add (Parser.Int 1, Parser.Neg (Parser.Int 2)));
    Test.assert_eq "mod" (Parser.parse @@ Lex.lex "3 mod 2" (Lex.initial_pos "test.ml"))
    (Parser.Mod (Parser.Int 3, Parser.Int 2));
    Test.assert_eq "add_r" (Parser.parse @@ Lex.lex "0+(1+2)" (Lex.initial_pos "test.ml"))
    (Parser.Add (Parser.Int 0, Parser.Paren (Parser.Add (Parser.Int 1, Parser.Int 2))));
    Test.assert_eq "4arith" (Parser.parse @@ Lex.lex "3*(200+300)/4-5" @@ Lex.initial_pos "test.ml")
    (Parser.Sub (
        Parser.Div (
            Parser.Mul (Parser.Int 3, Parser.Paren(Parser.Add (Parser.Int 200, Parser.Int 300))),
            Parser.Int 4
        ),
        Parser.Int 5
    ));
    Test.assert_eq "bool" (Parser.parse @@ Lex.lex "3>2 && not not (1 < 0)" @@ Lex.initial_pos "test.ml")
    (Parser.And (
        Parser.Gret (Parser.Int 3, Parser.Int 2),
        Parser.Not (Parser.Not (Parser.Paren (Parser.Less (Parser.Int 1, Parser.Int 0))))
    ));
    Test.assert_eq "bool2" (Parser.parse @@ Lex.lex  "true && false || 1 = 2 && false" @@ Lex.initial_pos "test.ml")
    (Parser.Or (
        Parser.And (Parser.Bool true, Parser.Bool false),
        Parser.And (Parser.Eq (Parser.Int 1, Parser.Int 2), Parser.Bool false)
    ));
    Test.assert_eq "let simple" (Parser.parse @@ Lex.lex "let x = 1 in x" @@ Lex.initial_pos "test.ml")
    (Parser.Let ("x", Parser.Int 1, Parser.Var "x"));
    Test.assert_eq "let add left" (Parser.parse @@ Lex.lex "1 + let x = 1 in x" @@ Lex.initial_pos "test.ml")
    (Parser.Add (Parser.Int 1, Parser.Let ("x", Parser.Int 1, Parser.Var "x")));
    Test.assert_eq "let add right" (Parser.parse @@ Lex.lex"(let x = 1 in x) + 1" @@ Lex.initial_pos "test.ml")
    (Parser.Add (Parser.Paren(Parser.Let ("x", Parser.Int 1, Parser.Var "x")), Parser.Int 1));
    Test.assert_eq "let complex" (Parser.parse @@ Lex.lex"let x = let y = 1 in y in let z = x in z" @@ Lex.initial_pos "test.ml")
    (Parser.Let ("x", Parser.Let ("y", Parser.Int 1, Parser.Var "y"), Parser.Let ("z", Parser.Var "x", Parser.Var "z")));
    Test.assert_eq "fun" (Parser.parse @@ Lex.lex "fun x y z -> x + y + z" @@ Lex.initial_pos "test.ml")
    (Parser.Fun (["x"; "y"; "z"], Parser.Add (Parser.Add (Parser.Var "x", Parser.Var "y"), Parser.Var "z")));
    Test.assert_eq "cons" (Parser.parse @@ Lex.lex "1 + 2 :: 3 :: [] = []" @@ Lex.initial_pos "test.ml")
    (Parser.Eq (
        Parser.Cons (Parser.Add (Parser.Int 1, Parser.Int 2), Parser.Cons (Parser.Int 3, Parser.Emp)),
        Parser.Emp
    ));
    Test.assert_eq "pipeline" (Parser.parse @@ Lex.lex "1 |> f |> g" @@ Lex.initial_pos "test.ml")
    (Parser.Pipeline (Parser.Pipeline (Parser.Int 1, Parser.Var "f"), Parser.Var "g"));
    Test.assert_eq "@@" (Parser.parse @@ Lex.lex "f @@ g @@ 1" @@ Lex.initial_pos "test.ml")
    (Parser.App (Parser.Var "f", Parser.App (Parser.Var "g", Parser.Int 1)));
    Test.assert_eq "seq" (Parser.parse @@ Lex.lex "1; 2 |> f; 3" @@ Lex.initial_pos "test.ml")
    (Parser.Seq (
        Parser.Int 1,
        Parser.Seq (
            Parser.Pipeline (Parser.Int 2, Parser.Var "f"),
            Parser.Int 3
        )
    ));
    Test.assert_eq "match1" (Parser.parse @@ Lex.lex "match x with y -> 0 | z -> match z with a -> a" @@ Lex.initial_pos "test.ml")
    (Parser.Match (Parser.Var "x", [
        (Parser.PVar "y", Parser.Int 0);
        (Parser.PVar "z", Parser.Match (Parser.Var "z", [
            (Parser.PVar "a", Parser.Var "a");
        ]));
    ]));
    Test.assert_eq "match2" (Parser.parse @@ Lex.lex "match x with y -> let x = 1 in x | z -> 1" @@ Lex.initial_pos "test.ml")
    (Parser.Match (Parser.Var "x", [
        (Parser.PVar "y", Parser.Let ("x", Parser.Int 1, Parser.Var "x"));
        (Parser.PVar "z", Parser.Int 1);
    ]));
    Test.assert_eq "match nested" (Parser.parse @@ Lex.lex "match match [] with [] -> [] with [] -> []" @@ Lex.initial_pos "test.ml")
    (Parser.Match (Parser.Match (Parser.Emp, [Parser.PEmp, Parser.Emp]), [Parser.PEmp, Parser.Emp]))
