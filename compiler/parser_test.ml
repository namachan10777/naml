let () =
    Test.assert_eq "parse_add" (Parser.parse @@ Lex.lex "0+1+2" (Lex.initial_pos "test.ml"))
    (Ast.Add (Ast.Add(Ast.Int 0, Ast.Int 1), Ast.Int 2));
    Test.assert_eq "parse_mul" (Parser.parse @@ Lex.lex "0*1*2" (Lex.initial_pos "test.ml"))
    (Ast.Mul (Ast.Mul(Ast.Int 0, Ast.Int 1), Ast.Int 2));
    Test.assert_eq "unary_minus" (Parser.parse @@ Lex.lex "1+-2" (Lex.initial_pos "test.ml"))
    (Ast.Add (Ast.Int 1, Ast.Neg (Ast.Int 2)));
    Test.assert_eq "mod" (Parser.parse @@ Lex.lex "3 mod 2" (Lex.initial_pos "test.ml"))
    (Ast.Mod (Ast.Int 3, Ast.Int 2));
    Test.assert_eq "add_r" (Parser.parse @@ Lex.lex "0+(1+2)" (Lex.initial_pos "test.ml"))
    (Ast.Add (Ast.Int 0, Ast.Add (Ast.Int 1, Ast.Int 2)));
    Test.assert_eq "4arith" (Parser.parse @@ Lex.lex "3*(200+300)/4-5" @@ Lex.initial_pos "test.ml")
    (Ast.Sub (
        Ast.Div (
            Ast.Mul (Ast.Int 3, Ast.Add (Ast.Int 200, Ast.Int 300)),
            Ast.Int 4
        ),
        Ast.Int 5
    ));
    Test.assert_eq "bool" (Parser.parse @@ Lex.lex "3>2 && not not (1 < 0)" @@ Lex.initial_pos "test.ml")
    (Ast.And (
        Ast.Gret (Ast.Int 3, Ast.Int 2),
        Ast.Not (Ast.Not (Ast.Less (Ast.Int 1, Ast.Int 0)))
    ));
    Test.assert_eq "bool2" (Parser.parse @@ Lex.lex  "true && false || 1 = 2 && false" @@ Lex.initial_pos "test.ml")
    (Ast.Or (
        Ast.And (Ast.Bool true, Ast.Bool false),
        Ast.And (Ast.Eq (Ast.Int 1, Ast.Int 2), Ast.Bool false)
    ));
    Test.assert_eq "let simple" (Parser.parse @@ Lex.lex "let x = 1 in x" @@ Lex.initial_pos "test.ml")
    (Ast.Let ("x", Ast.Int 1, Ast.Var "x"));
    Test.assert_eq "let add left" (Parser.parse @@ Lex.lex "1 + let x = 1 in x" @@ Lex.initial_pos "test.ml")
    (Ast.Add (Ast.Int 1, Ast.Let ("x", Ast.Int 1, Ast.Var "x")));
    Test.assert_eq "let add right" (Parser.parse @@ Lex.lex"(let x = 1 in x) + 1" @@ Lex.initial_pos "test.ml")
    (Ast.Add (Ast.Let ("x", Ast.Int 1, Ast.Var "x"), Ast.Int 1));
    Test.assert_eq "let complex" (Parser.parse @@ Lex.lex"let x = let y = 1 in y in let z = x in z" @@ Lex.initial_pos "test.ml")
    (Ast.Let ("x", Ast.Let ("y", Ast.Int 1, Ast.Var "y"), Ast.Let ("z", Ast.Var "x", Ast.Var "z")));
