let () =
    Test.assert_eq "parse_add" (Parser.parse @@ Lex.lex "0+1+2" (Lex.initial_pos "test.ml"))
    (Ast.Add (Ast.Add(Ast.Int 0, Ast.Int 1), Ast.Int 2));
