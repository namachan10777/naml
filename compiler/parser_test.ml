let test name src right =
    let left =Parser.parse @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    try Test.assert_eq name left right with
    | Test.UnitTestError err ->
        Printf.printf "left  : %s\nright : %s\n" (Parser.show left) (Parser.show right);
        raise @@ Test.UnitTestError err

let () =
    test "parse_add" "0+1+2"
    (Parser.Add (Parser.Add(Parser.Int 0, Parser.Int 1), Parser.Int 2));
    test "parse_mul" "0*1*2"
    (Parser.Mul (Parser.Mul(Parser.Int 0, Parser.Int 1), Parser.Int 2));
    test "unary_minus" "1+-2"
    (Parser.Add (Parser.Int 1, Parser.Neg (Parser.Int 2)));
    test "mod" "3 mod 2"
    (Parser.Mod (Parser.Int 3, Parser.Int 2));
    test "add_r" "0+(1+2)"
    (Parser.Add (Parser.Int 0, Parser.Paren (Parser.Add (Parser.Int 1, Parser.Int 2))));
    test "4arith" "3*(200+300)/4-5"
    (Parser.Sub (
        Parser.Div (
            Parser.Mul (Parser.Int 3, Parser.Paren(Parser.Add (Parser.Int 200, Parser.Int 300))),
            Parser.Int 4
        ),
        Parser.Int 5
    ));
    test "bool" "3>2 && not not (1 < 0)"
    (Parser.And (
        Parser.Gret (Parser.Int 3, Parser.Int 2),
        Parser.Not (Parser.Not (Parser.Paren (Parser.Less (Parser.Int 1, Parser.Int 0))))
    ));
    test "bool2"  "true && false || 1 = 2 && false"
    (Parser.Or (
        Parser.And (Parser.Bool true, Parser.Bool false),
        Parser.And (Parser.Eq (Parser.Int 1, Parser.Int 2), Parser.Bool false)
    ));
    test "let simple" "let x = 1 in x"
    (Parser.Let ("x", Parser.Int 1, Parser.Var "x"));
    test "let rec" "let rec x = 1 in x"
    (Parser.LetRec ("x", Parser.Int 1, Parser.Var "x"));
    test "let add left" "1 + let x = 1 in x"
    (Parser.Add (Parser.Int 1, Parser.Let ("x", Parser.Int 1, Parser.Var "x")));
    test "let add right" "(let x = 1 in x) + 1"
    (Parser.Add (Parser.Paren(Parser.Let ("x", Parser.Int 1, Parser.Var "x")), Parser.Int 1));
    test "let complex" "let x = let y = 1 in y in let z = x in z"
    (Parser.Let ("x", Parser.Let ("y", Parser.Int 1, Parser.Var "y"), Parser.Let ("z", Parser.Var "x", Parser.Var "z")));
    test "fun" "fun x y z -> x + y + z"
    (Parser.Fun (["x"; "y"; "z"], Parser.Add (Parser.Add (Parser.Var "x", Parser.Var "y"), Parser.Var "z")));
    test "fun" "(fun x y z -> x + y + z) 1"
    (Parser.App (
        Parser.Paren (Parser.Fun (["x"; "y"; "z"], Parser.Add (Parser.Add (Parser.Var "x", Parser.Var "y"), Parser.Var "z"))),
    Parser.Int 1));
    test "cons" "1 + 2 :: 3 :: [] = []"
    (Parser.Eq (
        Parser.Cons (Parser.Add (Parser.Int 1, Parser.Int 2), Parser.Cons (Parser.Int 3, Parser.Emp)),
        Parser.Emp
    ));
    test "pipeline" "1 |> f |> g"
    (Parser.Pipeline (Parser.Pipeline (Parser.Int 1, Parser.Var "f"), Parser.Var "g"));
    test "@@" "f @@ g @@ 1"
    (Parser.App (Parser.Var "f", Parser.App (Parser.Var "g", Parser.Int 1)));
    test "seq" "1; 2 |> f; 3"
    (Parser.Seq (
        Parser.Int 1,
        Parser.Seq (
            Parser.Pipeline (Parser.Int 2, Parser.Var "f"),
            Parser.Int 3
        )
    ));
    test "match1" "match x with y -> 0 | z -> match z with a -> a"
    (Parser.Match (Parser.Var "x", [
        (Parser.PVar "y", Parser.Bool true, Parser.Int 0);
        (Parser.PVar "z", Parser.Bool true, Parser.Match (Parser.Var "z", [
            (Parser.PVar "a",  Parser.Bool true, Parser.Var "a");
        ]));
    ]));
    test "match_when" "match x with y when is_primve y -> 0 | z -> 1"
    (Parser.Match (Parser.Var "x", [
        (Parser.PVar "y", Parser.App (Parser.Var "is_primve", Parser.Var "y"), Parser.Int 0);
        (Parser.PVar "z", Parser.Bool true, Parser.Int 1);
    ]));
    test "match2" "match x with y -> let x = 1 in x | z -> 1"
    (Parser.Match (Parser.Var "x", [
        (Parser.PVar "y", Parser.Bool true, Parser.Let ("x", Parser.Int 1, Parser.Var "x"));
        (Parser.PVar "z", Parser.Bool true, Parser.Int 1);
    ]));
    test "match nested" "match match [] with [] -> [] with [] -> []"
    (Parser.Match (Parser.Match (Parser.Emp, [Parser.PEmp, Parser.Bool true, Parser.Emp]), [Parser.PEmp, Parser.Bool true, Parser.Emp]));
    test "tuple1" "1, 2" (Parser.Tuple [Parser.Int 1; Parser.Int 2]);
    test "tuple1" "1+2, 2+3"
        (Parser.Tuple [
            Parser.Add (Parser.Int 1, Parser.Int 2);
            Parser.Add (Parser.Int 2, Parser.Int 3);
        ]);
    test "tuple1" "1, 2, 3, 4" (Parser.Tuple [Parser.Int 1; Parser.Int 2; Parser.Int 3; Parser.Int 4]);
    test "tuple nest" "1, (3, 4)" (Parser.Tuple [Parser.Int 1; Parser.Paren (Parser.Tuple [Parser.Int 3; Parser.Int 4])]);
    test "pattern_tuple" "match x with (x, y) -> x"
    (Parser.Match (Parser.Var "x", [
        (Parser.PParen (Parser.PTuple [Parser.PVar "x"; Parser.PVar "y"]), Parser.Bool true, Parser.Var "x");
    ]));
    test "pattern_tuple" "match x with (x, (y, z)) -> x"
    (Parser.Match (Parser.Var "x", [
        (Parser.PParen (Parser.PTuple [
            Parser.PVar "x";
            Parser.PParen (Parser.PTuple [Parser.PVar "y"; Parser.PVar "z"]);
        ]), Parser.Bool true, Parser.Var "x");
    ]));
    test "pattern_cons" "match x with x :: [] -> x"
    (Parser.Match (Parser.Var "x", [
        (Parser.PCons (Parser.PVar "x", Parser.PEmp), Parser.Bool true, Parser.Var "x")
    ]));
    test "parse_list1" "[1]" (Parser.Cons (Parser.Int 1, Parser.Emp));
    test "parse_list2" "[1;]" (Parser.Cons (Parser.Int 1, Parser.Emp));
    test "parse_list3" "[1;2;3]" (Parser.Cons (Parser.Int 1, Parser.Cons (Parser.Int 2, Parser.Cons (Parser.Int 3, Parser.Emp))));
    test "parse_list4" "[1;2;3;]" (Parser.Cons (Parser.Int 1, Parser.Cons (Parser.Int 2, Parser.Cons (Parser.Int 3, Parser.Emp))));
    test "parse_list5" "1 :: [2;3]" (Parser.Cons (Parser.Int 1, Parser.Cons (Parser.Int 2, Parser.Cons (Parser.Int 3, Parser.Emp))));
    test "parse_list1" "match [] with [1] -> []"
        (Parser.Match (Parser.Emp, [Parser.PCons (Parser.PInt 1, Parser.PEmp), Parser.Bool true, Parser.Emp]));
    test "parse_pattern_list2" "match [] with [1;] -> []"
        (Parser.Match (Parser.Emp, [Parser.PCons (Parser.PInt 1, Parser.PEmp), Parser.Bool true, Parser.Emp]));
    test "parse_pattern_list3" "match [] with [1;2;3] -> []"
        (Parser.Match (Parser.Emp, [Parser.PCons (Parser.PInt 1, Parser.PCons (Parser.PInt 2, Parser.PCons (Parser.PInt 3, Parser.PEmp))), Parser.Bool true, Parser.Emp]));
    test "parse_pattern_list4" "match [] with [1;2;3;] -> []"
        (Parser.Match (Parser.Emp, [Parser.PCons (Parser.PInt 1, Parser.PCons (Parser.PInt 2, Parser.PCons (Parser.PInt 3, Parser.PEmp))), Parser.Bool true, Parser.Emp]));
    test "parse_pattern_list5" "match [] with 1 :: [2;3] -> []"
        (Parser.Match (Parser.Emp, [Parser.PCons (Parser.PInt 1, Parser.PCons (Parser.PInt 2, Parser.PCons (Parser.PInt 3, Parser.PEmp))), Parser.Bool true, Parser.Emp]));
    test "app1" "1 + f 2"
        (Parser.Add (Parser.Int 1, Parser.App (Parser.Var "f", Parser.Int 2)));
    test "app2" "- f 2"
        (Parser.Neg (Parser.App (Parser.Var "f", Parser.Int 2)));
    test "app3" "f 1 + 2"
        (Parser.Add (Parser.App (Parser.Var "f", Parser.Int 1), Parser.Int 2));
    test "app4" "f 1 2"
        (Parser.App (Parser.App (Parser.Var "f", Parser.Int 1), Parser.Int 2));
    test "app5" "f 1 2 3"
        (Parser.App (Parser.App (Parser.App (Parser.Var "f", Parser.Int 1), Parser.Int 2), Parser.Int 3));
    test "if" "if f x then 1 else let x = 1 in x"
        (Parser.If (Parser.App (Parser.Var "f", Parser.Var "x"), Parser.Int 1, Parser.Let ("x", Parser.Int 1, Parser.Var "x")));
    test "if_nested" "if if x then true else false then true else if y then true else false"
        (Parser.If (
            Parser.If (Parser.Var "x", Parser.Bool true, Parser.Bool false),
            Parser.Bool true,
            Parser.If (Parser.Var "y", Parser.Bool true, Parser.Bool false)
        ));
