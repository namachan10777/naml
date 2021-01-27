module P = Parser

let test name src right =
    let left =P.parse @@ Lex.lex src @@ Lex.initial_pos "test.ml" in
    try Test.assert_eq name left right with
    | Test.UnitTestError err ->
        Printf.printf "left  : %s\nright : %s\n" (P.show left) (P.show right);
        raise @@ Test.UnitTestError err

let () =
    test "parse_add" "0+1+2"
    (P.Add (P.Add(P.Int 0, P.Int 1), P.Int 2));
    test "parse_mul" "0*1*2"
    (P.Mul (P.Mul(P.Int 0, P.Int 1), P.Int 2));
    test "unary_minus" "1+-2"
    (P.Add (P.Int 1, P.Neg (P.Int 2)));
    test "mod" "3 mod 2"
    (P.Mod (P.Int 3, P.Int 2));
    test "add_r" "0+(1+2)"
    (P.Add (P.Int 0, P.Paren (P.Add (P.Int 1, P.Int 2))));
    test "4arith" "3*(200+300)/4-5"
    (P.Sub (
        P.Div (
            P.Mul (P.Int 3, P.Paren(P.Add (P.Int 200, P.Int 300))),
            P.Int 4
        ),
        P.Int 5
    ));
    test "bool" "3>2 && not not (1 < 0)"
    (P.And (
        P.Gret (P.Int 3, P.Int 2),
        P.Not (P.Not (P.Paren (P.Less (P.Int 1, P.Int 0))))
    ));
    test "ref" "ref 1" (P.Ref (P.Int 1));
    test "bool2"  "true && false || 1 = 2 && false"
    (P.Or (
        P.And (P.Bool true, P.Bool false),
        P.And (P.Eq (P.Int 1, P.Int 2), P.Bool false)
    ));
    test "let simple" "let x = 1 in x"
    (P.Let ("x", P.Int 1, P.Var "x"));
    test "let rec" "let rec x = 1 in x"
    (P.LetRec ("x", P.Int 1, P.Var "x"));
    test "let add left" "1 + let x = 1 in x"
    (P.Add (P.Int 1, P.Let ("x", P.Int 1, P.Var "x")));
    test "let add right" "(let x = 1 in x) + 1"
    (P.Add (P.Paren(P.Let ("x", P.Int 1, P.Var "x")), P.Int 1));
    test "let complex" "let x = let y = 1 in y in let z = x in z"
    (P.Let ("x", P.Let ("y", P.Int 1, P.Var "y"), P.Let ("z", P.Var "x", P.Var "z")));
    test "letfun" "let add x y = x + y in add"
    (P.Let ("add", P.Fun (["x"; "y"], P.Add (P.Var "x", P.Var "y")), P.Var "add"));
    Parser.count := 0;
    test "letfun_pat" "let add (x, y) (z, w) = x + y in add"
    (P.Let ("add",
        P.Fun (["<anonymous2>"; "<anonymous1>"],
            P.Match (P.Var "<anonymous2>", [
                (
                    P.PParen(P.PTuple ([P.PVar "x"; P.PVar "y"])),
                    P.Bool true,
                    P.Match (P.Var "<anonymous1>", [
                        (
                            P.PParen(P.PTuple ([P.PVar "z"; P.PVar "w"])),
                            P.Bool true,
                            P.Add (P.Var "x", P.Var "y"))
                    ])
                )
            ])
        )
    , P.Var "add"));
    test "letrecfun" "let rec add x y = x + y in add"
    (P.LetRec ("add", P.Fun (["x"; "y"], P.Add (P.Var "x", P.Var "y")), P.Var "add"));
    Parser.count := 0;
    test "letrecfun_pat" "let rec add (x, y) (z, w) = x + y in add"
    (P.LetRec ("add",
        P.Fun (["<anonymous2>"; "<anonymous1>"],
            P.Match (P.Var "<anonymous2>", [
                (
                    P.PParen(P.PTuple ([P.PVar "x"; P.PVar "y"])),
                    P.Bool true,
                    P.Match (P.Var "<anonymous1>", [
                        (
                            P.PParen(P.PTuple ([P.PVar "z"; P.PVar "w"])),
                            P.Bool true,
                            P.Add (P.Var "x", P.Var "y"))
                    ])
                )
            ])
        )
    , P.Var "add"));
    test "fun" "fun x y z -> x + y + z"
    (P.Fun (["x"; "y"; "z"], P.Add (P.Add (P.Var "x", P.Var "y"), P.Var "z")));
    test "fun" "(fun x y z -> x + y + z) 1"
    (P.App (
        P.Paren (P.Fun (["x"; "y"; "z"], P.Add (P.Add (P.Var "x", P.Var "y"), P.Var "z"))),
    P.Int 1));
    Parser.count := 0;
    test "fun pat" "fun x, y -> x + y"
    (P.Fun (["<anonymous1>"], P.Match (
        P.Var "<anonymous1>",
        [
            P.PTuple [P.PVar "x"; P.PVar "y"],
            P.Bool true,
            P.Add (P.Var "x", P.Var "y")
        ]
    )));
    Parser.count := 0;
    test "fun pat" "fun (x, y) (z, w) -> 0"
    (P.Fun (["<anonymous2>"; "<anonymous1>"], P.Match (
        P.Var "<anonymous2>",
        [
            P.PParen (P.PTuple [P.PVar "x"; P.PVar "y"]),
            P.Bool true,
            P.Match (P.Var "<anonymous1>", [
                P.PParen (P.PTuple [P.PVar "z"; P.PVar "w"]),
                P.Bool true,
                P.Int 0
            ])
        ]
    )));
    test "cons" "1 + 2 :: 3 :: [] = []"
    (P.Eq (
        P.Cons (P.Add (P.Int 1, P.Int 2), P.Cons (P.Int 3, P.Emp)),
        P.Emp
    ));
    test "pipeline" "1 |> f |> g"
    (P.Pipeline (P.Pipeline (P.Int 1, P.Var "f"), P.Var "g"));
    test "@@" "f @@ g @@ 1"
    (P.App (P.Var "f", P.App (P.Var "g", P.Int 1)));
    test "seq" "1; 2 |> f; 3"
    (P.Seq (
        P.Int 1,
        P.Seq (
            P.Pipeline (P.Int 2, P.Var "f"),
            P.Int 3
        )
    ));
    test "match1" "match x with y -> 0 | z -> match z with a -> a"
    (P.Match (P.Var "x", [
        (P.PVar "y", P.Bool true, P.Int 0);
        (P.PVar "z", P.Bool true, P.Match (P.Var "z", [
            (P.PVar "a",  P.Bool true, P.Var "a");
        ]));
    ]));
    test "match_when" "match x with y when is_primve y -> 0 | z -> 1"
    (P.Match (P.Var "x", [
        (P.PVar "y", P.App (P.Var "is_primve", P.Var "y"), P.Int 0);
        (P.PVar "z", P.Bool true, P.Int 1);
    ]));
    test "match2" "match x with y -> let x = 1 in x | z -> 1"
    (P.Match (P.Var "x", [
        (P.PVar "y", P.Bool true, P.Let ("x", P.Int 1, P.Var "x"));
        (P.PVar "z", P.Bool true, P.Int 1);
    ]));
    test "match nested" "match match [] with [] -> [] with [] -> []"
    (P.Match (P.Match (P.Emp, [P.PEmp, P.Bool true, P.Emp]), [P.PEmp, P.Bool true, P.Emp]));
    test "tuple1" "1, 2" (P.Tuple [P.Int 1; P.Int 2]);
    test "tuple1" "1+2, 2+3"
        (P.Tuple [
            P.Add (P.Int 1, P.Int 2);
            P.Add (P.Int 2, P.Int 3);
        ]);
    test "tuple1" "1, 2, 3, 4" (P.Tuple [P.Int 1; P.Int 2; P.Int 3; P.Int 4]);
    test "tuple nest" "1, (3, 4)" (P.Tuple [P.Int 1; P.Paren (P.Tuple [P.Int 3; P.Int 4])]);
    test "pattern_tuple" "match x with (x, y) -> x"
    (P.Match (P.Var "x", [
        (P.PParen (P.PTuple [P.PVar "x"; P.PVar "y"]), P.Bool true, P.Var "x");
    ]));
    test "pattern_tuple" "match x with (x, (y, z)) -> x"
    (P.Match (P.Var "x", [
        (P.PParen (P.PTuple [
            P.PVar "x";
            P.PParen (P.PTuple [P.PVar "y"; P.PVar "z"]);
        ]), P.Bool true, P.Var "x");
    ]));
    test "pattern_cons" "match x with x :: [] -> x"
    (P.Match (P.Var "x", [
        (P.PCons (P.PVar "x", P.PEmp), P.Bool true, P.Var "x")
    ]));
    test "parse_list1" "[1]" (P.Cons (P.Int 1, P.Emp));
    test "parse_list2" "[1;]" (P.Cons (P.Int 1, P.Emp));
    test "parse_list3" "[1;2;3]" (P.Cons (P.Int 1, P.Cons (P.Int 2, P.Cons (P.Int 3, P.Emp))));
    test "parse_list4" "[1;2;3;]" (P.Cons (P.Int 1, P.Cons (P.Int 2, P.Cons (P.Int 3, P.Emp))));
    test "parse_list5" "1 :: [2;3]" (P.Cons (P.Int 1, P.Cons (P.Int 2, P.Cons (P.Int 3, P.Emp))));
    test "parse_list1" "match [] with [1] -> []"
        (P.Match (P.Emp, [P.PCons (P.PInt 1, P.PEmp), P.Bool true, P.Emp]));
    test "parse_pattern_list2" "match [] with [1;] -> []"
        (P.Match (P.Emp, [P.PCons (P.PInt 1, P.PEmp), P.Bool true, P.Emp]));
    test "parse_pattern_list3" "match [] with [1;2;3] -> []"
        (P.Match (P.Emp, [P.PCons (P.PInt 1, P.PCons (P.PInt 2, P.PCons (P.PInt 3, P.PEmp))), P.Bool true, P.Emp]));
    test "parse_pattern_list4" "match [] with [1;2;3;] -> []"
        (P.Match (P.Emp, [P.PCons (P.PInt 1, P.PCons (P.PInt 2, P.PCons (P.PInt 3, P.PEmp))), P.Bool true, P.Emp]));
    test "parse_pattern_list5" "match [] with 1 :: [2;3] -> []"
        (P.Match (P.Emp, [P.PCons (P.PInt 1, P.PCons (P.PInt 2, P.PCons (P.PInt 3, P.PEmp))), P.Bool true, P.Emp]));
    test "parse_as" "match [] with 1, 2 as x -> []"
        (P.Match (P.Emp, [P.As [P.PTuple [P.PInt 1; P.PInt 2]; P.PVar "x"], P.Bool true, P.Emp]));
    test "let x, y = z in x" "let x, y = z in x"
        (P.Match (P.Var "z", [P.PTuple [P.PVar "x"; P.PVar "y"], P.Bool true, P.Var "x"]));
    test "app1" "1 + f 2"
        (P.Add (P.Int 1, P.App (P.Var "f", P.Int 2)));
    test "app2" "- f 2"
        (P.Neg (P.App (P.Var "f", P.Int 2)));
    test "app3" "f 1 + 2"
        (P.Add (P.App (P.Var "f", P.Int 1), P.Int 2));
    test "app4" "f 1 2"
        (P.App (P.App (P.Var "f", P.Int 1), P.Int 2));
    test "app5" "f 1 2 3"
        (P.App (P.App (P.App (P.Var "f", P.Int 1), P.Int 2), P.Int 3));
    test "if" "if f x then 1 else let x = 1 in x"
        (P.If (P.App (P.Var "f", P.Var "x"), P.Int 1, P.Let ("x", P.Int 1, P.Var "x")));
    test "if_nested" "if if x then true else false then true else if y then true else false"
        (P.If (
            P.If (P.Var "x", P.Bool true, P.Bool false),
            P.Bool true,
            P.If (P.Var "y", P.Bool true, P.Bool false)
        ));
    test "assign" "a := 1 + 2" (P.Assign (P.Var "a", P.Add (P.Int 1, P.Int 2)));
    test "assign" "a <- 1 + 2" (P.ArrayAssign (P.Var "a", P.Add (P.Int 1, P.Int 2)));
