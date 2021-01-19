open OUnit2
open S8.K6ast
open S8.K6top

let parse_str _ =
    let ast = parse_repl_string "\"hoge\\\"fuga\"" in
    assert_equal ast (StrLit "hoge\\\"fuga")

let parse_add _ =
    let ast = parse_repl_string "1+2+3" in
    assert_equal ast (Add (Add (IntLit 1, IntLit 2), IntLit 3))

let eval_add _ =
    let value = eval_string "1+2+3" in
    assert_equal value (IntVal 6)

let parse_expr_with_paren _ =
    let ast = parse_repl_string "1+(2+3)" in
    assert_equal ast (Add (IntLit 1, Add (IntLit 2, IntLit 3)))

let eval_expr_with_paren _ =
    let value = eval_string "1+(2+3)" in
    assert_equal value (IntVal 6)

let parse_let _ =
    let ast = parse_repl_string "let x = 1 in x" in
    assert_equal ast (Let ("x", IntLit 1, Var "x"))

let parse_op_let _ =
    let ast = parse_repl_string "1 + let x = 1 in x" in
    assert_equal ast (Add (IntLit 1, Let ("x", IntLit 1, Var "x")))

let parse_let_op _ =
    let ast = parse_repl_string "(let x = 1 in x) + 1" in
    assert_equal ast (Add (Let ("x", IntLit 1, Var "x"), IntLit 1))

let parse_let_let _ =
    let ast = parse_repl_string "let x = let y = 1 in y in let z = x in z" in
    let expected = Let ("x", Let ("y", IntLit 1, Var "y"), Let ("z", Var "x", Var "z")) in
    assert_equal ast expected

let parse_let_stmts _ =
    let ast = parse_string "let x = 1 let y = 1" in
    assert_equal ast [LetStmt("x", IntLit 1); LetStmt("y", IntLit 1)]

let parse_emp_list1 _ =
    let ast = parse_repl_string "[ ]" in
    assert_equal ast Emp

let parse_emp_list2 _ =
    let ast = parse_repl_string "[]" in
    assert_equal ast Emp

let parse_list_single1 _ =
    let ast = parse_repl_string "[1]" in
    assert_equal ast (Cons (IntLit 1, Emp))

let parse_list_single2 _ =
    let ast = parse_repl_string "[1;]" in
    assert_equal ast (Cons (IntLit 1, Emp))

let parse_list1 _ =
    let ast = parse_repl_string "[1;2;3]" in
    assert_equal ast (Cons (IntLit 1, Cons (IntLit 2, Cons (IntLit 3, Emp))))

let parse_list2 _ =
    let ast = parse_repl_string "[1;2;3;]" in
    assert_equal ast (Cons (IntLit 1, Cons (IntLit 2, Cons (IntLit 3, Emp))))

let parse_tuple1 _ =
    let ast = parse_repl_string "1, 2" in
    assert_equal ast (Tuple [IntLit 1; IntLit 2])

let parse_tuple2 _ =
    let ast = parse_repl_string "1+2, 2+3" in
    assert_equal ast (Tuple [Add (IntLit 1, IntLit 2); Add(IntLit 2, IntLit 3)])

let parse_4arith _ =
    let ast = parse_repl_string "3*(200+300)/4-5" in
    let expected = Sub (
        Div (Mul (IntLit 3, Add (IntLit 200, IntLit 300)), IntLit 4),
        IntLit 5
    )
    in assert_equal ast expected

let eval_4arith _ =
    let ast = eval_string "3*(200+300)/4-5" in
    assert_equal ast (IntVal 370)

let parse_match1 _ =
    let ast = parse_repl_string "match x with y -> 0 | z -> match z with a -> a" in
    let expected = Match (Var "x", [
        (PVar "y", IntLit 0);
        (PVar "z", Match (Var "z", [PVar "a", Var "a"]));
    ])
    in assert_equal ast expected

let parse_match2 _ =
    let ast = parse_repl_string "match x with y -> let x = 1 in x | z -> 1" in
    let expected = Match (Var "x", [
        (PVar "y", Let ("x", IntLit 1, Var "x"));
        (PVar "z", IntLit 1);
    ])
    in assert_equal ast expected

let parse_builtin _ =
    let ast = parse_repl_string "builtin \"hd\"" in
    let expected = Builtin "hd" in
    assert_equal ast expected

let parse_app1 _ =
    let ast = parse_repl_string "a b c" in
    let expected = App (App (Var "a", Var "b"), Var "c") in
    assert_equal ast expected

let parse_app2 _ =
    let ast = parse_repl_string "1 + a b" in
    let expected = Add (IntLit 1, App (Var "a", Var "b")) in
    assert_equal ast expected

let parse_app3 _ =
    let ast = parse_repl_string "(a b) c" in
    let expected = App (App (Var "a", Var "b"), Var "c") in
    assert_equal ast expected

let suite =
    "Kadai6" >::: [
        "parse_str" >:: parse_str;
        "parse_add" >:: parse_add;
        "eval_add" >:: parse_add;
        "parse_expr_with_paren" >:: parse_expr_with_paren;
        "eval_expr_with_paren" >:: eval_expr_with_paren;
        "parse_let" >:: parse_let;
        "parse_op_let" >:: parse_op_let;
        "parse_let_let" >:: parse_let_let;
        "parse_emp_list1" >:: parse_emp_list1;
        "parse_emp_list2" >:: parse_emp_list2;
        "parse_list_single1" >:: parse_list_single1;
        "parse_list_single2" >:: parse_list_single2;
        "parse_list1" >:: parse_list1;
        "parse_list2" >:: parse_list2;
        "parse_tuple1" >:: parse_tuple1;
        "parse_tuple2" >:: parse_tuple2;
        "parse_4arith" >:: parse_4arith;
        "eval_4arith" >:: eval_4arith;
        "parse_match1" >:: parse_match1;
        "parse_match2" >:: parse_match2;
        "parse_builtin" >:: parse_builtin;
        "parse_app1" >:: parse_app1;
        "parse_app2" >:: parse_app2;
        "parse_app3" >:: parse_app3;
    ]
