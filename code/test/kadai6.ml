open OUnit2
open S8.K6ast
open S8.K6top

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

let suite =
    "Kadai6" >::: [
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
    ]
