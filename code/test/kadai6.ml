open OUnit2
open S8.K6ast
open S8.K6top

let parse_add _ =
    let ast = parse_string "1+2+3" in
    assert_equal ast (Add (Add (IntLit 1, IntLit 2), IntLit 3))

let eval_add _ =
    let value = eval_string "1+2+3" in
    assert_equal value (IntVal 6)

let parse_expr_with_paren _ =
    let ast = parse_string "1+(2+3)" in
    assert_equal ast (Add (IntLit 1, Add (IntLit 2, IntLit 3)))

let eval_expr_with_paren _ =
    let value = eval_string "1+(2+3)" in
    assert_equal value (IntVal 6)

let parse_let _ =
    let ast = parse_string "let x = 1 in x" in
    assert_equal ast (Let ("x", IntLit 1, Var "x"))

let parse_op_let _ =
    let ast = parse_string "1 + let x = 1 in x" in
    assert_equal ast (Add (IntLit 1, Let ("x", IntLit 1, Var "x")))

let parse_let_let _ =
    let ast = parse_string "let x = let y = 1 in y in let z = x in z" in
    let expected = Let ("x", Let ("y", IntLit 1, Var "y"), Let ("z", Var "x", Var "z")) in
    assert_equal ast expected

let suite =
    "Kadai6" >::: [
        "parse_add" >:: parse_add;
        "eval_add" >:: parse_add;
        "parse_expr_with_paren" >:: parse_expr_with_paren;
        "eval_expr_with_paren" >:: eval_expr_with_paren;
        "parse_let" >:: parse_let;
        "parse_op_let" >:: parse_op_let;
        "parse_let_let" >:: parse_let_let;
    ]
