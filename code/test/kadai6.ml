open OUnit2
open S8.K6ast
open S8.K6top

let parse_add _ =
    let ast = parse_string "1+2+3" in
    assert_equal ast (Add (Add (IntLit 1, IntLit 2), IntLit 3))

let eval_add _ =
    let ast = eval_string "1+2+3" in
    assert_equal ast (IntVal 6)

let suite =
    "Kadai6" >::: [
        "parse_add" >:: parse_add;
        "eval_add" >:: parse_add;
    ]
