open OUnit2
open S8.K5ast
open S8.K5top

let parse_int _ =
    let ast = parse_string "1" in
    assert_equal ast (IntLit 1)

let parse_add _ =
    let ast = parse_string "1+2" in
    assert_equal ast (Plus (IntLit 1, IntLit 2))

let parse_op_complex _ =
    let ast = parse_string "1*2+3*(4+5)+6" in
    let expected =
        Plus
          ( Plus
              ( Times (IntLit 1, IntLit 2)
              , Times (IntLit 3, Plus (IntLit 4, IntLit 5)) )
          , IntLit 6 )
    in
    assert_equal ast expected

let parse_let _ =
    let ast = parse_string "let a = b in a * 1" in
    let expected = Let ("a", Var "b", Times (Var "a", IntLit 1)) in
    assert_equal ast expected

let parse_double_let _ =
    let ast = parse_string "let a = let b = 1 in b in a * 1" in
    let expected =
        Let ("a", Let ("b", IntLit 1, Var "b"), Times (Var "a", IntLit 1))
    in
    assert_equal ast expected

let parse_match _ =
    let ast = parse_string "match x with a -> a | 1 :: [] -> 2" in
    let expected =
        Match (Var "x", [(Var "a", Var "a"); (Cons (IntLit 1, Empty), IntLit 2)])
    in
    assert_equal ast expected

let parse_exp_open _ =
    let ast = parse_string "1+let a = 2 in a" in
    let expected = Plus (IntLit 1, Let ("a", IntLit 2, Var "a")) in
    assert_equal ast expected

let parse_op_and_app _ =
    let ast = parse_string "1 * a 2" in
    let expected = Times (IntLit 1, App (Var "a", IntLit 2)) in
    assert_equal ast expected

let parse_list _ =
    let ast = parse_string "1 :: [2;3]" in
    let expected = Cons (IntLit 1, Cons (IntLit 2, Cons (IntLit 3, Empty))) in
    assert_equal ast expected

let parse_fun _ =
    let ast = parse_string "fun f -> fun x -> fun y -> 1" in
    let expected = Fun ("f", Fun ("x", Fun ("y", IntLit 1))) in
    assert_equal ast expected

let test_fun _ =
    let ret =
        eval_string
          "let f = fun x -> fun y -> fun z -> x * 100 + y * 10 + z in f 1 2 3"
    in
    assert_equal ret (IntVal 123)

let test_z _ =
    let fix =
        parse_string
          "fun f -> (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y))"
    in
    let fact =
        parse_string "fun f -> fun n -> if n = 1 then 1 else n * (f (n-1))"
    in
    let exp = App (App (fix, fact), IntLit 5) in
    assert_equal (eval (emptyenv ()) exp) (IntVal 120)

let test_fact _ =
    let ret =
        eval_string
          "let rec fact n = if n = 1 then 1 else n * fact (n-1) in fact 5"
    in
    let expected = IntVal (5 * 4 * 3 * 2 * 1) in
    assert_equal ret expected

let suite =
    "Kadai5"
    >::: [ "int" >:: parse_int
         ; "add" >:: parse_add
         ; "op_complex" >:: parse_op_complex
         ; "let" >:: parse_let
         ; "match" >:: parse_match
         ; "parse_exp_open" >:: parse_exp_open
         ; "parse_lit" >:: parse_list
         ; "parse_fun" >:: parse_fun
         ; "parse_op_and_app" >:: parse_op_and_app
         ; "test_fun" >:: test_fun
         ; "test_z" >:: test_z
         ; "test_fact" >:: test_fact ]
