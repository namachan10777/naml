open OUnit2
open S8.Zam

let emp = {
    code = [];
    env = [];
    astack = [];
    rstack = [];
}

let compile_ldi _ =
    assert_equal (S8.Compile_to_zam.compile (S8.K7ast.IntLit 42)) { emp with code = [Ldi 42] }

let compile_ldb _ =
    assert_equal (S8.Compile_to_zam.compile (S8.K7ast.BoolLit true)) { emp with code = [Ldb true] }

let compile_lds _ =
    assert_equal (S8.Compile_to_zam.compile (S8.K7ast.StrLit "hoge")) { emp with code = [Lds "hoge"] }

let run_ldi _ =
    let zam = S8.Compile_to_zam.compile (S8.K7ast.IntLit 42) in
    assert_equal (exec zam) (Int 42)

let run_ldb _ =
    let zam =  S8.Compile_to_zam.compile (S8.K7ast.BoolLit true) in
    assert_equal (exec zam) (Bool true)

let run_lds _ =
    let zam = S8.Compile_to_zam.compile (S8.K7ast.StrLit "hoge") in
    assert_equal (exec zam) (Str "hoge")

let run_4arith _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "2*(3+5)-1" in
    assert_equal (exec zam) (Int 15)

let run_bool _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "1 = 1 && 2 = 3 || 4 = 4" in
    assert_equal (exec zam) (Bool true)

let run_seq _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "1;2;3" in
    assert_equal (exec zam) (Int 3)

let run_let _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "let x = 1 in let y = x + 2 in y + 3" in
    assert_equal (exec zam) (Int 6)

let run_fun _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "(fun x y z -> x * 100 + y * 10 + z) 1 2 3" in
    assert_equal (exec zam) (Int 123)

let run_partial_app _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "let f = (fun x y z -> x * 100 + y * 10 + z) 1 2 in f 3" in
    assert_equal (exec zam) (Int 123)

let run_fact _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "let rec fact n = if n = 1 then 1 else n * fact (n-1) in fact 5" in
    assert_equal (exec zam) (Int 120)

let run_sum _ =
    let zam = S8.Compile_to_zam.compile @@ S8.K7top.parse_repl_string "let rec sum n = if n = 0 then 0 else n + sum (n-1) in sum 3"
    in assert_equal (exec zam) (Int 6)

let suite =
    "Compile_to_zam" >::: [
        "compile_ldi" >:: compile_ldi;
        "compile_ldb" >:: compile_ldb;
        "compile_lds" >:: compile_lds;
        "run_ldi" >:: run_ldi;
        "run_ldb" >:: run_ldb;
        "run_lds" >:: run_lds;
        "run_4arith" >:: run_4arith;
        "run_bool" >:: run_bool;
        "run_seq" >:: run_seq;
        "run_let" >:: run_let;
        "run_fun" >:: run_fun;
        "run_partial_app" >:: run_partial_app;
        "run_fact" >:: run_fact;
        "run_sum" >:: run_sum;
    ]
