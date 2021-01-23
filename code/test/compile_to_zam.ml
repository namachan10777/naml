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
    ]
