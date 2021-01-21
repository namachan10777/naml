open S8.K7top
open S8.Cam
open S8.Compile_to_cam
open OUnit2

let test_add _ =
    let ast = parse_repl_string "1+2" in
    let insts = [Ldi 2; Ldi 1; Add] in
    assert_equal (compile [] ast) (insts)

let test_4arith _ =
    let ast = parse_repl_string "(1+2*3)/4" in
    let insts = [Ldi 4; Ldi 3; Ldi 2; Mul; Ldi 1; Add; Div] in
    assert_equal (compile [] ast) (insts)

let test_compares _ =
    let ast = parse_repl_string "true || 1 > 2 && 2 = 3" in
    let insts = [
        Ldb true;
        Test (
            [Ldb true],
            [
                Ldi 2;
                Ldi 1;
                Gret;
                Test (
                    [Ldi 3; Ldi 2; Eq],
                    [Ldb false]
                );
            ]
        );
    ] in
    assert_equal (compile [] ast) (insts)

let test_if _ =
    let ast = parse_repl_string "if 1 = 1 then 1 else 2" in
    let insts = [
        Ldi 1;
        Ldi 1;
        Eq;
        Test (
            [Ldi 1],
            [Ldi 2]
        );
    ] in
    assert_equal (compile [] ast) insts

let test_seq _ =
    let ast = parse_repl_string "1; 2; 3" in
    let insts = [
        Ldi 1;
        Ldi 2;
        Ldi 3;
    ] in
    assert_equal (compile [] ast) insts

let test_let _ =
    let ast = parse_repl_string "let x = 1 in x" in
    let insts = [
        Ldi 1;
        Let;
        Access 0;
        EndLet;
    ] in
    assert_equal (compile [] ast) insts

let test_fun _ =
    let ast = parse_repl_string "fun x -> x" in
    let insts = [
        Closure [
            Access 0;
            Return;
        ];
    ] in
    assert_equal (compile [] ast) insts

let test_letrec _ =
    let ast = parse_repl_string "let rec f x = f x in f 0" in
    let insts = [
        Closure [
            Access 0;
            Access 1;
            Apply;
            Return;
        ];
        Let;
        Ldi 0;
        Access 0;
        Apply;
        EndLet;
    ] in
    assert_equal (compile [] ast) insts

let suite =
    "Compile_to_cam" >::: [
        "test_add" >:: test_add;
        "test_4arith" >:: test_4arith;
        "test_compares" >:: test_compares;
        "test_if" >:: test_if;
        "test_seq" >:: test_seq;
        "test_let" >:: test_let;
        "test_fun" >:: test_fun;
        "test_letrec" >:: test_letrec;
    ]
