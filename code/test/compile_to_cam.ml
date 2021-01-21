open S8.K7top
open S8.Cam
open S8.Compile_to_cam
open OUnit2

let test_add _ =
    let ast = parse_repl_string "1+2" in
    let insts = [Ldi 2; Ldi 1; Add] in
    assert_equal (compile ast) (insts)

let test_4arith _ =
    let ast = parse_repl_string "(1+2*3)/4" in
    let insts = [Ldi 4; Ldi 3; Ldi 2; Mul; Ldi 1; Add; Div] in
    assert_equal (compile ast) (insts)

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
    assert_equal (compile ast) (insts)

let suite =
    "Compile_to_cam" >::: [
        "test_add" >:: test_add;
        "test_4arith" >:: test_4arith;
        "test_compares" >:: test_compares;
    ]
