open OUnit2
open S8.Cam

let test_add _ =
    let insts = [Ldi 2; Ldi 1; Add] in
    assert_equal (exec insts) (Int 3)

let test_access _ =
    let insts = [Ldi 1; Let; Access 0] in
    assert_equal (exec insts) (Int 1)

let test_bool _ =
    let insts = [Ldb true] in
    assert_equal (exec insts) (Bool true)

let test_sub _ =
    let insts = [Ldi 2; Ldi 1; Sub] in
    assert_equal (exec insts) (Int (-1))

let test_mul _ =
    let insts = [Ldi 2; Ldi 1; Mul] in
    assert_equal (exec insts) (Int 2)

let test_div _ =
    let insts = [Ldi 2; Ldi 1; Div] in
    assert_equal (exec insts) (Int 0)

let test_mod _ =
    let insts = [Ldi 2; Ldi 1; Mod] in
    assert_equal (exec insts) (Int 1)

let test_and _ =
    let insts = [Ldb true; Ldb false; And] in
    assert_equal (exec insts) (Bool false)

let test_or _ =
    let insts = [Ldb true; Ldb false; Or] in
    assert_equal (exec insts) (Bool true)

let test_not _ =
    let insts = [Ldb true; Not] in
    assert_equal (exec insts) (Bool false)

let test_gret _ =
    let insts = [Ldi 1; Ldi 2; Gret] in
    assert_equal (exec insts) (Bool true)

let test_less _ =
    let insts = [Ldi 1; Ldi 2; Less] in
    assert_equal (exec insts) (Bool false)

let test_closure_apply _ =
    let insts = [
        Ldi 1;
        Closure [
            Access 0;
            Ldi 1;
            Add;
            Return;
        ];
        Apply;
        Ldi 1;
        Add;
    ] in
    assert_equal (exec insts) (Int 3)

let test_test _ =
    let insts = [
        Ldb true;
        Test (
            [Ldi 1],
            [Ldi 2]
        );
    ] in
    assert_equal (exec insts) (Int 1)

let test_sum_to_10 _ =
    let insts = [
        Closure [
            Ldi 1;
            Access 0;
            Eq;
            Test (
                [Ldi 1],
                [Ldi (-1); Access 0; Add; Access 1; Apply; Access 0; Add]
            );
            Return
        ];
        Let;
        Ldi 10;
        Access 0;
        Apply;
        EndLet
    ] in assert_equal (exec insts) (Int (1+2+3+4+5+6+7+8+9+10))


let suite =
    "Cam" >::: [
        "add"           >:: test_add;
        "sub"           >:: test_sub;
        "mul"           >:: test_mul;
        "div"           >:: test_div;
        "mod"           >:: test_mod;
        "and"           >:: test_and;
        "or"           >:: test_or;
        "not"           >:: test_not;
        "gret"           >:: test_gret;
        "less"           >:: test_less;
        "access"        >:: test_access;
        "bool"          >:: test_bool;
        "closure_apply" >:: test_closure_apply;
        "test"          >:: test_test;
        "sum_to_10"     >:: test_sum_to_10;
    ]
