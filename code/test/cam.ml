open OUnit2
open S8.Cam

let add _ =
    let insts = [Ldi 1; Ldi 2; Add] in
    assert_equal (exec insts) (Int 3)

let access _ =
    let insts = [Ldi 1; Let; Access 0] in
    assert_equal (exec insts) (Int 1)

let bool _ =
    let insts = [Ldb true] in
    assert_equal (exec insts) (Bool true)

let closure_apply _ =
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

let test _ =
    let insts = [
        Ldb true;
        Test (
            [Ldi 1],
            [Ldi 2]
        );
    ] in
    assert_equal (exec insts) (Int 1)

let sum_to_10 _ =
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
        "add" >:: add;
        "access" >:: access;
        "bool" >:: bool;
        "closure_apply" >:: closure_apply;
        "test" >:: test;
        "sum_to_10" >:: sum_to_10;
    ]
