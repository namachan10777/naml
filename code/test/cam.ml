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


let suite =
    "Cam" >::: [
        "add" >:: add;
        "access" >:: access;
        "bool" >:: bool;
        "closure_apply" >:: closure_apply;
        "test" >:: test;
    ]
