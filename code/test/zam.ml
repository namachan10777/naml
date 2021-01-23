open OUnit2
open S8.Zam

let emp_zam = {
    code = [];
    rstack = [];
    astack = [];
    env = [];
}

let test_finish _ =
    let zam = { emp_zam with astack = [Int 42] } in
    assert_equal (exec zam) (Int 42)

let test_finish2 _ =
    let zam = { emp_zam with astack = [Int 42]; env = [Int 42] } in
    assert_raises (Failure "invalid state") (fun _ -> exec zam)

let test_ldi _ =
    let zam = { emp_zam with code = [Ldi 42] } in
    assert_equal (exec zam) (Int 42)

let test_ldb _ =
    let zam = { emp_zam with code = [Ldb true] } in
    assert_equal (exec zam) (Bool true)

let test_lds _ =
    let zam = { emp_zam with code = [Lds "hoge"] } in
    assert_equal (exec zam) (Str "hoge")

let test_access _ =
    let zam = { emp_zam with code = [Access 1; EndLet; EndLet]; env = [Int 42; Str "hoge"] } in
    assert_equal (exec zam) (Str "hoge")

let test_let _ =
    let zam = { emp_zam with code = [Ldi 42; Let; Access 0; EndLet]; } in
    assert_equal (exec zam) (Int 42)

(* (fun x -> x) 42 *)
let test_clos_and_app _ =
    let zam = { emp_zam with
        code = [
            PushMark;
            Ldi 42;
            Closure [Access 0; Return];
            App;
        ]
    } in
    assert_equal (exec zam) (Int 42)

let test_test _ =
    let zam = { emp_zam with
        code = [
            Ldb true;
            Test (
                [Ldi 42],
                [Ldi 2]
            )
        ]
    } in
    assert_equal (exec zam) (Int 42)

let test_sum _ =
    let zam = { emp_zam with
        code = [
            PushMark;
            Ldi 3;
            Closure [
                Ldi 0;
                Access 0;
                Eq;
                Test (
                    [Ldi 0; Return],
                    [PushMark; Ldi 1; Access 0; Sub; Access 1; App; Access 0; Add; Return]
                );
            ];
            App;
        ]
    } in
    assert_equal (exec zam) (Int 6)

let test_2args_curried _ =
    let zam = { emp_zam with
        code = [
            PushMark;
            Ldi 1;
            PushMark;
            Ldi 2;
            Closure [
                Closure [
                    Access 0;
                    Access 2;
                    Add;
                    Return;
                ];
                Return;
            ];
            App;
            App;
        ]
    } in
    assert_equal (exec zam) (Int 3)

let test_2args_uncurried _ =
    let zam = { emp_zam with
        code = [
            PushMark;
            Ldi 1;
            Ldi 2;
            Closure [
                Grab;
                Access 0;
                Access 2;
                Add;
                Return;
            ];
            App;
        ]
    } in
    assert_equal (exec zam) (Int 3)

let test_partial_app_uncurried _ =
    let zam = { emp_zam with
        code = [
            PushMark;
            Ldi 2;
            PushMark;
            Ldi 1;
            Closure [
                Grab;
                Access 0;
                Access 2;
                Add;
                Return;
            ];
            App;
            App;
        ]
    } in
    Printf.printf "%s\n" @@ show_val_t (exec zam);
    assert_equal (exec zam) (Int 3)


let suite =
    "Zam" >::: [
        "finish" >:: test_finish;
        "finish2" >:: test_finish2;
        "test_ldi" >:: test_ldi;
        "test_ldb" >:: test_ldb;
        "test_lds" >:: test_lds;
        "test_access" >:: test_access;
        "test_let" >:: test_let;
        "test_test" >:: test_test;
        "test_app" >:: test_clos_and_app;
        "test_sum" >:: test_sum;
        "test_2args_curried" >:: test_2args_curried;
        "test_2args_uncurried" >:: test_2args_uncurried;
        "test_partial_app_uncurried" >:: test_partial_app_uncurried;
    ]
