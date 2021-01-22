open OUnit2
open S8.Zam

let test_finish _ =
    let zam = {
        code = [];
        rstack = [];
        astack = [Int 0];
        env = [];
    } in
    assert_equal (exec zam) (Int 0)

let test_finish2 _ =
    let zam = {
        code = [];
        rstack = [];
        astack = [Int 0];
        env = [Int 0];
    } in
    assert_raises (Failure "invalid state") (fun _ -> exec zam)

let suite =
    "Zam" >::: [
        "finish" >:: test_finish;
        "finish2" >:: test_finish2;
    ]
