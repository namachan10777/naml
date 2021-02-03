open OUnit2

let () =
    run_test_tt_main Kadai4.suite ;
    run_test_tt_main Kadai5.suite ;
    run_test_tt_main Kadai6.suite ;
    run_test_tt_main Kadai7.suite ;
    run_test_tt_main Cam.suite ;
    run_test_tt_main Compile_to_cam.suite ;
    run_test_tt_main Zam.suite ;
    run_test_tt_main Compile_to_zam.suite
