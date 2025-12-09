open OUnit2

(* Import all day test modules *)
module Test_day01 = Test_day01
module Test_day02 = Test_day02

let () = 
  run_test_tt_main ("All Tests" >::: [
    Test_day01.suite;
    Test_day02.suite;
  ])