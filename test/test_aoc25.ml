open OUnit2

(* Import all day test modules *)
module Test_day01 = Test_day01

let () = 
  run_test_tt_main Test_day01.suite