open OUnit2

(* Import all day test modules *)
module Test_day01 = Test_day01
module Test_day02 = Test_day02
module Test_day03 = Test_day03
module Test_day04 = Test_day04
module Test_day05 = Test_day05
module Test_day06 = Test_day06
module Test_day07 = Test_day07
module Test_day08 = Test_day08
module Test_day09 = Test_day09

let () = 
  run_test_tt_main ("All Tests" >::: [
    Test_day01.suite;
    Test_day02.suite;
    Test_day03.suite;
    Test_day04.suite;
    Test_day05.suite;
    Test_day06.suite;
    Test_day07.suite;
    Test_day08.suite;
    Test_day09.suite;
  ])