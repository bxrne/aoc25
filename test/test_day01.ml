open OUnit2
open Days

let test_day01_part1_sample =
  "Day01 Part1 Sample" >:: (fun _ ->
    let sample_input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82" in
    let result = Day01.Solution.part1 sample_input in
    assert_equal "3" result
  )

let test_day01_part2_sample =
  "Day01 Part2 Sample" >:: (fun _ ->
    let sample_input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82" in
    let result = Day01.Solution.part2 sample_input in
    assert_equal "6" result
  )

let suite = "Day01 Tests" >::: [
  test_day01_part1_sample;
  test_day01_part2_sample;
]