open OUnit2
open Days

let test_day04_part1_sample =
  "Day04 Part1 Sample" >:: (fun _ ->
    let sample_input = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." in
    let result = Day04.Solution.part1 sample_input in
    assert_equal "13" result
  )

let test_day04_part2_sample =
  "Day04 Part2 Sample" >:: (fun _ ->
    let sample_input = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." in
    let result = Day04.Solution.part2 sample_input in
    assert_equal "43" result
  )

let suite = "Day04 Tests" >::: [
  test_day04_part1_sample;
  test_day04_part2_sample;
]