open OUnit2
open Days

let test_day03_part1_sample =
  "Day03 Part1 Sample" >:: (fun _ ->
    let sample_input = "987654321111111
811111111111119
234234234234278
818181911112111" in
    let result = Day03.Solution.part1 sample_input in
    assert_equal "357" result
  )

let test_day03_part2_sample =
  "Day03 Part2 Sample" >:: (fun _ ->
    let sample_input = "987654321111111
811111111111119
234234234234278
818181911112111" in
    let result = Day03.Solution.part2 sample_input in
    assert_equal "3121910778619" result
  )

let suite = "Day03 Tests" >::: [
  test_day03_part1_sample;
  test_day03_part2_sample;
]