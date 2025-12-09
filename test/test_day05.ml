open OUnit2
open Days

let test_day05_part1_sample =
  "Day05 Part1 Sample" >:: (fun _ ->
    let sample_input = "3-5
10-14
16-20
12-18

1
5
8
11
17
32" in
    let result = Day05.Solution.part1 sample_input in
    assert_equal "3" result
  )

let test_day05_part2_sample =
  "Day05 Part2 Sample" >:: (fun _ ->
    let sample_input = "3-5
10-14
16-20
12-18" in
    let result = Day05.Solution.part2 sample_input in
    assert_equal "14" result
  )

let suite = "Day05 Tests" >::: [
  test_day05_part1_sample;
  test_day05_part2_sample;
]