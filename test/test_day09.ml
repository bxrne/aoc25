open OUnit2
open Days

let test_day09_part1_sample =
  "Day09 Part1 Sample" >:: (fun _ ->
    let sample_input = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
7,3" in
    let result = Day09.Solution.part1 sample_input in
    assert_equal "50" result
  )

let suite = "Day09 Tests" >::: [
  test_day09_part1_sample;
]