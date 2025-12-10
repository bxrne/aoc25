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

let test_day09_part2_sample =
  "Day09 Part2 Sample" >:: (fun _ ->
    let sample_input = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3" in
    let result = Day09.Solution.part2 sample_input in
    assert_equal "24" result
  )

let test_day09_part2_triangle =
  "Day09 Part2 Triangle" >:: (fun _ ->
    let sample_input = "0,0
4,0
2,3" in
    let result = Day09.Solution.part2 sample_input in
    assert_equal "6" result
  )

let suite = "Day09 Tests" >::: [
  test_day09_part1_sample;
  test_day09_part2_sample;
  test_day09_part2_triangle;
]
