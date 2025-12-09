open OUnit2
open Days

let test_day06_part1_sample =
  "Day06 Part1 Sample" >:: (fun _ ->
    let sample_input = "123 328  51  64 
 45 64  387  23 
  6 98  215  314
*   +   *   +" in
    let result = Day06.Solution.part1 sample_input in
    assert_equal "4277556" result
  )

let test_day06_part2_sample =
  "Day06 Part2 Sample" >:: (fun _ ->
    let sample_input = "123 328  51  64 
 45 64  387  23 
  6 98  215  314
*   +   *   +" in
    let result = Day06.Solution.part2 sample_input in
    assert_equal "3263823" result
  )

let suite = "Day06 Tests" >::: [
  test_day06_part1_sample;
  test_day06_part2_sample;
]