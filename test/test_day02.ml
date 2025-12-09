open OUnit2
open Days

let test_day02_part1_sample =
  "Day02 Part1 Sample" >:: (fun _ ->
    let sample_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" in
    let result = Day02.Solution.part1 sample_input in
    assert_equal "1227775554" result
  )

let test_day02_part2_sample =
  "Day02 Part2 Sample" >:: (fun _ ->
    let sample_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124" in
    let result = Day02.Solution.part2 sample_input in
    assert_equal "4174379265" result
  )

let suite = "Day02 Tests" >::: [
  test_day02_part1_sample;
  test_day02_part2_sample;
]