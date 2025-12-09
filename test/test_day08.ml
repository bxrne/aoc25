open OUnit2
open Days

let test_day08_part1_sample =
  "Day08 Part1 Sample" >:: (fun _ ->
    let sample_input = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689" in
    let result = Day08.Solution.part1 sample_input in
    assert_equal "20" result
  )

let test_day08_part2_sample =
  "Day08 Part2 Sample" >:: (fun _ ->
    let sample_input = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689" in
    let result = Day08.Solution.part2 sample_input in
    assert_equal "25272" result
  )

let suite = "Day08 Tests" >::: [
  test_day08_part1_sample;
  test_day08_part2_sample;
]