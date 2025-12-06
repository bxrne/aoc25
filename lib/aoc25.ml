let read_input day : string =
  let filename = Printf.sprintf "days/day%02d/input.txt" day in
  In_channel.with_open_text filename In_channel.input_all

let run_day day input =
  match day with
  | 1 -> Day01.Solution.part1 input, Day01.Solution.part2 input
  | 2 -> Day02.Solution.part1 input, Day02.Solution.part2 input
  | 3 -> Day03.Solution.part1 input, Day03.Solution.part2 input
  | 4 -> Day04.Solution.part1 input, Day04.Solution.part2 input
  | 5 -> Day05.Solution.part1 input, Day05.Solution.part2 input
  | 6 -> Day06.Solution.part1 input, Day06.Solution.part2 input
  | _ -> failwith "Day not implemented yet"
