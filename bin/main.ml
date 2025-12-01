open Printf
open Aoc25

let () =
  let args = Sys.argv in
  if Array.length args < 2 then
    printf "Usage: %s <day>\n" args.(0)
  else
    let day = int_of_string args.(1) in
    try
      let input = read_input day in
      let part1, part2 = run_day day input in
      printf "Day %d\n" day;
      printf "Part 1: %s\n" part1;
      printf "Part 2: %s\n" part2
    with
    | Failure msg -> printf "Error: %s\n" msg
    | Sys_error msg -> printf "File error: %s\n" msg
