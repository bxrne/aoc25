module Solution = struct
  (** Represents a range of integers [start, end_pos] inclusive *)
  type range = {
    start : int;
    end_pos : int;
  }

  (** Parse non-empty lines from input string *)
  let parse_input (input : string) : string list =
    String.split_on_char '\n' input
    |> List.filter (fun line -> String.trim line <> "")

  (** Parse a single range from "start-end" format *)
  let parse_range (s : string) : range =
    match String.split_on_char '-' s with
    | [start_str; end_str] ->
        { start = int_of_string start_str;
          end_pos = int_of_string end_str }
    | _ -> 
        failwith (Printf.sprintf "Invalid range format: '%s'" s)

  (** Parse comma-separated ranges from multiple lines *)
  let parse_ranges (lines : string list) : range list =
    lines
    |> List.map (String.split_on_char ',')
    |> List.flatten
    |> List.map parse_range

  (** Check if a number has a leading zero (when > 9) *)
  let has_leading_zero (n : int) : bool =
    let s = string_of_int n in
    String.length s > 1 && s.[0] = '0'

  (** Check if first half of string equals second half *)
  let has_repeating_halves (s : string) : bool =
    let len = String.length s in
    if len mod 2 <> 0 then false
    else
      let half = len / 2 in
      String.sub s 0 half = String.sub s half half

  (** A value is invalid if it has leading zeros or repeating halves *)
  let is_invalid_value (n : int) : bool =
    let s = string_of_int n in
    has_leading_zero n || has_repeating_halves s

  (** Generate all integers in a range [start, end_pos] *)
  let range_to_list (r : range) : int list =
    List.init (r.end_pos - r.start + 1) (fun i -> r.start + i)

  (** Collect all invalid values across multiple ranges *)
  let collect_invalid_values (ranges : range list) : int list =
    ranges
    |> List.map range_to_list
    |> List.flatten
    |> List.filter is_invalid_value

  let part1 (input : string) : string =
    input
    |> parse_input
    |> parse_ranges
    |> collect_invalid_values
    |> List.fold_left (+) 0
    |> string_of_int

  (** A value is invalid if it is a repeated sequence of a pattern of 2 chars or more **)
  let is_invalid_sequence s =
    let len = String.length s in
    let rec check_pattern pattern_len = 
      if pattern_len > len / 2 then false
      else if len mod pattern_len <> 0 then
        check_pattern (pattern_len + 1)
      else
        let pattern = String.sub s 0 pattern_len in
        let rec check_repeats start =
          if start >= len then true
          else if String.sub s start pattern_len = pattern then
            check_repeats (start + pattern_len)
          else
            false
        in
        if check_repeats 0 then true
        else check_pattern (pattern_len + 1)
    in
    check_pattern 1 


  let part2 (input : string) : string =
    input
    |> parse_input
    |> parse_ranges
    |> List.map range_to_list
    |> List.flatten
    |> List.filter (fun n -> is_invalid_sequence (string_of_int n))
    |> List.fold_left (+) 0
    |> string_of_int

end
