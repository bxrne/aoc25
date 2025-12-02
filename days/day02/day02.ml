module Solution = struct
  (* Problem domain type *)
  type range_value = {
    start: int;
    end_: int;
  }

  (* Accept string input and give list of lines *)
  let parse_input (input: string) : string list =
    String.split_on_char '\n' input
    |> List.filter (fun line -> String.length line > 0)

  (* Parse a single range from string to range_value *)
  let parse_range (input: string) : range_value =
    let bounds = String.split_on_char '-' input in
    match bounds with
    | [ start_str; end_str ] ->
        {
          start = int_of_string start_str;
          end_ = int_of_string end_str;
        }
    | _ -> failwith "Invalid range format"

  (* Parse multiple ranges from list of strings *)
  let parse_ranges (input : string list) : range_value list =
    input
    |> List.map (fun line ->
           line
           |> String.split_on_char ','
           |> List.map parse_range)
    |> List.concat

  let invalid_range_val(valu: int) : bool =
    match string_of_int valu with
    (* Cannot have a leading zero *)
    | s when String.length s > 1 && String.get s 0 = '0' -> true
    (* First half of string cannot equal the second half *)
    | s when String.length s mod 2 = 0 ->
        let half_len = String.length s / 2 in
        let first_half = String.sub s 0 half_len in
        let second_half = String.sub s half_len half_len in
        first_half = second_half
    | _ -> false


  let part1 (input: string) : string =
    let vals = parse_input input in 
    let ranges = parse_ranges vals in 
    let rec collect_invalids (ranges: range_value list) : int list =
      match ranges with
      | [] -> []
      | range :: rest ->
          let invalids_in_range =
            List.filter invalid_range_val
              (List.init (range.end_ - range.start + 1)
                 (fun i -> range.start + i))
          in
          invalids_in_range @ collect_invalids rest
    in
    let invalid_sum =
      collect_invalids ranges
      |> List.fold_left (fun acc x -> acc + x) 0
    in 
    string_of_int invalid_sum 


  let part2 (input: string) : string =
    input
end
