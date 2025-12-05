module Solution = struct
  let split_on_empty (xs : string list) : string list * string list =
    let rec aux acc = function
      | [] -> (List.rev acc, [])
      | "" :: rest -> (List.rev acc, rest)
      | x :: rest -> aux (x :: acc) rest
    in
    aux [] xs
  ;;

  type r = {
    s: int;
    e: int;
  }

  let range_from_str (line : string) : r =
    match String.split_on_char '-' line with
    | [a; b] -> {s = int_of_string a; e = int_of_string b}
    | _ -> failwith "bad range" 
  ;;

  let is_valid_ingredient (ranges : r list) (ingredient : int) : bool =
    List.exists (fun r -> ingredient >= r.s && ingredient <= r.e) ranges
  ;;

  let part1 (input : string) : string =
    let lines = String.trim input |> String.split_on_char '\n' in
    let (ranges_str, ingredients_str) = split_on_empty lines in
    let ranges = List.map range_from_str ranges_str in
    let ingredients = ingredients_str |> List.map int_of_string in
    
    let fresh_ingredients = 
      List.filter (fun ing -> is_valid_ingredient ranges ing) ingredients 
    in
    string_of_int (List.length fresh_ingredients)
  ;;

  let merge_ranges (ranges : r list) : r list =
    let sorted = List.sort (fun r1 r2 -> compare r1.s r2.s) ranges in
    
    let rec merge acc = function
      | [] -> List.rev acc
      | r :: rest ->
          match acc with
          | [] -> merge [r] rest
          | last :: acc_rest ->
              (* If current range overlaps or is adjacent to last *)
              if r.s <= last.e + 1 then
                (* Extend the last range *)
                let merged = {s = last.s; e = max last.e r.e} in
                merge (merged :: acc_rest) rest
              else
                (* No overlap, add new range *)
                merge (r :: acc) rest
    in
    merge [] sorted
  ;;

  let part2 (input : string) : string = 
    let lines = String.trim input |> String.split_on_char '\n' in 
    let (ranges_str, _) = split_on_empty lines in 
    let ranges = List.map range_from_str ranges_str in 
    let merged = merge_ranges ranges in
    let total = List.fold_left (fun acc r -> acc + (r.e - r.s + 1)) 0 merged in
    string_of_int total
  ;;
end
