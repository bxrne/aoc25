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

  let part2 (_input : string) : string = "tbi"
end
