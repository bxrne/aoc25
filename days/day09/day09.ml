
module Solution = struct
  type coord = { x: int; y: int }

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> List.filter (fun s -> s <> "")
  ;;

  let parse_coord s =
    match String.split_on_char ',' s with
    | [x; y] ->
        { x = int_of_string (String.trim x)
        ; y = int_of_string (String.trim y)
        }
    | _ -> failwith "bad coord"
  ;;

  let can_form_rectangle c1 c2 =
    c1.x <> c2.x && c1.y <> c2.y
  ;;

  (* inclusive rectangle area *)
  let area c1 c2 =
    if can_form_rectangle c1 c2 then
      (abs (c1.x - c2.x) + 1) * (abs (c1.y - c2.y) + 1)
    else
      0
  ;;

  let max_area coords =
    let rec outer lst best =
      match lst with
      | [] | [_] -> best
      | h :: t ->
          let best =
            List.fold_left
              (fun acc c2 -> max acc (area h c2))
              best
              t
          in
          outer t best
    in
    outer coords 0
  ;;

  let part1 input =
    input
    |> parse_input
    |> List.map parse_coord
    |> max_area
    |> string_of_int
  ;;

  let part2 _ = "tbi"
end

