module Solution = struct

  let parse input =
    input
    |> String.split_on_char '\n'
    |> List.filter ((<>) "")
    |> Array.of_list

  let height grid = Array.length grid
  let width  grid = String.length grid.(0)

  let in_bounds grid y x =
    y >= 0 && y < height grid &&
    x >= 0 && x < width grid

  let get grid y x =
    grid.(y).[x]

  let neighbour_offsets =
    [ (-1,-1); (-1,0); (-1,1);
      ( 0,-1);          ( 0,1);
      ( 1,-1); ( 1,0);  ( 1,1) ]

  let count_adjacent_rolls grid y x =
    List.fold_left (fun acc (dy,dx) ->
      let ny = y + dy in
      let nx = x + dx in
      if in_bounds grid ny nx && get grid ny nx = '@'
      then acc + 1
      else acc
    ) 0 neighbour_offsets

  let is_accessible grid y x =
    get grid y x = '@'
    && count_adjacent_rolls grid y x < 4

  let count_accessible grid =
    let count = ref 0 in
    for y = 0 to height grid - 1 do
      for x = 0 to width grid - 1 do
        if is_accessible grid y x then incr count
      done
    done;
    !count

  let part1 input =
    input
    |> parse
    |> count_accessible
    |> string_of_int

  let find_accessible (grid): (int * int) list =
    let accessible = ref [] in
    for y = 0 to height grid - 1 do
      for x = 0 to width grid - 1 do
        if is_accessible grid y x then
          accessible := (y, x) :: !accessible
      done
    done;
    !accessible 

  let remove_roll grid y x:int =
    let row = grid.(y) in
    let new_row = String.mapi (fun i c -> if i = x then '.' else c) row in
    grid.(y) <- new_row;
    1

  let part2 input =
    let grid = parse input in
    let removed = ref 0 in

    let rec loop () =
      let acc = find_accessible grid in
      match acc with
      | [] -> ()                    (* stop when no more accessible *)
      | rolls ->
          removed := !removed + List.length rolls;
          List.iter (fun (y,x) -> ignore (remove_roll grid y x)) rolls;
          loop ()
    in
    loop ();
    string_of_int !removed

end
