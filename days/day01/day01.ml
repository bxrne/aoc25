module Solution = struct
  type rotation = {
    add : bool; (* true for right, false for left *)
    qty: int;  
  }

  type dial = {
    position: int; (* 0-99 *)
    zeroed_counts: int; 
  }

  let parse_instructions (lines: string list) : rotation list =
    let parse_line line =
      let add = String.get line 0 = 'R' in
      let qty = int_of_string (String.sub line 1 (String.length line - 1)) in
      { add; qty }
    in
    List.map parse_line lines 

  let apply_rotations (dial: dial) (rotations: rotation list) : dial =
    List.fold_left (fun d rot ->
      let delta = if rot.add then rot.qty else -rot.qty in
      let new_position = (d.position + delta + 100) mod 100 in (* wrap around 0-99 *)
      let zeroed_counts = if new_position = 0 then d.zeroed_counts + 1 else d.zeroed_counts in
      { position = new_position; zeroed_counts }
    ) dial rotations

  let part1 (input: string) : string =
    let lines = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in 
    let instructions = parse_instructions lines in
    let initial_dial = { position = 50; zeroed_counts = 0 } in 
    let final_dial = apply_rotations initial_dial instructions in 
    string_of_int final_dial.zeroed_counts (* 1180  is the answer *)



  let part2 (_input: string) : string =
    "TODO: Implement part2"
end
