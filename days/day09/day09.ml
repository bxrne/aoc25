module Solution = struct
  type coord = { x: int; y: int }
  
  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> List.filter (fun s -> s <> "")
  ;;
  
  let parse_coord s =
    let trimmed = String.trim s in
    if trimmed = "" then
      failwith "empty coord"
    else
      match String.split_on_char ',' trimmed with
      | [x; y] ->
          { x = int_of_string (String.trim x)
          ; y = int_of_string (String.trim y)
          }
      | _ -> failwith ("bad coord: " ^ trimmed)
  ;;
  
  let can_form_rectangle c1 c2 =
    c1.x <> c2.x && c1.y <> c2.y
  ;;
  
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
  
  (* Check if point is on boundary between consecutive red tiles *)
  let point_on_boundary point coords =
    let n = List.length coords in
    let rec check_segment i =
      if i >= n then false
      else
        let c1 = List.nth coords i in
        let c2 = List.nth coords ((i + 1) mod n) in
        let on_segment =
          if c1.x = c2.x then
            point.x = c1.x && 
            point.y >= min c1.y c2.y && 
            point.y <= max c1.y c2.y
          else if c1.y = c2.y then
            point.y = c1.y && 
            point.x >= min c1.x c2.x && 
            point.x <= max c1.x c2.x
          else
            false
        in
        if on_segment then true else check_segment (i + 1)
    in
    check_segment 0
  ;;
  
  (* Fast point-in-polygon using ray casting *)
  let point_inside_polygon point coords =
    let n = List.length coords in
    let rec count_crossings i count =
      if i >= n then count
      else
        let c1 = List.nth coords i in
        let c2 = List.nth coords ((i + 1) mod n) in
        let y_min = min c1.y c2.y in
        let y_max = max c1.y c2.y in
        
        let crosses =
          point.y >= y_min && point.y < y_max &&
          (let x_intersect = 
            if c1.y = c2.y then c1.x
            else c1.x + (point.y - c1.y) * (c2.x - c1.x) / (c2.y - c1.y)
           in
           point.x < x_intersect)
        in
        count_crossings (i + 1) (if crosses then count + 1 else count)
    in
    (count_crossings 0 0) mod 2 = 1
  ;;
  
  (* Check if point is green (red or on boundary or inside) *)
  let is_green point coords red_set =
    if Hashtbl.mem red_set point then
      true
    else
      point_on_boundary point coords || 
      point_inside_polygon point coords
  ;;
  
  (* Check if rectangle is valid by checking all its points *)
  let rectangle_is_valid coords red_set c1 c2 =
    if not (can_form_rectangle c1 c2) then false
    else
      let x_min = min c1.x c2.x in
      let x_max = max c1.x c2.x in
      let y_min = min c1.y c2.y in
      let y_max = max c1.y c2.y in
      let exception Invalid in
      
      try
        for y = y_min to y_max do
          for x = x_min to x_max do
            if not (is_green { x; y } coords red_set) then
              raise Invalid
          done
        done;
        true
      with Invalid -> false
  ;;
  
let max_area_with_constraints coords =
    let red_set = Hashtbl.create 1000 in
    List.iter (fun c -> Hashtbl.add red_set c ()) coords;
    
    (* Get all edges including wrap-around *)
    let get_edges coords =
      let n = List.length coords in
      let rec collect_edges i acc =
        if i >= n then acc
        else
          let c1 = List.nth coords i in
          let c2 = List.nth coords ((i + 1) mod n) in
          collect_edges (i + 1) ((c1, c2) :: acc)
      in
      collect_edges 0 []
    in
    
    let edges = get_edges coords in
    
    (* Check if an edge cuts through a rectangle *)
    let edge_cuts_rect (c1, c2) rect_c1 rect_c2 =
      let x_min = min rect_c1.x rect_c2.x in
      let x_max = max rect_c1.x rect_c2.x in
      let y_min = min rect_c1.y rect_c2.y in
      let y_max = max rect_c1.y rect_c2.y in
      
      (* Check if edge is vertical line cutting through rectangle *)
      if c1.x = c2.x && c1.x > x_min && c1.x < x_max then
        let y1_min = min c1.y c2.y in
        let y1_max = max c1.y c2.y in
        not (y1_max <= y_min || y1_min >= y_max)
      (* Check if edge is horizontal line cutting through rectangle *)
      else if c1.y = c2.y && c1.y > y_min && c1.y < y_max then
        let x1_min = min c1.x c2.x in
        let x1_max = max c1.x c2.x in
        not (x1_max <= x_min || x1_min >= x_max)
      else
        false
    in
    
    (* Generate all coordinate pairs that can form rectangles *)
    let generate_pairs () =
      let n = List.length coords in
      let pairs = ref [] in
      
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          let c1 = List.nth coords i in
          let c2 = List.nth coords j in
          if can_form_rectangle c1 c2 then
            pairs := (c1, c2, area c1 c2) :: !pairs
        done
      done;
      
      !pairs
    in
    
    (* Sort pairs by area in descending order *)
    let pairs_sorted = 
      generate_pairs ()
      |> List.sort (fun (_, _, area_a) (_, _, area_b) -> 
          compare area_b area_a)  (* descending order *)
    in
    
    (* Check if rectangle is valid (not cut by any edge) *)
    let rectangle_is_valid c1 c2 =
      not (List.exists (fun edge -> edge_cuts_rect edge c1 c2) edges)
    in
    
    (* Find the first valid rectangle *)
    let rec find_first_valid = function
      | [] -> 0
      | (c1, c2, _) :: rest ->
          if rectangle_is_valid c1 c2 then
            area c1 c2
          else
            find_first_valid rest
    in
    
    string_of_int (find_first_valid pairs_sorted)
  ;;
  
  let part2 input =
    input
    |> parse_input
    |> List.map parse_coord
    |> max_area_with_constraints
  ;;
end