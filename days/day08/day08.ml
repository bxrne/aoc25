module Solution = struct 
  type junction_box = {
    x: int; y: int; z: int;
  }
  
  let pythagorean (p1:junction_box) (p2:junction_box) : float =
    let dx = p2.x - p1.x in
    let dy = p2.y - p1.y in
    let dz = p2.z - p1.z in
    sqrt (float_of_int (dx * dx + dy * dy + dz * dz))
  
  let read_line (line:string): junction_box = 
    let parts = String.split_on_char ',' line in
    { x = int_of_string (List.nth parts 0);
      y = int_of_string (List.nth parts 1);
      z = int_of_string (List.nth parts 2);
    }
  
  (* Build adjacency list from connections *)
  let build_graph n connections =
    let graph = Array.make n [] in
    List.iter (fun (i, j) ->
      graph.(i) <- j :: graph.(i);
      graph.(j) <- i :: graph.(j)
    ) connections;
    graph
  
  (* Find all nodes in a circuit using DFS *)
  let find_circuit graph start visited =
    let rec dfs node =
      if visited.(node) then []
      else begin
        visited.(node) <- true;
        node :: List.concat_map dfs graph.(node)
      end
    in
    dfs start
  
  (* Find all circuits in the graph *)
  let find_all_circuits graph =
    let n = Array.length graph in
    let visited = Array.make n false in
    let circuits = ref [] in
    for i = 0 to n - 1 do
      if not visited.(i) then begin
        let circuit = find_circuit graph i visited in
        circuits := circuit :: !circuits
      end
    done;
    !circuits
  
  let part1 (input:string) : string = 
    let lines = String.split_on_char '\n' input 
                |> List.filter (fun l -> l <> "") in 
    let boxes = List.map read_line lines in 
    let n = List.length boxes in
    
    (* Generate all pairs with their distances *)
    let pairs = 
      List.init n (fun i ->
        List.init (n - i - 1) (fun j ->
          let j' = i + j + 1 in
          let dist = pythagorean (List.nth boxes i) (List.nth boxes j') in
          (dist, i, j')
        )
      ) |> List.concat
    in
    
    (* Sort pairs by distance and take the 1000 shortest *)
    let shortest_pairs = 
      List.sort (fun (d1,_,_) (d2,_,_) -> compare d1 d2) pairs
      |> List.filteri (fun idx _ -> idx < 1000)
      |> List.map (fun (_dist, i, j) -> (i, j))
    in
    
    (* Build graph from connections *)
    let graph = build_graph n shortest_pairs in
    
    (* Find all circuits *)
    let circuits = find_all_circuits graph in
    
    (* Get sizes of three largest circuits *)
    let circuit_sizes = List.map List.length circuits in
    let top_three = 
      List.sort (fun a b -> compare b a) circuit_sizes
      |> List.filteri (fun idx _ -> idx < 3)
    in
    
    (* Multiply the three largest *)
    let result = List.fold_left ( * ) 1 top_three in
    string_of_int result
  
  let part2 (_input:string) : string = "tbi"
end
