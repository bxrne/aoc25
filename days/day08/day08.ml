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
  
  module UnionFind = struct
    type t = {
      parent: (int, int) Hashtbl.t;
      rank: (int, int) Hashtbl.t;
    }
    
    let create n =
      let parent = Hashtbl.create n in
      let rank = Hashtbl.create n in
      for i = 0 to n - 1 do
        Hashtbl.add parent i i;
        Hashtbl.add rank i 0;
      done;
      { parent; rank }
    
    let rec find uf x =
      let p = Hashtbl.find uf.parent x in
      if p = x then x
      else begin
        let root = find uf p in
        Hashtbl.replace uf.parent x root;
        root
      end
    
    let union uf x y =
      let rx = find uf x in
      let ry = find uf y in
      if rx <> ry then begin
        let rank_x = Hashtbl.find uf.rank rx in
        let rank_y = Hashtbl.find uf.rank ry in
        if rank_x < rank_y then
          Hashtbl.replace uf.parent rx ry
        else if rank_x > rank_y then
          Hashtbl.replace uf.parent ry rx
        else begin
          Hashtbl.replace uf.parent ry rx;
          Hashtbl.replace uf.rank rx (rank_x + 1)
        end
      end
  end
  
  let part1 (input:string) : string = 
    let lines = String.split_on_char '\n' input |> List.filter (fun l -> l <> "") in 
    let boxes = List.map read_line lines in 
    let n = List.length boxes in
    
    let pairs = ref [] in
    for i = 0 to n - 1 do
      for j = i + 1 to n - 1 do
        let b1 = List.nth boxes i in
        let b2 = List.nth boxes j in
        let dist = pythagorean b1 b2 in
        pairs := (dist, i, j) :: !pairs
      done
    done;
    
    let sorted_pairs = List.sort (fun (d1,_,_) (d2,_,_) -> compare d1 d2) !pairs in
    
    let uf = UnionFind.create n in
    let connections = min 1000 (List.length sorted_pairs) in
    List.iter (fun (_dist, i, j) ->
      UnionFind.union uf i j
    ) (List.filteri (fun idx _ -> idx < connections) sorted_pairs);
    
    let circuits = Hashtbl.create n in
    for i = 0 to n - 1 do
      let root = UnionFind.find uf i in
      let current = try Hashtbl.find circuits root with Not_found -> 0 in
      Hashtbl.replace circuits root (current + 1)
    done;
    
    let sizes = Hashtbl.fold (fun _ size acc -> size :: acc) circuits [] in
    let sorted_sizes = List.sort (fun a b -> compare b a) sizes in
    let three_largest = List.filteri (fun idx _ -> idx < 3) sorted_sizes in
    
    let result = List.fold_left ( * ) 1 three_largest in
    string_of_int result
  
  let part2 (_input:string) : string = "tbi"
end
