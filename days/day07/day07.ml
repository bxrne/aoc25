module Solution = struct 
  let traverse start lines = 
    let splits = ref 0 and visited = Hashtbl.create 1000 in
    let rec aux indices li = 
      if li < List.length lines then begin
        let line = List.nth lines li in
        let next = List.filter_map (fun i -> 
          if i < 0 || i >= String.length line || Hashtbl.mem visited (li, i) then None
          else (Hashtbl.add visited (li, i) (); 
                match line.[i] with 
                | '.' -> Some [i]
                | '^' -> incr splits; Some (List.filter (fun x -> x >= 0 && x < String.length line) [i-1; i+1])
                | _ -> None)
        ) indices |> List.flatten in
        aux next (li + 1)
      end
    in aux [start] 0; !splits
  
  let count_timelines start lines = 
    let counts = Hashtbl.create 10000 in
    let get k = try Hashtbl.find counts k with Not_found -> 0 in
    let add k v = Hashtbl.replace counts k (get k + v) in
    Hashtbl.add counts (0, start) 1;
    let w = String.length (List.hd lines) and h = List.length lines in
    for li = 0 to h - 1 do
      for c = 0 to w - 1 do
        let cnt = get (li, c) in
        if cnt > 0 then match (List.nth lines li).[c] with 
          | '.' -> add (li+1, c) cnt
          | '^' -> List.iter (fun x -> if x >= 0 && x < w then add (li+1, x) cnt) [c-1; c+1]
          | _ -> ()
      done
    done;
    Hashtbl.fold (fun (li, _) cnt acc -> if li = h then acc + cnt else acc) counts 0
  
  let part1 input = 
    let lines = String.split_on_char '\n' input |> List.filter ((<>) "") in
    traverse (String.index (List.hd lines) 'S') (List.tl lines) |> string_of_int
  
  let part2 input = 
    let lines = String.split_on_char '\n' input |> List.filter ((<>) "") in
    count_timelines (String.index (List.hd lines) 'S') (List.tl lines) |> string_of_int
end
