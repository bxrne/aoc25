module Solution = struct 
  let traverse (start: int) (lines: string list): int = 
    let split_count = ref 0 in 
    let visited = Hashtbl.create 1000 in
    
    let rec aux (current_indices: int list) (line_index: int) : unit = 
      if line_index >= List.length lines then ()
      else begin
        let line = List.nth lines line_index in 
        let new_indices = ref [] in 
        
        List.iter (fun idx -> 
          if idx < 0 || idx >= String.length line then ()
          else if Hashtbl.mem visited (line_index, idx) then ()
          else begin
            Hashtbl.add visited (line_index, idx) ();
            match String.get line idx with 
            | '.' -> new_indices := idx :: !new_indices
            | '^' -> 
              split_count := !split_count + 1;
              let left_idx = idx - 1 in 
              let right_idx = idx + 1 in 
              if left_idx >= 0 then new_indices := left_idx :: !new_indices;
              if right_idx < String.length line then new_indices := right_idx :: !new_indices;
            | _ -> ()
          end
        ) current_indices;
        
        aux !new_indices (line_index + 1)
      end
    in 
    aux [start] 0;
    !split_count
  
  let count_timelines (start: int) (lines: string list): int = 
    (* Count paths reaching each position: (line, col) -> path_count *)
    let path_counts = Hashtbl.create 10000 in
    Hashtbl.add path_counts (0, start) 1;
    
    let get_count key = 
      try Hashtbl.find path_counts key with Not_found -> 0
    in
    
    let width = String.length (List.hd lines) in
    let height = List.length lines in
    
    for line_idx = 0 to height - 1 do
      let line = List.nth lines line_idx in
      let next_counts = Hashtbl.create 1000 in
      
      for col = 0 to width - 1 do
        let count = get_count (line_idx, col) in
        if count > 0 then begin
          match String.get line col with 
          | '.' -> 
            (* Continue straight down *)
            let key = (line_idx + 1, col) in
            let prev = try Hashtbl.find next_counts key with Not_found -> 0 in
            Hashtbl.replace next_counts key (prev + count)
          | '^' -> 
            (* Split left and right *)
            let left_col = col - 1 in 
            let right_col = col + 1 in 
            if left_col >= 0 then begin
              let key = (line_idx + 1, left_col) in
              let prev = try Hashtbl.find next_counts key with Not_found -> 0 in
              Hashtbl.replace next_counts key (prev + count)
            end;
            if right_col < width then begin
              let key = (line_idx + 1, right_col) in
              let prev = try Hashtbl.find next_counts key with Not_found -> 0 in
              Hashtbl.replace next_counts key (prev + count)
            end
          | _ -> ()
        end
      done;
      
      Hashtbl.iter (fun k v -> Hashtbl.replace path_counts k v) next_counts
    done;
    
    (* Sum all paths that exit the bottom *)
    let total = ref 0 in
    Hashtbl.iter (fun (line_idx, _) count ->
      if line_idx = height then total := !total + count
    ) path_counts;
    !total
  
  let part1(input:string):string = 
    let lines = input |> String.split_on_char '\n' |> List.filter (fun s -> s <> "") in 
    let entrypoint = String.index (List.hd lines) 'S' in 
    let split_count = traverse entrypoint (List.tl lines) in 
    string_of_int split_count
  
  let part2(input:string):string = 
    let lines = input |> String.split_on_char '\n' |> List.filter (fun s -> s <> "") in 
    let entrypoint = String.index (List.hd lines) 'S' in 
    let timeline_count = count_timelines entrypoint (List.tl lines) in 
    string_of_int timeline_count
end
