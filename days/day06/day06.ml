module Solution = struct
  (* Helper to transpose a list of strings into columns *)
  let transpose (lines: string list): string list =
    if lines = [] then []
    else
      let max_len = List.fold_left (fun acc s -> max acc (String.length s)) 0 lines in
      let padded_lines = List.map (fun s -> 
        s ^ String.make (max_len - String.length s) ' '
      ) lines in
      List.init max_len (fun col_idx ->
        String.init (List.length padded_lines) (fun row_idx ->
          String.get (List.nth padded_lines row_idx) col_idx
        )
      )
  
  (* Check if a column is all spaces *)
  let is_all_spaces (col: string): bool =
    String.for_all (fun c -> c = ' ') col
  
  (* Split columns into groups (problems) based on all-space separators *)
  let split_problems (columns: string list): string list list =
    let rec aux acc current = function
      | [] -> if current = [] then List.rev acc else List.rev (List.rev current :: acc)
      | col :: rest ->
          if is_all_spaces col then
            if current = [] then aux acc [] rest
            else aux (List.rev current :: acc) [] rest
          else
            aux acc (col :: current) rest
    in
    aux [] [] columns
  
  (* Parse a single problem (group of columns) *)
  let parse_problem (cols: string list): int =
    if cols = [] then 0
    else
      (* Each column is a vertical string *)
      (* We need to extract numbers and operator from the vertical arrangement *)
      
      (* Transpose back to get horizontal rows *)
      let num_rows = if cols <> [] then String.length (List.hd cols) else 0 in
      let horizontal_rows = List.init num_rows (fun row_idx ->
        List.map (fun col -> String.get col row_idx) cols
        |> List.to_seq
        |> String.of_seq
      ) in
      
      (* Last row has the operator, others have numbers *)
      let operator_row = List.nth horizontal_rows (num_rows - 1) in
      let number_rows = List.mapi (fun i row -> (i, row)) horizontal_rows
                      |> List.filter (fun (i, _) -> i < num_rows - 1)
                      |> List.map (fun (_, row) -> row) in
      
      (* Extract operator (first non-space character) *)
      let operator = String.fold_left (fun acc c ->
        if acc = ' ' && c <> ' ' then c else acc
      ) ' ' operator_row in
      
      (* Extract numbers from each row, filtering out empty strings *)
      let numbers = List.filter_map (fun row ->
        let trimmed = String.trim row in
        if trimmed = "" then None
        else Some (int_of_string trimmed)
      ) number_rows in
      
      (* Apply the operation *)
      match operator with
      | '+' -> List.fold_left (+) 0 numbers
      | '*' -> List.fold_left ( * ) 1 numbers
      | _ -> 0
  
  let part1 (input: string): string =
    let lines = String.split_on_char '\n' input 
                |> List.filter (fun s -> s <> "") in
    
    if lines = [] then "0"
    else
      let columns = transpose lines in
      let problems = split_problems columns in
      let answers = List.map parse_problem problems in
      let grand_total = List.fold_left (+) 0 answers in
      string_of_int grand_total
  ;;
  
  let part2 (input: string): string =
    let lines = String.split_on_char '\n' input 
                |> List.filter (fun s -> s <> "") in
    
    if lines = [] then "0"
    else
      (* Calculate column widths (max token length per position) *)
      let get_col_widths line =
        String.split_on_char ' ' line
        |> List.filter (fun s -> s <> "")
        |> List.map String.length
      in
      
      let cols = List.fold_left (fun acc line ->
        let widths = get_col_widths line in
        if acc = [] then widths
        else List.map2 max acc widths
      ) [] lines in
      
      (* Initialize 2D array for accumulating digits *)
      let nums = Array.of_list (List.map (fun w -> Array.make w 0) cols) in
      
      (* Process each line *)
      let p2 = ref 0 in
      List.iter (fun line ->
        if line <> "" && (line.[0] = '+' || line.[0] = '*') then
          (* Operator line - process each token *)
          let tokens = String.split_on_char ' ' line 
                      |> List.filter (fun s -> s <> "") in
          List.iteri (fun i s ->
            if i < Array.length nums then
              match s with
              | "+" -> p2 := !p2 + Array.fold_left (+) 0 nums.(i)
              | "*" -> p2 := !p2 + Array.fold_left ( * ) 1 nums.(i)
              | _ -> ()
          ) tokens
        else
          (* Number line - build numbers digit by digit *)
          let a = ref 0 in
          let b = ref 0 in
          String.iter (fun c ->
            if c <> ' ' && c <> '\n' then (
              let digit = int_of_char c - int_of_char '0' in
              if !a < Array.length nums && !b < Array.length nums.(!a) then
                nums.(!a).(!b) <- nums.(!a).(!b) * 10 + digit
            );
            incr b;
            if !b > List.nth cols !a then (
              b := 0;
              incr a
            )
          ) line
      ) lines;
      
      string_of_int !p2
  ;;
end
