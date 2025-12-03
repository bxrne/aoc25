module Solution = struct
  (* Gets maximum joltage for a single bank by checking all digit pairs *)
  let parse_bank (line: string): int = 
    let len = String.length line in
    let max_joltage = ref 0 in
    
    (* Check all pairs (i, j) where i < j *)
    for i = 0 to len - 2 do
      let first_digit = line.[i] in
      if first_digit >= '0' && first_digit <= '9' then
        for j = i + 1 to len - 1 do
          let second_digit = line.[j] in
          if second_digit >= '0' && second_digit <= '9' then
            let pair_value = 
              (int_of_char first_digit - int_of_char '0') * 10 + 
              (int_of_char second_digit - int_of_char '0')
            in
            if pair_value > !max_joltage then
              max_joltage := pair_value
        done
    done;
    !max_joltage
  
  let part1 (input: string) : string =
    let lines = String.split_on_char '\n' input in
    let non_empty_lines = List.filter (fun l -> String.length (String.trim l) > 0) lines in
    let joltage_sum = List.fold_left (fun acc line -> acc + parse_bank line) 0 non_empty_lines in
    string_of_int joltage_sum
  
  (* Greedy algorithm to select exactly k digits forming the largest number *)
  let parse_bank_k_digits (line: string) (k: int): string =
    let digits = 
      String.to_seq line
      |> Seq.filter (fun c -> c >= '0' && c <= '9')
      |> List.of_seq
    in
    let n = List.length digits in
    
    if n < k then ""  (* Not enough digits *)
    else
      let result = ref [] in
      let start_pos = ref 0 in
      
      (* For each position in result, find the best digit *)
      for pos = 0 to k - 1 do
        let remaining_needed = k - pos - 1 in  (* digits still needed after this one *)
        let search_end = n - remaining_needed in  (* latest position we can pick from *)
        
        (* Find the maximum digit in the valid range *)
        let best_digit = ref '0' in
        let best_idx = ref !start_pos in
        
        for i = !start_pos to search_end - 1 do
          if List.nth digits i > !best_digit then begin
            best_digit := List.nth digits i;
            best_idx := i
          end
        done;
        
        result := !best_digit :: !result;
        start_pos := !best_idx + 1
      done;
      
      (* Convert result to string (reverse since we cons'd) *)
      !result |> List.rev |> List.to_seq |> String.of_seq
  
  let part2 (input: string) : string =
    input 
    |> String.split_on_char '\n'
    |> List.filter (fun l -> String.length (String.trim l) > 0)
    |> List.map (fun line -> 
         let max_joltage_str = parse_bank_k_digits line 12 in
         if max_joltage_str = "" then 0 else int_of_string max_joltage_str
       )
    |> List.fold_left (+) 0
    |> string_of_int
end
