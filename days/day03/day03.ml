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
  
  let part2 (_input: string) : string =
    "tbi"
end
