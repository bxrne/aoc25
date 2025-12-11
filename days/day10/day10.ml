module Solution = struct

  (* Indicator light status *)
  type indicator = On | Off ;;

  (* Button wiring instructions *)
  type button_wiring = {
    instructions: int list;
  } 

  (* Joltages (list of integers) *)
  type joltages = int list ;;

  (* Manual structure (per machine) *)
  type manual = {
    lights: indicator list;
    buttons: button_wiring list;
    joltages: joltages;
  }

  (* Parse lights from string *)
  let parse_lights (lights_str: string) : indicator list =
    let stripped = 
      lights_str
      |> String.trim
      |> (fun s -> String.sub s 1 (String.length s - 2))  (* Remove brackets *)
    in
    stripped
    |> String.to_seq
    |> Seq.map (function
        | '#' -> On
        | '.' -> Off
        | _ -> failwith "Invalid light character")
    |> List.of_seq
  ;;

  (* Parse button wiring from string *)
  let parse_button_wiring (wiring_str: string) : button_wiring =
    let nums = 
      wiring_str
      |> String.trim
      |> (fun s -> String.sub s 1 (String.length s - 2))  (* Remove parentheses *)
      |> String.split_on_char ','
      |> List.map int_of_string
    in
    { instructions = nums }
  ;;

  (* Parse joltages from string *)
  let parse_joltages (joltages_str: string) : joltages =
    joltages_str
    |> String.trim
    |> (fun s -> String.sub s 1 (String.length s - 2))  (* Remove curly braces *)
    |> String.split_on_char ','
    |> List.map int_of_string
  ;;

  (* Parse a single machine manual *)
  let parse_manual (machine_line: string) :manual = 
    let parts = String.split_on_char ' ' machine_line in
    let lights = parse_lights (List.nth parts 0) in
    let button_wirings = List.map parse_button_wiring (List.tl (List.rev (List.tl parts))) |> List.rev in 
    let joltages = parse_joltages (List.nth parts (List.length parts - 1)) in
    { lights = lights; buttons = button_wirings; joltages = joltages }
  ;;

  let parse_button_wirings (parts: string list) : button_wiring list =
    parts |> List.map parse_button_wiring
  ;;

  (* Toggle lights based on wiring *)
  let toggle_lights (lights: indicator list) (wiring: button_wiring) : indicator list =
    List.mapi (fun idx light ->
      if List.mem idx wiring.instructions then
        match light with
        | On -> Off
        | Off -> On
      else
        light
    ) lights
  ;;

  (* Compute button presses recursively *)
  let rec compute_presses (current_lights: indicator list) (buttons: button_wiring list) (target_lights: indicator list) (press_count: int) : int =
    if current_lights = target_lights then
      press_count
    else
      match buttons with
      | [] -> max_int  (* No more buttons to press, return a large number *)
      | btn::rest ->
          let press_with = compute_presses (toggle_lights current_lights btn) rest target_lights (press_count + 1) in
          let press_without = compute_presses current_lights rest target_lights press_count in
          min press_with press_without
  ;;

  (* Start machine presses buttons wrt wiring to get expected indicator status *)
  let count_start_prs (man: manual) : int =
    let initial_lights = List.init (List.length man.lights) (fun _ -> Off) in
    compute_presses initial_lights man.buttons man.lights 0
  ;;

  let part1 (input : string) : string = 
    input |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
    |> List.map parse_manual 
    |> List.map count_start_prs
    |> List.fold_left (+) 0
    |> string_of_int
  ;;

  let part2 (_input : string) : string = "tbi";;
end

