let go_right value position counter =
    let result = (value + position) mod 100 in
        match result with
            | 0 -> (0, counter + 1)
            | x -> (x mod 100, counter)


let go_left value position counter =
    let result = (position - (value mod 100)) in
        match result with
            | 0 -> (0, counter + 1)
            | x -> let is_smaller = x < 0 in match is_smaller with
                | true -> (100 - (Int.abs x), counter)
                | false -> (x, counter)


let read_direction s position counter = begin
    (* print_endline ("Position: " ^ (string_of_int position) ^ " Counter: " ^ (string_of_int counter) ); *)
    match Myutils.parts s with
        | direction, value -> match direction with
            | "L" -> go_left (int_of_string value) position counter
            | "R" -> go_right (int_of_string value) position counter
            | _ -> raise Exit
    end;;


(* recursive function that iterates the list and calculates the new postion and counter *)
let rec read_result values position counter =
    match values with
        | [] -> counter

        (* only value -> calculate new position, counter and return counter*)
        | value :: [] -> snd (read_direction value position counter)

        (* -------- or we have at least two items
            -> get new position, counter for value and call recursion with
            rest, new position, new counter
           ----- *)
        | value :: rest -> let (new_position, new_counter) =
            read_direction value position counter in
                read_result rest new_position new_counter

(* -------- DAY1 PART 1 ----- *)
let start_position = 50
let start_counter = 0
(* -------- test input calculation 
let test_inputs = [
    "L68";
    "L30";
    "R48";
    "L5";
    "R60";
    "L55";
    "L1";
    "L99";
    "R14";
    "L82"; (* end on 32 *)
    "L32"; (* should be 0, 4 *)
    "R77"; (* *)
    "R23"; (* 0, 5*)
    "R100"; (* 0, 6*)
    "R1"; (* 0, 6*)
]

let () = 
    let result = read_result test_inputs start_position start_counter in
        print_endline (string_of_int result)

----- *)
(* -------- Actual calculation begins  ----- *)
let file_path = "inputs/day1_1.txt"


let () = 
    let inputs = Stdio.In_channel.read_lines file_path in
    let result = read_result inputs start_position start_counter in
        print_endline (string_of_int result)
