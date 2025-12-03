(* Advent of Code 2025 Day 1 Part 2
   Puzzle:
    Simulate the same circular 0–99 as Day1 Part1,
    dial starting at 50 and apply each rotation from the input (direction L/R and step count)
    click by click. Count how many individual clicks (intermediate positions and final positions of rotations)
    land on 0 over the entire sequence, and output that total.
*)
(* CONSTANT DEFINITIONS *)
let file_path = "inputs/day1.txt"
let start_position = 50
let start_counter = 0


(* HELPER FUNCTIONS *)
let rec reduce_right value position counter =
    let new_position = position + value in begin
    (*print_endline (
            "new Position: " ^ (string_of_int new_position) ^
            " value: " ^ (string_of_int value) ^ 
            " counter: " ^ (string_of_int counter)
        );
    *)
    if value > 100 then
        let new_result = reduce_right (value - 100) position (counter + 1) in
            (fst new_result, snd new_result)
    else if new_position == 0 then
        (new_position, counter + 1)
    else if new_position > 100 then
        ((new_position mod 100), counter + 1)
    else if new_position == 100 then
        ((new_position mod 100), counter + 1)
    else (new_position, counter)
    end;;

let rec reduce_left value position counter =
    let new_position = position - value in begin
    (*print_endline (
            "new Position: " ^ (string_of_int new_position) ^
            " value: " ^ (string_of_int value) ^ 
            " counter: " ^ (string_of_int counter)
        );
    *)
    if value > 100 then
        let new_result = reduce_left (value - 100) position (counter + 1) in
            (fst new_result, snd new_result)
    else if position == 0 then
        (* everything that we now substract would increase the counter
            because our result is smaller than 0
        *)
        (100 - value, counter)
    else if new_position == 0 then
        (new_position, counter + 1)
    else if new_position < 0 then
        (100 - (Int.abs new_position), counter + 1)
    else (new_position, counter)
    end;;


let read_direction s position counter = begin
    (* print_endline ("Position: " ^ (string_of_int position) ^ " Counter: " ^ (string_of_int counter) ); *)
    match Myutils.parts s with
        | direction, value -> match direction with
            | "L" -> reduce_left (int_of_string value) position counter
            | "R" -> reduce_right (int_of_string value) position counter
            | _ -> raise Exit
    end;;



(* -------- test input calculation ----- *)
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
]

let[@warnerror "-unused-value-declaration"] solve_example =
    let result = Day1_1.read_result test_inputs start_position start_counter in
        print_endline (string_of_int result)

(* -------- Actual calculation begins ----- *)
let solve input_lines = begin 
    let result = Day1_1.read_result input_lines start_position start_counter in
        print_endline (string_of_int result)
    end;;

