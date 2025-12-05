(* Advent of Code 2025 Day 2 Part 1
   Puzzle:
Parse a single line of comma-separated numeric ranges a-b. For every integer ID in all inclusive ranges, check whether
its decimal representation has even length and consists of some nonempty digit sequence repeated twice
(first half equals second half, no leading zeros).
Count how many such invalid IDs there are across all ranges and output that count.
*)

(* CONSTANT DEFINITIONS *)
let file_path = "inputs/day2.txt"

(* -------- test input calculation ----- *)
let test_inputs = [
    "11-22";
    "95-115";
    "998-1012";
    "1188511880-1188511890";
    "222220-222224";
    "1698522-1698528";
    "446443-446449";
    "38593856-38593862";
    "565653-565659";
    "824824821-824824827";
    "2121212118-2121212124";
]

let is_even x =
     Int.equal 0 (x mod 2)


let rec power_of_10  ?(basevalue=10) times =
    (* helper function basically to add zeros to a one *)
    match times with
    | 0 -> basevalue
    | _ -> power_of_10 ~basevalue:(basevalue * 10) (times - 1)

let optimize_upper_bound upper: int =
    (* make sure we only use number with even amount of digits *)
    let len_upper = String.(length (string_of_int upper)) in
        match is_even len_upper with
        | true -> upper
        | false -> power_of_10 (len_upper -1) - 1

let optimize_lower_bound lower: int =
    (* make sure we only use number with even amount of digits *)
    let len_lower = String.(length (string_of_int lower)) in
        match is_even len_lower with
        | true -> lower
        | false -> power_of_10 (len_lower - 1 )


let get_optimized_range lower_as_string upper_as_string =
    let (lower, upper) = (int_of_string lower_as_string, int_of_string upper_as_string) in
        let len_lower, len_upper = (
            String.(length (string_of_int lower)),
            String.(length (string_of_int upper))
    ) in
        if Int.equal len_lower len_upper then
            match is_even len_lower with
                | true  -> Some (lower, upper) 
                | false -> None
        else Some (optimize_lower_bound lower, optimize_upper_bound upper)

let is_in_range lower upper x =
    lower <= x && x <= upper 



let split_number_by_daon x =
    (* can only be positive even numbers
        might need an assertion here

        return tuple of int that is_in_range
        left and right half
        of the given numbers digits
       *)
    let number_as_string = string_of_int x in
        let daon = String.length (number_as_string) in
            let half_amount = Int.div daon 2 in
        let ls, rs = String.(sub number_as_string 0 half_amount, sub number_as_string half_amount half_amount) in
            int_of_string ls, int_of_string rs

let get_lower_bound lower = 
    (* eg. lower = 5 1
        if lower_a is smaller than lower_b -> the lower bound is lower_a + 1
        if lower_a is bigger than lower_b -> the lower bound is lower_a
    *)
    let (lower_a, lower_b) = split_number_by_daon lower in begin
    match (lower_a < lower_b) with
        | true -> lower_a + 1 
        | false -> lower_a
    end;;

let get_upper_bound upper = begin
    (* upper = 5 9
        if upper_a is smaller than upper_b -> the upper bound is upper_a [nothing changes]
        if upper_a is bigger than upper_b -> the upper bound is upper_a - 1
        
        NOTE: do we handle case with upper_b is 0 (eg: 10)

    *)
    let (upper_a, upper_b) = split_number_by_daon upper in
    match (upper_a <= upper_b) with
        | true -> upper_a
        | false -> upper_a - 1
    end;;

let perm x =
    let x_as_string = string_of_int x in
        int_of_string (x_as_string ^ x_as_string)


let permutate lower upper =
    (*
    lower bound is of length DAONs / 2 of lower, same  for upper
    *)

    let (lower_bound, upper_bound) = get_lower_bound lower, get_upper_bound upper in begin
       (* print_endline ("boundaries: "  ^ (string_of_int lower_bound) ^ "|" ^ (string_of_int upper_bound)); *)
        let ranges = Myutils.range ~first:lower_bound ~last:upper_bound in
            match ranges with
            | [] -> []
            | _ -> List.map perm ranges
    end;;


let parse_range_of_string range_string =
    let sep = '-' in
        String.split_on_char sep range_string

let get_bounds ranges =
    (* parse list into tuple and then check for optimized boundaries *)
    let (lower_as_string, upper_as_string) = (List.nth ranges 0, List.nth ranges 1) in
       get_optimized_range lower_as_string upper_as_string
   

let compute range_string =
    (* print_endline ("range_string:"  ^ range_string ); *)

    let parsed_range = parse_range_of_string range_string in begin
            (* let a = List.nth parsed_range 0 in print_endline ("a:"  ^ a );
            let b = List.nth parsed_range 1 in print_endline ("b:"  ^ b );
            print_endline ("PARSED:"  ^ a  ^"|" ^ b); *)
        let bounds = get_bounds parsed_range in
            match bounds with
                | None -> [] 
                | Some (new_lower, new_upper) -> 
                let result = permutate new_lower new_upper in begin
               (* Myutils.print_int_list result; *)
                result
            end;
        end;;
                    
        
let create_tuple_of_list l =
    (* take in list with two items and return as tuple *)
    (List.nth l 0, List.nth l 1)


(*
let[@warnerror "-unused-value-declaration"] solve_example = begin
    let permutations_results = List.map compute test_inputs in
    let flattend_permutations = List.concat permutations_results in
        let result = sum flattend_permutations in
        print_endline (string_of_int result)
    end;;
*)


let solve input_lines = 
    let permutations_results = List.map compute input_lines in
    let flattend_permutations = List.concat permutations_results in
        let result = Myutils.sum flattend_permutations in
        print_endline (string_of_int result)
