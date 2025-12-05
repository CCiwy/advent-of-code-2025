(* PRINTS *)
let print_int_list l =
    let print_entry x = print_endline (string_of_int x) in
    List.iter print_entry l 

(* STRING *)
(* helper function that returns tuple direction (first char), value (digits after that) from given string *)
let parts s = String.(sub s 0 1, sub s 1 (length s - 1)) (* TODO: make generic with input x as position to slice *)

(* NUMBER *)
(* create range of ints *)
let rec range ~first:lo ~last:hi =
    (* labeleded version *)
    if lo > hi then []
    else lo :: range ~first:(lo + 1) ~last:hi;;


let rec factorial ?(l=[]) x  =
    match x with
    | x when x mod 2 = 0 -> let result = (x / 2) in factorial result ~l:( 2 :: l)
    | x when x mod 3 = 0 -> let result = (x / 3) in factorial result ~l:( 3 :: l)
    | x when x mod 5 = 0 -> let result = (x / 5) in factorial result ~l:( 5 :: l)
    | x when x mod 7 = 0 -> let result = (x / 7) in factorial result ~l:( 7 :: l)
    | _ -> 1 :: x :: l;;


(* INT LIST *)
let rec get_max ?(current=0) l =
    match l with
        | [] -> current 
        | t :: [] -> t
        | t :: h -> 
        get_max ~current:(Int.max t current) h

let rec sum ?(acc=0) l =
    (* sum up all ints in a list *)
    match l with
        | [] -> acc
        | t :: [] -> acc + t 
        | t :: h -> sum h ~acc:(acc + t)

(* lets define our own range function, because why not 
let rec range start stop r = match stop - start with
        | 0 -> List.rev (start :: r)
        | ( < ) 0 -> []
        | _ -> range (start + 1) stop (start :: r)
   *)

(* FILES *)
let rec find_project_root dir =
  let dune_project = Filename.concat dir "dune-project" in
  if Stdlib.Sys.file_exists dune_project then
    dir
  else
    let parent = Filename.dirname dir in
    if Stdlib.String.equal parent dir then
      failwith "Could not find dune-project up the directory tree"
    else
      find_project_root parent

let project_root = find_project_root (Stdlib.Sys.getcwd ())

let path_in_project rel = Filename.concat project_root rel



