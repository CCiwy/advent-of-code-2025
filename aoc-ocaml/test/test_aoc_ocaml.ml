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

(* get file *)
let project_root = find_project_root (Stdlib.Sys.getcwd ())

let path_in_project rel = Filename.concat project_root rel
(* 
    let file_path = path_in_project "test/fixtures/simple.txt" 
    let () = print_endline project_root
let () = print_endline file_path




*)
let file_path = path_in_project "inputs/day1_1.txt"


let countLines = begin
    let lines = Stdio.In_channel.read_lines file_path in
        print_endline (string_of_int (List.length lines))
    end

let printLines = begin
    let lines = Stdio.In_channel.read_lines file_path in
        List.iter print_endline lines
    end

let () = let a = 2 in match a with
    | 0 -> countLines
    | 1 -> printLines
    | _ -> ()
