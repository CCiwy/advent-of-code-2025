(* helper function that returns tuple direction (first char), value (digits after that) from given string *)
let parts s = String.(sub s 0 1, sub s 1 (length s - 1)) (* TODO: move to utils *)

(* file stuff *)
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

(* use this util function *)
let path_in_project rel = Filename.concat project_root rel
