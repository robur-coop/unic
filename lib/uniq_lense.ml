type name = string

(* is_a_fpath => is_a_directory => location
   | is_a_package_name => find_META_file => location
   | )

let is_a_fpath name = match Fpath.of_string name with
  | Ok v when Sys.file_exists name ->
    if Fpath.is_rel v
    then Some (`Fpath Fpath.(to_root // v))
    else Some (`Fpath v)
  | Error _ -> None

let is_a_directory (`Fpath v as location) =
  if Sys.is_directory (Fpath.to_string v)
  then Some (`Directory (Fpath.to_dir_path v))
  else Some location

let v name =
  match is_a_fpath name >>= is_a_directory >>= collect,
        is_a_package_name name >>= find_META_file >>= resolve_META_file,
        is_an_opam_package_name name >>= find_directories with

