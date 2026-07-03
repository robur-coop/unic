let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Bos

type elements = [ `Only_objects | `Only_sources | `All ]

let objects =
  let exts = [ ".cmo"; ".cma"; ".cmx"; ".cmxa"; ".cmi" ] in
  let fn location =
    Logs.debug (fun m -> m "Is an ocaml object? %a" Fpath.pp location);
    if Fpath.is_dir_path location then Ok false
    else Ok (Fpath.mem_ext exts location)
  in
  `Sat fn

let sources =
  let exts = [ ".ml"; ".mli" ] in
  let fn location =
    if Fpath.is_dir_path location then Ok false
    else Ok (Fpath.mem_ext exts location)
  in
  `Sat fn

let all =
  let (`Sat objects) = objects in
  let (`Sat sources) = sources in
  let fn location =
    let* a = objects location in
    let* b = sources location in
    Ok (a || b)
  in
  `Sat fn

let fold ?dotfiles ?(elements = `All) ?traverse fn acc roots =
  let elements =
    match elements with
    | `Only_sources -> sources
    | `Only_objects -> objects
    | `All -> all
  in
  OS.Path.fold ?dotfiles ~elements ?traverse fn acc roots

let only location =
  let fn location' = Ok (Fpath.equal location location') in
  `Sat fn

let run _quiet cfg recurse root no_stdlib =
  let ( let* ) = Result.bind in
  let sources = Uniq_resolve.Src.sources ~recurse root in
  let srcs =
    match cfg with
    | None -> [ sources ]
    | Some cfg ->
        begin if no_stdlib then [ sources ]
        else
          match Uniq_cfg.(get cfg ~key:"standard_library" Value.path) with
          | Some stdlib -> [ sources; Uniq_resolve.Src.objects stdlib ]
          | None -> [ sources ]
        end
  in
  let* ts = Uniq_resolve.qualify srcs in
  let module Set = Set.Make (Modname) in
  let intfs, impls =
    List.fold_left
      (fun (intfs, impls) t ->
        let intfs', impls' = Uniq_info.missing t in
        let intfs' = List.map fst intfs' in
        let intfs' = List.to_seq intfs' in
        let impls' = List.map fst impls' in
        let impls' = List.to_seq impls' in
        let intfs = Set.add_seq intfs' intfs in
        let impls = Set.add_seq impls' impls in
        (intfs, impls))
      Set.(empty, empty)
      ts
  in
  List.iter (Uniq_info.show Fmt.stdout) ts;
  let intfs = Set.to_list intfs in
  let impls = Set.to_list impls in
  if intfs <> [] then Fmt.pr "Missing interfaces:\n%!";
  List.iter (fun m -> Fmt.pr "\t%a\n%!" Fmt.(styled `Yellow Modname.pp) m) intfs;
  if impls <> [] then Fmt.pr "Missing implementations:\n%!";
  List.iter (fun m -> Fmt.pr "\t%a\n%!" Fmt.(styled `Yellow Modname.pp) m) impls;
  Ok 0

open Cmdliner
open Unic_cli

let path =
  let doc = "The OCaml project directory." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str ->
        if Sys.is_directory str then Ok (Fpath.to_dir_path v) else Ok v
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let existing_context = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  required
  & pos ~rev:true 0 (some existing_context) None
  & info [] ~doc ~docv:"DIRECTORY"

let recurse =
  let doc = "Include sub-directories." in
  Arg.(value & flag & info [ "r"; "recurse" ] ~doc)

let without_stdlib =
  let doc = "Do not add the standard library to the list of include sources." in
  Arg.(value & flag & info [ "without-stdlib" ] ~doc)

let term =
  let open Term in
  const run
  $ setup_logs
  $ setup_ocaml
  $ recurse
  $ path
  $ without_stdlib
  |> term_result

let cmd =
  let doc = "Print information about an OCaml file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "qualify" ~doc ~man) term
