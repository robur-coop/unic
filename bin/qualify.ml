open Rresult
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
    objects location >>= fun a ->
    sources location >>= fun b -> Ok (a || b)
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

let run cfg recurse root no_stdlib =
  let ( let* ) x f = Result.bind x f in
  let sources = Uniq_resolve.Src.sources ~recurse root in
  let srcs =
    match cfg with
    | None -> [ sources ]
    | Some cfg -> begin
        if no_stdlib then [ sources ]
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
        ( Set.add_seq (List.to_seq intfs') intfs
        , Set.add_seq (List.to_seq impls') impls ))
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
  Ok ()

let run () _quiet cfg recursive root no_stdlib =
  match run cfg recursive root no_stdlib with
  | Ok () -> `Ok ()
  | Error (`Msg msg) -> `Error (false, msg)

open Cmdliner
open Args

let path =
  let doc = "The OCaml project directory." in
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str ->
        if Sys.is_directory str then Ok (Fpath.to_dir_path v) else Ok v
    | Ok v -> R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let existing_context = Arg.conv (parser, Fpath.pp) in
  Arg.(required & pos ~rev:true 0 (some existing_context) None & info [] ~doc)

let recurse =
  let doc = "Include sub-directories." in
  Arg.(value & flag & info [ "r"; "recurse" ] ~doc)

let no_stdlib =
  let doc = "Do not add the standard library to the list of include sources." in
  Arg.(value & flag & info [ "no-stdlib" ] ~doc)

let term =
  let open Term in
  ret
    (const run
    $ setup_fmt
    $ setup_logs
    $ Uniq_cfg.setup
    $ recurse
    $ path
    $ no_stdlib)

let cmd =
  let doc = "Print information about an OCaml file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "info" ~doc ~man) term

let () = Cmd.(exit @@ eval cmd)
