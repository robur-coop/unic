module Set = Set.Make (Modname)

let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Elt = struct
  type t =
    | Resolved of Modname.t * Uniq_meta.Path.t
    | Ambiguous of Modname.t * Uniq_meta.Path.t list
    | Not_found of Modname.t

  let pp ppf = function
    | Resolved (modname, pkg) ->
        Fmt.pf ppf "%a => %a"
          Fmt.(styled (`Fg `Green) Modname.pp)
          modname Uniq_meta.Path.pp pkg
    | Not_found modname ->
        Fmt.pf ppf "%a" Fmt.(styled (`Fg `Red) Modname.pp) modname
    | Ambiguous (modname, pkgs) ->
        Fmt.pf ppf "%a => @[<hov>%a@]"
          Fmt.(styled (`Fg `Yellow) Modname.pp)
          modname
          Fmt.(Dump.list Uniq_meta.Path.pp)
          pkgs
end

let run cfg recurse root no_stdlib ocamlfind_roots =
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
  let* ts = Uniq_resolve.qualify ~stdlib:(not no_stdlib) srcs in
  let intfs, impls =
    let fn (intfs, impls) t =
      let intfs', impls' = Uniq_info.missing t in
      let intfs = Set.add_seq (List.to_seq intfs') intfs in
      let impls = Set.add_seq (List.to_seq impls') impls in
      (intfs, impls)
    in
    List.fold_left fn Set.(empty, empty) ts
  in
  let results =
    let missing = Set.union intfs impls in
    if Set.is_empty missing then []
    else
      let missing = Set.to_list missing in
      let providers = Uniq_meta.find_providers ~roots:ocamlfind_roots missing in
      let not_found =
        let fn s (m, _) = Set.add m s in
        let provided = List.fold_left fn Set.empty providers in
        let fn m = not (Set.mem m provided) in
        List.filter fn missing
      in
      let not_found = List.rev_map (fun m -> (m, [])) not_found in
      let elements = List.rev_append providers not_found in
      let fn (modname, pkgs) =
        match pkgs with
        | [ pkg ] -> Elt.Resolved (modname, pkg)
        | [] -> Elt.Not_found modname
        | pkgs -> Elt.Ambiguous (modname, pkgs)
      in
      List.map fn elements
  in
  List.iter (fun elt -> Fmt.pr "%a\n%!" Elt.pp elt) results;
  Ok ()

let run () _quiet cfg recursive root no_stdlib ocamlfind_roots =
  match run cfg recursive root no_stdlib ocamlfind_roots with
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
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
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

let prefer =
  let doc =
    "Prefer these packages when multiple provide the same module \
     (comma-separated)."
  in
  Arg.(
    value & opt (list string) [] & info [ "prefer" ] ~doc ~docv:"PKG1,PKG2,...")

let term =
  let open Term in
  let term =
    const run
    $ setup_fmt
    $ setup_logs
    $ Uniq_cfg.setup
    $ recurse
    $ path
    $ no_stdlib
    $ Uniq_meta.setup
  in
  ret term

let cmd =
  let doc = "Print information about an OCaml file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "info" ~doc ~man) term

let () = Cmd.(exit @@ eval cmd)
