let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let show quiet location =
  match Uniq_info.v location with
  | Ok t ->
      if not quiet then Uniq_info.show Fmt.stdout t;
      `Ok 0
  | Error (`Msg msg) ->
      if quiet then `Ok 1 else `Error (false, Fmt.str "%s." msg)

let search quiet filters prefer_library roots modname digest =
  match Uniq_mod.search ~filters ~roots modname digest with
  | Ok [] -> `Ok 1
  | Ok modules ->
      let modules =
        if prefer_library then
          match
            List.filter (fun (_, m) -> Uniq_info.is_a_library m) modules
          with
          | [] -> modules
          | libraries -> libraries
        else modules
      in
      List.iter (fun (path, _) -> Fmt.pr "%a\n%!" Fpath.pp path) modules;
      `Ok 0
  | Error (`Msg msg) ->
      if quiet then `Ok 1 else `Error (false, Fmt.str "%s." msg)

open Cmdliner
open Unic_cli

let file =
  let doc = "The OCaml object." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && Sys.is_directory str = false -> v
    | Ok v -> error_msgf "%a is not a file or does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let existing_file = Arg.conv (parser, Fpath.pp) in
  Arg.(required & pos ~rev:true 0 (some existing_file) None & info [] ~doc)

let term_show =
  let open Term in
  ret (const show $ setup_logs $ file)

let cmd_show =
  let doc = "Print informations about an OCaml file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "show" ~doc ~man) term_show

let directories =
  let doc = "The directory containing the OCaml files." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && Sys.is_directory str -> v
    | Ok v -> error_msgf "%a is not a directory or does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let open Arg in
  value
  & opt_all (conv (parser, Fpath.pp)) []
  & info [ "I" ] ~doc ~docv:"DIRECTORY"

let path =
  let doc = "The module name." in
  let parser str =
    let p = String.split_on_char '.' str in
    let fn acc str =
      match (acc, Modname.of_string str) with
      | (Error _ as err), _ -> err
      | _, (Error _ as err) -> err
      | Ok rpath, Ok m -> Ok (m :: rpath)
    in
    let ( let* ) = Result.bind in
    let* lst = List.fold_left fn (Ok []) p in
    let lst = List.rev lst in
    Ok (Uniq_info.Path.of_list lst)
  in
  let pp = Uniq_info.Path.pp in
  let v = Arg.conv (parser, pp) in
  let open Arg in
  required & pos 0 (some v) None & info [] ~doc ~docv:"MODNAME"

let digest =
  let doc = "The $(i,digest) of the module." in
  let digest = Arg.conv Uniq_digest.(of_string, pp) in
  let open Arg in
  value & opt (some digest) None & info [ "digest" ] ~doc ~docv:"DIGEST"

let kind_of_artifacts =
  let intf =
    let doc = "Select only interfaces." in
    let info = Arg.info [ "intf" ] ~doc in
    (`Intf, info)
  in
  let impl =
    let doc = "Select only implementations." in
    let info = Arg.info [ "impl" ] ~doc in
    (`Impl, info)
  in
  let open Arg in
  value & vflag `All [ intf; impl ]

let kind_of_objects =
  let sources =
    let doc = "Select only source files." in
    let info = Arg.info [ "sources" ] ~doc in
    (`Sources, info)
  in
  let objects =
    let doc = "Select only object files." in
    let info = Arg.info [ "objects" ] ~doc in
    (`Objects, info)
  in
  let open Arg in
  value & vflag `All [ sources; objects ]

let target =
  let native =
    let doc = "Select only native objects." in
    let info = Arg.info [ "native" ] ~doc in
    (`Native, info)
  in
  let bytecode =
    let doc = "Select only bytecode objects." in
    let info = Arg.info [ "bytecode" ] ~doc in
    (`Bytecode, info)
  in
  let open Arg in
  value & vflag `All [ native; bytecode ]

let prefer_library =
  let doc = "Prefer libraries (.cma & .cmxa) instead of unit modules." in
  let open Arg in
  value & flag & info [ "prefer-library" ] ~doc

let setup_filters kind_artifacts kind_objects target =
  (kind_artifacts, kind_objects, target)

let setup_filters =
  let open Term in
  const setup_filters $ kind_of_artifacts $ kind_of_objects $ target

let term_search =
  let open Term in
  ret
    begin
      const search
      $ setup_logs
      $ setup_filters
      $ prefer_library
      $ directories
      $ path
      $ digest
    end

let cmd_search =
  let doc = "Search a module from a module name and a $(i,digest)." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "search" ~doc ~man) term_search

let cmd =
  let doc = "A tool to manipulate OCaml objects." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group ~default (Cmd.info "info" ~doc ~man) [ cmd_show; cmd_search ]
