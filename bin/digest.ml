let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let run path (mpath : Uniq_info.Path.t option) =
  match Uniq_info.v path with
  | Error _ as err -> err
  | Ok v ->
      begin match (Uniq_info.exports v, mpath) with
      | [], _ -> assert false (* TODO(dinosaure): should never occur! *)
      | [ (_path, Some digest) ], None -> Ok digest
      | [ (_path, None) ], None ->
          error_msgf "%a does not export a digest (is it a source file?)"
            Fpath.pp path
      | [ (p, Some digest) ], Some p' ->
          if Uniq_info.Path.compare p p' = 0 then Ok digest
          else
            error_msgf "%a is not present into %a" Uniq_info.Path.pp p' Fpath.pp
              path
      | _exports, None ->
          error_msgf
            "%a exports multiple artifacts, you must precise the module name"
            Fpath.pp path
      | exports, Some p' ->
          begin match
            List.find_opt
              (fun (p, _) -> Uniq_info.Path.compare p p' = 0)
              exports
          with
          | None ->
              error_msgf "%a is not present into %a" Uniq_info.Path.pp p'
                Fpath.pp path
          | Some (p, None) ->
              error_msgf "%a does not export a digest of %a" Fpath.pp path
                Uniq_info.Path.pp p
          | Some (_, Some digest) -> Ok digest
          end
      end

let run () quiet path modname =
  match run path modname with
  | Ok digest when not quiet ->
      Fmt.pr "%a\n%!" Uniq_digest.pp digest;
      `Ok 0
  | Ok _ -> `Ok 0
  | Error (`Msg msg) when not quiet -> Fmt.epr "%s.\n%!" msg; `Ok 1
  | Error _ -> `Ok 1

open Cmdliner
open Args

let artifact =
  let doc = "The OCaml object." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && Sys.is_directory str = false -> v
    | Ok v -> error_msgf "%a is not a file or does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let artifact = Arg.conv (parser, Fpath.pp) in
  let open Arg in
  required & pos 0 (some artifact) None & info [] ~doc ~docv:"ARTIFACT"

let path : Uniq_info.Path.t option Term.t =
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
  value & pos 1 (some v) None & info [] ~doc ~docv:"MODNAME"

let term =
  let open Term in
  ret (const run $ setup_fmt $ setup_logs $ artifact $ path)

let cmd =
  let doc = "Try to extract the $(i,digest) from an OCaml object." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "digest" ~doc ~man) term

let () = Cmd.(exit @@ eval' cmd)
