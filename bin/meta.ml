let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let show () quiet meta =
  match Uniq_meta.parser meta with
  | Ok ts ->
      if not quiet then
        Fmt.pr "%a@\n%!" Fmt.(list ~sep:(any "@\n") Uniq_meta.pp) ts;
      Ok ()
  | Error _ as err -> err

let pp_descr ppf t =
  let longest_key =
    List.fold_left (fun acc (k, _) -> max (String.length k) acc) 0 t
  in
  let print (k, vs) =
    match vs with
    | [] | [ "" ] -> ()
    | [ v ] -> Fmt.pf ppf "%*s %s\n%!" longest_key k v
    | vs -> Fmt.pf ppf "%*s @[<hov>%a@]" longest_key k Fmt.(Dump.list string) vs
  in
  List.iter print t

let search () _quiet predicates roots path =
  match Uniq_meta.search ~roots ~predicates path with
  | Ok descrs ->
      List.iter
        (fun (path, descr) ->
          Fmt.pr "%a:\n%!" Fmt.(styled `Green Fpath.pp) path;
          Fmt.pr "%a\n%!" pp_descr descr)
        descrs;
      Ok ()
  | Error _ as err -> err

let ancestors () _quiet predicates roots path =
  match Uniq_meta.ancestors ~roots ~predicates path with
  | Ok descrs ->
      List.iter
        (fun (meta, path, descr) ->
          Fmt.pr "%a:\n%!" Fmt.(styled `Green Fpath.pp) path;
          Fmt.pr "%a:\n%!" Fmt.(styled `Yellow Uniq_meta.Path.pp) meta;
          Fmt.pr "%a\n%!" pp_descr descr)
        descrs;
      Ok ()
  | Error _ as err -> err

open Cmdliner
open Args

let path =
  let doc = "The META file." in
  let parser str =
    match Fpath.of_string str with
    | Ok _ as v when Sys.file_exists str && not (Sys.is_directory str) -> v
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let existing_file = Arg.conv (parser, Fpath.pp) in
  Arg.(required & pos ~rev:true 0 (some existing_file) None & info [] ~doc)

let meta_path =
  let doc = "The META path." in
  let meta_path = Arg.conv Uniq_meta.(Path.of_string, Path.pp) in
  let open Arg in
  required & pos ~rev:true 0 (some meta_path) None & info [] ~doc

let predicates =
  let doc = "A predicate to filter the resulted description." in
  let open Arg in
  value
  & opt_all string [ "native" ]
  & info [ "predicate" ] ~doc ~docv:"PREDICATE"

let term_show =
  let open Term in
  term_result ~usage:false (const show $ setup_fmt $ setup_logs $ path)

let term_search =
  let open Term in
  term_result ~usage:false
    (const search
    $ setup_fmt
    $ setup_logs
    $ predicates
    $ Uniq_meta.setup
    $ meta_path)

let term_ancestors =
  let open Term in
  term_result ~usage:false
    (const ancestors
    $ setup_fmt
    $ setup_logs
    $ predicates
    $ Uniq_meta.setup
    $ meta_path)

let cmd_show =
  let doc = "Parse & print META file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "show" ~doc ~man) term_show

let cmd_search =
  let doc =
    "Search a description from a META path, a directory and predicates."
  in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "search" ~doc ~man) term_search

let cmd_ancestors =
  let doc = "" in
  let man = [] in
  Cmd.v (Cmd.info "ancestors" ~doc ~man) term_ancestors

let () =
  let doc = "A tool to manipulate META files." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  let cmd =
    Cmd.group ~default
      (Cmd.info "meta" ~doc ~man)
      [ cmd_show; cmd_search; cmd_ancestors ]
  in
  Cmd.(exit @@ eval cmd)
