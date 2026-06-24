let src = Logs.Src.create "uniq.cli"
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Log = (val Logs.src_log src : Logs.LOG)
open Cmdliner

let existing_filepath =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_regular_file str -> Ok v
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  Arg.conv (parser, Fpath.pp)

let existing_dirpath =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_directory str ->
        Ok (Fpath.to_dir_path v)
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  Arg.conv (parser, Fpath.pp)

let s_output = "OUTPUT OPTIONS"
let s_logs = "LOGS OPTIONS"
let verbosity = Logs_cli.level ~docs:s_logs ()
let renderer = Fmt_cli.style_renderer ~docs:s_output ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let open Arg in
  value & opt bool true & info [ "with-utf-8" ] ~doc ~docs:s_output

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let neg fn = fun x -> not (fn x)

let reporter sources ppf =
  let re = Stdlib.Option.map Re.compile sources in
  let print src =
    let some re = (neg List.is_empty) (Re.matches re (Logs.Src.name src)) in
    Stdlib.Option.fold ~none:true ~some re
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let pp header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    match (level, print src) with
    | Logs.Debug, false -> k ()
    | _, true | _ -> msgf @@ fun ?header ?tags fmt -> pp header tags k ppf fmt
  in
  { Logs.report }

let regexp : (string * [ `None | `Re of Re.t ]) Arg.conv =
  let parser str =
    match Re.Pcre.re str with
    | re -> Ok (str, `Re re)
    | exception _ -> error_msgf "Invalid PCRegexp: %S" str
  in
  let pp ppf (str, _) = Fmt.string ppf str in
  Arg.conv (parser, pp)

let sources =
  let doc = "A regexp (PCRE syntax) to identify which log we print." in
  let open Arg in
  value
  & opt_all regexp [ ("", `None) ]
  & info [ "l" ] ~doc ~docs:s_logs ~docv:"REGEXP"

let setup_sources = function
  | [ (_, `None) ] -> None
  | res ->
      let res = List.map snd res in
      let res =
        List.fold_left
          (fun acc -> function `Re re -> re :: acc | _ -> acc)
          [] res
      in
      Some (Re.alt res)

let setup_sources = Term.(const setup_sources $ sources)

let setup_logs utf_8 style_renderer sources level =
  Stdlib.Option.iter (Fmt.set_style_renderer Fmt.stdout) style_renderer;
  Stdlib.Option.iter (Fmt.set_style_renderer Fmt.stderr) style_renderer;
  Fmt.set_utf_8 Fmt.stdout utf_8;
  Fmt.set_utf_8 Fmt.stderr utf_8;
  Logs.set_level level;
  Logs.set_reporter (reporter sources Fmt.stderr);
  Stdlib.Option.is_none level

let setup_logs =
  Term.(const setup_logs $ utf_8 $ renderer $ setup_sources $ verbosity)

let s_ocamlfind = "OCAMLFIND OPTIONS"

let ocamlfind's_directories =
  let doc = "The source directory containing the META files." in
  let open Arg in
  value
  & opt_all existing_dirpath []
  & info [ "I" ] ~doc ~docs:s_ocamlfind ~docv:"DIRECTORY"

let setup_ocamlfind toolchain user's_directories =
  let cmd =
    match toolchain with
    | None -> Bos.Cmd.(v "ocamlfind" % "printconf" % "path")
    | Some t ->
        Bos.Cmd.(v "ocamlfind" % "-toolchain" % t % "printconf" % "path")
  in
  let ( let* ) = Result.bind in
  let directories =
    let* exists = Bos.OS.Cmd.exists cmd in
    if exists then
      let r = Bos.OS.Cmd.run_out cmd in
      let* directories, _ = Bos.OS.Cmd.out_lines ~trim:true r in
      let directories =
        List.fold_left
          (fun acc path ->
            match Fpath.of_string path with
            | Ok fpath when Sys.file_exists path && Sys.is_directory path ->
                fpath :: acc
            | Ok _ -> acc
            | Error (`Msg _) ->
                Log.warn (fun m ->
                    m "ocamlfind returned an invalid path: %S" path);
                acc)
          [] directories
      in
      Ok directories
    else Ok []
  in
  let directories = Result.value ~default:[] directories in
  Log.debug (fun m ->
      m "ocamlfind directories: @[<hov>%a@]"
        Fmt.(list ~sep:(any ",@ ") Fpath.pp)
        directories);
  List.rev_append directories user's_directories

let toolchain =
  let doc =
    "Use the $(b,ocamlfind) toolchain $(i,NAME) (e.g. $(b,solo5)) instead of \
     the host one: both the OCaml configuration and the package rotos are \
     taken from that cross toolchain."
  in
  let open Arg in
  value
  & opt (some string) None
  & info [ "toolchain" ] ~doc ~docs:s_ocamlfind ~docv:"NAME"

let setup_ocamlfind =
  Term.(const setup_ocamlfind $ toolchain $ ocamlfind's_directories)

let s_ocaml = "OCAML OPTIONS"

let compiler =
  let doc = "The compiler chosen (bytecode or native)." in
  let parser str =
    match String.lowercase_ascii str with
    | "bytecode" -> Ok `Bytecode
    | "native" -> Ok `Native
    | _ -> error_msgf "Invalid compiler %S (must be bytecode or native)" str
  in
  let pp ppf = function
    | `Bytecode -> Fmt.string ppf "bytecode"
    | `Native -> Fmt.string ppf "native"
  in
  let compiler = Arg.conv (parser, pp) in
  let open Arg in
  value
  & opt compiler `Native
  & info [ "compiler" ] ~doc ~docs:s_ocaml ~docv:"COMPILER"

let setup_ocaml toolchain compiler =
  let compiler =
    match compiler with `Native -> "ocamlopt" | `Bytecode -> "ocamlc"
  in
  match Uniq_cfg.from ?toolchain compiler () with
  | Ok (where, cfg) -> Some (where, cfg)
  | Error (`Msg msg) ->
      Log.warn (fun m ->
          m "Impossible to get the configuration of OCaml: %s" msg);
      None

let setup_ocaml = Term.(const setup_ocaml $ toolchain $ compiler)

let modname =
  let parser str =
    match Modname.of_string str with
    | Ok _ as v -> v
    | Error (`Msg msg) -> Error (`Msg msg)
    (* NOTE(dinosaure): open it! *)
  in
  Arg.conv ~docv:"MODULE" (parser, Modname.pp)

let path =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str ->
        let v = if Sys.is_directory str then Fpath.to_dir_path v else v in
        Ok v
    | Ok v -> error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  Arg.conv (parser, Fpath.pp)

let setup_policy = function
  | None -> Uniq_policy.empty
  | Some filepath ->
      let policy = Uniq_policy.load filepath in
      let fn _ =
        Log.warn (fun m ->
            m "Invalid configuration file (you can validate it with %a): %a"
              Fmt.(styled `Bold string)
              "bcfg validate" Fpath.pp filepath)
      in
      Result.iter_error fn policy;
      Result.value ~default:Uniq_policy.empty policy

let configuration =
  let doc = "A $(b,bcfg) policy file to disambiguate package choices." in
  let open Arg in
  value
  & opt (some existing_filepath) None
  & info [ "c"; "config" ] ~doc ~docv:"FILE"

let setup_policy = Term.(const setup_policy $ configuration)
