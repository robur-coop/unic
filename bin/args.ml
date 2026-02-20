open Cmdliner

let run_fmt utf_8 style_renderer =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ()

let setup_fmt =
  let env = Cmd.Env.info "UNIQ_COLOR" in
  let style_renderer = Fmt_cli.style_renderer ~env () in
  let utf_8 =
    let doc = "Allow binaries to emit UTF-8 characters." in
    let env = Cmd.Env.info "UNIQ_UTF_8" in
    Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)
  in
  Term.(const run_fmt $ utf_8 $ style_renderer)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let run_logs level =
  Logs.set_level level;
  Logs.set_reporter (reporter Fmt.stderr);
  Stdlib.Option.is_none level

let setup_logs =
  let env = Cmd.Env.info "UNIQ_LOGS" in
  let verbosity = Logs_cli.level ~env () in
  Term.(const run_logs $ verbosity)
