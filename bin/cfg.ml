type pp = Pp : 'a Fmt.t * 'a -> pp

let run () _quiet native key =
  let cfg = Uniq_cfg.v () in
  let Pp (pp, v) = match key with
    | "version" -> Pp (Fmt.string, cfg.version)
    | ("standard_library_default" | "standard-library-default") -> Pp (Fpath.pp, cfg.standard_library_default)
    | ("standard_library" | "standard-library") -> Pp (Fpath.pp, cfg.standard_library)
    | ("ccomp_type" | "ccomp-type") -> Pp (Fmt.string, cfg.ccomp_type)
    | ("c_compiler" | "c-compiler") -> Pp (Fmt.string, cfg.c_compiler)
    | "CFLAGS" ->
      if cfg.ocamlc_cflags = cfg.ocamlopt_cflags
      || Option.fold ~none:false ~some:Fun.negate native
      then Pp (Fmt.(list ~sep:sp string), cfg.ocamlc_cflags)
      else if Option.value ~default:false native
      then Pp (Fmt.(list ~sep:sp string), cfg.ocamlopt_cflags)
      else Fmt.failwith "Impossible to infer CFLAGS"
    | "CPPFLAGS" ->
      if cfg.ocamlc_cppflags = cfg.ocamlopt_cppflags
      || Option.fold ~none:false ~some:Fun.negate native
      then Pp (Fmt.(list ~sep:sp string), cfg.ocamlc_cppflags)
      else if Option.value ~default:false native
      then Pp (Fmt.(list ~sep:sp string), cfg.ocamlopt_cppflags)
      else Fmt.failwith "Impossible to infer CFLAGS"
    | ("ocamlc_cflags" | "ocamlc-cflags" | "CFLAGS") -> Pp (Fmt.(list ~sep:sp string), cfg.ocamlc_cflags)
    | ("ocamlc_cppflags" | "ocamlc-cppflags") -> Pp (Fmt.(list ~sep:sp string), cfg.ocamlc_cppflags)
    | ("ocamlopt_cflags" | "ocamlopt-cppflags") -> Pp (Fmt.(list ~sep:sp string), cfg.ocamlopt_cflags)

open Cmdliner
open Args

let cmd =
  let doc = "Print informations about an OCaml file." in
  let man = [ `S Manpage.s_description; `P "$(tname)" ] in
  Cmd.v (Cmd.info "info" ~doc ~man) term

let () = Cmd.(exit @@ eval cmd)
