[@@@ocamlformat "disable"]

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let pp_m2l_kind ppf = function
  | M2l.Structure -> Format.pp_print_string ppf "Structure"
  | M2l.Signature -> Format.pp_print_string ppf "Signature"
;;

let ( / ) = Filename.concat ;;

#install_printer Modname.pp
#install_printer Namespaced.pp
#install_printer pp_m2l_kind
