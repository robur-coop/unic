let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

(* A configuration variable: its [ocamlc -config] name, the {!Uniq_cfg.Value}
   witness needed to read it back with the right type, and how to print it. *)
type entry = E : string * 'a Uniq_cfg.Value.t * 'a Fmt.t -> entry

let entries =
  let s key = E (key, Uniq_cfg.Value.string, Fmt.string) in
  let p key = E (key, Uniq_cfg.Value.path, Fpath.pp) in
  let b key = E (key, Uniq_cfg.Value.bool, Fmt.bool) in
  let i key = E (key, Uniq_cfg.Value.int, Fmt.int) in
  let l key =
    E (key, Uniq_cfg.Value.(list string), Fmt.(list ~sep:(any " ") string))
  in
  [
    s "version"; p "standard_library_default"; p "standard_library"
  ; s "ccomp_type"; s "c_compiler"; l "CFLAGS"; l "CPPFLAGS"; l "ocamlc_cflags"
  ; l "ocamlc_cppflags"; l "ocamlopt_cflags"; l "ocamlopt_cppflags"
  ; l "bytecode_c_compiler"; l "native_c_compiler"; l "bytecomp_c_libraries"
  ; l "native_c_libraries"; l "native_pack_linker"; b "native_compiler"
  ; s "architecture"; s "model"; i "int_size"; i "word_size"; s "system"
  ; s "asm"; b "asm_cfi_supported"; b "with_frame_pointers"; s "ext_exe"
  ; s "ext_obj"; s "ext_asm"; s "ext_lib"; s "ext_dll"; s "os_type"
  ; s "default_executable_name"; b "systhread_supported"; s "host"; s "target"
  ; b "flambda"; b "safe_string"; b "default_safe_string"; b "flat_float_array"
  ; b "function_sections"; b "afl_instrument"; b "windows_unicode"
  ; b "supports_shared_libraries"; b "native_dynlink"; b "naked_pointers"
  ; b "compression_supported"; s "exec_magic_number"; s "cmi_magic_number"
  ; s "cmo_magic_number"; s "cma_magic_number"; s "cmx_magic_number"
  ; s "cmxa_magic_number"; s "ast_impl_magic_number"; s "ast_intf_magic_number"
  ; s "cmxs_magic_number"; s "cmt_magic_number"; s "linear_magic_number"
  ]

let normalize = Astring.String.map (function '-' -> '_' | chr -> chr)

let run quiet cfg native key =
  let ( let* ) = Result.bind in
  let* cfg = match cfg with Some cfg -> Ok cfg | None -> Uniq_cfg.v () in
  match key with
  | None ->
      (* Dump the whole configuration, one variable per line. *)
      let print (E (key, w, pp)) =
        match Uniq_cfg.get ~native cfg ~key w with
        | Some v -> if not quiet then Fmt.pr "%-26s %a\n%!" key pp v
        | None -> ()
      in
      List.iter print entries; Ok 0
  | Some k -> begin
      let same (E (key, _, _)) = String.equal (normalize key) (normalize k) in
      match List.find_opt same entries with
      | None -> error_msgf "Unknown configuration variable: %S" k
      | Some (E (key, w, pp)) ->
          begin match Uniq_cfg.get ~native cfg ~key w with
          | Some v ->
              if not quiet then Fmt.pr "%a\n%!" pp v;
              Ok 0
          | None ->
              error_msgf
                "%S depends on the chosen toolchain; pass --native or \
                 --bytecode"
                k
          end
    end

open Cmdliner
open Unic_cli

let native =
  let open Arg in
  value
  & vflag None
      [
        (Some true, info [ "native" ] ~doc:"Assume the native toolchain.")
      ; (Some false, info [ "bytecode" ] ~doc:"Assume the bytecode toolchain.")
      ]

let key =
  let doc =
    "The configuration variable to print (e.g. $(b,standard_library)). When \
     omitted, every variable is printed."
  in
  let open Arg in
  value & pos 0 (some string) None & info [] ~doc ~docv:"VARIABLE"

let term =
  let open Term in
  term_result ~usage:false (const run $ setup_logs $ setup_ocaml $ native $ key)

let cmd =
  let doc = "Print the configuration of the OCaml toolchain." in
  let man =
    [
      `S Manpage.s_description
    ; `P
        "$(tname) prints the configuration of the OCaml toolchain, as \
         $(b,ocamlc -config) does. If a variable is given as an argument, only \
         its value is printed."
    ; `P
        "With the $(b,--toolchain) option, the configuration comes from the \
         given ocamlfind toolchain (such as $(b,solo5)) instead of the host \
         one."
    ; `P
        "Some variables depend on the compiler used. In this case, \
         $(b,--native) or $(b,--bytecode) must be given."
    ]
  in
  Cmd.v (Cmd.info "cfg" ~doc ~man) term
