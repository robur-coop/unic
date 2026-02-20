let src = Logs.Src.create "uniq.cfg"

module Log = (val Logs.src_log src : Logs.LOG)

type cfg = {
    version: string
  ; standard_library_default: Fpath.t
  ; standard_library: Fpath.t
  ; ccomp_type: string
  ; c_compiler: string
  ; ocamlc_cflags: string list
  ; ocamlc_cppflags: string list
  ; ocamlopt_cflags: string list
  ; ocamlopt_cppflags: string list
  ; bytecode_c_compiler: string list
  ; native_c_compiler: string list
  ; bytecomp_c_libraries: string list
  ; native_c_libraries: string list
  ; native_pack_linker: string list
  ; native_compiler: bool (* false *)
  ; architecture: string
  ; model: string
  ; int_size: int
  ; word_size: int
  ; system: string
  ; asm: string
  ; asm_cfi_supported: bool
  ; with_frame_pointers: bool
  ; ext_exe: string
  ; ext_obj: string
  ; ext_asm: string
  ; ext_lib: string
  ; ext_dll: string
  ; os_type: string
  ; default_executable_name: string
  ; systhread_supported: bool
  ; host: string
  ; target: string
  ; flambda: bool
  ; safe_string: bool
  ; default_safe_string: bool
  ; flat_float_array: bool
  ; function_sections: bool
  ; afl_instrument: bool
  ; windows_unicode: bool
  ; supports_shared_libraries: bool
  ; native_dynlink: bool (* false *)
  ; naked_pointers: bool
  ; compression_supported: bool (* false *)
  ; exec_magic_number: string
  ; cmi_magic_number: string
  ; cmo_magic_number: string
  ; cma_magic_number: string
  ; cmx_magic_number: string
  ; cmxa_magic_number: string
  ; ast_impl_magic_number: string
  ; ast_intf_magic_number: string
  ; cmxs_magic_number: string
  ; cmt_magic_number: string
  ; linear_magic_number: string
}

type src = Fpath.t
type t = src * cfg

type _ value =
  | String : string value
  | Bool : bool value
  | Int : int value
  | List : string * 'a value -> 'a list value
  | Path : Fpath.t value

let parse_output str =
  let fold acc str =
    match String.split_on_char ':' str with
    | [ key; value ] -> (String.trim key, String.trim value) :: acc
    | _ -> acc
  in
  List.fold_left fold [] (String.split_on_char '\n' str)

let dummy = Fpath.v "DXctMfpDO2nK3GsVGSQ5Ig=="

let parse_field : type a.
       default:a
    -> ?or_fail:(unit -> a)
    -> string
    -> a value
    -> (string * string) list
    -> a =
 fun ~default ?or_fail key t fields ->
  match (List.assoc_opt key fields, t, or_fail) with
  | None, _, None -> default
  | Some value, String, _ -> value
  | Some ("true" | "1"), Bool, _ -> true
  | Some ("false" | "0"), Bool, _ -> false
  | Some str, List (sep, String), _ -> Astring.String.cuts ~empty:false ~sep str
  | Some _, Bool, None -> default
  | Some str, Path, or_fail ->
      if Sys.is_directory str then Fpath.(to_dir_path (v str))
      else if Sys.file_exists str then Fpath.v str
      else Option.fold ~none:default ~some:(fun fn -> fn ()) or_fail
  | Some str, Int, or_fail -> begin
      try int_of_string str
      with _ -> Option.fold ~none:default ~some:(fun fn -> fn ()) or_fail
    end
  | _, _, None -> default
  | _, _, Some or_fail -> or_fail ()

let[@ocamlformat "disable"] parse str =
  let fields = parse_output str in
  let version                   = parse_field ~default:Sys.ocaml_version "version" String fields in
  let standard_library_default  = parse_field ~or_fail:(fun () -> failwith "OCaml standard library not found") ~default:dummy "standard_library_default" Path fields in
  let standard_library          = parse_field ~default:standard_library_default "standard_library" Path fields in
  let ccomp_type                = parse_field ~default:"cc" "ccomp_type" String fields in
  let c_compiler                = parse_field ~default:"cc" "c_compiler" String fields in
  let ocamlc_cflags             = parse_field ~default:[] "ocamlc_cflags" (List (" ", String)) fields in
  let ocamlc_cppflags           = parse_field ~default:[] "ocamlc_cppflags" (List (" ", String)) fields in
  let ocamlopt_cflags           = parse_field ~default:[] "ocamlopt_cflags" (List (" ", String)) fields in
  let ocamlopt_cppflags         = parse_field ~default:[] "ocamlopt_cppflags" (List (" ", String)) fields in
  let bytecode_c_compiler       = parse_field ~default:[] "bytecode_c_compiler" (List (" ", String)) fields in
  let native_c_compiler         = parse_field ~default:[] "native_c_compiler" (List (" ", String)) fields in
  let bytecomp_c_libraries      = parse_field ~default:[] "bytecomp_c_libraries" (List (" ", String)) fields in
  let native_c_libraries        = parse_field ~default:[] "native_c_libraries" (List (" ", String)) fields in
  let native_pack_linker        = parse_field ~default:[] "native_pack_linker" (List (" ", String)) fields in
  let native_compiler           = parse_field ~default:false "native_compiler" Bool fields in
  let architecture              = parse_field ~default:"unknown" "architecture" String fields in
  let model                     = parse_field ~default:"default" "model" String fields in
  let int_size                  = parse_field ~default:Sys.int_size "int_size" Int fields in
  let word_size                 = parse_field ~default:Sys.word_size "word_size" Int fields in
  let system                    = parse_field ~default:"unknown" "system" String fields in
  let asm                       = parse_field ~default:"as" "asm" String fields in
  let asm_cfi_supported         = parse_field ~default:false "asm_cfi_supported" Bool fields in
  let with_frame_pointers       = parse_field ~default:false "with_frame_pointers" Bool fields in
  let ext_exe                   = parse_field ~default:"" "ext_exe" String fields in
  let ext_obj                   = parse_field ~default:".o" "ext_obj" String fields in
  let ext_asm                   = parse_field ~default:".s" "ext_asm" String fields in
  let ext_lib                   = parse_field ~default:".a" "ext_lib" String fields in
  let ext_dll                   = parse_field ~default:".dll" "ext_dll" String fields in
  let os_type                   = parse_field ~default:Sys.os_type "os_type" String fields in
  let default_executable_name   = parse_field ~default:"a.out" "default_executable_name" String fields in
  let systhread_supported       = parse_field ~default:false "systhread_supported" Bool fields in
  let host                      = parse_field ~default:"unknown" "host" String fields in
  let target                    = parse_field ~default:"unknown" "target" String fields in
  let flambda                   = parse_field ~default:false "flambda" Bool fields in
  let safe_string               = parse_field ~default:false "safe_string" Bool fields in
  let default_safe_string       = parse_field ~default:false "default_safe_string" Bool fields in
  let flat_float_array          = parse_field ~default:false "flat_float_array" Bool fields in
  let function_sections         = parse_field ~default:false "function_sections" Bool fields in
  let afl_instrument            = parse_field ~default:false "afl_instrument" Bool fields in
  let windows_unicode           = parse_field ~default:false "windows_unicode" Bool fields in
  let supports_shared_libraries = parse_field ~default:false "supports_shared_libraries" Bool fields in
  let native_dynlink            = parse_field ~default:false "native_dynlink" Bool fields in
  let naked_pointers            = parse_field ~default:false "naked_pointers" Bool fields in
  let compression_supported     = parse_field ~default:false "compression_supported" Bool fields in
  let exec_magic_number         = parse_field ~default:Misc.Magic_number.(raw_kind Exec) "exec_magic_number" String fields in
  let cmi_magic_number          = parse_field ~default:Misc.Magic_number.(raw_kind Cmi) "cmi_magic_number" String fields in
  let cmo_magic_number          = parse_field ~default:Misc.Magic_number.(raw_kind Cmo) "cmo_magic_number" String fields in
  let cma_magic_number          = parse_field ~default:Misc.Magic_number.(raw_kind Cma) "cma_magic_number" String fields in
  let cmx_magic_number          = parse_field ~default:Misc.Magic_number.(raw_kind (Cmx { flambda })) "cmx_magic_number" String fields in
  let cmxa_magic_number         = parse_field ~default:Misc.Magic_number.(raw_kind (Cmxa { flambda })) "cmxa_magic_number" String fields in
  let ast_impl_magic_number     = parse_field ~default:Misc.Magic_number.(raw_kind Ast_impl) "ast_impl_magic_number" String fields in
  let ast_intf_magic_number     = parse_field ~default:Misc.Magic_number.(raw_kind Ast_intf) "ast_intf_magic_number" String fields in
  let cmxs_magic_number         = parse_field ~default:Misc.Magic_number.(raw_kind Cmxs) "cmxs_magic_number" String fields in
  let cmt_magic_number          = parse_field ~default:Misc.Magic_number.(raw_kind Cmt) "cmt_magic_number" String fields in
  let linear_magic_number       = parse_field ~default:"Caml1999" "linear_magic_number" String fields in
  Ok
    {
      version
    ; standard_library_default
    ; standard_library
    ; ccomp_type
    ; c_compiler
    ; ocamlc_cflags
    ; ocamlc_cppflags
    ; ocamlopt_cflags
    ; ocamlopt_cppflags
    ; bytecode_c_compiler
    ; native_c_compiler
    ; bytecomp_c_libraries
    ; native_c_libraries
    ; native_pack_linker
    ; native_compiler
    ; architecture
    ; model
    ; int_size
    ; word_size
    ; system
    ; asm
    ; asm_cfi_supported
    ; with_frame_pointers
    ; ext_exe
    ; ext_obj
    ; ext_asm
    ; ext_lib
    ; ext_dll
    ; os_type
    ; default_executable_name
    ; systhread_supported
    ; host
    ; target
    ; flambda
    ; safe_string
    ; default_safe_string
    ; flat_float_array
    ; function_sections
    ; afl_instrument
    ; windows_unicode
    ; supports_shared_libraries
    ; native_dynlink
    ; naked_pointers
    ; compression_supported
    ; exec_magic_number
    ; cmi_magic_number
    ; cmo_magic_number
    ; cma_magic_number
    ; cmx_magic_number
    ; cmxa_magic_number
    ; ast_impl_magic_number
    ; ast_intf_magic_number
    ; cmxs_magic_number
    ; cmt_magic_number
    ; linear_magic_number
    }

let ( <?> ) fn0 fn1 = match fn0 () with Ok _ as v -> v | Error _ -> fn1 ()

let from ?env compiler () =
  let open Rresult in
  let ( let* ) = Result.bind in
  let where = Bos.Cmd.(v "which" % compiler) in
  let where = Bos.OS.Cmd.run_out ?env where in
  let* where = Bos.OS.Cmd.out_string ~trim:true where in
  match where with
  | str, (_, `Exited 0) ->
      let where = Fpath.v str in
      Log.debug (fun m -> m "Use %a as the OCaml compiler" Fpath.pp where);
      let config = Bos.Cmd.(v compiler % "-config") in
      let config = Bos.OS.Cmd.run_out ?env config in
      begin match Bos.OS.Cmd.out_string ~trim:true config with
      | Ok (cfg, (_, `Exited 0)) -> parse cfg >>| fun cfg -> (where, cfg)
      | _ -> R.error_msgf "%s: impossible to get configuration" compiler
      end
  | _ -> R.error_msgf "which: impossible to find %s" compiler

let v ?env () = from ?env "ocamlc" <?> from ?env "ocamlopt"

module Value = struct
  type 'a t = 'a value

  let string = String
  let list ?(sep = " ") v = List (sep, v)
  let bool = Bool
  let int = Int
  let path = Path
end

let normalize str = Astring.String.map (function '-' -> '_' | chr -> chr) str

let cast : type a b. a value -> a -> b value -> b option =
 fun u value v ->
  match (u, v, value) with
  | Bool, Bool, v -> Some v
  | Int, Int, v -> Some v
  | Path, Path, v -> Some v
  | String, String, v -> Some v
  | List (_, String), List (_, String), v -> Some v
  | Bool, Int, true -> Some 1
  | Bool, Int, false -> Some 0
  | Bool, String, true -> Some "true"
  | Bool, String, false -> Some "false"
  | Bool, List (_, String), true -> Some [ "true" ]
  | Bool, List (_, String), false -> Some [ "false" ]
  | Path, String, v -> Some (Fpath.to_string v)
  | Path, List (_, String), v -> Some [ Fpath.to_string v ]
  | List (sep, String), String, v -> Some (String.concat sep v)
  | Int, String, v -> Some (string_of_int v)
  | String, Int, v -> begin try Some (int_of_string v) with _ -> None end
  | Int, Bool, 0 -> Some false
  | Int, Bool, _ -> Some true
  | String, Path, v -> begin try Some (Fpath.v v) with _ -> None end
  | _ -> None

let get : type a. ?native:bool option -> t -> key:string -> a value -> a option
    =
 fun ?(native = None) (_, cfg) ~key t ->
  match (normalize key, t) with
  | "version", String -> Some cfg.version
  | "standard_library_default", t -> cast Path cfg.standard_library_default t
  | "standard_library", t -> cast Path cfg.standard_library_default t
  | "ccomp_type", String -> Some cfg.ccomp_type
  | ("c_compiler" | "CC"), String -> Some cfg.c_compiler
  | "CFLAGS", List (_, String) ->
      if
        cfg.ocamlc_cflags = cfg.ocamlopt_cflags
        || Option.fold ~none:false ~some:Fun.(negate id) native
      then Some cfg.ocamlc_cflags
      else if Option.value ~default:false native then Some cfg.ocamlopt_cflags
      else None
  | "CPPFLAGS", List (_, String) ->
      if
        cfg.ocamlc_cppflags = cfg.ocamlopt_cppflags
        || Option.fold ~none:false ~some:Fun.(negate id) native
      then Some cfg.ocamlc_cflags
      else if Option.value ~default:false native then Some cfg.ocamlopt_cflags
      else None
  | "ocamlc_cflags", List (_, String) -> Some cfg.ocamlc_cflags
  | "ocamlc_cppflags", List (_, String) -> Some cfg.ocamlc_cppflags
  | "ocamlopt_cflags", List (_, String) -> Some cfg.ocamlopt_cflags
  | "ocamlopt_cppflags", List (_, String) -> Some cfg.ocamlopt_cppflags
  | "bytecode_c_compiler", List (_, String) -> Some cfg.bytecode_c_compiler
  | "native_c_compiler", List (_, String) -> Some cfg.native_c_compiler
  | "bytecomp_c_libraries", List (_, String) -> Some cfg.bytecomp_c_libraries
  | "native_c_libraries", List (_, String) -> Some cfg.native_c_libraries
  | "native_pack_linker", List (_, String) -> Some cfg.native_pack_linker
  | "native_compiler", t -> cast Bool cfg.native_compiler t
  | "architecture", String -> Some cfg.architecture
  | "model", String -> Some cfg.model
  | "int_size", t -> cast Int cfg.int_size t
  | "word_size", t -> cast Int cfg.word_size t
  | "system", String -> Some cfg.system
  | "asm", String -> Some cfg.asm
  | "asm_cfi_supported", t -> cast Bool cfg.asm_cfi_supported t
  | "with_frame_pointers", t -> cast Bool cfg.with_frame_pointers t
  | "ext_exe", String -> Some cfg.ext_exe
  | "ext_obj", String -> Some cfg.ext_obj
  | "ext_asm", String -> Some cfg.ext_asm
  | "ext_lib", String -> Some cfg.ext_lib
  | "ext_dll", String -> Some cfg.ext_dll
  | "os_type", String -> Some cfg.os_type
  | "default_executable_name", String -> Some cfg.default_executable_name
  | "systhread_supported", Bool -> Some cfg.systhread_supported
  | "host", String -> Some cfg.host
  | "target", String -> Some cfg.target
  | "flambda", t -> cast Bool cfg.flambda t
  | "safe_string", t -> cast Bool cfg.safe_string t
  | "default_safe_string", t -> cast Bool cfg.default_safe_string t
  | "flat_float_array", t -> cast Bool cfg.flat_float_array t
  | "function_sections", t -> cast Bool cfg.function_sections t
  | "afl_instrument", t -> cast Bool cfg.afl_instrument t
  | "windows_unicode", t -> cast Bool cfg.windows_unicode t
  | "supports_shared_libraries", t -> cast Bool cfg.supports_shared_libraries t
  | "native_dynlink", t -> cast Bool cfg.native_dynlink t
  | "naked_pointers", t -> cast Bool cfg.naked_pointers t
  | "compression_supported", t -> cast Bool cfg.compression_supported t
  | "exec_magic_number", String -> Some cfg.exec_magic_number
  | "cmi_magic_number", String -> Some cfg.cmi_magic_number
  | "cmo_magic_number", String -> Some cfg.cmo_magic_number
  | "cma_magic_number", String -> Some cfg.cma_magic_number
  | "cmx_magic_number", String -> Some cfg.cmx_magic_number
  | "cmxa_magic_number", String -> Some cfg.cmxa_magic_number
  | "ast_impl_magic_number", String -> Some cfg.ast_impl_magic_number
  | "ast_intf_magic_number", String -> Some cfg.ast_intf_magic_number
  | "cmxs_magic_number", String -> Some cfg.cmxs_magic_number
  | "cmt_magic_number", String -> Some cfg.cmt_magic_number
  | "linear_magic_number", String -> Some cfg.linear_magic_number
  | _ -> None

open Cmdliner

let compiler =
  let doc = "The compiler chosen (bytecode or native)." in
  let parser str =
    match String.lowercase_ascii str with
    | "bytecode" -> Ok `Bytecode
    | "native" -> Ok `Native
    | _ ->
        Rresult.R.error_msgf "Invalid compiler %S (must be bytecode or native)"
          str
  in
  let pp ppf = function
    | `Bytecode -> Fmt.string ppf "bytecode"
    | `Native -> Fmt.string ppf "native"
  in
  let compiler = Arg.conv (parser, pp) in
  let open Arg in
  value & opt compiler `Native & info [ "compiler" ] ~doc ~docv:"COMPILER"

let setup compiler =
  let compiler =
    match compiler with `Native -> "ocamlopt" | `Bytecode -> "ocamlc"
  in
  match from compiler () with
  | Ok (where, cfg) -> Some (where, cfg)
  | Error (`Msg msg) ->
      Log.warn (fun m ->
          m "Impossible to get the configuration of OCaml: %s" msg);
      None

let setup = Term.(const setup $ compiler)
let from (where, _) = where
