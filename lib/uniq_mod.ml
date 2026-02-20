let src = Logs.Src.create "uniq.mod"

module Log = (val Logs.src_log src : Logs.LOG)

type k = [ `All | `Intf | `Impl ]
type t = [ `All | `Sources | `Objects ]
type c = [ `All | `Bytecode | `Native ]

let is_ocaml_object filters =
  let exts =
    match filters with
    | `All, `All, `All ->
        [ ".ml"; ".mli"; ".cmo"; ".cmx"; ".cmi"; ".cma"; ".cmxa" ]
    | `Intf, `All, _ -> [ ".mli"; ".cmi" ]
    | `Intf, `Sources, _ -> [ ".mli" ]
    | `Intf, `Objects, _ -> [ ".cmi" ]
    | `Impl, `All, `All -> [ ".ml"; ".cmo"; ".cmx"; "cma"; ".cmxa" ]
    | `Impl, `Sources, _ -> [ ".ml" ]
    | `Impl, `Objects, `All -> [ ".cmo"; ".cmx"; ".cma"; ".cmxa" ]
    | `Impl, `Objects, `Native -> [ ".cmx"; ".cmxa" ]
    | `Impl, `Objects, `Bytecode -> [ ".cmo"; ".cma" ]
    | `Impl, `All, `Native -> [ ".ml"; ".cmx"; ".cmxa" ]
    | `Impl, `All, `Bytecode -> [ ".ml"; ".cmo"; ".cma" ]
    | `All, `All, `Native -> [ ".ml"; ".mli"; ".cmx"; ".cmi"; ".cmxa" ]
    | `All, `All, `Bytecode -> [ ".ml"; ".mli"; ".cmo"; ".cmi"; ".cma" ]
    | `All, `Sources, _ -> [ ".ml"; ".mli" ]
    | `All, `Objects, `Native -> [ ".cmx"; ".cmi"; ".cmxa" ]
    | `All, `Objects, `Bytecode -> [ ".cmo"; ".cmi"; ".cma" ]
    | `All, `Objects, `All -> [ ".cmo"; ".cmi"; ".cmx"; ".cma"; ".cmxa" ]
  in
  Fpath.mem_ext exts

let is_source_file = Fpath.mem_ext [ ".ml"; ".mli" ]
let is_source = function `All | `Sources -> true | `Objects -> false
let is_object = function `All | `Objects -> true | `Sources -> false
let is_intf = function `All | `Intf -> true | `Impl -> false
let is_impl = function `All | `Impl -> true | `Intf -> false
let is_native = function `All | `Native -> true | `Bytecode -> false
let is_bytecode = function `All | `Bytecode -> true | `Native -> false

let accept (filter_artifact, filter_object, filter_target) t =
  match t.Uniq_info.format with
  | Uniq_info.(Format (Mli, _)) ->
      is_intf filter_artifact && is_source filter_object
  | Uniq_info.(Format (Cmi, _)) ->
      is_intf filter_artifact && is_object filter_object
  | Uniq_info.(Format (Cmo, _)) | Uniq_info.(Format (Cma, _)) ->
      is_impl filter_artifact
      && is_object filter_object
      && is_bytecode filter_target
  | Uniq_info.(Format (Cmx, _)) | Uniq_info.(Format (Cmxa, _)) ->
      is_impl filter_artifact
      && is_object filter_object
      && is_native filter_target
  | Uniq_info.(Format (Ml, _)) ->
      is_impl filter_artifact && is_source filter_object

let export ~obj p digest =
  let exports = Uniq_info.exports obj in
  let fn (p', _) = Uniq_info.Path.compare p p' = 0 in
  match (List.find_opt fn exports, digest) with
  | Some (_, Some digest'), Some digest -> Uniq_digest.equal digest digest'
  | Some (_, Some _), None -> true
  | Some (_, None), Some _ -> false
  | Some (_, None), None -> true
  | None, _ -> false

let search ?(filters = (`All, `All, `All)) ~roots p digest =
  let elements path =
    if Sys.is_directory (Fpath.to_string path) then Ok false
    else if is_ocaml_object filters path then
      let unitname = Unitname.modulize (Fpath.to_string path) in
      let lst = Uniq_info.Path.to_list p in
      match (unitname, lst) with
      | unitname, [ modname ] when is_source_file path ->
          Ok (Modname.compare modname (Unitname.modname unitname) = 0)
      | _ -> Ok (is_source_file path = false)
      | exception _ -> Ok false
    else Ok false
  in
  let traverse _ = Ok true in
  let fold path acc =
    match Uniq_info.v path with
    | Ok obj when accept filters obj ->
        if export ~obj p digest then (path, obj) :: acc else acc
    | Ok _ -> acc
    | Error _ -> acc
  in
  let err _path _ = Ok () in
  Bos.OS.Path.fold ~err ~dotfiles:false ~elements:(`Sat elements)
    ~traverse:(`Sat traverse) fold [] roots
