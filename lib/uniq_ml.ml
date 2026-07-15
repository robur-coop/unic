(* NOTE(dinosaure): that's the core of [uniq] and how we use [codept] to infer
   dependencies. On top of that, we should never interact with [codept] then. *)

let src = Logs.Src.create "uniq.ml"

module Log = (val Logs.src_log src : Logs.LOG)

module Param = struct
  let fault_handler =
    {
      Fault.policy= Standard_policies.quiet
    ; err_formatter= Format.err_formatter
    }

  let epsilon_dependencies = false
  let transparent_aliases = false
  let transparent_extension_nodes = false
end

module Engine = Dep_zipper.Make (Envt.Core) (Param)
module Solver = Solver.Make (Envt.Core) (Param) (Engine)

exception Invalid_source_file of string

let add_info { Comp_unit.ml; mli } (filename, p) =
  let k =
    match Support.extension filename with
    | "ml" -> { Read.format= Src; kind= M2l.Structure }
    | "mli" -> { Read.format= Src; kind= M2l.Signature }
    | "cmi" -> { Read.format= Cmi; kind= M2l.Signature }
    | _ -> raise (Invalid_source_file filename)
  in
  let x = (k, filename, p) in
  match k.kind with
  | M2l.Structure -> { Comp_unit.ml= x :: ml; mli }
  | M2l.Signature -> { Comp_unit.mli= x :: mli; ml }

type _ Effect.t +=
  | Read_file : Read.kind * string * Namespaced.t -> Comp_unit.s Effect.t

let pp_format ppf = function
  | Read.Src -> Fmt.string ppf "source"
  | Read.M2l -> Fmt.string ppf "m2l"
  | Read.Parsetree -> Fmt.string ppf "parsetree"
  | Read.Cmi -> Fmt.string ppf "cmi"

let pp_kind ppf = function
  | M2l.Structure -> Fmt.string ppf "structure"
  | M2l.Signature -> Fmt.string ppf "signature"

let read (k, filename, n) =
  let eff = Read_file (k, filename, n) in
  Log.debug (fun m ->
      m "read file %S (from %a, type %a)" filename pp_format k.format pp_kind
        k.kind);
  Effect.perform eff

let analyze ?version:(release = Sys.ocaml_release) ?(stdlib = true) pkgs files =
  let version = (release.Sys.major, release.Sys.minor) in
  let fn =
    let open Comp_unit.Group in
    Fun.compose fst (Fun.compose split group)
  in
  let units : _ Comp_unit.pair =
    files |> Comp_unit.unimap (List.map read) |> fn
  in
  let namespace = List.map (fun (u : Comp_unit.s) -> u.path) units.mli in
  let implicits =
    if stdlib then
      let stdlib = Bundle.versioned_stdlib version |> Module.Dict.of_list in
      [ ([ "Stdlib" ], stdlib) ]
    else []
  in
  let env =
    Envt.start ~open_approximation:true ~libs:pkgs ~namespace ~implicits
      Module.Dict.empty
  in
  Log.debug (fun m ->
      m "start to solve dependencies of @[<hov>%a@]"
        Fmt.(list ~sep:(any ";@ ") Namespaced.pp)
        namespace);
  Solver.solve env units

let run ?version ?stdlib lst =
  let to_namespaced name =
    (* NOTE(dinosaure): we don't want to handle namespaces even if [codept] is
       able to handle them. *)
    let basename = Filename.basename name in
    let nms = [ String.capitalize_ascii basename ] in
    (name, Namespaced.of_path nms)
  in
  let lst = List.map to_namespaced lst in
  let files =
    List.fold_left add_info { Comp_unit.ml= []; Comp_unit.mli= [] } lst
  in
  analyze ?version ?stdlib [] files

let run_into ?version ?stdlib ~current lst =
  let open Effect.Deep in
  let retc = Fun.id in
  let exnc = raise in
  let effc : type c. c Effect.t -> ((c, 'r) continuation -> 'r) option =
    function
    | Read_file (kind, path, n) ->
        let path = Fpath.v path in
        let path =
          if Fpath.is_abs path then path else Fpath.(current // path)
        in
        let path = Fpath.to_string path in
        begin try
          let v = Comp_unit.read_file Param.fault_handler kind path n in
          Some (fun k -> continue k v)
        with exn -> raise exn
        end
    | _ -> None
  in
  match_with (run ?version ?stdlib) lst { retc; exnc; effc }
